(ns slate.editor-ui-state
  (:require-macros [slate.interceptors :refer [definterceptor]]
                   [slate.utils :refer [slurp-file]]
                   [dev.performance-utils :refer [inside-time-measurement!]])
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [drop.utils :as utils]
            [slate.model.common :as m]
            [slate.model.find-and-replace :as f+r]
            [slate.model.history :as history]
            [slate.model.editor-state :as es :refer [map->EditorState map->EditorUpdate]]
            [slate.model.doc :refer [map->Document]]
            [slate.model.paragraph :refer [map->Paragraph]]
            [slate.model.run :refer [map->Run]]
            [slate.model.selection :as sel :refer [map->Selection]]
            [slate.dll :refer [dll]]
            [slate.interceptors :as interceptors]
            [slate.default-interceptors :refer [default-interceptors]]
            [slate.measurement :refer [ruler-for-elem]]
            [slate.view :as view]
            [slate.viewmodel :as vm]
            [slate.style :as style]
            [slate.word-count :as word-count]
            [slate.utils :as slate-utils]
            [dev.performance-utils :as perf-utils]))

(s/def ::id uuid?)
(s/def ::history ::history/editor-state-history)
(s/def ::dom-elem #(instance? js/HTMLElement %))
(s/def ::hidden-input ::dom-elem)
(s/def ::measure-fn (s/fspec :args (s/cat :text string?
                                          :formats (s/coll-of keyword? :kind set?))
                             :ret int?))
(s/def ::input-history (s/coll-of any?))
(s/def ::viewmodels (s/coll-of any?))
(s/def ::interceptors ::interceptors/interceptor-map)
(s/def ::find-and-replace (s/keys :req-un [::active?
                                           ::ignore-case?
                                           ::location-before
                                           ::find-text
                                           ::occurrences
                                           ::current-occurrence-idx]))

(s/def ::editor-ui-state (s/keys :req-un [::id
                                          ::history
                                          ::dom-elem
                                          ::hidden-input
                                          ::measure-fn
                                          ::input-history
                                          ::viewmodels
                                          ::interceptors
                                          ::find-and-replace]))

(def ^:const history-timeout-ms 1000)

(def ^:const font-size-min 10)
(def ^:const font-size-max 70)
(def ^:const font-size-delta "Amount to increase/decrease font size by each time." 1)

(def slate-types-readers
  {'slate.model.selection.Selection map->Selection
   'slate.model.run.Run map->Run
   'slate.model.paragraph.Paragraph map->Paragraph
   'slate.model.doc.Document map->Document
   'slate.model.editor-state.EditorState map->EditorState
   'slate.model.editor-state.EditorUpdate map->EditorUpdate
   'DoublyLinkedList #(apply dll %)})

(defn serialize
  "Serializes the history object to EDN."
  [{:keys [history] :as _ui-state}]
  (prn-str {:version 1, :history history}))

(defn deserialize
  "Parses the EDN of the saved editor file and returns the data structure."
  [edn-str]
  (edn/read-string {:readers slate-types-readers} edn-str))

(defn active-formats [ui-state]
  (let [{:keys [selection doc] :as state} (history/current-state (:history ui-state))
        selected (m/selected-content state)
        selected-items (m/items selected)
        paragraph-type (if (or (sel/single? selection)
                               (sel/single-paragraph? selection))
                         (:type (get (:children doc) (sel/caret-para selection)))
                         (when (apply = (map :type selected-items))
                           (:type (first selected-items))))
        formats (:formats selection)]
    (if (some? paragraph-type)
      (conj formats paragraph-type)
      formats)))

(defn update-viewmodels
  "Updates the :viewmodels attribute of `ui-state` to match the tip of the ui state's
   :history object. See the history namespace for more info."
  [ui-state {:keys [editor-state changelist]}]
  (let [{:keys [viewmodels measure-fn]} ui-state
        new-viewmodels (vm/update-viewmodels viewmodels (:doc editor-state) (view/elem-width ui-state) measure-fn changelist)]
    (assoc ui-state :viewmodels new-viewmodels)))

(defn modifies-doc?
  "Returns true if the `EditorUpdate` makes changes to the Document."
  [{{:keys [inserted-uuids changed-uuids deleted-uuids]} :changelist :as _editor-update}]
  (or (seq inserted-uuids)
      (seq changed-uuids)
      (seq deleted-uuids)))

(def does-not-modify-doc?
  "Returns true if the `EditorUpdate` does not make changes to the Document."
  (complement modifies-doc?))

(defn should-integrate-tip-first?
  "Returns true if the history's tip should be integrated into the backstack before
   `new-editor-update` is added to the history."
  [history new-editor-update]
  (and (modifies-doc? new-editor-update)
       (does-not-modify-doc? (history/current history))))

(defn update-history
  [history editor-update interceptor]
  ;; if completion:
  ;;   Take state before interceptor fired, add that to the backstack immediately.
  ;;   Then set the tip to the result of the completion interceptor.
  ;; else if normal:
  ;;   Just update tip, will be integrated into backstack after debounced timeout
  (if-not (:include-in-history? interceptor)
    (history/set-tip history editor-update)
    (if (:add-to-history-immediately? interceptor)
      (-> history
          (history/add-tip-to-backstack)
          (history/set-tip editor-update))
      (as-> history $
        (if (should-integrate-tip-first? $ editor-update) (history/add-tip-to-backstack $) $)
        (history/set-tip $ editor-update)))))

(defn add-tip-to-backstack!
  [*ui-state]
  {:pre [(satisfies? IAtom *ui-state)]}
  ;; "Add tip to backstack."
  (swap! *ui-state update :history history/add-tip-to-backstack))

(defn add-tip-to-backstack-after-wait!
  [*ui-state]
  (js/clearTimeout (:add-tip-to-backstack-timer-id @*ui-state))
  (swap! *ui-state assoc :add-tip-to-backstack-timer-id (js/setTimeout #(add-tip-to-backstack! *ui-state)
                                                                       history-timeout-ms)))

(defn cancel-add-tip-to-backstack!
  [*ui-state]
  (js/clearTimeout (:add-tip-to-backstack-timer-id @*ui-state)))

(defn full-dom-render!
  "Updates all viewmodels and renders the entire document to the DOM. This is only called in special circumstances
   (such as on application startup), as normally the interceptor system will handle rendering changed/inserted/deleted
   paragraphs selectively."
  [*ui-state]
  (let [{:keys [dom-elem history measure-fn shadow-root hidden-input] :as ui-state} @*ui-state
        {:keys [doc] :as editor-state} (history/current-state history)
        dom-elem-width (.-width (.getBoundingClientRect dom-elem))
        viewmodels (vm/from-doc doc dom-elem-width measure-fn)
        new-ui-state (assoc ui-state :viewmodels viewmodels)
        viewmodels (map #(get viewmodels (:uuid %)) (:children doc))]
    (view/insert-all! dom-elem viewmodels editor-state)
    (view/relocate-hidden-input! shadow-root hidden-input)
    (reset! *ui-state new-ui-state)))

(defn sync-dom!
  "Sync editor DOM element to provided changelist, updating
  all paragraphs that have been inserted/changed/removed."
  [& {:keys [shadow-root dom-elem hidden-input editor-state prev-state viewmodels changelist focus? scroll-to-caret?]
      :or {focus? true, scroll-to-caret? false}}]
  (let [{:keys [doc selection]} editor-state
        {:keys [deleted-uuids changed-uuids inserted-uuids]} changelist
        rerender-uuids (set/difference (set/union (sel/all-uuids (:selection prev-state))
                                                  (sel/all-uuids selection))
                                       deleted-uuids
                                       inserted-uuids)]
    (doseq [uuid inserted-uuids]
      (view/insert-para! dom-elem uuid (get viewmodels uuid) editor-state))
    (doseq [uuid deleted-uuids]
      (view/remove-para! dom-elem uuid editor-state prev-state))
    (doseq [uuid (set/union changed-uuids rerender-uuids)]
      (view/update-para! dom-elem uuid (get viewmodels uuid) editor-state prev-state))

    (view/relocate-hidden-input! shadow-root hidden-input focus?)
    (when scroll-to-caret? (view/scroll-to-caret! shadow-root))))

(defn load-history!
  "Loads a serialized .drop file into the editor, discarding current document and history."
  [*ui-state history]
  (let [word-count (word-count/full-count (:doc (history/current-state history)))]
    (cancel-add-tip-to-backstack! *ui-state)
    (swap! *ui-state merge {:history history
                            :word-count word-count
                            :input-history []})
    (full-dom-render! *ui-state)))

(defn load-file!
  "Loads a serialized .drop file into the editor, discarding current document and history."
  [*ui-state file-contents-str]
  (let [deserialized (deserialize file-contents-str)]
    ;; TODO: throw user-visible error if the version of file is not compatible with this droplet version
    (load-history! *ui-state (:history deserialized))))

(defn load-document!
  "Loads a Document object into the editor with a fresh history, discarding current document and history."
  [*ui-state document]
  (load-history! *ui-state (history/init (es/editor-state document))))

(defn handle-resize!
  "Called when the window is resized, handles re-rendering the full doc."
  [*ui-state]
  (full-dom-render! *ui-state)
  ;; Add :resize to input-history if not already present
  (swap! *ui-state (fn [ui-state]
                     (if (= :resize (peek (:input-history ui-state)))
                       ui-state
                       (update ui-state :input-history interceptors/add-to-input-history :resize)))))

(defn handle-focus-in!
  [*ui-state]
  (style/gain-focus! (:shadow-root @*ui-state)))

(defn handle-focus-out!
  [*ui-state]
  (style/lose-focus! (:shadow-root @*ui-state)))

(defn set-font-size!
  [*ui-state new-size]
  (let [{:keys [dom-elem shadow-root]} @*ui-state]
    (set! (.. dom-elem -style -fontSize) (str new-size "px"))
    (swap! *ui-state assoc :measure-fn (ruler-for-elem dom-elem shadow-root))
    (full-dom-render! *ui-state)))

(definterceptor increase-font-size!
  {:manual? true}
  [*ui-state]
  (let [new-font-size (+ font-size-delta (view/font-size (:dom-elem @*ui-state)))]
    (when (<= new-font-size font-size-max)
      (set-font-size! *ui-state new-font-size))))

(definterceptor decrease-font-size!
  {:manual? true}
  [*ui-state]
  (let [new-font-size (- (view/font-size (:dom-elem @*ui-state)) font-size-delta)]
    (when (>= new-font-size font-size-min)
      (set-font-size! *ui-state new-font-size))))

(definterceptor undo!
  {:manual? true}
  [*ui-state _]
  (cancel-add-tip-to-backstack! *ui-state)
  (let [{:keys [viewmodels history input-history word-count measure-fn] :as ui-state} @*ui-state
        current-update (history/current history)]
    (when (history/has-undo? history)
      (let [new-input-history (interceptors/add-to-input-history input-history :undo)
            new-history (history/undo history)
            restored-update (history/current new-history)
            restored-state (:editor-state restored-update)
            changelist (es/reverse-changelist (:changelist current-update))
            new-vms (vm/update-viewmodels viewmodels (:doc restored-state) (view/elem-width ui-state) measure-fn changelist)
            new-word-count (word-count/update-count word-count (-> current-update :editor-state :doc) (:doc restored-state) changelist)
            new-ui-state (assoc ui-state
                                :input-history new-input-history
                                :history new-history
                                :viewmodels new-vms
                                :word-count new-word-count)]
        (sync-dom! :shadow-root (:shadow-root ui-state)
                   :dom-elem (:dom-elem new-ui-state)
                   :hidden-input (:hidden-input ui-state)
                   :editor-state (:editor-state restored-update)
                   :prev-state (history/current-state history)
                   :viewmodels (:viewmodels new-ui-state)
                   :changelist changelist)
        (reset! *ui-state new-ui-state)))))

(definterceptor redo!
  {:manual? true}
  [*ui-state _]
  (cancel-add-tip-to-backstack! *ui-state)
  (let [{:keys [viewmodels history input-history word-count measure-fn] :as ui-state} @*ui-state]
    (when (history/has-redo? history)
      (let [new-input-history (interceptors/add-to-input-history input-history :redo)
            new-history (history/redo history)
            restored-update (history/current new-history)
            restored-state (:editor-state restored-update)
            changelist (:changelist restored-update)
            new-vms (vm/update-viewmodels viewmodels (:doc restored-state) (view/elem-width ui-state) measure-fn changelist)
            new-word-count (word-count/update-count word-count (:doc (history/current-state history)) (:doc restored-state) changelist)
            new-ui-state (assoc ui-state
                                :input-history new-input-history
                                :history new-history
                                :viewmodels new-vms
                                :word-count new-word-count)]
        (sync-dom! :shadow-root (:shadow-root new-ui-state)
                   :dom-elem (:dom-elem new-ui-state)
                   :hidden-input (:hidden-input new-ui-state)
                   :editor-state (:editor-state restored-update)
                   :prev-state (history/current-state history)
                   :viewmodels (:viewmodels new-ui-state)
                   :changelist changelist)
        (reset! *ui-state new-ui-state)))))

(defn fire-update!
  "Update the UI state and UI in response to an EditorUpdate.
   If no event is supplied, nothing will be added to the input-history.
   Arg opts: {:include-in-history? :add-to-history-immediately?}."
  ([*ui-state editor-update event {:keys [include-in-history? :add-to-history-immediately? focus? scroll-to-caret?]
                                   :or {focus? true}, :as opts}]
   (let [ui-state @*ui-state ; only deref once a cycle
         editor-state (history/current-state (:history ui-state))
         old-doc (:doc editor-state)
         new-doc (-> editor-update :editor-state :doc)
         new-ui-state (-> ui-state
                          (update :history update-history editor-update opts)
                          (update :word-count word-count/update-count old-doc new-doc (:changelist editor-update))
                          (update :input-history #(if event (interceptors/add-to-input-history % opts event) %))
                          (update-viewmodels editor-update))]
     (sync-dom! :shadow-root (:shadow-root new-ui-state)
                :dom-elem (:dom-elem new-ui-state)
                :hidden-input (:hidden-input new-ui-state)
                :editor-state (:editor-state editor-update)
                :prev-state editor-state
                :viewmodels (:viewmodels new-ui-state)
                :changelist (:changelist editor-update)
                :focus? focus?
                :scroll-to-caret? scroll-to-caret?)
     (reset! *ui-state new-ui-state)

     (cond
       ;; Adding to history has already be handled, cancel the debounced func if one is waiting to fire
       (and add-to-history-immediately? include-in-history?)
       (cancel-add-tip-to-backstack! *ui-state)

       ;; Integrate the history tip into the backstack after a period of inactivity
       include-in-history?
       (add-tip-to-backstack-after-wait! *ui-state))))
  ([*ui-state editor-update opts]
   (fire-update! *ui-state editor-update nil opts)))

(defn fire-normal-interceptor!
  "This the core of Slate's main data loop.
   Any time an event happens which finds a matching interceptor, fire-interceptor!
   is called, which handles updating the state stored in the UIState atom and re-rendering
   elements in the DOM.

   Arguments:
   - `*ui-state`: Atom containing EditorUIState
   - `interceptor` Interceptor to fire
   - `event`: Raw JS event object"
  [*ui-state interceptor event]
  (let [ui-state @*ui-state ; only deref once a cycle
        editor-state (history/current-state (:history ui-state))
        editor-update (interceptor editor-state ui-state event)
        opts (select-keys interceptor [:scroll-to-caret? :add-to-history-immediately? :include-in-history? :input-name])]
    (fire-update! *ui-state editor-update event opts)))

(defn fire-interceptor!
  [*ui-state interceptor event]
  (if (:manual? interceptor)
    ;; Manual interceptors circumvent Slate's default data-loop and just fire as regular functions
    (interceptor *ui-state event)
    (fire-normal-interceptor! *ui-state interceptor event)))

(defn goto-location!
  "Sets the selection to the location provided and centers it in the viewport.
   location should be a Selection."
  [*ui-state location & {:keys [focus?] :or {focus? true}}]
  (let [editor-update (es/set-selection (history/current-state (:history @*ui-state)) location)]
    (fire-update! *ui-state editor-update {:include-in-history? false
                                           :focus? focus?})
    (view/scroll-to-caret! (:shadow-root @*ui-state))))

(defn goto-current-occurrence!
  "Equivalent to calling goto-location! with the current found location.
   Does nothing if there are no found occurrences of the search text."
  [*ui-state]
  (let [{:keys [find-and-replace]} @*ui-state]
    (when-not (empty? (:occurrences find-and-replace))
      (goto-location! *ui-state (f+r/current-occurrence find-and-replace) :focus? false))))

(defn goto-location-before!
  "Scrolls to the location that the cursor was at before the find operation was started."
  ([*ui-state & {:keys [focus?] :or {focus? false}}]
   (let [{:keys [find-and-replace]} @*ui-state]
     (goto-location! *ui-state (:location-before find-and-replace) :focus? focus?))))

(defn next-occurrence!
  "Handles going to the next-occurrence in the find."
  [*ui-state]
  (swap! *ui-state update :find-and-replace f+r/next-occurrence)
  (goto-current-occurrence! *ui-state))

(defn prev-occurrence!
  "Handles going to the previous occurerence in the find."
  [*ui-state]
  (swap! *ui-state update :find-and-replace f+r/prev-occurrence)
  (goto-current-occurrence! *ui-state))

(defn replace-current! [*ui-state replacement-text]
  (let [{:keys [history]} @*ui-state
        editor-update (f+r/replace-current-selection (history/current-state history) replacement-text)]
    (fire-update! *ui-state editor-update {:add-to-history-immediately? true
                                           :focus? false})))

(defn replace-all! [*ui-state replacement-text]
  (let [{:keys [find-and-replace history]} @*ui-state
        editor-update (f+r/replace-all-occurrences find-and-replace
                                                   (history/current-state history)
                                                   replacement-text)]
    (fire-update! *ui-state editor-update {:add-to-history-immediately? true
                                           :focus? false})))

(defn set-find-text!
  "Sets the find text in the find-and-replace state map.
   This __does not__ actually find the occurrence, go to them, etc.
   Those operations are expected to be done on a debounce via find!"
  [*ui-state text]
  (swap! *ui-state assoc-in [:find-and-replace :find-text] text))

(defn find!
  "Starts find operation, if text search term is not blank."
  ([*ui-state]
   (let [{history :history, {:keys [find-text]} :find-and-replace} @*ui-state
         editor-state (history/current-state history)]
     (if (str/blank? find-text)
       (goto-location-before! *ui-state :focus? false)
       (do
         (swap! *ui-state update :find-and-replace f+r/find-occurrences editor-state)
         (goto-current-occurrence! *ui-state))))))

(defn toggle-ignore-case!
  [*ui-state]
  (swap! *ui-state update-in [:find-and-replace :ignore-case?] not)
  (find! *ui-state))

(defn cancel-find! [*ui-state]
  (swap! *ui-state update :find-and-replace f+r/cancel-find)
  (when-not (:active? @*ui-state)
    (goto-location-before! *ui-state :focus? true)))

(defn new-document!
  "Resets the editor surface to a new document."
  [*ui-state]
  (let [{:keys [shadow-root measure-fn]} @*ui-state
        editor-state (es/editor-state)
        available-width (.-width (.getBoundingClientRect (.-host shadow-root)))]
    (swap! *ui-state merge {:viewmodels (vm/from-doc (:doc editor-state) available-width measure-fn)
                            :history (history/init editor-state)
                            :add-tip-to-backstack-timer-id nil
                            :word-count 0
                            :input-history []})
    (full-dom-render! *ui-state)))

(defn focus!
  "Focuses the hidden-input, if it is not already focused."
  [{:keys [hidden-input] :as _ui-state}]
  (.focus hidden-input #js {:preventScroll true}))

(definterceptor activate-find!
  {:manual? true}
  [*ui-state _]
  (let [{:keys [history on-focus-find]} @*ui-state]
    (on-focus-find)
    (swap! *ui-state update :find-and-replace f+r/activate-find (history/current-state history))
    ;; Goto current occurrence if restarting find dialog with previous search
    (goto-current-occurrence! *ui-state)))

(definterceptor new-file!
  {:manual? true}
  [*ui-state _]
  ((:on-new @*ui-state)))

(definterceptor save!
  {:manual? true}
  [*ui-state _]
  (let [ui-state @*ui-state]
    ((:on-save ui-state) (serialize ui-state))))

(definterceptor save-as!
  {:manual? true}
  [*ui-state _]
  (let [ui-state @*ui-state]
    ((:on-save-as ui-state) (serialize ui-state))))

(defn find-interceptor
  "Returns the interceptor inside the ui-state's interceptor map
    that matches the interceptor pattern or event, if one exists."
  [ui-state pattern-or-event]
  (let [current-editor-state (history/current-state (:history ui-state))
        matching-interceptor (interceptors/find-interceptor (:interceptors ui-state) pattern-or-event)]
    (when (and matching-interceptor
               ;; TODO: should this instead by integrated into the custom implementation of (-invoke) for interceptors?
               ((:should-fire? matching-interceptor) current-editor-state))
      matching-interceptor)))

(defn init-event-handlers!
  "Registers event listeners for the editor surface with their default interceptors."
  [*ui-state]
  (swap! *ui-state assoc :add-tip-to-backstack-callback #(add-tip-to-backstack-after-wait! *ui-state))
  (let [#_#_get-interceptor (partial interceptors/find-interceptor (:interceptors @*ui-state))
        get-interceptor (fn [pattern-or-event] (find-interceptor @*ui-state pattern-or-event))
        get-completion (fn [key-pressed]
                         (let [{:keys [interceptors history input-history]} @*ui-state
                               current-editor-state (history/current-state history)
                               matching-interceptor (interceptors/find-completion key-pressed interceptors input-history)]
                           (when (and matching-interceptor
                                      ;; TODO: should this instead by integrated into the custom implementation of (-invoke) for interceptors?
                                      ((:should-fire? matching-interceptor) current-editor-state))
                             matching-interceptor)))
        {editor-elem :dom-elem
         ; outer-dom-elem :outer-dom-elem
         hidden-input :hidden-input} @*ui-state
        *editor-surface-clicked? (atom false :validator boolean?)
        bind-hidden-input-event! (fn [event-name handler]
                                   (.addEventListener hidden-input event-name
                                                      (fn [e]
                                                        (when-not @*editor-surface-clicked?
                                                          (handler e)))))]
    (let [*mousedown-event (atom nil :validator #(instance? js/MouseEvent %))]
      (.addEventListener editor-elem "mousedown"
                         (fn [e]
                           (.preventDefault e)
                           (.focus hidden-input #js {:preventScroll true})
                           (reset! *editor-surface-clicked? true)
                           (reset! *mousedown-event e)
                           (let [click-interceptor (case (.-detail e)
                                                     3 (get-interceptor :triple-click)
                                                     2 (get-interceptor :double-click)
                                                     (get-interceptor :click))]
                             (fire-interceptor! *ui-state click-interceptor e))))

      (.addEventListener js/window "mousemove"
                         (utils/throttle 50
                                         (fn [e]
                                           (when (and @*editor-surface-clicked?
                                      ;; Make sure it's actually still clicked down, if the user moved the mouse
                                      ;; off-window and back the 'mouseup' event will not have set the atom back to false.
                                                      (= 1 (.-which e)))
                             ;; *last-mousedown-event* is passed this way for optimization purposes
                                             (binding [view/*last-mousedown-event* @*mousedown-event]
                                               (fire-interceptor! *ui-state (get-interceptor :drag) e))))))

      (.addEventListener js/window "mouseup"
                         (fn [_e]
                           (reset! *editor-surface-clicked? false))))

    (bind-hidden-input-event! "keydown"
      (fn [e]
        (when-let [interceptor-fn (get-interceptor e)]
          (.preventDefault e)
          (fire-interceptor! *ui-state interceptor-fn e))))

    (bind-hidden-input-event! "beforeinput"
      (fn [e]
        (.preventDefault e)
        (case (.-inputType e)
          "insertText"
          (let [;; If the data is a single key and matches a completion, fire that instead of the insert interceptor
                completion-interceptor (when (= 1 (.. e -data -length))
                                          (get-completion (.-data e)))
                interceptor (or completion-interceptor (get-interceptor :insert))]
            (fire-interceptor! *ui-state interceptor e))

          "deleteContentBackward"
          (let [{:keys [input-history]} @*ui-state]
            (if (= :completion (peek input-history))
              (fire-interceptor! *ui-state undo! e)
              (fire-interceptor! *ui-state (get-interceptor :delete) e)))

          nil)))

    (bind-hidden-input-event! "cut"
      (fn [e]
        (.preventDefault e)
        (fire-interceptor! *ui-state (get-interceptor :cut) e)))

    (bind-hidden-input-event! "copy"
      (fn [e]
        (.preventDefault e)
        (fire-interceptor! *ui-state (get-interceptor :copy) e)))

    (bind-hidden-input-event! "paste"
      (fn [e]
        (.preventDefault e)
        (fire-interceptor! *ui-state (get-interceptor :paste) e)))

    (bind-hidden-input-event! "focusout"
      (fn [e]
        (let [{:keys [hidden-input shadow-root]} @*ui-state]
          (when (not= hidden-input (.-activeElement shadow-root))
            (.preventDefault e)
            (handle-focus-out! *ui-state)))))

    (bind-hidden-input-event! "focusin"
      (fn [e]
        (.preventDefault e)
        (handle-focus-in! *ui-state)))

    (.addEventListener js/window "resize" #(handle-resize! *ui-state))))

(def manual-interceptors
  "Some manual interceptors that are defined here rather than default_interceptors."
  (if slate-utils/is-mac?
    {:cmd+z undo!
     :cmd+shift+z redo!
     :cmd+= increase-font-size!
     :cmd+- decrease-font-size!
     :cmd+n new-file!
     :cmd+f activate-find!
     :cmd+s save!
     :cmd+shift+s save-as!}
    {:ctrl+z undo!
     :ctrl+shift+z redo!
     :ctrl+= increase-font-size!
     :ctrl+- decrease-font-size!
     :ctrl+n new-file!
     :ctrl+s save!
     :ctrl+shift+s save-as!
     :ctrl+f activate-find!}))

(defn- init-shadow-dom!
  "Initializes the shadow dom within the top level container element where the Slate instance lives,
   and creates and returns the [editor element within shadow dom, shadowRoot]"
  [slate-top-level-elem]
  (set! (.-innerHTML slate-top-level-elem) "")
  (let [shadow-dom-wrapper (js/document.createElement "div")
        shadow-dom-wrapper-style (.-style shadow-dom-wrapper)]
    (set! (.-className shadow-dom-wrapper) "slate-shadow-dom-wrapper")
    (set! (.-maxWidth shadow-dom-wrapper-style) "700px")
    (set! (.-minWidth shadow-dom-wrapper-style) "300px")
    (set! (.-margin shadow-dom-wrapper-style) "0 auto")

    (.appendChild slate-top-level-elem shadow-dom-wrapper)

    (.attachShadow shadow-dom-wrapper #js {:mode "open"})
    (set! (.. shadow-dom-wrapper -shadowRoot -innerHTML)
          (str "<style>" style/shadow-elem-css-rendered "</style>"))

    ;; There are some things you cannot do (like set outerHTML on elements, among other
    ;; general weirdness) if an element is the immediate child of a <html> or ShadowRoot,
    ;; so a top-level wrapper element is desirable over inserting straight into the shadow DOM.
    (let [editor-elem (doto (js/document.createElement "div")
                        (.. -classList (add "slate-editor")))]
      (.. shadow-dom-wrapper -shadowRoot (appendChild editor-elem))
      [editor-elem, (.-shadowRoot shadow-dom-wrapper)])))

(defn load-fonts!
  "Returns a Promise that resolves when the necessary fonts for rendering the document are loaded."
  []
  (js/Promise.all #js [(js/document.fonts.load "16px Merriweather")
                       (js/document.fonts.load "italic 16px Merriweather")
                       (js/document.fonts.load "bold 16px Merriweather")
                       (js/document.fonts.load "bold italic 16px Merriweather")]))

(defn init!
  "Initializes the editor surface, and returns an atom containing the EditorUIState. This
   atom will continue to be updated throughout the lifetime of the editor. Takes a series
   of keyword arguments:

   Required:

   :dom-elem - The DOM element that the editor will be displayed in

   Optional:

   :editor-state - The initial EditorState to load into the editor. Will default to an empty document.
   OR
   :save-file-contents - The restored, deserialized history object.

   :*atom IAtom into which the editor state will be intialized. If one is not provided, an atom will be initialized and returned."
  [& {:keys [*atom editor-state save-file-contents dom-elem on-new on-save on-save-as on-open on-focus-find]
      :or {*atom (atom nil), on-new #(), on-save #(), on-save-as #(), on-open #(), on-focus-find #()}}]
  ;; Load global styles if not already loaded
  (style/install-global-styles!)
  ;; If fonts are not loaded prior to initialization, measurements will be wrong and layout chaos will ensue
  (.. (load-fonts!)
      ;; TODO: use core-async or something to clean this up?
      (then #(let [uuid (random-uuid)
                   ;; Slate operates inside a shadow DOM to prevent global styles from interfering
                   [editor-elem, shadow-root] (init-shadow-dom! dom-elem)
                   available-width (.-width (.getBoundingClientRect (.-host shadow-root)))
                   measure-fn (ruler-for-elem editor-elem shadow-root)
                   editor-state (or editor-state (es/editor-state))
                   history (if save-file-contents
                             (:history (deserialize save-file-contents))
                             (history/init editor-state))
                   interceptors-map (-> (interceptors/interceptor-map)
                                        (interceptors/reg-interceptors default-interceptors)
                                        (interceptors/reg-interceptors manual-interceptors))
                   hidden-input (view/create-hidden-input! shadow-root)
                   current-doc (:doc (history/current-state history))]
               ;; Focus hidden input without scrolling to it (it will be at the bottom)
               (.focus hidden-input #js {:preventScroll true})
               (reset! *atom {:id uuid
                              :viewmodels (vm/from-doc current-doc available-width measure-fn)
                              :history history
                              :word-count (word-count/full-count current-doc)
                              :input-history []
                              :find-and-replace (f+r/init)
                              :interceptors interceptors-map
                              :hidden-input hidden-input
                              :add-tip-to-backstack-timer-id nil
                              :outer-dom-elem dom-elem
                              :dom-elem editor-elem
                              :shadow-root shadow-root
                              :measure-fn measure-fn
                              :on-new on-new
                              :on-save on-save
                              :on-save-as on-save-as
                              :on-load on-open
                              :on-focus-find on-focus-find})
               (init-event-handlers! *atom)
               (full-dom-render! *atom))))
  *atom)
