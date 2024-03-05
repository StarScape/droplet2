(ns slate.editor-ui-state
  (:require-macros [slate.interceptors :refer [definterceptor]])
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [drop.utils :as utils]
            [slate.model.dll :as dll]
            [slate.model.common :as m]
            [slate.model.find-and-replace :as f+r]
            [slate.model.history :as history]
            [slate.model.editor-state :as es]
            [slate.model.doc]
            [slate.model.paragraph]
            [slate.model.run]
            [slate.model.selection :as sel]
            [slate.interceptors :as interceptors]
            [slate.default-interceptors :refer [default-interceptors]]
            [slate.measurement :refer [ruler-for-elem]]
            [slate.renderer.core :as renderer]
            [slate.serialization :refer [serialize deserialize]]
            [slate.view :as view]
            [slate.viewmodel :as vm]
            [slate.style :as style]
            [slate.word-count :as word-count]
            [slate.utils :as slate-utils]
            [slate.model.run :as r]))

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

(defn active-formats [ui-state]
  (let [{:keys [selection doc] :as state} (history/current-state (:history ui-state))
        selected (m/selected-content state)
        paragraph-type (if (sel/single-paragraph? selection)
                         (:type (get (:children doc) (sel/caret-para selection)))
                         (when (apply = (map :type (:children selected)))
                           (:type (first (:children selected)))))
        formats (:formats selection)]
    (if (some? paragraph-type)
      (conj formats paragraph-type)
      formats)))

(defn update-viewmodels
  "Updates the :viewmodels attribute of `ui-state` to match the tip of the ui state's
   :history object. See the history namespace for more info."
  [ui-state editor-state changelist]
  (let [{:keys [viewmodels measure-fn]} ui-state
        new-viewmodels (vm/update-viewmodels viewmodels (:doc editor-state) (view/elem-width ui-state) measure-fn changelist)]
    (assoc ui-state :viewmodels new-viewmodels)))

(defn modifies-doc?
  "Returns true if the changelist makes changes to the Document."
  [{:keys [inserted-indices changed-indices deleted-indices] :as _changelist}]
  (or (seq inserted-indices)
      (seq changed-indices)
      (seq deleted-indices)))

(defn should-integrate-tip-first?
  "Returns true if the history's tip should be integrated into the backstack before the next state is added to the history."
  [history changelist]
  (and (modifies-doc? changelist)
       (not (modifies-doc? (:changelist (history/current history))))))

(defn update-history
  [history new-editor-state changelist interceptor]
  ;; if completion:
  ;;   Take state before interceptor fired, add that to the backstack immediately.
  ;;   Then set the tip to the result of the completion interceptor.
  ;; else if normal:
  ;;   Just update tip, will be integrated into backstack after debounced timeout
  (if-not (:include-in-history? interceptor)
    (history/set-tip history new-editor-state changelist)
    (if (:add-to-history-immediately? interceptor)
      (-> history
          (history/add-tip-to-backstack)
          (history/set-tip new-editor-state changelist))
      (as-> history $
        (if (should-integrate-tip-first? $ changelist)
          (history/add-tip-to-backstack $)
          $)
        (history/set-tip $ new-editor-state changelist)))))

(defn add-tip-to-backstack!
  [*ui-state]
  {:pre [(satisfies? IAtom *ui-state)]}
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
  #_(let [{:keys [dom-elem history measure-fn shadow-root hidden-input] :as ui-state} @*ui-state
        {:keys [doc] :as editor-state} (history/current-state history)
        dom-elem-width (.-width (.getBoundingClientRect dom-elem))
        viewmodels (vm/from-doc doc dom-elem-width measure-fn)
        new-ui-state (assoc ui-state :viewmodels viewmodels)]
    (view/insert-all! dom-elem viewmodels editor-state)
    (view/relocate-hidden-input! shadow-root hidden-input)
    (reset! *ui-state new-ui-state)))

(defn sync-dom!
  "Sync editor DOM element to provided changelist, updating
  all paragraphs that have been inserted/changed/removed."
  [& {:keys [shadow-root dom-elem hidden-input editor-state prev-state viewmodels changelist focus? scroll-to-caret?]
      :or {focus? true, scroll-to-caret? false}}]
  (let [{:keys [deleted-indices changed-indices inserted-indices]} changelist
        rerender-indices (set/difference (set/union (es/all-selected-indices prev-state)
                                                    (es/all-selected-indices editor-state))
                                         deleted-indices
                                         inserted-indices)]
    (doseq [idx deleted-indices]
      (view/remove-para! dom-elem idx editor-state prev-state))
    (view/insert-all! dom-elem (sort inserted-indices) viewmodels editor-state)
    (doseq [idx (set/union changed-indices rerender-indices)]
      (view/update-para! dom-elem idx (get viewmodels idx) editor-state prev-state))

    (view/relocate-hidden-input! shadow-root hidden-input focus?)
    (when scroll-to-caret? (view/scroll-to-caret! shadow-root))))

(defn load-editor-state!
  "Loads an editor-state into the editor with a blank history, discarding current document and history."
  [*ui-state editor-state]
  (cancel-add-tip-to-backstack! *ui-state)
  (swap! *ui-state merge {:history (history/init editor-state)
                          :word-count (word-count/init editor-state)
                          :input-history []})
  (full-dom-render! *ui-state)
  ((:on-ready @*ui-state)))

(defn load-file!
  "Loads a serialized .drop file into the editor, discarding current document and history."
  [*ui-state file-contents-str]
  (let [deserialized (deserialize file-contents-str)]
    (if-not (contains? deserialized :error-message)
      (load-editor-state! *ui-state (:editor-state deserialized))
      (do
        (when-let [e (:exception deserialized)] (js/console.log e))
        ((:on-load-file-error @*ui-state) (:error-message deserialized))))))

(defn load-document!
  "Loads a Document object into the editor with a fresh history, discarding current document and history."
  [*ui-state document]
  (load-editor-state! *ui-state (history/init (es/editor-state document))))

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
  [*ui-state e]
  (when ((:should-lose-focus? @*ui-state) e)
    (style/lose-focus! (:shadow-root @*ui-state))))

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
  (let [{:keys [viewmodels history input-history word-count measure-fn] :as ui-state} @*ui-state]
    (when (history/has-undo? history)
      (let [{current-editor-state :editor-state, last-changelist :changelist} (history/current history)
            new-input-history (interceptors/add-to-input-history input-history :undo)
            new-history (history/undo history)
            restored-state (history/current-state new-history)
            undo-changelist (dll/reverse-changelist last-changelist)
            new-vms (vm/update-viewmodels viewmodels (:doc restored-state) (view/elem-width ui-state) measure-fn undo-changelist)
            new-word-count (word-count/update word-count current-editor-state restored-state undo-changelist)
            new-ui-state (assoc ui-state
                                :input-history new-input-history
                                :history new-history
                                :viewmodels new-vms
                                :word-count new-word-count)]
        (sync-dom! :shadow-root (:shadow-root ui-state)
                   :dom-elem (:dom-elem new-ui-state)
                   :hidden-input (:hidden-input ui-state)
                   :editor-state restored-state
                   :prev-state current-editor-state
                   :viewmodels (:viewmodels new-ui-state)
                   :changelist undo-changelist)
        (reset! *ui-state new-ui-state)))))

(definterceptor redo!
  {:manual? true}
  [*ui-state _]
  (cancel-add-tip-to-backstack! *ui-state)
  (let [{:keys [viewmodels history input-history word-count measure-fn] :as ui-state} @*ui-state]
    (when (history/has-redo? history)
      (let [{current-editor-state :editor-state} (history/current history)
            new-input-history (interceptors/add-to-input-history input-history :redo)
            new-history (history/redo history)
            {restored-state :editor-state, redo-changelist :changelist} (history/current new-history)
            new-vms (vm/update-viewmodels viewmodels (:doc restored-state) (view/elem-width ui-state) measure-fn redo-changelist)
            new-word-count (word-count/update word-count current-editor-state restored-state redo-changelist)
            new-ui-state (assoc ui-state
                                :input-history new-input-history
                                :history new-history
                                :viewmodels new-vms
                                :word-count new-word-count)]
        (sync-dom! :shadow-root (:shadow-root new-ui-state)
                   :dom-elem (:dom-elem new-ui-state)
                   :hidden-input (:hidden-input new-ui-state)
                   :editor-state restored-state
                   :prev-state current-editor-state
                   :viewmodels (:viewmodels new-ui-state)
                   :changelist redo-changelist)
        (reset! *ui-state new-ui-state)))))

(defn- fire-doc-and-selection-changed-callbacks!
  "Checks if doc and selection callbacks need to be fired based on
   and old and new ui-state, and fires one or both of them if they do."
  [old-ui-state new-ui-state on-doc-changed on-selection-changed]
  (let [old-state (history/current-state (:history old-ui-state))
        new-state (history/current-state (:history new-ui-state))]
    ;; Fire doc changed listener if new and old docs are not the same object (apart from changelists)
    (when-not (dll/entries-identical? (:children (:doc old-state)) (:children (:doc new-state)))
      (on-doc-changed))
    ;; Fire selection changed listener if new and old sels are not equal
    ;; (Selection is a much smaller object, so a full equality check isn't as expensive.)
    (when-not (= (:selection old-state) (:selection new-state))
      (on-selection-changed (:selection new-state)))))

(defn fire-update!
  "Update the UI state and UI in response to a new EditorState.
   If no event is supplied, nothing will be added to the input-history.
   Arg opts: {:include-in-history? :add-to-history-immediately?}."
  ([*ui-state new-editor-state event {:keys [include-in-history? add-to-history-immediately? focus? scroll-to-caret?]
                                      :or {focus? true}, :as opts}]
   (let [ui-state @*ui-state ; only deref once a cycle
         changelist (es/get-changelist new-editor-state)
         new-editor-state (es/clear-changelist new-editor-state)
         current-editor-state (history/current-state (:history ui-state))
         new-ui-state (-> ui-state
                          (update :history update-history new-editor-state changelist opts)
                          (update :word-count word-count/update current-editor-state new-editor-state changelist)
                          (update :input-history #(if event (interceptors/add-to-input-history % opts event) %))
                          (update-viewmodels new-editor-state changelist))]
     (sync-dom! :shadow-root (:shadow-root new-ui-state)
                :dom-elem (:dom-elem new-ui-state)
                :hidden-input (:hidden-input new-ui-state)
                :editor-state new-editor-state
                :prev-state current-editor-state
                :viewmodels (:viewmodels new-ui-state)
                :changelist changelist
                :focus? focus?
                :scroll-to-caret? scroll-to-caret?)
     (reset! *ui-state new-ui-state)

     (cond
       ;; Adding to history has already be handled, cancel the debounced func if one is waiting to fire
       (and add-to-history-immediately? include-in-history?)
       (cancel-add-tip-to-backstack! *ui-state)

       ;; Integrate the history tip into the backstack after a period of inactivity
       include-in-history?
       (add-tip-to-backstack-after-wait! *ui-state))

     ;; Fire doc and/or selection changed callbacks IF they have changed
     (fire-doc-and-selection-changed-callbacks! ui-state @*ui-state (:on-doc-changed ui-state) (:on-selection-changed ui-state))))
  ([*ui-state new-editor-state opts]
   (fire-update! *ui-state new-editor-state nil opts)))

(defn fire-normal-interceptor!
  "This is the core of Slate's main data loop.
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
        new-editor-state (interceptor editor-state ui-state event)
        opts (select-keys interceptor [:scroll-to-caret? :add-to-history-immediately? :include-in-history? :input-name])]
    (fire-update! *ui-state new-editor-state event opts)))

(defn fire-manual-interceptor!
  [*ui-state interceptor event]
  (let [{:keys [on-doc-changed on-selection-changed] :as initial-ui-state} @*ui-state]
    (interceptor *ui-state event)
    ;; Fire doc and/or selection changed callbacks IF they have changed
    ;; There is also a check for this in (fire-update!), but it needs to be handled separately for manual interceptors
    (fire-doc-and-selection-changed-callbacks! initial-ui-state @*ui-state on-doc-changed on-selection-changed)))

(defn fire-interceptor!
  "Main function for firing an interceptor."
  [*ui-state interceptor event]
  (if (:manual? interceptor)
    ;; Manual interceptors circumvent Slate's default data-loop and just fire as regular functions
    (fire-manual-interceptor! *ui-state interceptor event)
    (fire-normal-interceptor! *ui-state interceptor event)))

(defn goto-location!
  "Sets the selection to the location provided and centers it in the viewport.
   location should be a Selection."
  [*ui-state location & {:keys [focus?] :or {focus? true}}]
  (let [new-editor-state (es/set-selection (history/current-state (:history @*ui-state)) location)]
    (fire-update! *ui-state new-editor-state {:include-in-history? false
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
        new-editor-state (f+r/replace-current-selection (history/current-state history) replacement-text)]
    (fire-update! *ui-state new-editor-state {:add-to-history-immediately? true
                                              :focus? false})))

(defn replace-all! [*ui-state replacement-text]
  (let [{:keys [find-and-replace history]} @*ui-state
        new-editor-state (f+r/replace-all-occurrences find-and-replace
                                                      (history/current-state history)
                                                      replacement-text)]
    (fire-update! *ui-state new-editor-state {:add-to-history-immediately? true
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
                            :word-count (word-count/init)
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
  ((:on-new @*ui-state) *ui-state))

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
                                                      ;; Make sure it's actually still clicked down,
                                                      ;; if the user moved the mouse off-window and
                                                      ;; back the 'mouseup' event will not have set
                                                      ;; the atom back to false.
                                                      (= 1 (.-which e)))
                                             ;; *last-mousedown-event* is passed this way because interceptors don't take extra parameters
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
                                    (handle-focus-out! *ui-state e)))))

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

(def wrapper-elem-class "slate-shadow-dom-wrapper")
(def style-elem-id "slate-style")

(defn toggle-theme!
  "Switch from light to dark mode or vice versa."
  [*slate-instance]
  (let [{:keys [shadow-root font-family dark-mode?]} @*slate-instance]
    (set! (.. shadow-root (querySelector (str "#" style-elem-id)) -innerHTML)
          (style/get-rendered-shadow-elem-css font-family (not dark-mode?)))
    (swap! *slate-instance update :dark-mode? not)))

(defn- init-shadow-dom!
  "Initializes the shadow dom within the top level container element where the Slate instance lives,
   and creates and returns the [editor element within shadow dom, shadowRoot]"
  [slate-top-level-elem font-family dark-mode?]
  (set! (.-innerHTML slate-top-level-elem) "")
  (let [shadow-dom-wrapper (js/document.createElement "div")
        shadow-dom-wrapper-style (.-style shadow-dom-wrapper)]
    (set! (.-className shadow-dom-wrapper) wrapper-elem-class)
    (set! (.-maxWidth shadow-dom-wrapper-style) "650px")
    (set! (.-minWidth shadow-dom-wrapper-style) "300px")
    (set! (.-height shadow-dom-wrapper-style) "100%")
    (set! (.-margin shadow-dom-wrapper-style) "0 auto")

    (.appendChild slate-top-level-elem shadow-dom-wrapper)

    (.attachShadow shadow-dom-wrapper #js {:mode "open"})
    (set! (.. shadow-dom-wrapper -shadowRoot -innerHTML)
          (str "<style id='" style-elem-id "'>" (style/get-rendered-shadow-elem-css font-family dark-mode?) "</style>"))

    ;; There are some things you cannot do (like set outerHTML on elements, among other
    ;; general weirdness) if an element is the immediate child of a <html> or ShadowRoot,
    ;; so a top-level wrapper element is desirable over inserting straight into the shadow DOM.
    #_(let [editor-elem (doto (js/document.createElement "div")
                          (.. -classList (add "slate-editor")))]
        (.. shadow-dom-wrapper -shadowRoot (appendChild editor-elem))
        [editor-elem, (.-shadowRoot shadow-dom-wrapper)])
    (let [canvas-elem (js/document.createElement "canvas")]
      (.. shadow-dom-wrapper -shadowRoot (appendChild canvas-elem))
      [canvas-elem, (.-shadowRoot shadow-dom-wrapper)])))

(defn load-fonts!
  "Returns a Promise that resolves when the necessary fonts for rendering the document are loaded."
  [font-family-name]
  (js/Promise.all #js [(js/document.fonts.load (str "16px " font-family-name))
                       (js/document.fonts.load (str "italic 16px " font-family-name))
                       (js/document.fonts.load (str "bold 16px " font-family-name))
                       (js/document.fonts.load (str "bold italic 16px " font-family-name))]))

(defn- nop [])

(def sample-doc (slate.model.doc/document (dll/dll (slate.model.paragraph/paragraph [(r/run "Hello, world!")]))))

(defn init!
  "Initializes the editor surface, and returns an atom containing the EditorUIState. This
   atom will continue to be updated throughout the lifetime of the editor. Takes a series
   of keyword arguments:

   Required:

   :dom-elem - The DOM element that the editor will be displayed in

   Optional:

   :editor-state - The initial EditorState to load into the editor. Will default to an empty document.
   OR
   :save-file-contents - The restored, deserialized history object (deprecated).

   :*atom IAtom into which the editor state will be intialized. If one is not provided, an atom will be initialized and returned."
  [& {:keys [*atom
             font-family
             theme
             ;; save-file-contents
             dom-elem
             on-ready
             on-new
             on-save
             on-save-as
             on-open
             on-load-file-error
             on-focus-find
             on-doc-changed
             on-selection-changed
             should-lose-focus?]
      :or {*atom (atom nil)
           on-ready nop
           on-new nop
           on-save nop
           on-save-as nop
           on-open nop
           on-load-file-error nop
           on-focus-find nop
           on-doc-changed nop
           on-selection-changed nop
           should-lose-focus? (constantly true)}}]
  ;; Load global styles if not already loaded
  (style/install-global-styles!)
  ;; If fonts are not loaded prior to initialization, measurements will be wrong and layout chaos will ensue
  (.. (load-fonts! font-family)
      (then #(let [uuid (random-uuid)
                   dark-mode? (= theme :dark)
                   ;; Slate operates inside a shadow DOM to prevent global styles from interfering
                   [canvas-elem, shadow-root] (init-shadow-dom! dom-elem font-family dark-mode?)
                   #_#_available-width (.-width (.getBoundingClientRect (.-host shadow-root)))
                   #_#_measure-fn (ruler-for-elem editor-elem shadow-root)
                   editor-state (es/editor-state)
                   history (history/init editor-state)
                   interceptors-map (-> (interceptors/interceptor-map)
                                        (interceptors/reg-interceptors default-interceptors)
                                        (interceptors/reg-interceptors manual-interceptors))
                   hidden-input (view/create-hidden-input! shadow-root)
                   #_#_current-state (history/current-state history)
                   #_#_current-doc (:doc current-state)]
               ;; Focus hidden input without scrolling to it (it will be at the bottom)
               (.focus hidden-input #js {:preventScroll true})
               (reset! *atom {:id uuid
                              #_#_:viewmodels (vm/from-doc current-doc available-width measure-fn)
                              :dark-mode? dark-mode?
                              #_#_:viewmodels (vm/from-doc current-doc available-width measure-fn)
                              :history history
                              :word-count (word-count/init editor-state)
                              :input-history []
                              :find-and-replace (f+r/init)
                              :interceptors interceptors-map
                              :hidden-input hidden-input
                              :add-tip-to-backstack-timer-id nil
                              :outer-dom-elem dom-elem
                              ;; :dom-elem editor-elem
                              :font-family font-family
                              :shadow-root shadow-root
                              ;; :measure-fn measure-fn
                              :on-ready on-ready
                              :on-new on-new
                              :on-save on-save
                              :on-save-as on-save-as
                              :on-load on-open
                              :on-load-file-error on-load-file-error
                              :on-focus-find on-focus-find
                              :on-doc-changed on-doc-changed
                              :on-selection-changed on-selection-changed
                              :should-lose-focus? should-lose-focus?
                              :ready? false})
               (renderer/init! canvas-elem sample-doc font-family 16 25)
               #_(init-event-handlers! *atom)
               #_(full-dom-render! *atom)
               (swap! *atom assoc :ready? true)
               (on-ready))))
  *atom)

