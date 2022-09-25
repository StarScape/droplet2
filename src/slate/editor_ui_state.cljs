(ns slate.editor-ui-state
  (:require-macros [slate.interceptors :refer [definterceptor]])
  (:require-macros [garden.def :refer [defkeyframes]])
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [drop.utils :as utils]
            [slate.model.common :as m]
            [slate.model.find-and-replace :as f+r]
            [slate.model.history :as history]
            [slate.model.editor-state :as es :refer [EditorState map->EditorState map->EditorUpdate]]
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
            [slate.utils :as slate-utils]))

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
                                           ::location-before
                                           ::found-locations
                                           ::current-location]))

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
(def ^:const font-size-delta "Amount to increase/decrease font size by each time." 5)

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
  (prn-str history))

(defn deserialize
  "Parses the EDN of the saved editor file and returns the data structure (atm, the :history map)."
  [edn-str]
  (edn/read-string {:readers slate-types-readers} edn-str))

(defn active-formats [ui-state]
  (let [{:keys [selection doc] :as state} (history/current-state (:history ui-state))
        selected (m/selected-content state)
        paragraph-type (if (or (sel/single? selection)
                               (sel/single-paragraph? selection))
                         (:type (get (:children doc) (sel/caret-para selection)))
                         (when (apply = (map :type selected))
                           (:type (first selected))))
        formats (:formats selection)]
    (if (some? paragraph-type)
      (conj formats paragraph-type)
      formats)))

(defn update-viewmodels-to-history-tip
  "Updates the :viewmodels attribute of `ui-state` to match the tip of the ui state's
   :history object. See the history namespace for more info."
  [ui-state]
  (let [{:keys [viewmodels history measure-fn]} ui-state
        {:keys [editor-state changelist]} (:tip history)
        new-viewmodels (vm/update-viewmodels viewmodels (:doc editor-state) (view/elem-width ui-state) measure-fn changelist)]
    (assoc ui-state :viewmodels new-viewmodels)))

(defn update-history
  [history editor-update interceptor]
  ;; if completion:
  ;;   Take state before interceptor fired, add that to the backstack immediately.
  ;;   Then set the tip to the result of the completion interceptor.
  ;; if normal:
  ;;   Just update tip, integrate into backstack after debounced timeout
  (if (and (:add-to-history-immediately? interceptor)
           (:include-in-history? interceptor))
    (-> history
        (history/add-tip-to-backstack)
        (history/set-tip editor-update))
    (history/set-tip history (es/merge-updates (:tip history) editor-update))))

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
  [shadow-root dom-elem hidden-input editor-state prev-state viewmodels changelist
   & {:keys [focus?] :or {focus? true}}]
  (let [{:keys [doc selection]} editor-state
        {:keys [deleted-uuids changed-uuids inserted-uuids]} changelist]
    (doseq [uuid inserted-uuids]
      #_(js/console.log (str "Inserting " uuid))
      (view/insert-para! dom-elem uuid (get viewmodels uuid) editor-state))
    (doseq [uuid deleted-uuids]
      #_(js/console.log (str "Removing " uuid))
      (view/remove-para! dom-elem uuid editor-state prev-state))
    (doseq [uuid changed-uuids]
      #_(js/console.log (str "Updating " uuid))
      (view/update-para! dom-elem uuid (get viewmodels uuid) editor-state prev-state))

    (view/relocate-hidden-input! shadow-root hidden-input focus?)))

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
  (let [deserialized-history (deserialize file-contents-str)]
    (load-history! *ui-state deserialized-history)))

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

#_(defn highlight!
  [*ui-state locations]
  (let [{:keys [shadow-root
                dom-elem
                measure-fn
                hidden-input
                viewmodels
                history]
         :as ui-state} @*ui-state
        editor-state (history/current-state history)
        changed-uuids (->> locations (map sel/caret-para) (set))
        new-children (reduce (fn [new-children location]
                               (update new-children (sel/caret-para location) m/apply-format location :highlight))
                             (-> editor-state :doc :children) locations)
        new-state (assoc-in editor-state [:doc :children] new-children)
        changelist (es/changelist :changed-uuids changed-uuids)
        new-viewmodels (vm/update-viewmodels viewmodels (:doc new-state) (view/elem-width ui-state) measure-fn changelist)]
    (sync-dom! shadow-root dom-elem hidden-input new-state editor-state new-viewmodels changelist :focus? false)))

#_(defn unhighlight!
  [*ui-state locations]
  (let [{:keys [shadow-root
                dom-elem
                measure-fn
                hidden-input
                viewmodels
                history]
         :as ui-state} @*ui-state
        editor-state (history/current-state history)
        changed-uuids (->> locations (map sel/caret-para) (set))
        new-children (reduce (fn [new-children location]
                               (update new-children (sel/caret-para location) m/remove-format location :highlight))
                             (-> editor-state :doc :children) locations)
        new-state (assoc-in editor-state [:doc :children] new-children)
        changelist (es/changelist :changed-uuids changed-uuids)
        new-viewmodels (vm/update-viewmodels viewmodels (:doc new-state) (view/elem-width ui-state) measure-fn changelist)]
    (sync-dom! shadow-root dom-elem hidden-input new-state editor-state new-viewmodels changelist :focus? false)))

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
        (sync-dom! (:shadow-root ui-state)
                   (:dom-elem new-ui-state)
                   (:hidden-input ui-state)
                   (:editor-state restored-update)
                   (history/current-state history)
                   (:viewmodels new-ui-state)
                   changelist)
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
        (sync-dom! (:shadow-root new-ui-state)
                   (:dom-elem new-ui-state)
                   (:hidden-input new-ui-state)
                   (:editor-state restored-update)
                   (history/current-state history)
                   (:viewmodels new-ui-state)
                   changelist)
        (reset! *ui-state new-ui-state)))))

(definterceptor new-file!
  {:manual? true}
  [*ui-state _]
  (let [{:keys [shadow-root measure-fn] :as ui-state} @*ui-state
        editor-state (es/editor-state)
        available-width (.-width (.getBoundingClientRect (.-host shadow-root)))]
    (swap! *ui-state merge {:viewmodels (vm/from-doc (:doc editor-state) available-width measure-fn)
                            :history (history/init editor-state)
                            :add-tip-to-backstack-timer-id nil
                            :input-history []})
    (full-dom-render! *ui-state)
    ((:on-new ui-state) @*ui-state)))

(defn fire-update!
  "Update the UI state and UI in response to an EditorUpdate.
   If no event is supplied, nothing will be added to the input-history.
   Arg opts: {:include-in-history? :add-to-history-immediately?}."
  ([*ui-state editor-update event {:keys [include-in-history? :add-to-history-immediately? focus?]
                                   :or {focus? true}, :as opts}]
   (let [ui-state @*ui-state ; only deref once a cycle
         editor-state (history/current-state (:history ui-state))
         old-doc (:doc editor-state)
         new-doc (-> editor-update :editor-state :doc)
         new-ui-state (-> ui-state
                          (update :history update-history editor-update opts)
                          (update :word-count word-count/update-count old-doc new-doc (:changelist editor-update))
                          (update :input-history #(if event (interceptors/add-to-input-history % opts event) %))
                          (update-viewmodels-to-history-tip))]
     (sync-dom! (:shadow-root new-ui-state)
                (:dom-elem new-ui-state)
                (:hidden-input new-ui-state)
                (:editor-state editor-update)
                editor-state
                (:viewmodels new-ui-state)
                (:changelist editor-update)
                :focus? focus?)
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
        opts (select-keys interceptor [:add-to-history-immediately? :include-in-history? :input-name])]
    (fire-update! *ui-state editor-update event opts)))

(defn fire-interceptor!
  [*ui-state interceptor event]
  (if (:manual? interceptor)
    ;; Manual interceptors circumvent Slate's default data-loop and just fire as regular functions
    (interceptor *ui-state event)
    (fire-normal-interceptor! *ui-state interceptor event)))

(defn inc-current-location
  [{:keys [found-locations current-location] :as find-and-replace-state}]
  (if (< current-location (dec (count found-locations)))
    (update find-and-replace-state :current-location inc)
    (assoc find-and-replace-state :current-location 0)))

(defn dec-current-location
  [{:keys [found-locations current-location] :as find-and-replace-state}]
  (if (zero? current-location)
    (assoc find-and-replace-state :current-location (dec (count found-locations)))
    (update find-and-replace-state :current-location dec)))

(comment
  (inc-current-location {:found-locations [1 2 3] :current-location 0})
  (inc-current-location {:found-locations [1 2 3] :current-location 1})
  (inc-current-location {:found-locations [1 2 3] :current-location 2})

  (dec-current-location {:found-locations [1 2 3] :current-location 0})
  (dec-current-location {:found-locations [1 2 3] :current-location 1})
  (dec-current-location {:found-locations [1 2 3] :current-location 2}))

(defn get-current-location
  "If find is active, returns current found location, as a selection."
  [{:keys [active? current-location found-locations] :as _find-and-replace-info}]
  (when active?
    (nth found-locations current-location)))

(defn goto-location!
  "Sets the selection to the location provided and centers it in the viewport.
   location should be a Selection."
  [*ui-state location & {:keys [focus?] :or {focus? true}}]
  (let [editor-update (es/set-selection (history/current-state (:history @*ui-state)) location)]
    (fire-update! *ui-state editor-update {:include-in-history? false
                                           :focus? focus?})))

(defn goto-current-found!
  "Equivalent to calling goto-location! with the current found location."
  [*ui-state]
  (let [{:keys [find-and-replace]} @*ui-state]
    (when-not (empty? (:found-locations find-and-replace))
      (goto-location! *ui-state (get-current-location find-and-replace) :focus? false))))

(defn goto-location-before!
  [*ui-state]
  (let [{:keys [find-and-replace]} @*ui-state]
    (goto-location! *ui-state (:location-before find-and-replace) :focus? false)))

(defn next-occurence! [*ui-state]
  (let [{{:keys [found-locations]} :find-and-replace} @*ui-state]
    (when (seq found-locations)
      (swap! *ui-state update :find-and-replace inc-current-location)
      (goto-current-found! *ui-state))))

(defn prev-occurence! [*ui-state]
  (let [{{:keys [found-locations]} :find-and-replace} @*ui-state]
    (when (seq found-locations)
      (swap! *ui-state update :find-and-replace dec-current-location)
      (goto-current-found! *ui-state))))

(defn replace-current! [*ui-state replacement-text]
  (let [{:keys [find-and-replace history]} @*ui-state
        current-location (get-current-location find-and-replace)
        editor-update (f+r/replace (history/current-state history) current-location replacement-text)]
    (fire-update! *ui-state editor-update {:add-to-history-immediately? true
                                           :focus? false})))

(defn replace-all! [*ui-state replacement-text]
  (let [{:keys [find-and-replace history]} @*ui-state
        editor-update (f+r/replace-all (history/current-state history)
                                       (:found-locations find-and-replace)
                                       replacement-text)]
    (fire-update! *ui-state editor-update {:add-to-history-immediately? true
                                           :focus? false})))

(defn cancel-find! [*ui-state]
  (let [{{:keys [active? location-before]} :find-and-replace} @*ui-state]
    (when active?
      ;; (unhighlight! *ui-state found-locations)
      (goto-location! *ui-state location-before)
      (swap! *ui-state update :find-and-replace merge {:active? false
                                                       :text ""
                                                       :found-locations []
                                                       :current-location 0}))))

(definterceptor activate-find!
  {:manual? true}
  [*ui-state _]
  (let [{:keys [history find-and-replace on-focus-find]} @*ui-state
        current-selection (:selection (history/current-state history))]
    (on-focus-find)
    (when-not (:active? find-and-replace)
      (swap! *ui-state update :find-and-replace merge {:active? true
                                                       :location-before current-selection}))))

(defn find!
  "Starts find operation, if text search term is not blank."
  ([*ui-state text force-restart?]
   (if (str/blank? text)
     (goto-location-before! *ui-state)
     (let [{history :history
            {:keys [location-before ignore-case?] :as f+r-state} :find-and-replace} @*ui-state
           editor-state (history/current-state history)]
       (if (and (= text (:text f+r-state))
                (not force-restart?))
         (if (= (:selection editor-state) (get-current-location f+r-state))
           (next-occurence! *ui-state)
           (goto-current-found! *ui-state))
         (let [occurences (f+r/find editor-state text ignore-case?)
              ;; No previous find, set current selection as place to return to when find deactivated
               new-location-before (if (empty? (:found-locations f+r-state))
                                     (:selection editor-state)
                                     location-before)
               new-fields {:active? true
                           :text text
                           :current-location 0
                           :found-locations occurences
                           :location-before new-location-before}]
           (swap! *ui-state update :find-and-replace merge new-fields)
           (goto-current-found! *ui-state))))))
  ([*ui-state text] (find! *ui-state text false)))

(defn toggle-ignore-case!
  [*ui-state]
  (swap! *ui-state update-in [:find-and-replace :ignore-case?] not)
  (let [{{:keys [text active?]} :find-and-replace} @*ui-state]
    (when active?
      (find! *ui-state text true))))

(defn init-event-handlers!
  "Registers event listeners for the editor surface with their default interceptors."
  [*ui-state]
  (swap! *ui-state assoc :add-tip-to-backstack-callback #(add-tip-to-backstack-after-wait! *ui-state))
  (let [#_#_get-interceptor (partial interceptors/find-interceptor (:interceptors @*ui-state))
        get-interceptor (fn [pattern]
                          (let [ui-state @*ui-state
                                current-editor-state (history/current-state (:history ui-state))
                                matching-interceptor (interceptors/find-interceptor (:interceptors ui-state) pattern)]
                            (when (and matching-interceptor
                                       ((:should-fire? matching-interceptor) current-editor-state))
                              matching-interceptor)))
        get-completion (fn [key-pressed]
                         (let [{:keys [interceptors history input-history]} @*ui-state
                               current-editor-state (history/current-state history)
                               matching-interceptor (interceptors/find-completion key-pressed interceptors input-history)]
                           (when (and matching-interceptor
                                      ((:should-fire? matching-interceptor) current-editor-state))
                             matching-interceptor)))
        {editor-elem :dom-elem
         outer-dom-elem :outer-dom-elem
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
                           (fire-interceptor! *ui-state (get-interceptor :click) e)))

      (.addEventListener outer-dom-elem "mousedown"
                         (fn [e]
                           (.preventDefault e)
                           (.focus hidden-input #js {:preventScroll true})
                           (fire-interceptor! *ui-state (get-interceptor :click) e)))

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
       (.preventDefault e)
       (handle-focus-out! *ui-state)))

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
     :cmd+f activate-find!}
    {:ctrl+z undo!
     :ctrl+shift+z redo!
     :ctrl+= increase-font-size!
     :ctrl+- decrease-font-size!
     :ctrl+n new-file!
     :ctrl+f activate-find!}))

(defn- init-shadow-dom!
  "Initializes the shadow dom within the top level container element where the Slate instance lives,
   and creates and returns the [editor element within shadow dom, shadowRoot]"
  [slate-top-level-elem]
  (set! (.-innerHTML slate-top-level-elem) "")
  (let [shadow-dom-wrapper (js/document.createElement "div")
        shadow-dom-wrapper-style (.-style shadow-dom-wrapper)]
    (set! (.-className shadow-dom-wrapper) "slate-shadow-dom-wrapper")
    (set! (.-maxWidth shadow-dom-wrapper-style) "800px")
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

(defn init!
  "Initializes the editor surface, and returns an atom containing the EditorUIState. This
   atom will continue to be updated throughout the lifetime of the editor. Takes a series
   of keyword arguments:

   Required:

   :dom-elem - The DOM element that the editor will be displayed in

   Optional:

   :editor-state - The initial EditorState to load into the editor. Will default to an empty document.
   OR
   :history - The restored, deserialized history object.

   :*atom IAtom into which the editor state will be intialized. If one is not provided, an atom will be initialized and returned."
  [& {:keys [*atom editor-state history dom-elem on-new on-save on-save-as on-open on-focus-find]
      :or {*atom (atom nil), on-new #(), on-save #(), on-save-as #(), on-open #(), on-focus-find #()}}]
  ;; Load global styles if not already loaded
  (style/install-global-styles!)
  ;; If fonts are not loaded prior to initialization, measurements will be wrong and layout chaos will ensue
  (.. js/document -fonts (load "16px Merriweather")
      ;; TODO: use core-async or something to clean this up?
      (then #(let [uuid (random-uuid)
                   ;; Slate operates inside a shadow DOM to prevent global styles from interfering
                   [editor-elem, shadow-root] (init-shadow-dom! dom-elem)
                   available-width (.-width (.getBoundingClientRect (.-host shadow-root)))
                   measure-fn (ruler-for-elem editor-elem shadow-root)
                   editor-state (or editor-state (es/editor-state))
                   history (or history (history/init editor-state))
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
