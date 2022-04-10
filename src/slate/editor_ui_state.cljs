(ns slate.editor-ui-state
  (:require-macros [slate.interceptors :refer [definterceptor]])
  (:require [clojure.spec.alpha :as s]
            [slate.model.editor-state :as es :refer [>>=]]
            [slate.model.history :as history]
            [slate.interceptors :as interceptors]
            [slate.default-interceptors :refer [default-interceptors]]
            [slate.measurement :refer [ruler-for-elem]]
            [slate.view :as view]
            [slate.viewmodel :as vm]
            [slate.utils :as utils]))

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

(s/def ::editor-ui-state (s/keys :req-un [::id
                                          ::history
                                          ::dom-elem
                                          ::hidden-input
                                          ::measure-fn
                                          ::input-history
                                          ::viewmodels
                                          ::interceptors]))

(def ^:const history-timeout-ms 1000)

(def ^:const font-size-min 10)
(def ^:const font-size-max 70)
(def ^:const font-size-delta "Amount to increase/decrease font size by each time." 5)

(defn elem-width
  "Returns the width of the UIState's dom element, in pixels."
  [ui-state]
  (.-width (.getBoundingClientRect (:dom-elem ui-state))))

(defn update-viewmodels-to-history-tip
  "Updates the :viewmodels attribute of `ui-state` to match the tip of the ui state's
   :history object. See the history namespace for more info."
  [ui-state]
  (let [{:keys [viewmodels history measure-fn]} ui-state
        {:keys [editor-state changelist]} (:tip history)
        new-viewmodels (vm/update-viewmodels viewmodels (:doc editor-state) (elem-width ui-state) measure-fn changelist)]
    (assoc ui-state :viewmodels new-viewmodels)))

#_(defn update-all-viewmodels-to-current-state
  [ui-state]
  (let [{:keys [dom-elem history measure-fn] :as ui-state} ui-state
        editor-state (history/current-state history)
        dom-elem-width (.-width (.getBoundingClientRect dom-elem))
        new-viewmodels (vm/from-doc (:doc editor-state) dom-elem-width measure-fn)]
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
  {:pre [(instance? Atom *ui-state)]}
  ;; #p "Add tip to backstack."
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
  (let [{:keys [dom-elem history measure-fn] :as ui-state} @*ui-state
        {:keys [doc] :as editor-state} (history/current-state history)
        dom-elem-width (.-width (.getBoundingClientRect dom-elem))
        viewmodels (vm/from-doc doc dom-elem-width measure-fn)
        new-ui-state (assoc ui-state :viewmodels viewmodels)
        viewmodels (map #(get viewmodels (:uuid %)) (:children doc))]
    (view/insert-all! dom-elem viewmodels editor-state)
    (reset! *ui-state new-ui-state)))

(defn sync-dom!
  "Sync editor DOM element to provided changelist, updating
  all paragraphs that have been inserted/changed/removed."
  [dom-elem editor-state prev-state viewmodels changelist]
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
      (view/update-para! dom-elem uuid (get viewmodels uuid) editor-state prev-state))))

(defn handle-resize!
  "Called when the window is resized, handles re-rendering the full doc."
  [*ui-state]
  (full-dom-render! *ui-state))

(defn set-font-size!
  [*ui-state new-size]
  (let [{:keys [id dom-elem]} @*ui-state]
    (set! (.. dom-elem -style -fontSize) (str new-size "px"))
    (swap! *ui-state assoc :measure-fn (ruler-for-elem id dom-elem))
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
  (let [{:keys [viewmodels history input-history measure-fn] :as ui-state} @*ui-state
        current-update (history/current history)]
    (when (history/has-undo? history)
      (let [new-input-history (interceptors/add-to-input-history input-history :undo)
            new-history (history/undo history)
            restored-update (history/current new-history)
            restored-state (:editor-state restored-update)
            changelist (es/reverse-changelist (:changelist current-update))
            new-vms (vm/update-viewmodels viewmodels (:doc restored-state) (elem-width ui-state) measure-fn changelist)
            new-ui-state (assoc ui-state
                                :input-history new-input-history
                                :history new-history
                                :viewmodels new-vms)]
        (sync-dom! (:dom-elem new-ui-state)
                   (:editor-state restored-update)
                   (history/current-state history)
                   (:viewmodels new-ui-state)
                   changelist)
        (reset! *ui-state new-ui-state)))))

(definterceptor redo!
  {:manual? true}
  [*ui-state _]
  (cancel-add-tip-to-backstack! *ui-state)
  (let [{:keys [viewmodels history input-history measure-fn] :as ui-state} @*ui-state]
    (when (history/has-redo? history)
      (let [new-input-history (interceptors/add-to-input-history input-history :redo)
            new-history (history/redo history)
            restored-update (history/current new-history)
            restored-state (:editor-state restored-update)
            changelist (:changelist restored-update)
            new-vms (vm/update-viewmodels viewmodels (:doc restored-state) (elem-width ui-state) measure-fn changelist)
            new-ui-state (assoc ui-state
                                :input-history new-input-history
                                :history new-history
                                :viewmodels new-vms)]
        (sync-dom! (:dom-elem new-ui-state)
                   (:editor-state restored-update)
                   (history/current-state history)
                   (:viewmodels new-ui-state)
                   changelist)
        (reset! *ui-state new-ui-state)))))

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
        new-ui-state (-> ui-state
                         (update :history update-history editor-update interceptor)
                         (update :input-history interceptors/add-to-input-history interceptor event)
                         (update-viewmodels-to-history-tip))]
    (sync-dom! (:dom-elem new-ui-state)
               (:editor-state editor-update)
               editor-state
               (:viewmodels new-ui-state)
               (:changelist editor-update))

    (reset! *ui-state new-ui-state)

    (cond
      ;; Adding to history has already be handled, cancel the debounced func if one is waiting to fire
      (and (:add-to-history-immediately? interceptor)
           (:include-in-history? interceptor))
      (cancel-add-tip-to-backstack! *ui-state)

      ;; Integrate the history tip into the backstack after a period of inactivity
      (:include-in-history? interceptor)
      (add-tip-to-backstack-after-wait! *ui-state))))

(defn fire-interceptor!
  [*ui-state interceptor event]
  (if (:manual? interceptor)
    ;; Manual interceptors circumvent Slate's default data-loop and just fire as regular functions
    (interceptor *ui-state event)
    (fire-normal-interceptor! *ui-state interceptor event)))

(defn init-event-handlers!
  "Registers event listeners for the editor surface with their default interceptors."
  [*ui-state]
  (swap! *ui-state assoc :add-tip-to-backstack-callback #(add-tip-to-backstack-after-wait! *ui-state))
  (let [get-interceptor (partial interceptors/find-interceptor (:interceptors @*ui-state))
        {editor-elem :dom-elem, hidden-input :hidden-input} @*ui-state]
    (let [editor-surface-clicked? (atom false :validator boolean?)
          mousedown-event (atom nil :validator #(instance? js/MouseEvent %))]

      (.addEventListener editor-elem "mousedown"
                         (fn [e]
                           (.preventDefault e)
                           (.focus hidden-input)
                           (reset! editor-surface-clicked? true)
                           (reset! mousedown-event e)
                           (fire-interceptor! *ui-state (get-interceptor :click) e)))

      (.addEventListener js/window "mousemove"
                         (fn [e]
                           (when (and @editor-surface-clicked?
                                      ;; Make sure it's actually still clicked down, if the user moved the mouse
                                      ;; off-window and back the 'mouseup' event will not have set the atom back to false.
                                      (= 1 (.-which e)))
                             ;; *last-mousedown-event* is passed this way for optimization purposes
                             (binding [view/*last-mousedown-event* @mousedown-event]
                               (fire-interceptor! *ui-state (get-interceptor :drag) e)))))

      (.addEventListener js/window "mouseup"
                         (fn [_e]
                           (reset! editor-surface-clicked? false))))
    (.addEventListener
     hidden-input
     "keydown"
     (fn [e]
       (when-let [interceptor-fn (get-interceptor e)]
         (.preventDefault e)
         (fire-interceptor! *ui-state interceptor-fn e))))
    (.addEventListener
     hidden-input
     "beforeinput"
     (fn [e]
       (case (.-inputType e)
         "insertText"
         (let [{:keys [interceptors input-history]} @*ui-state
               ;; If the data is a single key and matches a completion, fire that instead of the insert interceptor
               completion-interceptor (when (= 1 (.. e -data -length))
                                        (interceptors/find-completion (.-data e) interceptors input-history))
               interceptor (or completion-interceptor (get-interceptor :insert))]
           (fire-interceptor! *ui-state interceptor e))

         "deleteContentBackward"
         (let [{:keys [input-history]} @*ui-state]
           (if (= :completion (peek input-history))
             (fire-interceptor! *ui-state undo! e)
             (fire-interceptor! *ui-state (get-interceptor :delete) e)))

         nil)))

    (.addEventListener js/window "resize" #(handle-resize! *ui-state))))

(def manual-interceptors
  "Some manual interceptors that are not pure and therefore defined here rather than default_interceptors."
  (if utils/is-mac?
    {:cmd+z undo!
     :cmd+shift+z redo!
     :cmd+= increase-font-size!
     :cmd+- decrease-font-size!}
    {:ctrl+z undo!
     :ctrl+shift+z redo!
     :ctrl+= increase-font-size!
     :ctrl+- decrease-font-size!}))

(defn init!
  "Initializes the editor surface, and returns an atom containing the EditorUIState. This
   atom will continue to be updated throughout the lifetime of the editor. Takes a series
   of keyword arguments:

   Required:
   :dom-elem - The DOM element that the editor will be displayed in
   :hidden-input - The hidden <input> element needed by the editor to capture keystrokes

   Optional:
   :editor-state - The initial editor-state to load into the editor. Will default to an empty document."
  [& {:keys [editor-state dom-elem hidden-input]}]
  (let [uuid (random-uuid)
        dom-elem-width (.-width (.getBoundingClientRect dom-elem))
        measure-fn (ruler-for-elem uuid dom-elem)
        editor-state (or editor-state (es/editor-state))
        interceptors-map (-> (interceptors/interceptor-map)
                             (interceptors/reg-interceptors default-interceptors)
                             (interceptors/reg-interceptors manual-interceptors))
        *ui-state (atom {:id uuid
                         ;; TODO: get width
                         :viewmodels (vm/from-doc (:doc editor-state) dom-elem-width measure-fn)
                         :history (history/init editor-state)
                         :add-tip-to-backstack-timer-id nil
                         :dom-elem dom-elem
                         :hidden-input hidden-input
                         :measure-fn measure-fn
                         :input-history []
                         :interceptors interceptors-map})]
    (init-event-handlers! *ui-state)
    (full-dom-render! *ui-state)

    *ui-state))
