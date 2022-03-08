(ns slate.editor-ui-state
  (:require-macros [slate.interceptors :refer [definterceptor]])
  (:require [clojure.spec.alpha :as s]
            [slate.model.editor-state :as es]
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

(defn update-viewmodels-to-history-tip
  "Updates the :viewmodels attribute of `ui-state` to match the tip of the ui state's
   :history object. See the history namespace for more info."
  [ui-state]
  (let [{:keys [viewmodels history measure-fn]} ui-state
        {:keys [editor-state changelist]} (:tip history)
        new-viewmodels (vm/update-viewmodels viewmodels (:doc editor-state) measure-fn changelist)]
    (assoc ui-state :viewmodels new-viewmodels)))

(defn add-tip-to-backstack!
  [*ui-state]
  {:pre [(instance? Atom *ui-state)]}
  (swap! *ui-state update :history history/add-tip-to-backstack))

(defn add-tip-to-backstack-after-wait!
  [*ui-state]
  (js/clearTimeout (:add-tip-to-backstack-timer-id @*ui-state))
  (swap! *ui-state assoc :add-tip-to-backstack-timer-id (js/setTimeout #(add-tip-to-backstack! *ui-state)
                                                                       history-timeout-ms)))

(defn cancel-add-tip-to-backstack!
  [*ui-state]
  (js/clearTimeout (:add-tip-to-backstack-timer-id @*ui-state)))

(defn init-dom!
  "Perform initial DOM render. This will be called on application startup,
   then sync-dom! will take over and fire after every interceptor."
  [ui-state]
  (let [{:keys [dom-elem viewmodels history]} ui-state
        {:keys [doc selection]} (history/current-state history)
        viewmodels (map #(get viewmodels (:uuid %)) (:children doc))]
    (view/insert-all! dom-elem viewmodels selection)))

(defn sync-dom!
  "Sync editor DOM element to provided changelist, updating
  all paragraphs that have been inserted/changed/removed."
  [dom-elem editor-state viewmodels changelist]
  (let [{:keys [doc selection]} editor-state
        {:keys [deleted-uuids changed-uuids inserted-uuids]} changelist]
    (doseq [uuid inserted-uuids]
      (view/insert-para! dom-elem uuid (get viewmodels uuid) doc selection))
    (doseq [uuid deleted-uuids]
      (view/remove-para! dom-elem uuid))
    (doseq [uuid changed-uuids]
      (view/update-para! dom-elem uuid (get viewmodels uuid) selection))))

;; if normal:
;;   Just update tip, integrate into backstack after debounced timeout
;; if completion:
;;   Take state before interceptor fired, add that to the backstack immediately.
;;   Then set the tip to the result of the completion interceptor.

(defn update-history
  [history editor-update interceptor]
  (cond
    (and (:add-to-history-immediately? interceptor)
         (:include-in-history? interceptor))
    (-> history
        (history/add-tip-to-backstack)
        (history/set-tip editor-update))

    (:include-in-history? interceptor)
    (history/set-tip history editor-update)

    :else history))

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
    ;; Manual interceptors don't rely on Slate's default data-loop
    (interceptor *ui-state event)
    (fire-normal-interceptor! *ui-state interceptor event)))

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
            new-vms (vm/update-viewmodels viewmodels (:doc restored-state) measure-fn changelist)
            new-ui-state (assoc ui-state
                                :input-history new-input-history
                                :history new-history
                                :viewmodels new-vms)]
        (sync-dom! (:dom-elem new-ui-state)
                   (:editor-state restored-update)
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
            new-vms (vm/update-viewmodels viewmodels (:doc restored-state) measure-fn changelist)
            new-ui-state (assoc ui-state
                                :input-history new-input-history
                                :history new-history
                                :viewmodels new-vms)]
        (sync-dom! (:dom-elem new-ui-state)
                   (:editor-state restored-update)
                   (:viewmodels new-ui-state)
                   changelist)
        (reset! *ui-state new-ui-state)))))

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
        ;; TODO: how to undo a completion with a backspace immediately after the
        ;; completion fires? I think one method might be to FIRST let the normal
        ;; :insert interceptor fire, THEN afterward fire the completion. Then a 
        ;; special case must be added to the :delete interceptor which undoes what
        ;; just happened IF the last thing to happen was a completion interceptor.
        ;; This would also necessitate that every completion add :completion or something
        ;; similar to the input-history.
        ;;
        ;; I think it could work but I'm not yet totally sure how I feel about it.
        ;; Need to think it over.
       (case (.-inputType e)
         "insertText"
         (let [{:keys [interceptors input-history]} @*ui-state
               ;; If the data is a single key and matches a completion, fire that instead of the insert interceptor
               completion-interceptor (when (= 1 (.. e -data -length))
                                        (interceptors/find-completion (.-data e) interceptors input-history))
               interceptor (or completion-interceptor (get-interceptor :insert))]
           (fire-interceptor! *ui-state interceptor e))

         "deleteContentBackward"
         (fire-interceptor! *ui-state (get-interceptor :delete) e)

         nil)))))

(def manual-interceptors
  "Some manual interceptors that are not pure and therefore defined here rather than default_interceptors."
  (if utils/is-mac?
    {:cmd+z undo!
     :cmd+shift+z redo!}
    {:ctrl+z undo!
     :ctrl+shift+z redo!}))

(defn init
  "Initializes the editor surface, and returns an atom containing the EditorUIState. This
   atom will continue to be updated throughout the lifetime of the editor. Takes a series
   of keyword arguments:

   Required:
   :dom-elem - The DOM element that the editor will be displayed in
   :hidden-input - The hidden <input> element needed by the editor to capture keystrokes

   Optional:
   :editor-state - The initial editor-state to load into the editor. Will default to an empty document."
  [& {:keys [editor-state dom-elem hidden-input]}]
  (let [measure-fn (ruler-for-elem dom-elem)
        editor-state (or editor-state (es/editor-state))
        interceptors-map (-> (interceptors/interceptor-map)
                             (interceptors/reg-interceptors default-interceptors)
                             (interceptors/reg-interceptors manual-interceptors))
        *ui-state (atom {:id (random-uuid)
                         :viewmodels (vm/from-doc (:doc editor-state) 200 measure-fn)
                         :history (history/init editor-state)
                         :add-tip-to-backstack-timer-id nil
                         :dom-elem dom-elem
                         :hidden-input hidden-input
                         :measure-fn measure-fn
                         :input-history []
                         :interceptors interceptors-map})]
    (init-event-handlers! *ui-state)
    (init-dom! @*ui-state)

    *ui-state))
