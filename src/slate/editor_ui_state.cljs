(ns slate.editor-ui-state
  (:require-macros [slate.interceptors :refer [interceptor definterceptor]])
  (:require [slate.model.editor-state :as es]
            [slate.model.editor-state-history :as history]
            [slate.events :as events]
            [slate.interceptors :as interceptors]
            [slate.measurement :refer [ruler-for-elem]]
            [slate.utils :refer [debounce]]
            [slate.viewmodel :as vm]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(s/def ::id uuid?)
(s/def ::history ::history/editor-state-history)
(s/def ::dom-elem #(instance? js/HTMLElement %))
(s/def ::hidden-input ::dom-elem)
(s/def ::measure-fn (s/spec :args (s/cat :text string?
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

(defn init-dom!
  "Perform initial DOM render. This will be called on application startup,
   then sync-dom! will take over and fire after every interceptor."
  [ui-state])

(defn sync-dom! [dom-elem viewmodels changelist measure-fn]
  ;; sync-dom should need: viewmodels, changelist, dom-elem, and measure-fn
  (let [{:keys [deleted-uuids changed-uuids inserted-uuids]} changelist]
    (doseq [uuid deleted-uuids]
      (view/remove-para uuid))
    (doseq [uuid changed-uuids]
      (view/update-para uuid (get viewmodels uuid)))
    (doseq [uuid inserted-uuids]
      (view/insert-para uuid (get viewmodels uuid)))))

(defn add-tip-to-backstack!
  [ui-state-atom]
  (swap! ui-state-atom update :history history/add-tip-to-backstack))

(def add-tip-to-backstack-after-wait! (debounce 3000 add-tip-to-backstack!))

(defn update-viewmodels-to-history-tip
  "Updates the :viewmodels attribute of `ui-state` to match the tip of the ui state's
   :history object. See the history namespace for more info."
  [ui-state]
  (let [{:keys [viewmodels history measure-fn]} ui-state
        tip-editor-state (:tip history)
        get-para (partial get (-> tip-editor-state :doc :children))
        {:keys [changed-uuids
                inserted-uuids
                deleted-uuids]} (:changelist tip-editor-state)
        updated-vms (as-> viewmodels vms
                      (apply dissoc vms deleted-uuids)
                      (reduce (fn [new-vms uuid]
                                (assoc new-vms uuid (vm/from-para (get-para uuid) 200 measure-fn)))
                              vms (concat inserted-uuids changed-uuids)))]
    (assoc ui-state :viewmodels updated-vms)))

(defn fire-interceptor!
  "The fire-interceptor! function is at the core of Slate's main data loop.
   Any time an event happens which finds a matching interceptor, fire-interceptor!
   is called, which handles the state stored in the UIState atom and updating the DOM."
  [ui-state-atom interceptor event]
  (let [ui-state @ui-state-atom ; only deref once a cycle
        editor-state (history/current-state (:history ui-state))
        new-editor-state (interceptor editor-state ui-state event)
        new-ui-state (-> ui-state
                         (update :history history/set-tip new-editor-state)
                         (update :input-history events/add-key-to-history (:input-name interceptor))
                         (update-viewmodels-to-history-tip))
        new-ui-state (if (and (:add-to-history-immediately? interceptor)
                              (:include-in-history? interceptor))
                       (update new-ui-state
                               :history
                               history/add-tip-to-backstack
                               new-editor-state)
                       new-ui-state)]
    (when-not (:no-dom-sync? interceptor)
      (sync-dom! (:dom-elem new-ui-state)
                 (:viewmodels new-ui-state)
                 (:changelist new-ui-state)
                 (:measure-fn new-ui-state)))

    (reset! ui-state-atom new-ui-state)

    (when (and (:include-in-history? interceptor)
               (not (:add-to-history-immediately? interceptor)))
      ;; NOTE: make sure wait is on a per-instance basis
      (add-tip-to-backstack-after-wait! ui-state-atom))))

(defn find-completion
  "Takes the editor's interceptor map and input history, and returns
   a matching completion interceptor if one exists, or nil otherwise."
  [key-pressed interceptor-map input-history]
  {:pre [(vector? input-history)]}
  (let [completions (:completions interceptor-map)
        path (reverse (conj input-history key-pressed))] ; [..., "c" "b", "a"]
    (loop [current-level completions
           [p & ps] path]
      (let [next (current-level p)]
        (if (or (nil? next) (fn? next)) next (recur next ps))))))

(defn init-event-handlers
  "Registers event listeners for the editor surface with their default interceptors."
  [*ui-state]
  (let [get-interceptor (partial interceptors/find-interceptor (:interceptors @*ui-state))
        {editor-elem :dom-elem, hidden-input :hidden-input} @*ui-state]
    ;; TODO: should probably add an explanatory comment about the relationship between these three events
    (let [clicked? (atom false :validator boolean?)
          mousedown-event (atom nil :validator #(instance? js/MouseEvent %))]

      (.addEventListener
       editor-elem
       "mousedown"
       (fn [e]
         (.preventDefault e)
         (.focus hidden-input)
         (reset! clicked? true)
         (reset! mousedown-event e)
         (fire-interceptor! *ui-state (get-interceptor :click) e)))

      (.addEventListener js/window
                         "mousemove"
                         (fn [e]
                           (when @clicked?
                             ;; TODO: previously this was taking the @mousedown-event as a final param.
                             ;; May need to add back the ability to add additional parameters in order
                             ;; to get this to work.
                             (fire-interceptor! *ui-state (get-interceptor :drag) e))))

      (.addEventListener js/window "mouseup" (fn [_e] (reset! clicked? false))))
    (.addEventListener
     hidden-input
     "keydown"
     (fn [e]
        ;; TODO: check for completions here (or possibly in the :insert case below?) and if one exists fire its interceptor
       (when-let [interceptor-fn (get-interceptor e)]
         (.preventDefault e)
         (fire-interceptor! interceptor-fn *ui-state e))))
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
                                        (find-completion (.-data e) interceptors input-history))
               interceptor (or completion-interceptor (get-interceptor :insert))]
           (fire-interceptor! *ui-state interceptor e))

         "deleteContentBackward"
         (fire-interceptor! *ui-state (get-interceptor :delete) e)

         nil)))))

(defn init
  "Initializes the editor surface, and returns an atom containing the editor state. This
   atom will continue to be update throughout the lifetime of the editor. Takes a series
   of keyword arguments:

   Required:
   :editor-elem - The DOM element for the editor
   :hidden-input - The hidden <input> element needed by the editor to capture keystrokes

   Optional:
   :editor-state - The initial editor-state to load into the editor"
  [& {:keys [editor-state editor-elem hidden-input]}]
  (let [measure-fn (ruler-for-elem editor-elem)
        editor-state (or editor-state (es/editor-state))
        ui-state (atom {:id (random-uuid)
                        :viewmodels (vm/from-doc (:doc editor-state) 200 measure-fn)
                        :history (history/init editor-state)
                        :dom-elem editor-elem
                        :hidden-input hidden-input
                        :measure-fn measure-fn
                        :input-history []
                        :interceptors (interceptors/interceptor-map)})]
    (init-event-handlers ui-state)
    (init-dom! @ui-state)

    editor-state))
