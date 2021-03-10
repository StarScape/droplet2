;; TODO change name to core and separate out the shits in the current core.cljs
(ns slate.main
  (:require [clojure.string :as str]
            #_[slate.events :as events :refer [interceptors fire-interceptor]]
            [slate.core :as sl]
            [slate.measurement :refer [ruler-for-elem]]
            [slate.navigation :as nav]
            [slate.selection :as sel]
            [slate.view :as view]
            [slate.viewmodel :as vm]))

(defn sync-dom
  "Brings the DOM up to date with the latest changes changes to the document.
   Takes the editor state and the editor DOM element as arguments."
  [{:keys [viewmodels doc selection] :as _editor-state} root-elem]
  (let [vm-paras (map #(get viewmodels (:uuid %)) (:children doc))
        rendered (view/vm-paras->dom vm-paras selection)]
    (set! (.-innerHTML root-elem) rendered)))

;; TODO NEXT: extract some of this into the events namespace

;; TODO (maybe, here for my mental posterity): this can be changed to a `find-interceptor`
;; function that takes an event and a map of all the interceptors and returns
;; one if it exists or null otherwise (maybe a no-op otherwise?). This will also
;; give us more flexibility in defining how events cascade (if at all?) and allow
;; modifier keys to be written in any order.

;; TODO: once I incorporate automatic parsing of events, can turn this into a multimethod/protocol
;; and implement instances for different sublcasses of Events :)
(defn parse-event [e]
  (let [modifiers (cond-> (transient [])
                    (.-ctrlKey e) (conj! "ctrl")
                    (.-altKey e) (conj! "alt")
                    (.-shiftKey e) (conj! "shift")
                    :always (persistent!))
        key (case (.-key e)
              "ArrowLeft" "left"
              "ArrowRight" "right"
              "ArrowUp" "up"
              "ArrowDown" "down"
              "Tab" "tab"
              (-> (.-key e) .toLowerCase))]
    (->> (conj modifiers key)
         (str/join "+")
         (keyword))))

(defn fire-interceptor
  "Calls the interceptor with the current editor state and the Event object as its args
   (and optionally, any additional args you wish to pass it) and re-synces the DOM.

   If no interceptor function is provided, the event will be parsed and the matching registered
   interceptor (if any) will be fired (TODO TODO TODO)."
  [interceptor-fn state-atom event & args]
  (let [state @state-atom
        ;; Interceptors return an update, not a full new editor state. TODO: transactions
        update-object (apply interceptor-fn @state-atom event args)
        merged (merge state update-object)
        new-state (assoc merged :viewmodels (vm/from-doc (:doc merged) 200 (:measure-fn state)))]
    (reset! state-atom new-state)
    (sync-dom new-state (:dom-elem new-state))))

;; TODO: allow interceptors of the type "# " which fires when the user types a key sequence of pound then space.
;; These need to coexist with the existing control-key oriented interceptors. I think there should be three types:
;;
;; 1. Keyword (e.g. :click, :ctrl+left) - the ones that currently exist
;; 2. String (e.g. "# " or "1. ") - fires once the sequence of characters in the string is typed
;; 3. Vectors (e.g. [:ctrl+a "1"], [:ctrl+a, :ctrl+b]) - used to mix types 1 and 2, or to create chords
;;    out of keyboard shortcuts. This type is probably not as strictly necessary as the first two, but
;;    it should probably be added for the sake of completeness/extensibility.
;;
;; Definitely implement types 1 and 2 first. I'll have to give some careful thought about the data structures I need
;; to use to achieve this.

(def default-interceptors
  {:click (fn [state e]
            (let [new-sel (view/mouse-event->selection e state (:measure-fn state))]
              (assoc state :selection new-sel)))
   :drag (fn [state mousemove-event mousedown-event]
           (update state :selection #(view/drag mousedown-event mousemove-event state (:measure-fn state))))

   :insert (fn [{:keys [doc selection] :as state} e]
             (let [text (.-data e)
                   new-doc (sl/insert doc selection text)
                   new-selection (sel/shift-single selection (count text))]
               (assoc state :doc new-doc :selection new-selection)))
   :delete (fn [{:keys [doc selection] :as state} _e]
             (let [[new-doc, new-sel] (sl/delete doc selection)]
               (assoc state :doc new-doc :selection new-sel)))
   :enter (fn [{:keys [doc selection] :as state} _e]
            (let [[new-doc, new-sel] (sl/enter doc selection)]
              (assoc state :doc new-doc :selection new-sel)))
   :tab (fn [{:keys [doc selection] :as state} _e]
          (let [new-doc (sl/insert doc selection "\u2003")
                new-selection (sel/shift-single selection 1)]
            (assoc state :doc new-doc :selection new-selection)))

   :left (fn [state _e]
           (update state :selection #(nav/prev-char (:doc state) %)))
   :ctrl+left (fn [state _e]
                (update state :selection #(nav/prev-word (:doc state) %)))
   :shift+left (fn [state _e]
                 (update state :selection #(nav/shift+left (:doc state) (:selection state))))
   :ctrl+shift+left (fn [state _e]
                      (update state :selection #(nav/ctrl+shift+left (:doc state) (:selection state))))

   :right (fn [state _e]
            (update state :selection #(nav/next-char (:doc state) %)))
   :ctrl+right (fn [state _e]
                 (update state :selection #(nav/next-word (:doc state) %)))
   :shift+right (fn [state _e]
                  (update state :selection #(nav/shift+right (:doc state) (:selection state))))
   :ctrl+shift+right (fn [state _e]
                       (update state :selection #(nav/ctrl+shift+right (:doc state) (:selection state))))

   :down (fn [state _e]
           (update state :selection #(view/down state (:measure-fn state))))
   :shift+down (fn [state _e]
                 (update state :selection #(view/shift+down state (:measure-fn state))))
   :up (fn [state _e]
         (update state :selection #(view/up state (:measure-fn state))))
   :shift+up (fn [state _e]
               (update state :selection #(view/shift+up state (:measure-fn state))))})

(defn init-default-events [editor-state-atom editor-elem hidden-input]
  (let [get-interceptor (fn [key] (get (:interceptors @editor-state-atom) key))]
    ;; TODO: should probably add an explanatory comment about the relationship between these three events
    (let [clicked? (atom false :validator boolean?)
          mousedown-event (atom nil :validator #(instance? js/MouseEvent %))]
      (.addEventListener editor-elem "mousedown"
        (fn [e]
          (.preventDefault e)
          (.focus hidden-input)

          (reset! clicked? true)
          (reset! mousedown-event e)
          (fire-interceptor (get-interceptor :click) editor-state-atom e)))

      (.addEventListener js/window "mousemove"
        (fn [e]
          (when @clicked?
            (fire-interceptor (get-interceptor :drag) editor-state-atom e @mousedown-event))))

      (.addEventListener js/window "mouseup"
        (fn [_e]
          (reset! clicked? false))))

    (.addEventListener hidden-input "keydown"
      (fn [e]
        (when-let [interceptor-fn (get-interceptor (parse-event e))]
          (.preventDefault e)
          (fire-interceptor interceptor-fn editor-state-atom e))))

    (.addEventListener hidden-input "beforeinput"
      (fn [e]
        (case (.-inputType e)
          "insertText" (fire-interceptor (get-interceptor :insert) editor-state-atom e)
          "deleteContentBackward" (fire-interceptor (get-interceptor :delete) editor-state-atom e)
          nil)))))


;; TODO: just create the hidden-elem programmatically here
(defn init
  "Initializes the editor surface, and returns an atom contain the editor state. This
   atom will continue to be update throughout the lifetime of the editor. Takes a series
   of keyword arguments:

   Required:
   :editor-elem - The DOM element for the editor
   :hidden-input - The hidden <input> needed by the editor to capture keystrokes

   Optional:
   :doc - The initial document to load into the editor (default to an empty document)
   :selection - Initial selection (defaults to the start of the document)"
  [& {:keys [doc selection editor-elem hidden-input]}]
  (let [measure-fn (ruler-for-elem editor-elem)
        editor-state (atom {:doc doc
                            :selection (or selection (nav/start doc))
                            ;; TODO: just change to a DLL of viewmodels?
                            :viewmodels (vm/from-doc doc 200 measure-fn)
                            :dom-elem editor-elem
                            :hidden-input hidden-input
                            :measure-fn measure-fn
                            :interceptors default-interceptors})]
    (init-default-events editor-state editor-elem hidden-input)
    (sync-dom @editor-state editor-elem)

    editor-state))
