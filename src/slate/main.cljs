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

;; TODO: this can be changed to a `find-interceptor` function that takes
;; an event and a map of all the interceptors and returns one if it exists
;; or null otherwise (maybe a no-op otherwise?). This will also give us more
;; flexibility in defining how events cascade (if at all?) and allow modifier
;; keys to be written in any order.
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

;; TODO: change to a reg-interceptors! function call.
(defn fire-interceptor
  "Calls the interceptor with the provided args (typically
   state and the Event object) and re-synces the DOM."
  [state-atom editor-elem interceptor-fn & args]
  (let [measure-fn (:measure-fn @state-atom)
        old-state @state-atom
        changed (apply interceptor-fn args)
        doc (get changed :doc (:doc old-state))
        new-state (-> (merge old-state changed)
                      (assoc :viewmodels (vm/from-doc doc 200 measure-fn)))]
    (reset! state-atom new-state)
    (sync-dom @state-atom editor-elem)))

(def interceptors
  {:click (fn [state e]
            (let [new-sel (view/mouse-event->selection e state (:measure-fn state))]
              (assoc state :selection new-sel)))
   :drag (fn [state mousedown-event mousemove-event]
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
  (let [clicked? (atom false :validator boolean?)
        mousedown-event (atom nil :validator #(instance? js/MouseEvent %))]
    (.addEventListener editor-elem "mousedown"
      (fn [e]
        (.preventDefault e)
        (.focus hidden-input)

        (reset! clicked? true)
        (reset! mousedown-event e)
        (fire-interceptor editor-state-atom editor-elem (:click interceptors) @editor-state-atom e)))

    (.addEventListener js/window "mousemove"
      (fn [e]
        (when @clicked?
          (fire-interceptor editor-state-atom editor-elem (:drag interceptors) @editor-state-atom @mousedown-event e))))

    (.addEventListener js/window "mouseup"
      (fn [_e]
        (reset! clicked? false))))

    (.addEventListener hidden-input "keydown"
      (fn [e]
        (when-let [interceptor-fn (get interceptors (parse-event e))]
          (.preventDefault e)
          (fire-interceptor editor-state-atom editor-elem interceptor-fn @editor-state-atom e))))

    (.addEventListener hidden-input "beforeinput"
      (fn [e]
        (case (.-inputType e)
          "insertText" (fire-interceptor editor-state-atom editor-elem (:insert interceptors) @editor-state-atom e)
          "deleteContentBackward" (fire-interceptor editor-state-atom editor-elem (:delete interceptors) @editor-state-atom e)
          nil))))

;; TODO NEXT: include editor-elem and hidden-input in editor-state and extract some of this into the events namespace


;; TODO: just create the hidden-elem programmatically here
(defn init
  "Initializes the editor surface, and returns an atom contain the editor state. This
   atom will continue to be update throughout the lifetime of the editor. Takes a series
   of keyword-arg pairs:

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
                            :measure-fn measure-fn})]
    (init-default-events editor-state editor-elem hidden-input)
    (sync-dom @editor-state editor-elem)

    editor-state))
