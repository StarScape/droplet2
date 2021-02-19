(ns drop.app.main
  (:require [clojure.string :as str]
            [drop.app.view :as view]
            [drop.editor.core :as c]
            [drop.editor.navigation :as nav]
            [drop.editor.selection :as sel]
            [drop.editor.measurement :refer [ruler-for-elem]]
            [drop.editor.viewmodel :as vm]))

;; main

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
              (-> (.-key e) .toLowerCase))]
    (->> (conj modifiers key)
         (str/join "+")
         (keyword))))

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))
(def measure-fn (ruler-for-elem fake-editor))
(def para1
  (c/paragraph (uuid "p1") [(c/run "Hello world, this is an example of a paragraph ")
                     (c/run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                     (c/run "Don't know what else to say. Hmmmm..." #{:bold})]))
(def para3
  (c/paragraph (uuid "p3") [(c/run "And this is paragraph numero dos.")]))

;; TODO: maybe change this to "editor-state" and include dom references and current ruler inside it
;; TODO: hide this behind an initializer function which returns the shit we need and takes an elem as its argument
(def initial-doc (c/document [para1 (c/paragraph) para3]))
(def doc-state (atom {:doc initial-doc
                      :selection (sel/selection [(:uuid para1) 0])
                      ;; TODO: just change to a DLL of viewmodels?
                      :viewmodels (vm/from-doc initial-doc 200 measure-fn)}))

(defn sync-dom
  [{:keys [viewmodels doc selection] :as _doc-state} root-elem]
  (let [vm-paras (map #(get viewmodels (:uuid %)) (:children doc))
        rendered (view/vm-paras->dom vm-paras selection)]
    (set! (.-innerHTML root-elem) rendered)))

(defn fire-interceptor
  "Calls the interceptor with the provided args (typically
   state and the event object) and re-synces the DOM."
  [interceptor-fn & args]
  (let [old-state @doc-state
        changed (apply interceptor-fn args)
        doc (get changed :doc (:doc old-state))
        new-state (-> (merge old-state changed)
                      (assoc :viewmodels (vm/from-doc doc 200 measure-fn)))]
    (reset! doc-state new-state)
    (sync-dom @doc-state fake-editor)))

;; TODO: change to a reg-interceptors! function call.
(def interceptors
  {:click (fn [state clicked-elem e]
            (let [clicked-para-vm (get-in state [:viewmodels (uuid (.-id clicked-elem))])
                  new-sel (view/click e clicked-elem clicked-para-vm measure-fn)]
              (assoc state :selection new-sel)))
   :drag (fn [state mousedown-event mousemove-event]
           (update state :selection #(view/drag mousedown-event mousemove-event state measure-fn)))

   :insert (fn [{:keys [doc selection] :as state} e]
             (let [text (.-data e)
                   new-doc (c/insert doc selection text)
                   new-selection (sel/shift-single selection (count text))]
               (assoc state :doc new-doc :selection new-selection)))
   :delete (fn [{:keys [doc selection] :as state} _e]
             (let [[new-doc, new-sel] (c/delete doc selection)]
               (assoc state :doc new-doc :selection new-sel)))
   :enter (fn [{:keys [doc selection] :as state} _e]
            (let [[new-doc, new-sel] (c/enter doc selection)]
             (assoc state :doc new-doc :selection new-sel)))

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
           (update state :selection #(view/down state measure-fn)))
   :shift+down (fn [state _e]
                 (update state :selection #(view/shift+down state measure-fn)))
   :up (fn [state _e]
         (update state :selection #(view/up state measure-fn)))
   :shift+up (fn [state _e]
               (update state :selection #(view/shift+up state measure-fn)))})

;; Handle case of: click, hold, type some stuff, THEN release

(defn main []
  (let [clicked? (atom false)
        mousedown-event (atom nil :validator #(instance? js/MouseEvent %))]
    (.addEventListener fake-editor "mousedown"
      (fn [e]
        (.preventDefault e)
        (.focus hidden-input)
        ; TODO: handle case where you click above the first paragraph or below the last paragraph
        ; UPDATE: I think we can do this by switching this call to match-elem-in-path below to
        ;         a call to mousevent->selection and generalizing that function so that it works
        ;         even if you click off the side/top/bottom. This block below could like be shifted
        ;         up into the interceptor as well.
        (when-let [clicked-para (view/match-elem-in-path e ".paragraph")]
          (reset! clicked? true)
          (reset! mousedown-event e)
          (fire-interceptor (:click interceptors) @doc-state clicked-para e))))

    (.addEventListener js/window "mousemove"
      (fn [e]
        (when @clicked?
          (fire-interceptor (:drag interceptors) @doc-state @mousedown-event e))))

    (.addEventListener js/window "mouseup"
      (fn [_e]
        (reset! clicked? false))))

  (.addEventListener hidden-input "keydown"
    (fn [e]
      (when-let [interceptor-fn (get interceptors (parse-event e))]
        (.preventDefault e)
        (fire-interceptor interceptor-fn @doc-state e))))

  (.addEventListener hidden-input "beforeinput"
    (fn [e]
      (println "hello?")
      (case (.-inputType e)
        "insertText" (fire-interceptor (:insert interceptors) @doc-state e)
        "deleteContentBackward" (fire-interceptor (:delete interceptors) @doc-state e)
        nil)))

  (sync-dom @doc-state fake-editor))

(defn ^:dev/after-load reload []
  (sync-dom @doc-state fake-editor))

;; TODO: Handle drag selection (selection/expand-to function maybe?)
;; TODO: Handle inserting with styles (maybe add a 'current-style' to the doc-state object?) 

;; TODO: update everything to return a doc change object (significant)
