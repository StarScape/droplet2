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
  (c/paragraph "p1" [(c/run "Hello world, this is an example of a paragraph ")
                     (c/run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                     (c/run "Don't know what else to say. Hmmmm..." #{:bold})]))
(def para3
  (c/paragraph "p3" [(c/run "And this is paragraph numero dos.")]))

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
  "Calls the interceptor with the provided args."
  [interceptor-fn state e]
  (let [old-state @doc-state
        changed (interceptor-fn state e)
        doc (get changed :doc (:doc old-state))
        new-state (-> (merge old-state changed)
                      (assoc :viewmodels (vm/from-doc doc 200 measure-fn)))]
    (reset! doc-state new-state)
    (sync-dom @doc-state fake-editor)))

;; TODO: test to make sure all of these work correctly with multiple paragraphs
;; TODO: change to a reg-interceptors! function call.
(def interceptors
  {:insert (fn [{:keys [doc selection] :as state} e]
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

(defn main []
  (.addEventListener fake-editor "click"
    (fn [_e]
      (.focus hidden-input)))

  (.addEventListener hidden-input "keydown"
    (fn [e]
      (when-let [interceptor-fn (get interceptors (parse-event e))]
        (.preventDefault e)
        (fire-interceptor interceptor-fn @doc-state e))))

  (.addEventListener hidden-input "beforeinput"
    (fn [e]
      (case (.-inputType e)
        "insertText" (fire-interceptor (:insert interceptors) @doc-state e)
        "deleteContentBackward" (fire-interceptor (:delete interceptors) @doc-state e)
        nil)))

  (sync-dom @doc-state fake-editor))

(defn ^:dev/after-load reload []
  (sync-dom @doc-state fake-editor))

;; TODO: Handle clicking
;; TODO: Handle drag selection (selection/expand-to function maybe?)
;; TODO: Make sure everything still works with 3 paragraphs

;; TODO: update everything to return a doc change object (significant)