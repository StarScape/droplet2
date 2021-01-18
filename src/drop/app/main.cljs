(ns drop.app.main
  (:require [clojure.string :as str]
            [drop.app.view :as view]
            [drop.editor.core :as c]
            [drop.editor.navigation :as nav]
            [drop.editor.selection :as sel]
            [drop.editor.measurement :refer [ruler-for-elem]]
            [drop.editor.viewmodel :as vm]))

;; main

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
              (-> (.-key e) .toLowerCase keyword))]
    (->> (conj modifiers key)
         (str/join "+")
         (keyword))))

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))
(def measure-fn (ruler-for-elem fake-editor))
(def para1
  (c/paragraph [(c/run "Hello world, this is an example of a paragraph ")
                (c/run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                (c/run "Don't know what else to say. Hmmmm..." #{:bold})]))
(def para2
  (c/paragraph [(c/run "And this is paragraph numero dos.")]))

;; TODO: maybe change this to "editor-state" and include dom references and current ruler inside it
;; TODO: hide this behind an initializer function which returns the shit we need and takes an elem as its argument
(def doc-state (atom {:doc (c/document [para1 para2])
                      :selection (sel/selection [(:uuid para1) 0])
                      ;; TODO: just change to a DLL of viewmodels?
                      :viewmodels {(:uuid para1) (vm/from-para para1 200 measure-fn)
                                   (:uuid para2) (vm/from-para para2 200 measure-fn)}}))

(defn sync-dom [elem doc-state measure-fn]
  (let [state @doc-state
        doc (:doc state)
        sel (:selection state)
        vm-paras (map #(vm/from-para % 200 measure-fn) (:children doc))]
    (set! (.-innerHTML elem) (view/vm-paras->dom vm-paras sel))))

;; TODO: test to make sure all of these work correctly with multiple paragraphs
;; TODO: change to a reg-interceptors! function call.
(def interceptors
  {:left (fn [state _e]
           (update state :selection #(nav/prev-char (:doc state) %)))
   :ctrl+left (fn [state _e]
                (update state :selection #(nav/prev-word (:doc state) %)))
   :shift+left (fn [state _e]
                 (update state :selection #(nav/shift+left (:doc state) (:selection state))))

   :right (fn [state _e]
            (update state :selection #(nav/next-char (:doc state) %)))
   :ctrl+right (fn [state _e]
                (update state :selection #(nav/next-word (:doc state) %)))
   :shift+right (fn [state _e]
                  (update state :selection #(nav/shift+right (:doc state) (:selection state))))

   :down (fn [state _e]
           (update state :selection #(view/down state measure-fn)))
   :up (fn [state _e]
         (update state :selection #(view/up state measure-fn)))})

(defn main []
  (.addEventListener js/document "keydown"
    (fn [e]
      (when-let [interceptor-fn (get interceptors (parse-event e))]
        (.preventDefault e)
        (reset! doc-state (interceptor-fn @doc-state e)))
      (sync-dom fake-editor doc-state measure-fn)))
  (sync-dom fake-editor doc-state measure-fn))

(defn ^:dev/after-load reload []
  (sync-dom fake-editor doc-state measure-fn))

;; TODO: Handle shifting selection left/right
;; TODO: Handle shifting selection up/down
;; TODO: Handle ctrl+left and ctrl+right
;; TODO: Handle ctrl+shift+left and ctrl+shift+right
;; TODO: Handle input and deletion
;; TODO: Handle clicking
;; TODO: Handle drag selection (selection/expand-to function maybe?)
;; TODO: Handle enter
