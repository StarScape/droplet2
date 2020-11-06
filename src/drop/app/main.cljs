(ns drop.app.main
  (:require [drop.editor.core :as c]
            [drop.editor.selection :as sel]
            [drop.editor.viewmodel :as vm]
            ["/drop/editor/CharRuler" :refer (CharRuler)]))

(defn ruler-for-element [elem]
  (let [style (js/getComputedStyle elem)]
    (CharRuler.
     (.getPropertyValue style "font-size")
     (.getPropertyValue style "font-family"))))

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))
(def ruler (ruler-for-element fake-editor))

(def test-para
  (c/paragraph [(c/run "Hello world, this is an example of a paragraph ")
                (c/run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                (c/run "Don't know what else to say. Hmmmm..." #{:bold})]))

(def doc-state (atom {:doc test-para
                      :selection (sel/selection [0 0])
                      :paragraph-viewmodels [(vm/from-para test-para 200 ruler)]
                      :action-history []}))

;; TODO: this or classes?
(defn formats->css-str [formats]
  (->> formats
       (map #(case %
               :italic "font-style: italic;"
               :bold "font-weight: bold;"
               :underline "text-decoration: line-through;"))
       (apply str)))

;; TODO: these can be moved to viewmodel NS
(defn vm-span->dom
  "Convert viewmodel span to DOM element."
  [span]
  (str "<span class='span'"
       "style='" (formats->css-str (:formats span)) "'>"
       (:text span)
       "</span>"))

(defn vm-line->dom
  "Convert viewmodel line to DOM element."
  [line]
  (str "<div class='line'>"
       (apply str (map vm-span->dom (:spans line)))
       "</div>"))

(defn vm->dom
  "Convert viewmodel to DOM element."
  [viewmodel]
  (str "<div class='paragraph'>"
       (apply str (map vm-line->dom (:lines #p viewmodel)))
       "</div>"))

(defn sync-dom [elem doc-state ruler]
  (let [state @doc-state
        doc (:doc state)
        vm (vm/from-para doc 200 ruler)]
    (set! (.-innerHTML elem) (vm->dom vm))))

(defn main []
  (sync-dom fake-editor doc-state ruler))

(defn ^:dev/after-load reload []
  (main))
