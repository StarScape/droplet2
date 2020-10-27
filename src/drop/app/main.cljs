(ns drop.app.main
  (:require [drop.editor.core :as c]
            [drop.editor.selection :as sel]
            [drop.editor.viewmodel :as vm]
            ["/drop/editor/CharRuler" :refer (CharRuler fakeRuler)]))

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))
(def override (.getElementById js/document "override-checkbox"))
(def style (js/getComputedStyle fake-editor))
(def font-size (.getPropertyValue style "font-size"))
(def font-family (.getPropertyValue style "font-family"))

(def ruler (CharRuler. font-size font-family))
(def test-para
  (c/paragraph [(c/run "Hello world, this is an example of a paragraph ")
                (c/run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                (c/run "Don't know what else to say. Hmmmm..." #{:bold})]))

(def paragraph (atom test-para))
(def selection (atom (sel/selection [0 0])))
(def viewmodel (atom (vm/from-para @paragraph 200 ruler)))

(defn ^:dev/after-load reload [] ())

(defn main []
  (println (str "Main class x3!")))
