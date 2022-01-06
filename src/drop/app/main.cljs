(ns drop.app.main
  (:require [slate.model.run :refer [run]]
            [slate.model.paragraph :refer [paragraph]]
            [slate.model.doc :refer [document]]
            [slate.model.editor-state :refer [editor-state]]
            [slate.core :as sl]))

;; TODO: Handle case of click, hold, type some stuff, THEN release
;; TODO: Make a React element that encapsulates the editor. This should
;; live at the app level, not the Slate library level.
;; TODO: Handle resizing of the text area

;; TODO: Handle inserting with styles (maybe add a 'current-style' to the doc-state object?)

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))
(def para1 (paragraph (uuid "p1")
                         [(run "Hello world, this is an example of a paragraph ")
                          (run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                          (run "Don't know what else to say. Hmmmm..." #{:bold})]))
(def para2 (paragraph (uuid "p2") [(run)]))
(def para3 (paragraph (uuid "p3") [(run "And this is paragraph numero dos.")]))
(def doc (document [para1 para2 para3]))

(def *ui-state (sl/init :editor-state (editor-state doc)
                        :dom-elem fake-editor
                        :hidden-input hidden-input))

(defn main [])

(defn ^:dev/after-load reload []
  #_(sync-dom @state))

