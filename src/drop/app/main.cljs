(ns drop.app.main
  (:require [slate.model.run :refer [run]]
            [slate.model.paragraph :refer [paragraph]]
            [slate.model.doc :refer [document]]
            ;; [slate.editor :refer [sync-dom]]
            [slate.core :as sl]))

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))
(def para1 (paragraph (uuid "p1")
                         [(run "Hello world, this is an example of a paragraph ")
                          (run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                          (run "Don't know what else to say. Hmmmm..." #{:bold})]))

(def para2 (paragraph))
(def para3 (paragraph (uuid "p3") [(run "And this is paragraph numero dos.")]))

;; TODO: handle case of click, hold, type some stuff, THEN release
;; TODO: make a react element that encapsulates the editor
(def initial-doc (document [para1 para2 para3]))
(def state (sl/init :editor-elem fake-editor
                    :hidden-input hidden-input
                    :doc initial-doc))

(defn main [])

(defn ^:dev/after-load reload []
  #_(sync-dom @state)
  )

;; TODO: Handle inserting with styles (maybe add a 'current-style' to the doc-state object?)
;; TODO: Handle resizing of the text area

;; TODO: update everything to return a doc change object (significant)
