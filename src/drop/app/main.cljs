(ns drop.app.main
  (:require [clojure.string :as str]
            [slate.model.run :refer [run]]
            [slate.model.paragraph :refer [paragraph]]
            [slate.model.doc :refer [document]]
            [slate.model.editor-state :refer [editor-state] :as es]
            [slate.model.history :as history]
            [slate.core :as sl]))

;; DONE: Keep track of :formats on Selection object
;; DONE: update (selection) constructor to allow a single [] arg followed by kw-args

;; TODO: undo/redo
;; TODO: handle completion undo with backspace -- I think the best thing might be just to
;;       fire undo! in the delete handler if the last action was a completion.
;; TODO: add cmd+left and cmd+right shortcuts to go to start/end of line
;; TODO: nav functions for moving between clauses, sentences, and paragraphs

;; TODO: Handle resizing of the text area
;; TODO: add support for ordered and unordered lists
;; TODO: Handle case of click, hold, type some stuff, THEN release
;; TODO: Make a React element that encapsulates the editor. This should
;; live at the app level, not the Slate library level.

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))
(def para1 (paragraph (uuid "p1") [(run "Hello world, this is an example of a paragraph ")
                                   (run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                                   (run "Don't know what else to say. Hmmmm..." #{:bold})]))
(def para2 (paragraph (uuid "p2") [(run)]))
(def para3 (paragraph (uuid "p3") [(run "And this is paragraph número dos.")]))
(def doc (document [para1 para2 para3]))

(def *ui-state (sl/init :editor-state (editor-state doc)
                        :dom-elem fake-editor
                        :hidden-input hidden-input))

(defn update-formats-elem
  [key atom old-state new-state]
  (let [sel (:selection (history/current-state (:history new-state)))
        formats-str (str "Formats: " (str/join \, (:formats sel)))
        elem (js/document.getElementById "formats")]
    (set! (.-innerHTML elem) formats-str)))
(update-formats-elem nil nil nil @*ui-state)

(add-watch *ui-state :formats-watcher update-formats-elem)

(defn main [])

(defn ^:dev/after-load reload []
  #_(sync-dom @state))

