(ns drop.app.main
  (:require [clojure.string :as str]
            [slate.model.run :refer [run]]
            [slate.model.paragraph :refer [paragraph]]
            [slate.model.doc :refer [document]]
            [slate.model.editor-state :refer [editor-state]]
            [slate.model.history :as history]
            [slate.core :as sl]))

;; DONE: Keep track of :formats on Selection object
;; DONE: update (selection) constructor to allow a single [] arg followed by kw-args

;; TODO: separate keymappings for macOS
;; TODO: undo/redo

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

