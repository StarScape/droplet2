(ns drop.app.main
  (:require [clojure.string :as str]
            [slate.model.run :refer [run]]
            [slate.model.paragraph :refer [paragraph]]
            [slate.model.doc :refer [document]]
            [slate.model.editor-state :refer [editor-state] :as es]
            [slate.editor-ui-state :as ui-state]
            [slate.model.history :as history]
            [slate.core :as sl]))

;; DONE: add cmd+left and cmd+right shortcuts to go to start/end of line
;; DONE: Handle resizing of the text area
;; DONE: Handle font resizing of the text area

;; TODO: Add support for h1 and h2
;;   DONE: support rendering h1 and h2 correctly
;;   TODO: fix click and other measurement operations
;;   TODO: support preserving type on pressing enter

;; TODO: Add support for ordered and unordered lists (prefer using actual <ul> / <ol> elements)
;; TODO: Copy and paste
;; TODO: Find and replace
;; TODO: Nav functions for moving between clauses, sentences, and paragraphs
;; TODO: Probably worth breaking out all of the history fns into a protocol and also implementing it for UIState
;; TODO: Make so that cmd+i, cmd+b, etc only get added to history when done with a range selection (how much do I care?)
;; TODO: Handle case of click, hold, type some stuff, THEN release
;; TODO: Make a React element that encapsulates the editor. This should live at the app level, not in Slate.
;; TODO: Set up Electron
;; TODO: File saving/loading
;; TODO: Interface design/impl

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))

(def para0 (paragraph "p0" :h1 [(run "A Title")]))
(def para05 (paragraph "p05" :h2 [(run "A subtitle")]))
(def para1 (paragraph (uuid "p1") [(run "Hello world, this is an example of a paragraph ")
                                   (run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                                   (run "Don't know what else to say. Hmmmm..." #{:bold})]))
(def para2 (paragraph (uuid "p2") [(run)]))
(def para3 (paragraph (uuid "p3") [(run "And this is paragraph número dos.")]))
(def doc (document [para0 para05 (paragraph [(run)]) para1 para2 para3]))

(def *ui-state (sl/init! :editor-state (editor-state doc)
                         :dom-elem fake-editor
                         :hidden-input hidden-input))

(defn update-formats-elem
  [_key _atom _old-state new-state]
  (let [sel (:selection (history/current-state (:history new-state)))
        formats-str (str "Formats: " (str/join \, (:formats sel)))
        elem (js/document.getElementById "formats")]
    (set! (.-innerHTML elem) formats-str)))

(defn main []
  (update-formats-elem nil nil nil @*ui-state)
  (add-watch *ui-state :formats-watcher update-formats-elem))

(defn ^:dev/after-load reload []
  #_(sync-dom @state))

