(ns drop.app.main
  (:require [clojure.string :as str]
            [slate.editor-ui-state :as ui-state]
            [slate.model.run :refer [run]]
            [slate.model.paragraph :refer [paragraph]]
            [slate.model.doc :refer [document]]
            [slate.model.editor-state :refer [editor-state] :as es]
            [slate.model.history :as history]
            [slate.model.selection :as sel]
            [slate.core :as sl]
            [slate.utils :as utils]))

;; DONE: Undo broken when trying to undo the insertion of a paragraph.

;; PROG: Copy and paste to/from droplet doc
;; DONE: Bug - select two paragraphs, copy, paste directly after, then press up -- cursor is duplicated incorrectly.
;; DONE: Bug - select list paragraph, copy, paste, then undo results in error thrown
;; DONE: selected-content returns list of Paragraphs when multiple whole paragraphs selected, but list of runs when
;;       a single paragraph is selected in its entirety.
;; DONE: implement cut

;; TODO: Copy and paste to/from outside source
;;   TODO: plain text
;;   TODO: rich text

;; TODO: Find and replace
;; TODO: Nav functions for moving between clauses, sentences, and paragraphs
;; TODO: cmd+shift+right, cmd+shift+left
;; TODO: when _only_ going up and down, support remembering the pixel offset where the up/down operation _began_, instead of
;;       just going up/down from the previous. The remembering should be cancelled if any other operation is performed, including
;;       navigation with left/right arrows, inserting text, etc. Ideally all logic for this should be _confined_ to the up/down (and possibly
;;       left/right) interceptors.
;; TODO: don't fire ' autocomplete if char immediately before or after cursor is alphanumeric,
;; TODO: Investigate fonts that looks good _without_ kerning (Merriweather seems to do well)
;; TODO: Probably worth breaking out all of the history fns into a protocol and also implementing it for UIState
;; TODO: Make so that cmd+i, cmd+b, etc only get added to history when done with a range selection (how much do I care?)
;; TODO: Handle case of click, hold, type some stuff, THEN release
;; TODO: Make a React element that encapsulates the editor. This should live at the app level, not in Slate.
;; TODO: Set up Electron
;; TODO: File saving/loading
;; TODO: Interface design/impl

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))

(def paragraphs
  [(paragraph (uuid "p1") :h1 [(run "A Title")])
   (paragraph (uuid "p2") :h2 [(run "A subtitle")])
   (paragraph (uuid "s1") :ul [(run "A bullet")])
   (paragraph (uuid "s2") :ul [(run "And anotha")])
   (paragraph (uuid "div1") [(run)])
   (paragraph (uuid "p3") [(run "Hello world, this is an example of a paragraph ")
                           (run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                           (run "Don't know what else to say. Hmmmm..." #{:bold})])
   (paragraph (uuid "p4") [(run "And another paragraph here")])
   (paragraph (uuid "div2") [(run)])
   (paragraph (uuid "ol1") :ul [(run "Bullet 1")])
   (paragraph (uuid "ol2") :ul [(run "Bullet 2")])
   (paragraph (uuid "ol3") :ul [(run "Bullet 3")])
   (paragraph (uuid "emptyboi")[(run "")])
   (paragraph [(run "(Take a break.)")])
   (paragraph (uuid "ul1") :ol [(run "Ordered item 1")])
   (paragraph (uuid "ul2") :ol [(run "Ordered item 2")])
   (paragraph (uuid "ul3") :ol [(run "Ordered item 3")])
   (paragraph (uuid "div3") [(run)])
   (paragraph (uuid "p5") [(run "And this is paragraph número dos.")])])
(def doc (document paragraphs))

(def *ui-state (sl/init! :editor-state (editor-state doc (sel/selection :start [(uuid "p1") 0]
                                                                        :end [(uuid "p1") 7]))
                         :dom-elem fake-editor
                         :hidden-input hidden-input))

(set! js/dumpHistory #(js/console.log (utils/pretty-history-stack (:history @*ui-state))))

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

