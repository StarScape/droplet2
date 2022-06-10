(ns drop.app.components.slate-editor
  (:require [slate.editor-ui-state :as ui-state]
            [slate.model.run :refer [run]]
            [slate.model.paragraph :refer [paragraph]]
            [slate.model.doc :refer [document]]
            [slate.model.editor-state :refer [editor-state] :as es]
            [slate.model.selection :as sel]
            [slate.core :as sl]
            [slate.utils :as utils]
            [reagent.core :as r :refer [with-let]]))

;; (defn update-formats-elem
;;   [_key _atom _old-state new-state]
;;   (let [sel (:selection (history/current-state (:history new-state)))
;;         formats-str (str "Formats: " (str/join \, (:formats sel)))
;;         elem (js/document.getElementById "formats")]
;;     (set! (.-innerHTML elem) formats-str)))

(defn- init-test-editor-state []
  (let [paragraphs [(paragraph (uuid "p1") :h1 [(run "A Title")])
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
                    (paragraph (uuid "emptyboi") [(run "")])
                    (paragraph [(run "(Take a break.)")])
                    (paragraph (uuid "ul1") :ol [(run "Ordered item 1")])
                    (paragraph (uuid "ul2") :ol [(run "Ordered item 2")])
                    (paragraph (uuid "ul3") :ol [(run "Ordered item 3")])
                    (paragraph (uuid "div3") [(run)])
                    (paragraph (uuid "p5") [(run "And this is paragraph número dos.")])]
        doc (document paragraphs)
        selection (sel/selection [(uuid "p3") 1])]
    (editor-state doc selection)))

(defn slate-editor []
  (with-let [*ui-state (atom nil)
             ref-callback (fn [elem]
                            (when elem
                              (reset! *ui-state (sl/init! :editor-state (init-test-editor-state)
                                                          :dom-elem elem))
                                 ;; Utility for viewing editor history from console
                              (set! js/dumpHistory #(js/console.log (utils/pretty-history-stack (:history @*ui-state))))))]
    [:div.slate-editor {:ref ref-callback}]))
