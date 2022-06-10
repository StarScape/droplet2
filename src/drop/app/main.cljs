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
            [slate.utils :as utils]
            [drop.app.components.core :as components]
            [reagent.dom :as rdom]))

;; PROG: Set up Electron

;; TODO: File saving/loading
;; TODO: Investigate fonts that looks good _without_ kerning (Merriweather seems to do well)
;; TODO: Learn about TailwindCSS
;; TODO: Look into React animation libraries
;; TODO: Interface design/impl
;; TODO: DOCX export/import w/ pandoc

;; TODO: Make so that cmd+i, cmd+b, etc only get added to history when done with a range selection (how much do I care?)
;; TODO: Probably worth breaking out all of the history fns into a protocol and also implementing it for UIState
;; TODO: Copy (and maybe paste) rich text

(defn remove-old-hidden-inputs []
  (-> (.querySelectorAll js/document ".hidden-input")
      (.forEach (fn [elem] (.remove #p elem)))))

(defn mount-main-component []
  (let [elem (js/document.getElementById "reagent-main")]
    (rdom/render [components/app] elem)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn main []
  (mount-main-component))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load reload []
  (remove-old-hidden-inputs)
  (mount-main-component))
