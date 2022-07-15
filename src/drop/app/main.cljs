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

;; BUG: persistent atom causes issues with live-reloading.
;; It's nothing with persistent-atom specifically; something is happen when file loading

;; BUG: italicize the last work in the document, up till the very end. It will italicize correctly but the italic button is not hightlighted.
;; It's not consistent, I think the styles on the selection object are simply not getting updated correctly.

;; TODO: Investigate fonts that looks good _without_ kerning (Merriweather seems to do well)
;; TODO: Look into React animation libraries
;; TODO: Interface design/impl
;; TODO: DOCX export/import w/ pandoc

;; TODO: Make so that cmd+i, cmd+b, etc only get added to history when done with a range selection (how much do I care?)
;; TODO: Probably worth breaking out all of the history fns into a protocol and also implementing it for UIState
;; TODO: Copy (and maybe paste) rich text

;; TODO: bug in manual interceptors when upgrading to lastest shadow-cljs; to
;; repro, upgrade shadow-cljs and then fire the save interceptor with cmd+s

(defn mount-main-component []
  (let [elem (js/document.getElementById "reagent-main")]
    (rdom/render [components/app] elem)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn main []
  (mount-main-component))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load reload []
  (mount-main-component))
