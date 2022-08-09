(ns drop.app.main
  (:require [drop.app.components.core :as components]
            [drop.utils :as utils]
            [reagent.dom :as rdom]
            [orchestra.core :refer-macros [defn-spec]]
            [orchestra-cljs.spec.test :as st]
            [clojure.spec.alpha :as s]))

;; BUG: persistent atom causes issues with live-reloading.
;; It's nothing with persistent-atom specifically; something is happen when file loading

;; TODO: put the hidden input _at_ the cursor, like VS code does. Just moving it to the current slate-caret element every time
;; sync-dom is called might be a reasonable solution

;; TODO: try a different approach with click/drag events that fall outside of the paragraph: make an invisible :after element for
;; each one with a width of 100vw, give it a high z-index, and test for it clicking clicked, dragged on rather than doing math to
;; find an overlapping element
;; EDIT: holy shit, there is a document.elementFromPoint method that I never knew existed...

;; TODO: render font locally
;; TODO: fix menubar

;; TODO: Set app title to include current file name
;; TODO: add * to app title bar when file has been modified but not saved
;; TODO: Ensure that app behaves fails gracefully when last opened file no longer exists

;; TODO: fullscreen
;; TODO: find and replace UI
;; TODO: wordcount UI
;; TODO: command pallette
;; TODO: DOCX import/export
;; TODO: Look into React animation libraries
;; TODO: Interface design/impl
;; TODO: DOCX export/import w/ pandoc

;; TODO: Make so that cmd+i, cmd+b, etc only get added to history when done with a range selection (how much do I care?)
;; TODO: Probably worth breaking out all of the history fns into a protocol and also implementing it for UIState
;; TODO: Copy (and maybe paste) rich text

;; TODO: bug in manual interceptors when upgrading to lastest shadow-cljs; to
;; repro, upgrade shadow-cljs and then fire the save interceptor with cmd+

(when utils/DEV
  (st/instrument))

(defn mount-main-component []
  (let [elem (js/document.getElementById "reagent-main")]
    (rdom/render [components/app] elem)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn main []
  (mount-main-component))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load reload []
  (mount-main-component))
