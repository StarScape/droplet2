(ns drop.app.main
  (:require [clojure.core.async :refer-macros [go]]
            [drop.app.components.core :as components]
            [drop.app.components.slate-editor :as slate-editor]
            [drop.utils :as utils]
            [reagent.dom :as rdom]
            [orchestra-cljs.spec.test :as st]))

;; BUG: persistent atom causes issues with live-reloading.
;; It's nothing with persistent-atom specifically; something is happen when file loading

;; TODO: try a different approach with click/drag events that fall outside of the paragraph: make an invisible :after element for
;; each one with a width of 100vw, give it a high z-index, and test for it clicking clicked, dragged on rather than doing math to
;; find an overlapping element
;; EDIT: holy shit, there is a document.elementFromPoint method that I never knew existed...

;; TODO: put the hidden input _at_ the cursor, like VS code does. Just moving it to the current slate-caret element every time
;; sync-dom is called might be a reasonable solution

;; TODO: when cursor is at or near the bottom of the screen, auto-scroll down to it.
;; Or, an alternative, have a 'locked-on' mode where, when enter is hit, the app always
;; auto-scrolls so that the cursor is the same distance down the screen that it was previously

;; TODO: auto-transparent-tify actionbar when in fullscreen, after period of inactivity

;; TODO: fullscreen for windows and linux, plus shortcuts for entering and exiting fullscreen
;; TODO: find and replace UI
;; TODO: wordcount UI
;; TODO: command palette
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
  (slate-editor/on-startup)
  (mount-main-component))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load reload []
  (mount-main-component))
