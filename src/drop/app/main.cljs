(ns drop.app.main
  (:require [drop.app.components.core :as components]
            [drop.app.components.slate-editor :as slate-editor]
            [drop.utils :as utils]
            [reagent.dom :as rdom]
            [orchestra-cljs.spec.test :as st]))

;; TODO: Bug - focus breaks when clicking off to one side

;; Prioritized:
;; PROG: Test with large documents (100 pages)
;; TODO: Drag n drog files
;; TODO: Change tab rendering from em space to <span> so that width can be styled
;; TODO: Better actionbar timeout when in fullscreen (maybe copy iA Writer)
;; TODO: Marketing website
;; TODO: More pastes tests
;; TODO: Fullscreen for Windows and Linux, plus shortcuts for entering and exiting fullscreen
;; TODO: Dark mode
;; TODO: Space out paragraphs somewhat
;; TODO: Bug - pressing fn+f to go to fullscreen still enters an 'f' inside the document
;; TODO: Break out all history fns into a protocol and implement for UIState
;; TODO: Rework/get rid of most of the slate.model.common protocols
;; TODO: Implement highlight part of find and replace
;; TODO: Further polish criteria for history getting added to the backstack

;; Usability/Polish:
;; TODO: Double/triple click should select word/paragraph
;; TODO: When a paragraph is already an h1/h2, "1. " autocomplete should not make it a list para
;; TODO: when cursor is at or near the bottom of the screen, auto-scroll down to it.
;; Or, an alternative, have a 'locked-on' mode where, when enter is hit, the app always
;; auto-scrolls so that the cursor is the same distance down the screen that it was previously
;; TODO: shorten interval between cmd++'s and cmd+-'s. Current difference in font size is too large.

;; TODO: command palette
;; TODO: learn about React Spring
;; TODO: learn about DataScript
;; TODO: Make so that cmd+i, cmd+b, etc only get added to history when done with a range selection (how much do I care?)
;; TODO: bug in manual interceptors when upgrading to lastest shadow-cljs; to repro, upgrade shadow-cljs and then fire the save interceptor with cmd+
;; BUG: persistent atom causes issues with live-reloading. Nothing with persistent-atom specifically; something happening when file loading

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
