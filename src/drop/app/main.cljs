(ns drop.app.main
  (:require [drop.app.events] ;; These two are only required to make the compiler
            [drop.app.subs]
            [drop.app.effects]
            [drop.app.components.core :as components]
            [drop.app.components.slate-editor :as slate-editor]
            [drop.utils :as utils]
            [reagent.dom :as rdom]
            [re-frame.core :as rf]
            [orchestra-cljs.spec.test :as st]))

;; TODO: BUG - files don't open when clicked
;; TODO: BUG - leading tab doesn't appear to be being measured correctly
;; TODO: doubling clicking a word should select it, triple clicking should select paragraph
;; TODO: going to start of sentence that is at the beginning of an indented paragraph should not select the leading tab
;; TODO: consider not changing focus when switched windows
;; TODO: selected words counter

;; CORE CLEANUP

;; TODO: Evaluate the wisdom of changing ParagraphFragment and DocumentFragment to just Paragraph and Document, respectively (don't for now)
;; TODO: Possibly remove Selectable protocol in common.cljs
;; TODO: Remove all multimethods and protocols in common.cljs that don't have a consistent interface across ALL types, or have no need for polymorphism

;; FEATURES

;; PROG: Test with large documents (100 pages)
;; PROG: Test with large documents (100 pages)
;; TODO: Add auto-updates capability
;; TODO: Add automated crash reporting
;; TODO: Add payment system
;; TODO: Add spellcheck
;; TODO: Add ⌃F/Alt+F/Hold shortcut button to display all shortcuts in actionbar
;; TODO: Add Command Palette
;; TODO: Rewrite render engine with Canvas approach
;; TODO: Tabs
;; TODO: File browser
;; TODO: Right-click context menu
;; TODO: Drag n drog files
;; TODO: Fullscreen for Windows and Linux, plus shortcuts for entering and exiting fullscreen
;; TODO: Dark mode
;; TODO: Bug - pressing fn+f to go to fullscreen still enters an 'f' inside the document
;; TODO: Implement highlight part of find and replace
;; TODO: Change tab rendering from em space to <span> so that width can be styled
;; TODO: Further polish criteria for history getting added to the backstack


;; USABILITY/POLISH

;; TODO: File -> Open Recent menu
;; TODO: Right click on Dock Icon -> open file
;; TODO: When merging two paragraphs, the merged paragraph has the :type of the first. Make it have the type of the second **iff** the first is blank.
;; TODO: Select from end of one paragraph down into the next/multiple, there should be a "nub" at the end of the first one
;; TODO: Double/triple click should select word/paragraph
;; TODO: When a paragraph is already an h1/h2, "1. " autocomplete should not make it a list para
;; TODO: when cursor is at or near the bottom of the screen, auto-scroll down to it.
;; Or, an alternative, have a 'locked-on' mode where, when enter is hit, the app always
;; auto-scrolls so that the cursor is the same distance down the screen that it was previously
;; TODO: shorten interval between cmd++'s and cmd+-'s. Current difference in font size is too large.
;; TODO: notable edge case for next/prev clause: , in middle of number, like 200,000

;; TECHNICAL IMPROVEMENTS

;; TODO: Break out all history fns into a protocol and implement for UIState
;; TODO: More pastes tests
;; TODO: bug in manual interceptors when upgrading to lastest shadow-cljs; to repro, upgrade shadow-cljs and then fire the save interceptor with cmd+
;; TODO: Graphemes - make grapheme-at and grapheme-before functions; this can wait for now
;; TODO: Make so that cmd+i, cmd+b, etc only get added to history when done with a range selection (how much do I care?)

;; LEARNING

;; TODO: learn about React Spring
;; TODO: learn about DataScript

(rf/dispatch-sync [:initialise-db])

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
