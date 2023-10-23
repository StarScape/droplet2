(ns drop.app.main
  (:require [drop.app.events] ;; These two are only required to make the compiler
            [drop.app.subs]
            [drop.app.effects]
            [drop.app.ipc :as ipc]
            [drop.app.components.core :as components]
            [drop.app.components.slate-editor :as slate-editor]
            [drop.utils :as utils]
            [reagent.dom :as rdom]
            [re-frame.core :as rf]
            [re-frame.db :as db]
            [orchestra-cljs.spec.test :as st]
            ["electron" :refer [ipcRenderer #_desktopCapturer]]))

;; TODO: Add ability for page break
;; TODO: Add print
;; TODO: Change actionbar to shut off once user starts typing
;; TODO: I would like it if the paragraph automatically cleaned itself up s.t. spaces that are between unstyled text,
;; or hanging off the front/end of it, cannot be italic/bold/whatever
;; TODO: Right-click menu -- copy, paste, cut
;; TODO: Improve error handling for opening files. Add a case that catches any errors opening the file and automatically backs up the file to a private folder.
;;       Deliberately induce some errors.
;; TODO: cmd+w should close main window on macOS

;; TODO: Write release script that automates running tests, checking if there is a tag for existing release version in package.json (prompting
;; to create one if not, or to roll the tag version if so), building new release version for each platform, record new marketing videos using
;; automated process, pushing those to the S3 repo, and pushing a new version of the website.

;; TODO: Clicking off to the left/right side of text should be equivalent to clicking start/end of line
;; TODO: going to start of sentence that is at the beginning of an indented paragraph should not select the leading tab

;; FEATURES

;; PROG: Test with large documents (100 pages)
;; PROG: Test with large documents (1,000 pages)
;; TODO: Tabs
;; TODO: File browser
;; TODO: Right-click context menu
;; TODO: Drag n drog files
;; TODO: Rewrite render engine with Canvas approach?
;; TODO: Add auto-updates capability
;; TODO: Add automated crash reporting
;; TODO: Add payment system
;; TODO: Add spellcheck
;; TODO: Add ⌃F/Alt+F/Hold shortcut button to display all shortcuts in actionbar
;; TODO: Add Command Palette
;; TODO: Make work with IME (https://w3c.github.io/uievents/#events-compositionevents)
;; TODO: Fullscreen for Windows and Linux, plus shortcuts for entering and exiting fullscreen
;; TODO: Bug - pressing fn+f to go to fullscreen still enters an 'f' inside the document
;; TODO: Implement highlight part of find and replace
;; TODO: Further polish criteria for history getting added to the backstack


;; USABILITY/POLISH

;; TODO: Right click on Dock Icon -> open file
;; TODO: When merging two paragraphs, the merged paragraph has the :type of the first. Make it have the type of the second **iff** the first is blank.
;; TODO: Select from end of one paragraph down into the next/multiple, there should be a "nub" at the end of the first one
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

;; CORE CLEANUP

;; TODO: Evaluate the wisdom of changing ParagraphFragment and DocumentFragment to just Paragraph and Document, respectively (don't for now)
;; TODO: Possibly remove Selectable protocol in common.cljs
;; TODO: Remove all multimethods and protocols in common.cljs that don't have a consistent interface across ALL types, or have no need for polymorphism

;; LEARNING

;; TODO: learn about React Spring
;; TODO: learn about DataScript

(when utils/DEV
  (st/instrument))

(defn mount-main-component []
  (let [elem (js/document.getElementById "reagent-main")]
    (rdom/render [components/app] elem)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn main []
  (let [theme (ipc/get-theme-sync)]
    (when (= :dark theme)
      (.. js/document -documentElement -classList (toggle "dark"))))
  (rf/dispatch-sync [:boot])
  (ipc/init-handlers!)
  (mount-main-component))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load reload []
  (.send ipcRenderer "-reload-last-file")
  (mount-main-component))
