(ns drop.app.main
  (:require [drop.app.components.core :as components]
            [reagent.dom :as rdom]
            [orchestra.core :refer-macros [defn-spec]]
            [orchestra-cljs.spec.test :as st]
            [clojure.spec.alpha :as s]
            ["electron" :as e :refer [app ipcRenderer]]))

;; BUG: persistent atom causes issues with live-reloading.
;; It's nothing with persistent-atom specifically; something is happen when file loading

;; TODO: put the hidden input _at_ the cursor, like VS code does. Just moving it to the current slate-caret element every time
;; sync-dom is called might be a reasonable solution

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

;; TODO: only do if dev
(st/instrument)

(defn mount-main-component []
  (let [elem (js/document.getElementById "reagent-main")]
    (rdom/render [components/app] elem)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn main []
  (mount-main-component))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load reload []
  (mount-main-component))
