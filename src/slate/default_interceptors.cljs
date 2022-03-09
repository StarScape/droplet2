(ns slate.default-interceptors
  "Default set of interceptors that are added to a new editor.

  Interceptors are meant for extensibility, but most of the default events are also
  implemented using them, the idea being that eating our own dogfood is the only
  way to end up with a sufficiently flexible and ergonomic system."
  (:require-macros [slate.interceptors :refer [definterceptor]])
  (:require [slate.interceptors]
            [slate.model.common :as m]
            [slate.model.editor-state :as es :refer [>>=]]
            [slate.model.navigation :as nav]
            [slate.utils :as utils]
            [slate.view :as view]))

;; TODO: for interceptors that aren't dependent on the viewmodel or DOM state, there's no reason
;; I couldn't write unit tests for them. Should probably do that, so I can quickly identify any
;; regressions. However, probably a good idea to wait until the history system is in place and
;; stable, so we can test the whole shebang.

(definterceptor insert
  [editor-state _ e]
  (m/insert editor-state (.-data e)))

(definterceptor delete
  [editor-state _ui-state _e]
  (m/delete editor-state))

(definterceptor enter
  [editor-state _ui-state _e]
  (es/enter editor-state))

(definterceptor tab
  [editor-state _ui-state _e]
  (m/insert editor-state "\u2003"))

;; Movement / navigation ;;
(definterceptor click
  {:include-in-history? false}
  [editor-state ui-state event]
  (let [new-sel (view/mouse-event->selection event
                                             (:doc editor-state)
                                             (:viewmodels ui-state)
                                             (:measure-fn ui-state))]
    (es/set-selection editor-state new-sel)))

(definterceptor drag
  {:include-in-history? false}
  [editor-state ui-state event]
  (es/set-selection editor-state (view/drag event
                                            (:doc editor-state)
                                            (:viewmodels ui-state)
                                            (:measure-fn ui-state))))

(definterceptor left
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/prev-char editor-state))

(definterceptor ctrl+left
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/prev-word editor-state))

(definterceptor shift+left
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/shift+left editor-state))

(definterceptor ctrl+shift+left
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/ctrl+shift+left editor-state))

(definterceptor right
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/next-char editor-state))

(definterceptor ctrl+right
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/next-word editor-state))

(definterceptor shift+right
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/shift+right editor-state))

(definterceptor ctrl+shift+right
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/ctrl+shift+right editor-state))

(definterceptor down
  {:include-in-history? false}
  [editor-state ui-state _e]
  (view/down editor-state (:viewmodels ui-state) (:measure-fn ui-state)))

(definterceptor shift+down
  {:include-in-history? false}
  [editor-state ui-state _e]
  (view/shift+down editor-state (:viewmodels ui-state) (:measure-fn ui-state)))

(definterceptor up
  {:include-in-history? false}
  [editor-state ui-state _e]
  (view/up editor-state (:viewmodels ui-state) (:measure-fn ui-state)))

(definterceptor shift+up
  {:include-in-history? false}
  [editor-state ui-state _e]
  (view/shift+up editor-state (:viewmodels ui-state) (:measure-fn ui-state)))

(definterceptor start-of-line
  {:include-in-history? false}
  [editor-state ui-state _e]
  (view/start-of-line editor-state (:viewmodels ui-state)))

(definterceptor end-of-line
  {:include-in-history? false}
  [editor-state ui-state _e]
  (view/end-of-line editor-state (:viewmodels ui-state)))

;; Auto-surrounding ;;
(definterceptor auto-surround-double-quote
  {:add-to-history-immediately? true}
  [editor-state _ui-state _e]
  (es/auto-surround editor-state \"))

(definterceptor auto-surround-single-quote
  {:add-to-history-immediately? true}
  [editor-state _ui-state _e]
  (es/auto-surround editor-state \'))

(definterceptor auto-surround-paren
  {:add-to-history-immediately? true}
  [editor-state _ui-state _e]
  (es/auto-surround editor-state "(" ")"))

;; Autocompletions ;;
(definterceptor transform-double-dash-to-em-dash
  {:add-to-history-immediately? true}
  [editor-state _ _]
  (-> (m/delete editor-state)
      (>>= m/delete)
      (>>= m/insert "—")))

;; Formatting
(definterceptor ctrl+i
  [editor-state _ _]
  (m/toggle-format editor-state :italic))

(definterceptor ctrl+b
  [editor-state _ _]
  (m/toggle-format editor-state :bold))

(def universal-interceptors
  {:click click
   :drag drag
   :insert insert
   :delete delete
   :enter enter
   :tab tab
   :left left
   :shift+left shift+left
   :right right
   :shift+right shift+right
   :ctrl+shift+right ctrl+shift+right
   :down down
   :shift+down shift+down
   :up up
   :shift+up shift+up
   "\"" auto-surround-double-quote
   "'" auto-surround-single-quote
   "(" auto-surround-paren
   "-- " transform-double-dash-to-em-dash})

(def win-linux-interceptors
  {:ctrl+left ctrl+left
   :ctrl+shift+left ctrl+shift+left
   :ctrl+right ctrl+right
   :ctrl+shift+right ctrl+shift+right
   :ctrl+i ctrl+i
   :ctrl+b ctrl+b
   ;; TODO: page up and page down shortcuts for win/linux
   })

(def mac-interceptors
  {:alt+left ctrl+left
   :alt+shift+left ctrl+shift+left
   :alt+right ctrl+right
   :alt+shift+right ctrl+shift+right
   :cmd+i ctrl+i
   :cmd+b ctrl+b
   :cmd+right end-of-line
   :cmd+left start-of-line})

(def default-interceptors
  (merge universal-interceptors (if (utils/is-mac?)
                                  mac-interceptors
                                  win-linux-interceptors)))
