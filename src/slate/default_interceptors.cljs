(ns slate.default-interceptors
  (:require-macros [slate.interceptors :refer [definterceptor]])
  (:require [slate.interceptors]
            [slate.model.common :as m]
            [slate.model.editor-state :as es]
            [slate.model.navigation :as nav]
            [slate.view :as view]))

;; TODO: can separate all the interceptors and their implementations (including those in view.cljs,
;; which could be changed into interceptors) out into an "interceptors" namespace maybe?

;; Default set of interceptors that are added to a new editor.
;;
;; Interceptors are meant for extensibility, but most of the default events are also
;; implemented using them, the idea being that eating our own dogfood is the only
;; way to end up with a sufficiently flexible and ergonomic system.

;; TODO: for interceptors that aren't dependent on the viewmodel or DOM state, there's no reason
;; I couldn't write unit tests for them. Should probably do that, so I can quickly identify any
;; regressions. However, probably a good idea to wait until the history system is in place and
;; stable, so we can test the whole shebang.

(definterceptor click
  [editor-state ui-state event]
  (let [new-sel (view/mouse-event->selection event
                                             (:doc editor-state)
                                             (:viewmodels ui-state)
                                             (:measure-fn ui-state))]
    (es/set-selection editor-state new-sel)))

(definterceptor drag
  [editor-state ui-state event]
  (es/set-selection editor-state (view/drag event
                                            (:doc editor-state)
                                            (:viewmodels ui-state)
                                            (:measure-fn ui-state))))

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

(def default-interceptors
  {:click click
   :drag drag
   :insert insert
   :delete delete
   :enter enter
   :tab tab
   :left left
   :ctrl+left ctrl+left
   :shift+left shift+left
   :ctrl+shift+left ctrl+shift+left
   :right right
   :ctrl+right ctrl+right
   :shift+right shift+right
   :ctrl+shift+right ctrl+shift+right
   :down down
   :shift+down shift+down
   :up up
   :shift+up shift+up
   #_#_"\"" (fn [{:keys [doc selection], :as state} _e _default-interceptor]
          ;; TODO: add logic for only auto-surrounding at appropriate times,
          ;; e.g. NOT when next char is alphanumeric, or previous is. Also add a case
          (let [new-doc (m/insert doc selection "\"\"")
                new-selection (sel/shift-single selection 1)]
            (assoc state
                   :doc new-doc
                   :selection new-selection)))})