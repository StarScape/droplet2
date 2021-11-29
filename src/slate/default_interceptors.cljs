(ns slate.default-interceptors
  (:require [slate.model.editor-state :as es :refer [assoc-state]])
  (:require-macros [slate.interceptors :refer [definterceptor]]))

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
  [editor-state _ e]
  (let [new-sel (view/mouse-event->selection e editor-state (:measure-fn editor-state))]
    (assoc-state editor-state
                 :selection new-sel
                 :changelist )))

#_(def default-interceptors
  {:click (fn [state e]
            (let [new-sel
                  (view/mouse-event->selection e state (:measure-fn state))]
              (with-input-history :click (assoc state :selection new-sel))))
   :drag (fn [state mousemove-event mousedown-event]
           (update state
                   :selection
                   #(view/drag mousedown-event
                               mousemove-event
                               state
                               (:measure-fn state))))
   :insert (fn [{:keys [doc selection], :as state} e]
             (let [text (.-data e)
                   transaction (m/insert doc selection text)
                   input-for-history (if (= 1 (.-length text)) text :PASTE)] ;; TODO?
               ;; TODO:
               #_(with-input-history input-for-history
                   (assoc state
                          :doc new-doc
                          :selection new-selection))
               transaction))
   :delete (fn [{:keys [doc selection], :as state} _e]
             (let [[new-doc new-sel] (m/delete doc selection)]
               (with-input-history :delete
                 (assoc state
                        :doc new-doc
                        :selection new-sel))))
   :enter (fn [{:keys [doc selection], :as state} _e]
            (let [[new-doc new-sel] (doc/enter doc selection)]
              (with-input-history :enter
                (assoc state
                       :doc new-doc
                       :selection new-sel))))
   :tab (fn [{:keys [doc selection], :as state} _e]
          (let [new-doc (m/insert doc selection "\u2003")
                new-selection (sel/shift-single selection 1)]
            (with-input-history :tab
              (assoc state
                     :doc new-doc
                     :selection new-selection))))
   :left (fn [state _e]
           (with-input-history
             :left
             (update state :selection #(nav/prev-char (:doc state) %))))
   :ctrl+left (fn [state _e]
                (with-input-history
                  :ctrl+left
                  (update state :selection #(nav/prev-word (:doc state) %))))
   :shift+left (fn [state _e]
                 (with-input-history
                   :shift+left
                   (update state
                           :selection
                           #(nav/shift+left (:doc state) (:selection state)))))
   :ctrl+shift+left (fn [state _e]
                      (with-input-history
                        :ctrl+shift+left
                        (update state
                                :selection
                                #(nav/ctrl+shift+left (:doc state)
                                                      (:selection state)))))
   :right (fn [state _e]
            (with-input-history
              :right
              (update state :selection #(nav/next-char (:doc state) %))))
   :ctrl+right (fn [state _e]
                 (with-input-history
                   :ctrl+right
                   (update state :selection #(nav/next-word (:doc state) %))))
   :shift+right (fn [state _e]
                  (with-input-history
                    :shift+right
                    (update state
                            :selection
                            #(nav/shift+right (:doc state)
                                              (:selection state)))))
   :ctrl+shift+right (fn [state _e]
                       (with-input-history
                         :ctrl+shift+right
                         (update state
                                 :selection
                                 #(nav/ctrl+shift+right (:doc state)
                                                        (:selection state)))))
   :down (fn [state _e]
           (with-input-history
             :down
             (update state :selection #(view/down state (:measure-fn state)))))
   :shift+down (fn [state _e]
                 (with-input-history
                   :shift+down
                   (update state
                           :selection
                           #(view/shift+down state (:measure-fn state)))))
   :up (fn [state _e]
         (with-input-history
           :up
           (update state :selection #(view/up state (:measure-fn state)))))
   :shift+up
   (fn [state _e]
     (with-input-history
       :shift+up
       (update state :selection #(view/shift+up state (:measure-fn state)))))
   ;; This completion isn't actually done yet (the behavior is fairly complex
   ;; and
   ;; needs to work with range selection etc) just a good example interceptor
   ;; for testing
   "\"" (fn [{:keys [doc selection], :as state} _e _default-interceptor]
          ;; TODO: add logic for only auto-surrounding at appropriate times,
          ;; e.g. not when next char is
          ;; alphanumeric, or previous is. Also add a case for range selection,
          ;; and auto-surround selection if so.
          (let [new-doc (m/insert doc selection "\"\"")
                new-selection (sel/shift-single selection 1)]
            (assoc state
                   :doc new-doc
                   :selection new-selection)))})