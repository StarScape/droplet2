(ns slate.events)

;; ;; TODO: this can be changed to a `find-interceptor` function that takes
;; ;; an event and a map of all the interceptors and returns one if it exists
;; ;; or null otherwise (maybe a no-op otherwise?). This will also give us more
;; ;; flexibility in defining how events cascade (if at all?) and allow modifier
;; ;; keys to be written in any order.
;; (defn parse-event [e]
;;   (let [modifiers (cond-> (transient [])
;;                     (.-ctrlKey e) (conj! "ctrl")
;;                     (.-altKey e) (conj! "alt")
;;                     (.-shiftKey e) (conj! "shift")
;;                     :always (persistent!))
;;         key (case (.-key e)
;;               "ArrowLeft" "left"
;;               "ArrowRight" "right"
;;               "ArrowUp" "up"
;;               "ArrowDown" "down"
;;               "Tab" "tab"
;;               (-> (.-key e) .toLowerCase))]
;;     (->> (conj modifiers key)
;;          (str/join "+")
;;          (keyword))))

;; (defn fire-interceptor
;;   "Calls the interceptor with the provided args (typically
;;    state and the Event object) and re-synces the DOM."
;;   [interceptor-fn & args]
;;   (let [old-state @doc-state
;;         changed (apply interceptor-fn args)
;;         doc (get changed :doc (:doc old-state))
;;         new-state (-> (merge old-state changed)
;;                       (assoc :viewmodels (vm/from-doc doc 200 measure-fn)))]
;;     (reset! doc-state new-state)
;;     (sync-dom @doc-state fake-editor)))

;; ;; TODO: change to a reg-interceptors! function call.
;; (def interceptors
;;   {:click (fn [state e]
;;             (let [new-sel (view/mouse-event->selection e state measure-fn)]
;;               (assoc state :selection new-sel)))
;;    :drag (fn [state mousedown-event mousemove-event]
;;            (update state :selection #(view/drag mousedown-event mousemove-event state measure-fn)))

;;    :insert (fn [{:keys [doc selection] :as state} e]
;;              (let [text (.-data e)
;;                    new-doc (sl/insert doc selection text)
;;                    new-selection (sel/shift-single selection (count text))]
;;                (assoc state :doc new-doc :selection new-selection)))
;;    :delete (fn [{:keys [doc selection] :as state} _e]
;;              (let [[new-doc, new-sel] (sl/delete doc selection)]
;;                (assoc state :doc new-doc :selection new-sel)))
;;    :enter (fn [{:keys [doc selection] :as state} _e]
;;             (let [[new-doc, new-sel] (sl/enter doc selection)]
;;               (assoc state :doc new-doc :selection new-sel)))
;;    :tab (fn [{:keys [doc selection] :as state} _e]
;;           (let [new-doc (sl/insert doc selection "\u2003")
;;                 new-selection (sel/shift-single selection 1)]
;;             (assoc state :doc new-doc :selection new-selection)))

;;    :left (fn [state _e]
;;            (update state :selection #(nav/prev-char (:doc state) %)))
;;    :ctrl+left (fn [state _e]
;;                 (update state :selection #(nav/prev-word (:doc state) %)))
;;    :shift+left (fn [state _e]
;;                  (update state :selection #(nav/shift+left (:doc state) (:selection state))))
;;    :ctrl+shift+left (fn [state _e]
;;                       (update state :selection #(nav/ctrl+shift+left (:doc state) (:selection state))))

;;    :right (fn [state _e]
;;             (update state :selection #(nav/next-char (:doc state) %)))
;;    :ctrl+right (fn [state _e]
;;                  (update state :selection #(nav/next-word (:doc state) %)))
;;    :shift+right (fn [state _e]
;;                   (update state :selection #(nav/shift+right (:doc state) (:selection state))))
;;    :ctrl+shift+right (fn [state _e]
;;                        (update state :selection #(nav/ctrl+shift+right (:doc state) (:selection state))))

;;    :down (fn [state _e]
;;            (update state :selection #(view/down state measure-fn)))
;;    :shift+down (fn [state _e]
;;                  (update state :selection #(view/shift+down state measure-fn)))
;;    :up (fn [state _e]
;;          (update state :selection #(view/up state measure-fn)))
;;    :shift+up (fn [state _e]
;;                (update state :selection #(view/shift+up state measure-fn)))})
