(ns slate.events
  (:require [clojure.string :as str]
            #_[slate.events :as events :refer [interceptors fire-interceptor]]
            [slate.core :as sl]
            [slate.editor :as editor]
            [slate.navigation :as nav]
            [slate.selection :as sel]
            [slate.view :as view]))

;; Maximum number of keys typed to remember
(def ^:const max-input-history 10)

(defn add-key-to-history
  "Adds the key to the vector of input history, dropping the least recent key
   typed off the history vector if necessary, and returns the new history vector."
  [history key]
  (conj (if (< (count history) max-input-history)
          history
          (subvec history 1))
        key))

(defn fire-interceptor
  "Calls the interceptor with the current editor state and the Event object as its args
   (and optionally, any additional args you wish to pass it) and re-synces the DOM.

   If no interceptor function is provided, the event will be parsed and the matching registered
   interceptor (if any) will be fired (TODO TODO TODO)."
  [interceptor-fn state-atom event & args]
  (let [transaction (apply interceptor-fn @state-atom event args)
        new-state (editor/apply-transaction! state-atom transaction)]
    (editor/sync-dom new-state)))

;; TODO (maybe, here for my mental posterity): this can be changed to a `find-interceptor`
;; function that takes an event and a map of all the interceptors and returns
;; one if it exists or null otherwise (maybe a no-op otherwise?). This will also
;; give us more flexibility in defining how events cascade (if at all?) and allow
;; modifier keys to be written in any order.

;; TODO: once I incorporate automatic parsing of events, can turn this into a multimethod/protocol
;; and implement instances for different sublcasses of Events :)
(defn parse-event [e]
  (let [modifiers (cond-> (transient [])
                    (.-ctrlKey e) (conj! :ctrl)
                    (.-altKey e) (conj! :alt)
                    (.-shiftKey e) (conj! :shift)
                    :then (persistent!))
        key (case (.-key e)
              "ArrowLeft" :left
              "ArrowRight" :right
              "ArrowUp" :up
              "ArrowDown" :down
              "Tab" :tab
              (-> (.-key e) .toLowerCase))]
    (set (conj modifiers key))))

(def valid-interceptor-keywords
  ^{:doc "Set of legal modifier keys and events for an interceptor."}
  #{"ctrl" "shift" "alt" "up" "down" "left" "right" "enter" "tab" "click" "drag" "insert" "delete"})

(defn valid-interceptor-key?
  "Returns true if key (a string) is a valid interceptor key, otherwise false."
  [key]
  {:pre [(string? key)]}
  (cond
    (= 1 (count key)) true ;; Single char is just input
    (valid-interceptor-keywords key) true ;; otherwise confirm it is an allowed key
    :else false))

(defn validate-keys
  "Validates that every key in the interceptor pattern is a legal one. (For example, :ctrk+:shyft+a is illegal.)
   Will throw an error if an illegal key is for in the pattern, otherwise returns the collection of keys supplied."
  [keys]
  (doseq [key keys]
    (when-not (valid-interceptor-key? key)
      (throw (js/Error. (str "Slate error: " key " is not a validate key in an interceptor!")))))
  keys)

(defn reg-interceptor
  "TODO: docstring"
  [interceptors-map pattern interceptor-fn]
  (condp = (type pattern)
    Keyword (assoc interceptors-map
                   (->> (str/split (name pattern) "+")
                        (validate-keys)
                        (map keyword)
                        (set))
                   interceptor-fn)
    js/String "TODO: make reverse map doohicky"))

(defn reg-interceptors [interceptors-map new-interceptors]
  (reduce-kv reg-interceptor interceptors-map new-interceptors))

(defn init-default-events [editor-state-atom editor-elem hidden-input]
  (let [get-interceptor (fn [key]
                          (get (:interceptors @editor-state-atom) key))]
    ;; TODO: should probably add an explanatory comment about the relationship between these three events
    (let [clicked? (atom false :validator boolean?)
          mousedown-event (atom nil :validator #(instance? js/MouseEvent %))]
      (.addEventListener editor-elem "mousedown"
        (fn [e]
          (.preventDefault e)
          (.focus hidden-input)

          (reset! clicked? true)
          (reset! mousedown-event e)
          (fire-interceptor (get-interceptor #{:click}) editor-state-atom e)))

      (.addEventListener js/window "mousemove"
        (fn [e]
          (when @clicked?
            (fire-interceptor (get-interceptor #{:drag}) editor-state-atom e @mousedown-event))))

      (.addEventListener js/window "mouseup"
        (fn [_e]
          (reset! clicked? false))))

    (.addEventListener hidden-input "keydown"
      (fn [e]
        (when-let [interceptor-fn (get-interceptor (parse-event e))]
          (.preventDefault e)
          (fire-interceptor interceptor-fn editor-state-atom e))))

    (.addEventListener hidden-input "beforeinput"
      (fn [e]
        (case (.-inputType e)
          "insertText" (fire-interceptor (get-interceptor #{:insert}) editor-state-atom e)
          "deleteContentBackward" (fire-interceptor (get-interceptor #{:delete}) editor-state-atom e)
          nil)))))

(def default-interceptors
  {:click (fn [state e]
            (let [new-sel (view/mouse-event->selection e state (:measure-fn state))]
              (assoc state :selection new-sel)))
   :drag (fn [state mousemove-event mousedown-event]
           (update state :selection #(view/drag mousedown-event mousemove-event state (:measure-fn state))))

   :insert (fn [{:keys [doc selection] :as state} e]
             (let [text (.-data e)
                   new-doc (sl/insert doc selection text)
                   new-selection (sel/shift-single selection (count text))]
               (assoc state :doc new-doc :selection new-selection)))
   :delete (fn [{:keys [doc selection] :as state} _e]
             (let [[new-doc, new-sel] (sl/delete doc selection)]
               (assoc state :doc new-doc :selection new-sel)))
   :enter (fn [{:keys [doc selection] :as state} _e]
            (let [[new-doc, new-sel] (sl/enter doc selection)]
              (assoc state :doc new-doc :selection new-sel)))
   :tab (fn [{:keys [doc selection] :as state} _e]
          (let [new-doc (sl/insert doc selection "\u2003")
                new-selection (sel/shift-single selection 1)]
            (assoc state :doc new-doc :selection new-selection)))

   :left (fn [state _e]
           (update state :selection #(nav/prev-char (:doc state) %)))
   :ctrl+left (fn [state _e]
                (update state :selection #(nav/prev-word (:doc state) %)))
   :shift+left (fn [state _e]
                 (update state :selection #(nav/shift+left (:doc state) (:selection state))))
   :ctrl+shift+left (fn [state _e]
                      (update state :selection #(nav/ctrl+shift+left (:doc state) (:selection state))))

   :right (fn [state _e]
            (update state :selection #(nav/next-char (:doc state) %)))
   :ctrl+right (fn [state _e]
                 (update state :selection #(nav/next-word (:doc state) %)))
   :shift+right (fn [state _e]
                  (update state :selection #(nav/shift+right (:doc state) (:selection state))))
   :ctrl+shift+right (fn [state _e]
                       (update state :selection #(nav/ctrl+shift+right (:doc state) (:selection state))))

   :down (fn [state _e]
           (update state :selection #(view/down state (:measure-fn state))))
   :shift+down (fn [state _e]
                 (update state :selection #(view/shift+down state (:measure-fn state))))
   :up (fn [state _e]
         (update state :selection #(view/up state (:measure-fn state))))
   :shift+up (fn [state _e]
               (update state :selection #(view/shift+up state (:measure-fn state))))})
