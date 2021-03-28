(ns slate.events
  (:require [clojure.string :as str]
            [slate.core :as sl]
            [slate.editor :as editor]
            [slate.navigation :as nav]
            [slate.selection :as sel]
            [slate.view :as view]))

(declare add-key-to-history)

(defn with-history
  "Helper for automatically adding a value to the input history. See use below in interceptors."
  [key state]
  (update state :input-history add-key-to-history key))

;; Default set of interceptors that are added to a new editor.
(def default-interceptors
  {:click (fn [state e]
            (let [new-sel (view/mouse-event->selection e state (:measure-fn state))]
              (with-history :click
                (assoc state :selection new-sel))))
   :drag (fn [state mousemove-event mousedown-event]
           (update state :selection #(view/drag mousedown-event mousemove-event state (:measure-fn state))))

   :insert (fn [{:keys [doc selection] :as state} e]
             (let [text (.-data e)
                   new-doc (sl/insert doc selection text)
                   new-selection (sel/shift-single selection (count text))
                   new-state (assoc state :doc new-doc :selection new-selection)]
               (with-history text new-state)
               #_(cond->> new-state
                 (pos? (.-length text)) (with-history text))))
   :delete (fn [{:keys [doc selection] :as state} _e]
             (let [[new-doc, new-sel] (sl/delete doc selection)]
               (with-history :delete
                 (assoc state :doc new-doc :selection new-sel))))
   :enter (fn [{:keys [doc selection] :as state} _e]
            (let [[new-doc, new-sel] (sl/enter doc selection)]
              (with-history :enter
                (assoc state :doc new-doc :selection new-sel))))
   :tab (fn [{:keys [doc selection] :as state} _e]
          (let [new-doc (sl/insert doc selection "\u2003")
                new-selection (sel/shift-single selection 1)]
            (with-history :tab
              (assoc state :doc new-doc :selection new-selection))))

   :left (fn [state _e]
           (with-history :left
             (update state :selection #(nav/prev-char (:doc state) %))))
   :ctrl+left (fn [state _e]
                (with-history :ctrl+left
                  (update state :selection #(nav/prev-word (:doc state) %))))
   :shift+left (fn [state _e]
                 (with-history :shift+left
                   (update state :selection #(nav/shift+left (:doc state) (:selection state)))))
   :ctrl+shift+left (fn [state _e]
                      (with-history :ctrl+shift+left
                        (update state :selection #(nav/ctrl+shift+left (:doc state) (:selection state)))))

   :right (fn [state _e]
            (with-history :right
              (update state :selection #(nav/next-char (:doc state) %))))
   :ctrl+right (fn [state _e]
                 (with-history :ctrl+right
                   (update state :selection #(nav/next-word (:doc state) %))))
   :shift+right (fn [state _e]
                  (with-history :shift+right
                    (update state :selection #(nav/shift+right (:doc state) (:selection state)))))
   :ctrl+shift+right (fn [state _e]
                       (with-history :ctrl+shift+right
                         (update state :selection #(nav/ctrl+shift+right (:doc state) (:selection state)))))

   :down (fn [state _e]
           (with-history :down
             (update state :selection #(view/down state (:measure-fn state)))))
   :shift+down (fn [state _e]
                 (with-history :shift+down
                   (update state :selection #(view/shift+down state (:measure-fn state)))))
   :up (fn [state _e]
         (with-history :up
           (update state :selection #(view/up state (:measure-fn state)))))
   :shift+up (fn [state _e]
               (with-history :shift+up
                 (update state :selection #(view/shift+up state (:measure-fn state)))))})

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

(defn matching-completion?
  "Takes the editor's interceptor map and input history, and returns
   a matching completion interceptor if one exists, or nil otherwise."
  [key interceptor-map history]
  {:pre [(vector? history)]}
  (let [completions (:completions interceptor-map)
        path (reverse (conj history key))] ; [..., "c" "b", "a"]
    (loop [current-level completions
           [p & ps] path]
      (let [next (current-level p)]
        (if (or (nil? next) (fn? next))
          next
          (recur next ps))))))

;; TODO: once I incorporate automatic parsing of events, can turn this into a multimethod/protocol
;; and implement instances for different sublcasses of Events :)
;;
;; TODO: make a test with Ctrl, Alt, and Shift each by themselves and with a primary key
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
              (-> (.-key e) .toLowerCase))
        pressed-keys (cond-> modifiers
                       (not (#{"control" "shift" "alt" "meta"} key))
                       (conj key))]
    (set (map keyword pressed-keys))))

(def valid-interceptor-keywords
  ^{:doc "Set of legal modifier keys and events for an interceptor."}
  #{"ctrl" "shift" "alt" "up" "down" "left" "right" "enter" "tab" "click" "drag" "insert" "delete"})

(defn valid-interceptor-key?
  "Returns true if key (a string) is a valid interceptor key, otherwise false."
  [key]
  {:pre [(string? key)]}
  (cond
    (= 1 (count key)) true ;; Single char is just the primary key pressed
    (valid-interceptor-keywords key) true ;; otherwise confirm it is an allowed modifier key or other event
    :else false))

(defn validate-keys
  "Validates that every key in the interceptor pattern is a legal one. (For example, :ctrk+:shyft+a is illegal.)
   Will throw an error if an illegal key is for in the pattern, otherwise returns the collection of keys supplied."
  [keys]
  (doseq [key keys]
    (when-not (valid-interceptor-key? key)
      (throw (js/Error. (str "Slate error: " key " is not a validate key in an interceptor!")))))
  keys)

(defn shortcut-pattern->set
  "Transforms an interceptor pattern like `:shift+a`, `:ctrl+z`, etc into a set of all the
   keys pressed. So for example:

   ```
   (shortcut-pattern->set :shift+a)
   (shortcut-pattern->set :ctrl+alt+enter)
   ```

   This is necessary because we want to be able to find an interceptor for a given
   key combination in constant time, but allow the user to describe them in any order.
   For example, `:ctrl+shift+a` and `:shift+ctrl+a` are equivalent. Therefore we use
   a set of all the keys pressed for indexing inside the interceptor map."
  [pattern]
  (->> (str/split (name pattern) "+")
       (validate-keys)
       (map keyword)
       (set)))

(defn completion-pattern->lookup-path
  "TODO: add explanation of why string patterns are reversed."
  [pattern]
  (concat [:completions] (reverse (.split pattern ""))))

(defmulti find-interceptor
  "Given a pattern or an event, returns the interceptor associated with it, or nil if one does not exist."
  (fn [_interceptors arg] (type arg)))

(defmethod find-interceptor Keyword
  [interceptor-map pattern]
  (get-in interceptor-map [:shortcuts (shortcut-pattern->set pattern)]))

(defmethod find-interceptor js/String
  [interceptor-map pattern]
  (get-in interceptor-map (completion-pattern->lookup-path pattern)))

(defmethod find-interceptor js/KeyboardEvent
  [interceptor-map event]
  (get-in interceptor-map [:shortcuts (parse-event event)]))

(comment
  (def sample-ints {:shortcuts {#{:ctrl :shift :a} :foo}
                    :completions {"c" {"b" {"a" :bar}}}})
  (find-interceptor :ctrl+shift+a))

(defn fire-interceptor
  "Calls the interceptor with the current editor state and the Event object as its args
   (and optionally, any additional args you wish to pass it) and re-synces the DOM.

   If no interceptor function is provided, the event will be parsed and the matching registered
   interceptor (if any) will be fired (TODO TODO TODO)."
  [interceptor-fn state-atom event & args]
  ;; These two are common-ish bugs when I refactor things around, and the error message
  ;; is less than helpful, so it's good to just give an explicit failure here instead.
  {:pre [(some? interceptor-fn) (some? state-atom)]}
  (let [transaction (apply interceptor-fn @state-atom event args)
        new-state (editor/apply-transaction! state-atom transaction)]
    (editor/sync-dom new-state)))

;; TODO: add option for fallthrough? Or an option to revert to default event in a certain situation...
(defn reg-interceptor
  "Takes the editor's interceptor map an interceptor pattern, and an interceptor function,
   and will return a new interceptor map with that interceptor registered.

   TODO: documentation on patterns.

   See also: `reg-interceptors`."
  [interceptors-map pattern interceptor-fn]
  (condp = (type pattern)
    Keyword (assoc-in interceptors-map
                      [:shortcuts (shortcut-pattern->set pattern)]
                      interceptor-fn)
    js/String (assoc-in interceptors-map
                        (completion-pattern->lookup-path pattern)
                        interceptor-fn)))

(defn reg-interceptors
  "Takes the editor's interceptor map and a map of interceptor patterns -> interceptors
   and returns a new interceptor map with the new interceptors added. For example:

   See also [[reg-interceptor]]."
  [interceptors-map new-interceptors]
  (reduce-kv reg-interceptor interceptors-map new-interceptors))

(defn interceptor-map
  "Initialize the map that holds all the interceptors for the editor."
  []
  (reg-interceptors {:shortcuts {}, :completions {}} default-interceptors))

(defn init-default-events [editor-state-atom]
  (let [get-interceptor (partial find-interceptor (:interceptors @editor-state-atom))
        {editor-elem :dom-elem
         hidden-input :hidden-input} @editor-state-atom]
    ;; TODO: should probably add an explanatory comment about the relationship between these three events
    (let [clicked? (atom false :validator boolean?)
          mousedown-event (atom nil :validator #(instance? js/MouseEvent %))]
      (.addEventListener editor-elem "mousedown"
        (fn [e]
          (.preventDefault e)
          (.focus hidden-input)

          (reset! clicked? true)
          (reset! mousedown-event e)
          (fire-interceptor (get-interceptor :click) editor-state-atom e)))

      (.addEventListener js/window "mousemove"
        (fn [e]
          (when @clicked?
            (fire-interceptor (get-interceptor :drag) editor-state-atom e @mousedown-event))))

      (.addEventListener js/window "mouseup"
        (fn [_e]
          (reset! clicked? false))))

    (.addEventListener hidden-input "keydown"
      (fn [e]
        ;; TODO: check for completions here (or possibly in the :insert case below?) and if one exists fire its interceptor
        (when-let [interceptor-fn (get-interceptor e)]
          (.preventDefault e)
          (fire-interceptor interceptor-fn editor-state-atom e))))

    (.addEventListener hidden-input "beforeinput"
      (fn [e]
        (case (.-inputType e)
          "insertText" (fire-interceptor (get-interceptor :insert) editor-state-atom e)
          "deleteContentBackward" (fire-interceptor (get-interceptor :delete) editor-state-atom e)
          nil)))))
