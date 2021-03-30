(ns slate.events
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [slate.core :as sl]
            [slate.editor :as editor]
            [slate.navigation :as nav]
            [slate.selection :as sel]
            [slate.view :as view]))

;; Maximum number of keys typed to remember
(def ^:const max-input-history 10)

(declare add-key-to-history)

(defn with-input-history
  "Helper for automatically adding a value to the input history. See use below in interceptors."
  [key state]
  (update state :input-history add-key-to-history key))

;; TODO: can separate all the interceptors and their implementations (including those in view.cljs,
;; which could be changed into interceptors) out into an "interceptors" namespace maybe?

;; Default set of interceptors that are added to a new editor.
;;
;; Interceptors are meant for extensibility, but even so, most of the default events
;; are implemented using them, the idea being that eating our own dogfood is the only
;; way to end up with a sufficiently flexible and ergonomic system.
(def default-interceptors
  {:click (fn [state e]
            (let [new-sel (view/mouse-event->selection e state (:measure-fn state))]
              (with-input-history :click
                (assoc state :selection new-sel))))
   :drag (fn [state mousemove-event mousedown-event]
           (update state :selection #(view/drag mousedown-event mousemove-event state (:measure-fn state))))

   :insert (fn [{:keys [doc selection] :as state} e]
             (let [text (.-data e)
                   new-doc (sl/insert doc selection text)
                   new-selection (sel/shift-single selection (count text))
                   ;;  new-state (assoc state :doc new-doc :selection new-selection)
                   input-for-history (if (= 1 (.-length text)) text :PASTE)] ;; TODO?
               (with-input-history input-for-history
                 (assoc state :doc new-doc :selection new-selection))))
   :delete (fn [{:keys [doc selection] :as state} _e]
             (let [[new-doc, new-sel] (sl/delete doc selection)]
               (with-input-history :delete
                 (assoc state :doc new-doc :selection new-sel))))
   :enter (fn [{:keys [doc selection] :as state} _e]
            (let [[new-doc, new-sel] (sl/enter doc selection)]
              (with-input-history :enter
                (assoc state :doc new-doc :selection new-sel))))
   :tab (fn [{:keys [doc selection] :as state} _e]
          (let [new-doc (sl/insert doc selection "\u2003")
                new-selection (sel/shift-single selection 1)]
            (with-input-history :tab
              (assoc state :doc new-doc :selection new-selection))))

   :left (fn [state _e]
           (with-input-history :left
             (update state :selection #(nav/prev-char (:doc state) %))))
   :ctrl+left (fn [state _e]
                (with-input-history :ctrl+left
                  (update state :selection #(nav/prev-word (:doc state) %))))
   :shift+left (fn [state _e]
                 (with-input-history :shift+left
                   (update state :selection #(nav/shift+left (:doc state) (:selection state)))))
   :ctrl+shift+left (fn [state _e]
                      (with-input-history :ctrl+shift+left
                        (update state :selection #(nav/ctrl+shift+left (:doc state) (:selection state)))))

   :right (fn [state _e]
            (with-input-history :right
              (update state :selection #(nav/next-char (:doc state) %))))
   :ctrl+right (fn [state _e]
                 (with-input-history :ctrl+right
                   (update state :selection #(nav/next-word (:doc state) %))))
   :shift+right (fn [state _e]
                  (with-input-history :shift+right
                    (update state :selection #(nav/shift+right (:doc state) (:selection state)))))
   :ctrl+shift+right (fn [state _e]
                       (with-input-history :ctrl+shift+right
                         (update state :selection #(nav/ctrl+shift+right (:doc state) (:selection state)))))

   :down (fn [state _e]
           (with-input-history :down
             (update state :selection #(view/down state (:measure-fn state)))))
   :shift+down (fn [state _e]
                 (with-input-history :shift+down
                   (update state :selection #(view/shift+down state (:measure-fn state)))))
   :up (fn [state _e]
         (with-input-history :up
           (update state :selection #(view/up state (:measure-fn state)))))
   :shift+up (fn [state _e]
               (with-input-history :shift+up
                 (update state :selection #(view/shift+up state (:measure-fn state)))))
   
   "\"" (fn [{:keys [doc selection] :as state} _e]
          (let [new-doc (sl/insert doc selection "\"\"")
                new-selection (sel/shift-single selection 1)]
            (assoc state :doc new-doc :selection new-selection)))})

;; TODO: should this be const?
(def modifier-keys #{:ctrl :shift :alt :meta})

(def valid-interceptor-keywords
  ^{:doc "Set of legal modifier keys and events for an interceptor pattern."}
  (set/union modifier-keys #{:tab :right :up :delete :click :drag :down :insert :enter :left}))

(defn valid-interceptor-key?
  "Returns true if key (a string) is a valid interceptor key, otherwise false."
  [key]
  {:pre [(or (keyword? key) (string? key))]}
  (cond
    (or (string? key) (= 1 (count (name key)))) true ;; Single char is just the primary key pressed
    (valid-interceptor-keywords key) true ;; otherwise confirm it is an allowed modifier key or other event
    :else false))

(defn validate-keys
  "Validates that every key in the interceptor pattern is a legal one. (For example, :ctrk+:shyft+a is illegal.)
   Will throw an error if an illegal key is for in the pattern, otherwise returns the collection of keys supplied."
  [keys]
  (doseq [key keys]
    (when-not (valid-interceptor-key? key)
      (throw (js/Error. (str "Slate error: " key " is not a valid key in an interceptor!")))))
  keys)

(defn event->key
  "Returns the correct string or keyword for JS KeyboardEvent, based on how it
   would appear in an interceptor string, e.g. `:ctrl`, `:alt`, `\"a\"`, whatever."
  [event]
  {:pre [(instance? js/KeyboardEvent event)]}
  (let [k (.-key event)]
    (case k
      "ArrowLeft" :left
      "ArrowRight" :right
      "ArrowUp" :up
      "ArrowDown" :down
      "Tab" :tab
      "Control" :ctrl
      (-> k (.toLowerCase) (keyword)))))

(defn event->key-set
  "Takes a JS event and returns a set of all the keys pressed, e.g. #{:ctrl :shift :a}."
  [e]
  (let [modifiers-pressed (cond-> (transient [])
                            (.-ctrlKey e) (conj! :ctrl)
                            (.-altKey e) (conj! :alt)
                            (.-shiftKey e) (conj! :shift)
                            :then (persistent!))
        key (event->key e)
        pressed-keys (cond-> modifiers-pressed
                       (not (modifier-keys key)) (conj key))]
    (set (map keyword pressed-keys))))

(defn shortcut-pattern->key-set
  "Transforms an interceptor pattern like `:shift+a`, `:ctrl+z`, etc into a set of all the
   keys pressed. So for example:

   ```
   (shortcut-pattern->key-set :shift+a)
   (shortcut-pattern->key-set :ctrl+alt+enter)
   ```

   This is necessary because we want to be able to find an interceptor for a given
   key combination in constant time, but allow the user to describe them in any order.
   For example, `:ctrl+shift+a` and `:shift+ctrl+a` are equivalent. Therefore we use
   a set of all the keys pressed for indexing inside the interceptor map."
  [pattern]
  (->> (str/split (name pattern) "+")
       (map keyword)
       (validate-keys)
       (set)))

(defn completion-pattern->lookup-path
  "TODO: add explanation of why string patterns are reversed."
  [pattern]
  (concat [:completions] (reverse (.split pattern ""))))

(defn add-key-to-history
  "Adds the key to the vector of input history, dropping the least recent key
   typed off the history vector if necessary, and returns the new history vector."
  [input-history key]
  (conj (if (< (count input-history) max-input-history)
          input-history
          (subvec input-history 1))
        key))

(defn matching-completion?
  "Takes the editor's interceptor map and input history, and returns
   a matching completion interceptor if one exists, or nil otherwise."
  [key-pressed interceptor-map input-history]
  {:pre [(vector? input-history)]}
  (let [completions (:completions interceptor-map)
        path (reverse (conj input-history key-pressed))] ; [..., "c" "b", "a"]
    (loop [current-level completions
           [p & ps] path]
      (let [next (current-level p)]
        (if (or (nil? next) (fn? next))
          next
          (recur next ps))))))

(defmulti find-interceptor
  "Given a pattern or an event, returns the interceptor associated with it, or nil if one does not exist."
  (fn [_interceptors arg] (type arg)))

(defmethod find-interceptor Keyword
  [interceptor-map pattern]
  (get-in interceptor-map [:shortcuts (shortcut-pattern->key-set pattern)]))

;; TODO: needed?
(defmethod find-interceptor js/String
  [interceptor-map pattern]
  (get-in interceptor-map (completion-pattern->lookup-path pattern)))

(defmethod find-interceptor js/KeyboardEvent
  [interceptor-map event]
  (get-in interceptor-map [:shortcuts (event->key-set event)]))

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
                      [:shortcuts (shortcut-pattern->key-set pattern)]
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
        ;; TODO: how to undo a completion with a backspace immediately after the completion fires?
        ;; I think one method might be to FIRST let the normal :insert interceptor fire, THEN afterward
        ;; fire the completion. Then a special case must be added to the :delete interceptor which undoes
        ;; what just happened IF the last thing to happen was a completion interceptor. This would also
        ;; necessitate that every completion add :completion or something similar to the input-history.
        ;;
        ;; I think it could work but I'm not yet totally sure how I feel about it. Need to think it over.
        (case (.-inputType e)
          "insertText"
          (let [{:keys [interceptors input-history]} @editor-state-atom
                completion-interceptor (when (= 1 (.. e -data -length))
                                         (matching-completion? (.-data e) interceptors input-history))]
            (if completion-interceptor
              (fire-interceptor completion-interceptor editor-state-atom e)
              (fire-interceptor (get-interceptor :insert) editor-state-atom e)))

          "deleteContentBackward"
          (fire-interceptor (get-interceptor :delete) editor-state-atom e)
          nil)))))
