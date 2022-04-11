(ns slate.interceptors
  (:require-macros [slate.interceptors :refer [interceptor definterceptor]])
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [slate.model.editor-state :as es]))

;; An Interceptor is a a function of (EditorState, EditorUiState?, Event?) -> EditorUpdate
(defrecord Interceptor [interceptor-fn
                        input-name
                        include-in-history?
                        add-to-history-immediately?
                        manual?]
  IFn
  (-invoke [this editor-state extras event-obj]
    ((:interceptor-fn this) editor-state extras event-obj)))

(defn interceptor?
  "Returns true if argument is an Interceptor."
  [i]
  (instance? Interceptor i))

(comment
  (definterceptor fake-interceptor
    {:input-name :click
     :include-in-history? true}
    [editor-state full-ui-state event]
    (prn editor-state full-ui-state event))
  (def i (interceptor
          {:input-name :click
           :include-in-history? true}
          [editor-state full-ui-state event]
          (prn editor-state full-ui-state event)))
  )

(s/def ::pattern (s/or :string string? :keyword keyword?))
(s/def ::interceptor-fn any? #_(s/spec :args (s/cat :state ::es/editor-state
                                             :ui-state any?)
                                :ret ::es/editor-state))
(s/def ::input-name (s/nilable ::pattern))
(s/def ::include-in-history? boolean?)
(s/def ::add-to-history-immediately? boolean?)
(s/def ::interceptor (s/keys :req-un [::interceptor-fn
                                      ::input-name
                                      ::include-in-history?
                                      ::add-to-history-immediately?]))
(s/def ::shortcuts (s/map-of ::pattern ::interceptor))
(s/def ::completions (s/map-of ::pattern (s/or ::interceptor ::completions)))
(s/def ::interceptor-map (s/keys :req-un [::completions
                                          ::shortcuts]))

;; TODO: should this be const?
(def modifier-keys #{:cmd :ctrl :shift :alt})

(def valid-interceptor-keywords
  ^{:doc "Set of legal modifier keys and events for an interceptor pattern."}
  (set/union modifier-keys #{:tab :right :up :delete :click :drag :down :insert :enter :left :cut :copy :paste}))

(defn valid-interceptor-key?
  "Returns true if key (a string) is a valid interceptor key, otherwise false."
  [key]
  {:pre [(or (keyword? key) (string? key))]}
        ;; Single char is just the primary key pressed
  (cond (= 1 (count (name key))) true
        ;; otherwise confirm it is an allowed modifier key or other event
        (valid-interceptor-keywords key) true
        :else false))

(defn validate-keys
  "Validates that every key in the interceptor pattern is a legal one. (For example, :ctrk+:shyft+a is illegal.)
   Will throw an error if an illegal key is for in the pattern, otherwise returns the collection of keys supplied."
  [keys]
  (doseq [key keys]
    (when-not (valid-interceptor-key? key)
      (throw (js/Error. (str "Slate error: "
                             key
                             " is not a valid key in an interceptor!")))))
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
      (-> k
          (.toLowerCase)
          (keyword)))))

(defn event->key-set
  "Takes a JS event and returns a set of all the keys pressed, e.g. #{:ctrl :shift :a}."
  [e]
  (let [modifiers-pressed (cond-> (transient [])
                            (.-ctrlKey e) (conj! :ctrl)
                            (.-altKey e) (conj! :alt)
                            (.-shiftKey e) (conj! :shift)
                            (.-metaKey e) (conj! :cmd)
                            :then (persistent!))
        key (event->key e)
        pressed-keys (cond-> modifiers-pressed
                       (not (modifier-keys key)) (conj key))]
    (set (map keyword pressed-keys))))

(defn shortcut-pattern->key-set
  "Transforms an interceptor pattern like `:shift+a`, `:ctrl+z`, etc into a set of all the
   keys pressed. So for example:

   ```
   (shortcut-pattern->key-set :shift+a) ; #{:shift :a}
   (shortcut-pattern->key-set :ctrl+alt+enter) ; #{:ctrl :alt :enter}
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
  "Completion patterns look something like 'abc ', meaning the interceptor
   will be triggered whenever the user types 'abc' followed by a space.

   However, we can only know that the pattern has been matched when the *last* key in it
   (e.g. the space, in the example above) has been pressed, hence why the lookup path is
   reversed."
  [pattern]
  (concat [:completions] (reverse (.split pattern ""))))

(defn find-completion
  "Takes the interceptor map and input history, and returns
   a matching completion interceptor if one exists, or nil otherwise."
  [key-pressed interceptor-map input-history]
  {:pre [(vector? input-history)]}
  (let [completions (:completions interceptor-map)
        completion-path (reverse (conj input-history key-pressed))] ; [..., "c" "b", "a"]
    (loop [current-level completions
           [p & ps] completion-path]
      (let [next-level-or-interceptor (get current-level p)]
        (if (or (nil? next-level-or-interceptor)
                (interceptor? next-level-or-interceptor))
          next-level-or-interceptor
          (recur next-level-or-interceptor ps))))))

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

(defn reg-interceptor
  "Takes interceptor map, an interceptor pattern, and an interceptor
   function, and returns a new interceptor map with that interceptor registered.

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
  "Initialize the map that holds all the interceptors for the editor.

   Under the hood, the interceptor map looks like this:
   ```
   interceptor-map :: {:shortcuts :: {shorcut-pattern -> interceptor}
                       :completions :: {completion-key -> (interceptor | completions)}}
   ```

   Where a shortcut-pattern is a '+' delineated list of keys, e.g. :ctrl+alt+a,
   and a completion-key is a string containing any typeable character."
  []
  {:shortcuts {}, :completions {}})

;; Input history

(def ^:const max-input-history 10)

(defn add-to-input-history
  "Adds the key to the vector of input history, dropping the least recent key
   typed off the history vector if necessary, and returns the new history vector."
  ([input-history input-name]
   (conj (if (< (count input-history) max-input-history)
           input-history
           (subvec input-history 1))
         input-name))
  ([input-history interceptor event]
   (let [input-name (cond
                      ;; TODO: may need to change this to a map with :completion true and other info at some point
                      ;; May need to also change to the interceptor just having a field like :undoable-with-delete?
                      ;; or something, depending on future needs. This behavior may not make sense e.g. for paren completion
                      (and (:add-to-history-immediately? interceptor)
                           (:include-in-history? interceptor))
                      :completion

                      (= :insert (:input-name interceptor))
                      (.-data event)

                      :else (:input-name interceptor))]
     (add-to-input-history input-history input-name))))
