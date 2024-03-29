(ns slate.utils
  "General purpose utilities, such as high-order or collection-
   manipulation functions not found in the standard library."
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn is-mac? [] true) ;; just return true for now in development

(defn cmd-or-ctrl-key
  "Takes a KeyboardEvent, returns true if the CMD key is pressed
   (on macOS), or if the ctrlKey is pressed (on Windows and Linux)."
  [keyboard-event]
  (if (is-mac?)
    (.-metaKey keyboard-event)
    (.-ctrlKey keyboard-event)))

(defn common-elements [& colls]
  (let [freqs (map frequencies colls)]
    (mapcat (fn [e] (repeat (apply min (map #(% e) freqs)) e))
            (apply set/intersection (map (comp set keys) freqs)))))

(defn pretty-paragraph [para]
  (str "|(" "P " (:type para) "), \""
       (->> (:runs para)
            (map :text)
            (flatten)
            (apply str)) "\"|"))

(defn pretty-history-update-update [{:keys [editor-state changelist]}]
  (let [{:keys [doc]} editor-state
        changed-paras (map #(get (:children doc) %) (:changed-indices changelist))
        inserted-paras (map #(get (:children doc) %) (:inserted-indices changelist))
        deleted-paras (map #(get (:children doc) %) (:deleted-indices changelist))
        pretty-paras (fn [paras] (str/join ", " (map pretty-paragraph #_(fn [p] (pretty-paragraph p)) paras)))
        render-paras (fn [title paras]
                       (if (seq paras)
                         (str title " (" (count paras) "): " (pretty-paras paras) "\n")
                         ""))]
    (str "----------------------------------------\n"
         (render-paras "changed" changed-paras)
         (render-paras "inserted" inserted-paras)
         (render-paras "deleted" deleted-paras)
         "----------------------------------------\n")))

(defn pretty-history-stack
  ([history n]
   (let [current-state-index (:current-state-index history)
         backstack-strs (map-indexed (fn [idx, history-update]
                                       (let [original-idx (dec (- (count (:backstack history)) idx))]
                                         (str (if (= original-idx current-state-index)
                                                (str "%c>>%c ## Backstack " original-idx ":\n")
                                                (str "## Backstack " original-idx ":\n"))
                                              (pretty-history-update-update history-update) "\n")))
                                     (take n (reverse (:backstack history))))
         pretty-backstack (str/join "\n" backstack-strs)
         fmt-str (str (if (= current-state-index (count (:backstack history)))
                      "%c>>%c # Tip:\n"
                      "# Tip:\n")
                    (if-let [tip (:tip history)]
                      (pretty-history-update-update tip)
                      "No tip\n")
                    "\n"

                    "# Backstack:\n"
                    pretty-backstack)]
     (js/console.log fmt-str "color: red;", "color: black;")))
  ([history] (pretty-history-stack history (count (:backstack history)))))

(defn paragraph-type->css-class [paragraph-type]
  (if paragraph-type
    (str (name paragraph-type) "-format")
    ""))

(defn formats->css-classes [formats]
  (map #(str (name %) "-format") formats))

(def ^:private -weak-caches (js/WeakMap.))

(defn weak-cache
  "Caches the results of functions `f` via a weak reference to `o`.
   So long as `o` remains in memory, the result of `f` will remain in the cache. When `o`
   is garbage-collected, the result will be evicted from the cache automatically."
  [o f]
  (let [cache-for-o (or (.get -weak-caches o) {})]
    (if-let [cached-val (get cache-for-o f)]
      cached-val
      (let [computed-val (f)]
        (.set -weak-caches o (assoc cache-for-o f computed-val))
        computed-val))))

(defn assoc-last
  "Replaces the last item in a vector with a new value."
  [v new-val]
  {:pre [(vector? v)]}
  (assoc v (dec (count v)) new-val))

(defn- find-first-index
  "Returns the index of the first item matching `pred`."
  [pred coll]
  (ffirst (filter (comp pred second) (map-indexed list coll))))
