(ns slate.model.selection
  "Functions for creating and manipulating Selection objects,
   a basic building block of the editor which indicate where
   the text cursor and selection are."
  (:require [clojure.set :as set]))

(defrecord Selection
  [start end backwards? formats])

(defn- constructor-impl
  [& {:keys [start end backwards? formats]
      :or {backwards? false
           formats #{}}}]
  (let [[start-paragraph start-offset] start
        [end-paragraph end-offset] (or end start)]
    (map->Selection {:start {:paragraph start-paragraph
                             :offset start-offset}
                     :end {:paragraph end-paragraph
                           :offset end-offset}
                     :backwards? backwards?
                     :formats formats})))

(defn selection
  "Creates a new selection.

   A Selection is composed of these parts:

   - `:start` and `:end`: both maps containing `:paragraph`, the index of the paragraph
   referenced, and `:offset`, a integer indicating how many characters into the paragraph
   that side of the selection is. `:start` **always** comes *before* `:end` in the document.

   - `:backwards?`: a boolean indicating if the range selection is backwards, i.e. if
   the text caret should be visible at the start instead of the end.

   - `:formats`: a set of the formats to use when inserting at the current single selection, or,
   for range selections, the set of formats shared by _all_ of the selection."
  ([arg1, arg2, & args]
   (cond
     (and (vector? arg1) (vector? arg2))
     (apply constructor-impl :start arg1, :end arg2, args)

     (vector? arg1)
     (apply constructor-impl :start arg1, :end arg1, arg2, args)

     :else
     (apply constructor-impl arg1 arg2 args)))
  ([arg1 arg2]
   (constructor-impl :start arg1 :end arg2))
  ([arg1]
   (constructor-impl :start arg1 :end arg1)))

(defn single?
  "Returns true if argument is a single selection."
  [sel]
  (= (:start sel) (:end sel)))

(defn from-singles
  "Constructs a Selection from two single Selections, where the first one will
   be the start point of the new selection and the second one will be the end."
  [sel1 sel2]
  (selection [(-> sel1 :start :paragraph) (-> sel1 :start :offset)]
             [(-> sel2 :end :paragraph) (-> sel2 :end :offset)]))

(defn caret
  "Returns the location the caret will be rendered at."
  [sel]
  (if (:backwards? sel)
    (-> sel :start :offset)
    (-> sel :end :offset)))

(defn caret-para
  "Returns the index of the paragraph that the caret is inside of."
  [sel]
  (if (:backwards? sel)
    (-> sel :start :paragraph)
    (-> sel :end :paragraph)))

(defn start-para
  "Shortcut for (-> sel :start :paragraph)."
  [sel]
  (-> sel :start :paragraph))

(defn end-para
  "Shortcut for (-> sel :end :paragraph)."
  [sel]
  (-> sel :end :paragraph))

(def range?
  "Returns true if argument is a range selection."
  (complement single?))

(defn single-paragraph?
  "Returns true if the selection is contained within a single paragraph."
  [sel]
  (= (-> sel :start :paragraph) (-> sel :end :paragraph)))

(defn shift-single
  "Shift a single-selection by `n` characters (can be positive or negative)."
  [{{paragraph :paragraph offset :offset} :start :as sel} n]
  {:pre [(single? sel)]
   :post [(single? %), (nat-int? (caret %))]}
  (selection [paragraph (+ n offset)]))

(defn set-single
  "Sets a single-selection to a given offset. Formats will be automatically reset to an empty set."
  [sel offset]
  {:pre [(single? sel), (nat-int? offset)]}
  (-> sel
      (assoc-in [:start :offset] offset)
      (assoc-in [:end :offset] offset)))

(defn correct-orientation
  "Sets the selection's :backwards? field to false if it is a single selection."
  [sel]
  (if (single? sel)
    (assoc sel :backwards? false)
    sel))

(defn shift-start
  "Expands or contracts the left side of the selection by `n` characters (can be positive or negative).
   If selection is not a range selection it is made one. Note the returned selection will always be backwards,
   unless it is a single selection."
  [sel n]
  {:pre [(if (single? sel) (<= n 0) true)]
   :post [(>= (-> % :start :offset) 0)]}
  (-> sel
      (update-in [:start :offset] + n)
      (assoc :backwards? true)
      (correct-orientation)))

(defn shift-end
  "Expands or contracts the right side of the selection by `n` characters (can be positive or negative).
   If selection is not a range-selection it is made one. Note the returned selection will always be forwards."
  [sel n]
  (-> sel
      (update-in [:end :offset] + n)
      (assoc :backwards? false)))

(defn shift-caret
  "Moves the side of the selection with the caret on it by `n` characters. The other side
   (the anchor) **remains unchanged.** If selection is not a range it will be made one, and
   the *right side* will be the one shifted."
  [sel n]
  (if (and (range? sel) (:backwards? sel))
    (shift-start sel n)
    (shift-end sel n)))

(defn collapse-start
  "Returns a new single-selection at the start of the selection"
  [sel]
  (selection [(-> sel :start :paragraph) (-> sel :start :offset)]))

(defn collapse-end
  "Returns a new single-selection at the end of the selection"
  [sel]
  (selection [(-> sel :end :paragraph) (-> sel :end :offset)]))

(defn smart-collapse
  "Collapses to the side of the selection that the text caret is on.
   This tends to be the preferred way of collapsing a selection, unless
   you specifically need `collapse-start` or `collapse-end`. Notably, most
   text editors **do not** collapse this way (instead always collapsing to
   the end when e.g. the user hits the down arrow), but I like this functionality.

   Examples:
   ```
   (smart-collapse (selection [0 2] [0 5] :backwards? true)) ; collapses to selection with offset 2
   (smart-collapse (selection [0 2] [0 5] :backwards? false)) ; collapses to selection with offset 5
   ```"
  [sel]
  (if (single? sel)
    sel
    (if (:backwards? sel)
      (collapse-start sel)
      (collapse-end sel))))

(defn toggle-format
  "Add format `f` to :formats if it is not present, removes it if it is."
  [{:keys [formats] :as sel} f]
  (assoc sel :formats (if (contains? formats f)
                        (disj formats f)
                        (conj formats f))))
