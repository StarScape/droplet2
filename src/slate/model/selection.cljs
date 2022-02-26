(ns slate.model.selection
  "Functions for creating and manipulating Selection objects,
   a basic building block of the editor which indicate where
   the text cursor and selection are.")

(defrecord Selection
  [start end between backwards? formats])

(defn selection-impl
  [& {:keys [start end backwards? between formats]
      :or {backwards? false
           between #{}
           formats #{}}}]
  (let [[start-paragraph start-offset] start
        [end-paragraph end-offset] (or end start)]
    (map->Selection {:start {:paragraph start-paragraph
                             :offset start-offset}
                     :end {:paragraph end-paragraph
                           :offset end-offset}
                     :backwards? backwards?
                     :between between
                     :formats formats})))

(defn selection
  "Creates a new selection.

   A Selection is composed of these parts:

   - `:start` and `:end`: both maps containing `:paragraph`, a UUID of the paragraph
   referenced, and `:offset`, a integer indicating how many characters into the paragraph
   that side of the selection is. `:start` **always** comes *before* `:end` in the document.

   - `:between`: a set of UUIDs, indicating all the paragraphs *between* the `:start` and `:end`.
     This **can** be null/empty set, but if so, it is the responsibility of the consumer to ensure
     that there actually are no paragraphs between `:start` and `:end`.

   - `:backwards?`: a boolean indicating if the range selection is backwards, i.e. if
   the text caret should be visible at the start instead of the end.

   - `:formats`: a set of the formats to use when inserting at the current single selection, or,
   for range selections, the set of formats shared by _all_ of the selection."
  ([arg1, arg2, & args]
   (cond
     (and (vector? arg1) (vector? arg2))
     (apply selection-impl :start arg1, :end arg2, args)

     (vector? arg1)
     (apply selection-impl :start arg1, :end arg1, arg2, args)

     :else
     (apply selection-impl arg1 arg2 args)))
  ([arg1 arg2]
   (selection-impl :start arg1 :end arg2))
  ([arg1]
   (selection-impl :start arg1 :end arg1)))

(defn caret
  "Returns the location the caret will be rendered at."
  [sel]
  (if (:backwards? sel)
    (-> sel :start :offset)
    (-> sel :end :offset)))

(defn caret-para
  "Returns the UUID of the paragraph that the caret is inside of."
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

(defn single?
  "Returns true if argument is a single selection."
  [sel]
  (= (:start sel) (:end sel)))

(def range?
  "Returns true if argument is a range selection."
  (complement single?))

(defn single-paragraph?
  "Returns true if the selection is contained within a single paragraph."
  [sel]
  (= (-> sel :start :paragraph) (-> sel :end :paragraph)))

(defn add-to-between
  "Adds the UUID to the selection's :between set if it is not also the UUID of the start or end paragraph."
  [sel uuid]
  (if (or (= uuid (-> sel :start :paragraph))
          (= uuid (-> sel :end :paragraph)))
    sel
    (update sel :between conj uuid)))

(defn remove-ends-from-between
  "If the UUID of the start of end paragraphs are in the :between set,
  removes them. Otherwise, just return the selection as is."
  [sel]
  (update sel :between #(disj % (-> sel :start :paragraph) (-> sel :end :paragraph))))

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
      (assoc-in [:end :offset] offset)
      (assoc :formats #{})))

(defn correct-orientation
  "Sets the selection's :backwards? field to false if it is a single selection."
  [sel]
  (if (single? sel)
    (assoc sel :backwards? false)
    sel))

(defn shift-start
  "Expands or contracts the left side of the selection by `n` characters (can be positive or negative).
   If selection is not a range-selection it is made one. Note the returned selection will always be backwards,
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

(defn all-uuids
  "Returns a set of **all** the paragraphs' UUIDs which are
  inside the selection (i.e. :start, :between, and :end)."
  [sel]
  (conj (:between sel) (-> sel :start :paragraph) (-> sel :end :paragraph)))

;; TODO: change :paragraph in :start and :end to :uuid
