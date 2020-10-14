(ns drop.editor.selection)

;; TODO: Change selection to use index instead of a reference to a paragraph

(defrecord Selection [start end backwards?])

(defn selection
  "Creates a new selection."
  ([[start-paragraph start-offset] [end-paragraph end-offset] backwards?]
   (map->Selection {:start {:paragraph start-paragraph
                            :offset start-offset}
                    :end {:paragraph end-paragraph
                          :offset end-offset}
                    :backwards? backwards?}))
  ([start end]
   (selection start end false))
  ([start]
   (selection start start false)))

(defn caret
  "Returns the location the caret will be rendered at."
  [sel]
  (if (:backwards? sel)
    (-> sel :start :offset)
    (-> sel :end :offset)))

(defn start-para
  "Shortcut for (-> sel :start :paragraph) for a single selection."
  [sel]
  (-> sel :start :paragraph))

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

(defn shift-single
  "Shift a single-selection by `n` characters (can be positive or negative)."
  [{{paragraph :paragraph offset :offset} :start :as sel} n]
  {:pre [(single? sel)]
   :post [(nat-int? (caret %))]}
  (selection [paragraph (+ n offset)]))

(defn set-single
  "Sets a single-selection to a given offset."
  [sel offset]
  {:pre [(single? sel), (nat-int? offset)]}
  (-> sel
      (assoc-in [:start :offset] offset)
      (assoc-in [:end :offset] offset)))

(defn expand-left
  "Expands the left side of the selection by `n` characters (can be positive or negative).
   If selection is not a range-selection it is made one. Note that the returned selection
   will always be `backwards?`."
  [sel n]
  {:pre  [(when (single? sel) (>= n 0))]
   :post [(>= (-> % :start :offset) 0)]}
  (-> sel (update-in [:start :offset] - n) (assoc :backwards? true)))

(defn expand-right
  "Expands the left side of the selection by `n` characters (can be positive or negative).
   If selection is not a range-selection it is made one. Note that the returned selection
   will always be `backwards?`."
  [sel n]
  {:pre  [(when (single? sel) (>= n 0))]}
  (-> sel (update-in [:end :offset] + n) (assoc :backwards? false)))

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
   (smart-collapse (selection [0 2] [0 5] true)) ; collapses to selection with offset 2
   (smart-collapse (selection [0 2] [0 5] false)) ; collapses to selection with offset 5
   ```"
  [sel]
  (if (single? sel)
    sel
    (if (:backwards? sel)
      (collapse-start sel)
      (collapse-end sel))))

;; TODO: expand-left and expand-right functions from JS implementation.
;; (Could probably be renamed shift-left and shift-right)
