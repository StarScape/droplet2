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
  "Shift a single-selection by n characters (can be positive or negative)."
  [{{paragraph :paragraph offset :offset} :start} n]
  (selection [paragraph (+ n offset)]))

(defn set-single
  "Sets a single-selection to a given offset."
  [sel offset]
  (-> sel
      (assoc-in [:start :offset] offset)
      (assoc-in [:end :offset] offset)))

(defn collapse-start
  "Returns a new single-selection at the start of the selection"
  [sel]
  (selection [(-> sel :start :paragraph) (-> sel :start :offset)]))

(defn collapse-end
  "Returns a new single-selection at the end of the selection"
  [sel]
  (selection [(-> sel :end :paragraph) (-> sel :end :offset)]))

;; TODO is this needed? see Paragraph.js
(defn smart-collapse [sel]
  (if (single? sel)
    sel
    (if (:backwards? sel)
      (collapse-start sel)
      (collapse-end sel))))

;; Some operations used across core datatypes

