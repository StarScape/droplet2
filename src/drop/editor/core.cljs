(ns drop.editor.core
  (:require [clojure.set :as set]))

;; TODO: spec all this out. Also learn spec :)

;;; Selection operations ;;;
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

(defn single?
  "Returns true if argument is a single selection."
  [sel]
  (= (:start sel) (:end sel)))

(def range?
  "Returns true if argument is a range selection."
  (complement single?))

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
    (if (:backwards sel)
      (collapse-start sel)
      (collapse-end sel))))

;; Some operations used across core datatypes

;; This protocol could stand a better name if we're honest
(defprotocol TextContainer
  (len [this] "Returns the number of chars in container (run/paragraph)."))

(defn type-dispatch [& args] (mapv type args))
(defmulti insert "Inserts into a Run/Paragraph/Document." #'type-dispatch)
(defmulti delete "Deletes from a Run/Paragraph/Document." #'type-dispatch)

;;; Run operations ;;;
(defrecord Run [text formats]
  TextContainer
  (len [r] (count (:text r))))

(defn run
  "Returns a new run. A run is defined as text with associating formatting."
  ([text formats]
   (->Run text formats))
  ([text]
   (->Run text #{})))

(defn empty-run "Returns an empty run." [] (run ""))

(defn empty-run?
  "Returns true if the run is empty."
  [r]
  (or (= "" (:text r)) (= nil (:text r))))

;; TODO: test split
(defn split
  "Splits the run at `offset`, returning a vector of two new runs containing the text to either side."
  [r offset]
  (cond
    (zero? offset) [(empty-run) r]
    (= offset (len r)) [r (empty-run)]
    :else (let [text-before (.slice (:text r) 0 offset)
                text-after (.slice (:text r) offset (len r))]
            [(run text-before (:formats r)), (run text-after (:formats r))])))

;; Range selection, remove block-selected text and then insert.
(defmethod insert [Run js/String js/Number js/Number]
  [r text start end]
  (let [before (.slice (:text r) 0 start)
        after (.slice (:text r) end)]
    (assoc r :text (str before text after))))

;; Single selection insert, nothing removed.
(defmethod insert [Run js/String js/Number]
  [r text caret]
  (insert r text caret caret))

(defn insert-start
  "Shortcut for inserting text at the start of a run."
  [run text]
  (insert run text 0))

(defn insert-end
  "Shortcut for inserting text at the end of a run."
  [run text]
  (insert run text (count (:text run))))

;; Delete between start and end
(defmethod delete [Run js/Number js/Number]
  [run start end]
  (let [before (.slice (:text run) 0 start)
        after (.slice (:text run) end)]
    (assoc run :text (str before after))))

;; Delete at, acts like backspace
(defmethod delete [Run js/Number]
  [run caret]
  (delete run (dec caret) caret))

;; I believe these toggle-format related functions can be done away with
(defn- toggle-set-entry [s v]
  (if (contains? s v) (disj s v) (conj s v)))

(defn toggle-format
  "Toggles the provided format on Run `r`. E.g. if `format` is not present it will be added, if
   it is it will be turned off. Providing a format that is not present in `r` will have no effect."
  [r format]
  (update r :formats #(toggle-set-entry % format)))

(defn toggle-formats
  "The same as `toggle-format`, but takes a coll of all formats to toggle."
  [r formats-coll]
  (assoc r :formats (reduce toggle-set-entry (:formats r) formats-coll)))

(defn apply-formats
  "Returns a new run with the all the supplied formats applied."
  [r formats]
  (update r :formats #(apply conj % formats)))

(defn apply-format
  "Returns a new run with the format applied."
  [r format]
  (update r :formats #(conj % format)))

(defn remove-formats
  "Returns a new run with all the supplied formats removed."
  [r formats]
  (update r :formats #(apply disj % formats)))

(defn remove-format
  "Returns a new run with the format removed."
  [r format]
  (update r :formats #(disj % format)))

;;; Paragraph operations ;;;
(defrecord Paragraph [runs]
  TextContainer
  (len [p] (reduce #(+ %1 (len %2)) 0 (:runs p))))

(declare optimize-runs) ;; Forward declare for use in `paragraph` function.

;; TODO: should we change these to ->paragraph and ->run to make more clear?
(defn paragraph
  "Creates a new paragraph."
  [runs]
  (->Paragraph (optimize-runs runs)))

;; Paragraph helper functions
(defn optimize-runs
  "Merges adjacent runs that have the same formatting, and removes empty runs.
   Will return an empty run if one is passed in, or all runs have no content."
  [runs]
  (let [non-empty-runs (filterv (complement empty-run?) runs)]
    (if (empty? non-empty-runs)
      [(run "")]
      (reduce (fn [optimized next-run]
                (let [top (peek optimized)]
                  (if (= (:formats next-run) (:formats top))
                    (-> (pop optimized)
                        (conj (insert-end top (:text next-run))))
                    (conj optimized next-run))))
              (vector (first non-empty-runs))
              (subvec non-empty-runs 1)))))

;; These two functions are poorly named, really. Should probably be something like
;; run-idx-and-run-relative-offset-for-paragraph offset, but I haven't come up with
;; anything that's descriptive enough and without being verbose.
(defn at-offset
  "Returns the index of the run `offset` falls inside of, as well as the
   number of characters that `offset` lies inside that run, as a vector pair."
  [runs offset]
  (loop [run-idx -1, offset-into-run 0, sum-prev-offsets 0]
    (if (> sum-prev-offsets offset)
      [run-idx offset-into-run]
      (recur
       (inc run-idx)
       (- offset sum-prev-offsets)
       (+ sum-prev-offsets (len (nth runs (inc run-idx))))))))

(defn before-offset
  "Returns the index of the run immediately before `offset`, as well as the
   number of characters that `offset` lies inside that run, as a vector pair.

   The difference from `at-offset`, is that it will return info for the run *before*
   the offset -- i.e. the one that would get backspaced if it were the cursor. At a
   boundary between runs before-offset will favor the first run."
  [runs offset]
  (loop [run-idx -1, offset-into-run 0, sum-prev-offsets 0]
    (if (>= sum-prev-offsets offset)
      [run-idx offset-into-run]
      (recur
       (inc run-idx)
       (- offset sum-prev-offsets)
       (+ sum-prev-offsets (len (nth runs (inc run-idx))))))))

(defn- split-runs
  "Splits runs at offset, returning a vector of [runs before, runs after].
   Will break a run apart if `offset` is inside that run."
  [runs offset]
  (let [offset-fun (if (zero? offset)
                     at-offset
                     before-offset)

        ;; Split the run at the caret position
        [target-run-idx target-run-offset] (offset-fun runs offset)
        target-run (nth runs target-run-idx)
        [target-before target-after] (split target-run target-run-offset)

        ;; Get runs before and after the run the caret is inside of
        runs-before (vec (take target-run-idx runs))
        runs-after (vec (drop (inc target-run-idx) runs))

        before (conj runs-before target-before)
        after (into [target-after] runs-after)]
    [before after]))

;; Main Paragraph operations
(defmethod insert [Paragraph Selection PersistentVector]
  [para sel runs]
  (if (single? sel)
    (let [[before after] (split-runs (:runs para) (caret sel))
          new-runs (concat before runs after)]
      (assoc para :runs (optimize-runs new-runs)))
    (let [selection-removed (delete para sel)]
      (insert selection-removed (collapse-start sel) runs))))

(defmethod insert [Paragraph Selection Run]
  [para sel r]
  (insert para sel [r]))

(defn- single-delete-paragraph [para sel]
  (let [[run-idx run-offset] (before-offset (:runs para) (caret sel))
        new-run (-> (nth (:runs para) run-idx)
                    (delete run-offset))
        new-runs (assoc (:runs para) run-idx new-run)]
    (assoc para :runs (optimize-runs new-runs))))

(defn- range-delete-paragraph [para sel]
  (let [runs (:runs para)
        ;; [start-idx start-offset] (at-offset runs (-> sel :start :offset))
        ;; [end-idx end-offset] (at-offset runs (-> sel :end :offset))
        [before _] (split-runs runs (-> sel :start :offset))
        [_ after] (split-runs runs (-> sel :end :offset))
        new-runs (optimize-runs (concat before after))]
    (assoc para :runs new-runs)))

(defmethod delete [Paragraph Selection]
  [para sel]
  (if (single? sel)
    (single-delete-paragraph para sel)
    (range-delete-paragraph para sel)))

(defn delete-after
  "Removes everything in paragraph `para` after the provided offset."
  [para offset]
  (delete para (selection [para offset] [para (len para)])))

(defn delete-before
  "Removes everything in paragraph `para` before the provided offset."
  [para offset]
  (delete para (selection [para 0] [para offset])))

(defn shared-formats
  "Returns the set of all the formats shared by each run that is inside (wholly or
   partially) the selection. Will return an empty set if there are no formats shared."
  [para sel]
  (let [runs (:runs para)
        [start-run-idx _] (at-offset runs (-> sel :start :offset))
        [end-run-idx _] (before-offset runs (-> sel :end :offset))
        selected-runs (subvec runs start-run-idx (inc end-run-idx))]
    (->> selected-runs
         (map :formats)
         (apply set/intersection))))

;; TODO: Paragraph format functions apply-format and remove-format

(def my-runs [(run "foo" #{:italic})
              (run "bar" #{:bold :italic})
              (run "bizz" #{:italic})
              (run "buzz" #{:bold})])

(def p (paragraph my-runs))
(def s (selection [p 1]))

;; foobarbizzbuzz
(shared-formats p (selection [p 0] [p 10]))

(def simplep (paragraph [(run "foobar1" #{:bold})
                         (run "goobar2")
                         (run "hoobar3" #{:italic})]))

(insert simplep (selection [simplep 21]) (run "post"))
