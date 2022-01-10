(ns slate.model.paragraph
  "Paragraphs contain a series of one or more runs. This namespace
   contains functionality for using and manipulating paragraphs."
  (:require [clojure.set :as set]
            [slate.model.common :refer [TextContainer
                                        Selectable
                                        Formattable
                                        insert
                                        delete
                                        insert-start
                                        insert-end
                                        len
                                        blank?
                                        apply-format
                                        remove-format
                                        shared-formats
                                        char-before
                                        char-at]]
            [slate.model.run :as r]
            [slate.model.selection :as sel :refer [Selection selection]]))

(declare optimize-runs)

(defrecord Paragraph [uuid runs]
  TextContainer
  (len [p] (reduce #(+ %1 (len %2)) 0 (:runs p)))
  (blank? [p] (zero? (len p))))

(defn paragraph
  "Creates a new paragraph. If no UUID is supplied a random one is created."
  ([runs]
   (->Paragraph (random-uuid) (optimize-runs runs)))
  ([]
   (->Paragraph (random-uuid) [(r/empty-run)]))

  ;; This arity is mostly for testing (to make it easier to track that the UUID remains what it should)
  ([uuid runs]
   (->Paragraph uuid (optimize-runs runs))))

(defn empty-paragraph
  "Creates an empty paragraph, optionally taking a UUID to assign to it."
  ([uuid]
   (paragraph uuid [(r/run)]))
  ([]
   (paragraph [(r/run)])))

(defn- assoc-last
  "Replaces the last item in a vector with a new value."
  [v new-val]
  (assoc v (dec (count v)) new-val))

;; Paragraph helper functions
(defn optimize-runs
  "Given a vector/seq of runs, merges adjacent runs with the same formatting, and removes
   empty runs. Returns a vector. Return an empty run if one is passed in, or if all runs have
   no content."
  [runs]
  {:post [(vector? %)]}
  (let [non-empty-runs (filterv (complement blank?) runs)]
    (if (empty? non-empty-runs)
      [(r/empty-run)]
      (reduce (fn [optimized next-run]
                (let [last-optimized-run (peek optimized)]
                  (if (= (:formats next-run) (:formats last-optimized-run))
                    (assoc-last optimized (insert-end last-optimized-run (:text next-run)))
                    (conj optimized next-run))))
              (vector (first non-empty-runs))
              (subvec non-empty-runs 1)))))

;; These two functions are poorly named, really. Should probably be something like
;; run-idx-and-run-relative-offset-from-paragraph-offset, but I haven't come up with
;; anything that's descriptive enough and without being verbose.
;;
;; There are a bit more complicated than I would like them to be, but at least the
;; complexity is mostly contained here.

(defn at-offset
  "Returns the index of the run that `offset` falls inside of, as well as the
   number of characters that `offset` lies inside that run, as a vector pair,
   like so: [run-idx, offset-into-run]."
  [runs offset]
  (if (zero? offset)
    [0 0]
    (loop [run-idx -1, offset-into-run 0, sum-prev-offsets 0]
      (if (> sum-prev-offsets offset)
        [run-idx offset-into-run]
        (recur
         (inc run-idx)
         (- offset sum-prev-offsets)
         (+ sum-prev-offsets (len (nth runs (inc run-idx)))))))))

(defn before-offset
  "Returns the index of the run immediately before `offset`, as well as the
   number of characters that `offset` lies inside that run, as a vector pair,
   like so: [run-idx, offset-into-run].

   The difference from `at-offset`, is that it will return info for the run *before*
   the offset -- i.e. the one that would get backspaced if it were the cursor. At a
   boundary between runs before-offset will favor the first run."
  [runs offset]
  (if (zero? offset)
    [0 0]
    (loop [run-idx -1, offset-into-run 0, sum-prev-offsets 0]
      (if (>= sum-prev-offsets offset)
        [run-idx offset-into-run]
        (recur
         (inc run-idx)
         (- offset sum-prev-offsets)
         (+ sum-prev-offsets (len (nth runs (inc run-idx)))))))))

;; TODO: I think split-runs and separate-selection could be merged into one
;; function that takes a SELECTION, and either splits AROUND the selection (if
;; single) and returns [before, after], or [before, inside, after] (if range).
;; Better: return a map with :before, :inside, and :after members.
;;
;; Performance implications and so forth will need to be considered.
(defn split-runs
  "Splits runs at offset, returning a vector of [runs before, runs after].
   Will break a run apart if `offset` is inside that run."
  [runs offset]
  (let [run-end-offsets (set (reductions + (map len runs)))
        ;; runs-len (reduce + (map len runs))
        offset-fn (if (run-end-offsets offset) before-offset at-offset)

        ;; Split the run at the caret position
        [target-run-idx target-run-offset] (offset-fn runs offset)
        target-run (nth runs target-run-idx)
        [target-before target-after] (r/split target-run target-run-offset)

        ;; Get runs before and after the run that the caret is inside of
        runs-before (take target-run-idx runs)
        runs-after (drop (inc target-run-idx) runs)

        before (conj (vec runs-before) target-before)
        after (into [target-after] runs-after)]
    (mapv optimize-runs [before after])))

(defn separate-selected
  "Splits the runs at the beginning and end of the selection and returns three vectors
   of runs: [runs before selection start, runs inside selection, runs after selection end].

   This may seem like an esoteric operation but it's a useful helper for some of the
   core functions (see below)."
  [runs sel]
  (let [[runs-before runs-after] (split-runs runs (-> sel :start :offset))
        runs-before-len (reduce + (map len runs-before))
        adjusted-end-offset (- (-> sel :end :offset) runs-before-len)
        [within-sel after-sel] (split-runs runs-after adjusted-end-offset)]
    [runs-before within-sel after-sel]))

;; Main Paragraph operations
(defmethod insert [Paragraph Selection [r/Run]]
  [para sel runs]
  (if (sel/single? sel)
    (let [[before after] (split-runs (:runs para) (sel/caret sel))
          new-runs (concat before runs after)]
      (assoc para :runs (optimize-runs new-runs)))
    (let [selection-removed (delete para sel)]
      (insert selection-removed (sel/collapse-start sel) runs))))

(defmethod insert [Paragraph Selection r/Run]
  [para sel r]
  (insert para sel [r]))

(defmethod insert [Paragraph Selection Paragraph]
  [para sel para-to-insert]
  (insert para sel (:runs para-to-insert)))

;; TODO: these might stand some mild testing
(defmethod insert-start [Paragraph [r/Run]]
  [para runs]
  (insert para (selection [(:uuid para) 0]) runs))

(defmethod insert-start [Paragraph r/Run]
  [para run]
  (insert para (selection [(:uuid para) 0]) run))

(defmethod insert-end [Paragraph [r/Run]]
  [para runs]
  (insert para (selection [(:uuid para) (len para)]) runs))

(defmethod insert-end [Paragraph r/Run]
  [para run]
  (insert para (selection [(:uuid para) (len para)]) run))

(defn- paragraph-single-delete [para sel]
  (if (zero? (sel/caret sel))
    para
    (let [[run-idx run-offset] (before-offset (:runs para) (sel/caret sel))
          new-run (delete ((:runs para) run-idx) run-offset)
          new-runs (assoc (:runs para) run-idx new-run)]
      (assoc para :runs (optimize-runs new-runs)))))

(defn- paragraph-range-delete [para sel]
  (let [runs (:runs para)
        [before _] (split-runs runs (-> sel :start :offset))
        [_ after] (split-runs runs (-> sel :end :offset))
        new-runs (optimize-runs (concat before after))]
    (assoc para :runs new-runs)))

(defmethod delete [Paragraph Selection]
  [para sel]
  (if (sel/single? sel)
    (paragraph-single-delete para sel)
    (paragraph-range-delete para sel)))

(defn delete-after
  "Removes everything in paragraph `para` after the provided offset."
  [para offset]
  (let [para-len (len para)]
    (if (= offset para-len)
      para
      (delete para (selection [(:uuid para) offset] [(:uuid para) para-len])))))

(defn delete-before
  "Removes everything in paragraph `para` before the provided offset."
  [para offset]
  (delete para (selection [(:uuid para) 0] [(:uuid para) offset])))

(defn update-selected-runs
  "Returns a new paragraph with f applied to each run inside the selection."
  [para sel f]
  (let [[before selected after] (separate-selected (:runs para) sel)
        new-runs (-> [before (map f selected) after]
                     flatten vec optimize-runs)]
    (assoc para :runs new-runs)))

(extend-type Paragraph
  Selectable
  (char-at [para sel]
    (let [[run-idx run-offset] (at-offset (:runs para) (sel/caret sel))
          run-text (:text ((:runs para) run-idx))]
      (nth run-text run-offset)))

  (char-before [para sel]
    (if (zero? (sel/caret sel))
      "\n" ; TODO: Not sure if this is the right approach here
      (char-at para (sel/shift-single sel -1))))

  ;; TODO: should this return a paragraph instead of a list of runs?
  (selected-content [para sel] (second (separate-selected (:runs para) sel)))

  (shared-formats
   ([para] (shared-formats para (selection [(:uuid para) 0] [(:uuid para) (len para)])))
   ([para sel]
    (let [runs (:runs para)
          [start-run-idx _] (at-offset runs (-> sel :start :offset))
          [end-run-idx _] (before-offset runs (-> sel :end :offset))
          selected-runs (subvec runs start-run-idx (inc end-run-idx))]
      (->> selected-runs
           (map :formats)
           (apply set/intersection)))))

  (toggle-format
   [para sel format]
   (let [[before in-selection after]
         (separate-selected (:runs para) sel)

         common-formats
         (->> in-selection (map :formats) (apply set/intersection))

         selected-runs-toggled
         (if (contains? common-formats format)
           (mapv #(remove-format % format) in-selection)
           (mapv #(apply-format % format) in-selection))

         new-runs
         (optimize-runs (concat before selected-runs-toggled after))]
     (assoc para :runs new-runs)))

  ;; TODO: test these
  Formattable
  (apply-format
   ([p format] (update p :runs (partial map #(apply-format % format))))
   ([p sel format] (update-selected-runs p sel #(apply-format % format))))

  (remove-format
   ([p format] (update p :runs (partial map #(remove-format % format))))
   ([p sel format] (update-selected-runs p sel #(remove-format % format)))))

;; Operations on multiple paragraphs ;;
(defn merge-paragraphs
  "Merges the two paragraphs. By default new paragraph will have the UUID of `p1`.
   Optionally takes a third parameter to set the UUID to whatever you want."
  ([p1 p2 uuid]
   (paragraph uuid (vec (concat (:runs p1) (:runs p2)))))
  ([p1 p2]
   (merge-paragraphs p1 p2 (:uuid p1))))
