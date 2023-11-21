(ns slate.model.paragraph
  "Paragraphs contain a series of one or more runs. This namespace
   contains functionality for using and manipulating paragraphs."
  (:require [clojure.set :as set]
            [slate.model.common :refer [TextContainer
                                        Selectable
                                        Fragment
                                        text
                                        len
                                        blank?
                                        graphemes
                                        formatting
                                        char-at
                                        items]]
            [slate.model.run :as r]
            [slate.model.selection :as sel :refer [Selection selection]]
            [slate.utils :as utils :refer-macros [weak-cache-val]]))

(declare optimize-runs)

(defrecord Paragraph [runs type]
  TextContainer
  (text [p] (reduce str (map text (:runs p))))
  (len [p] (reduce #(+ %1 (len %2)) 0 (:runs p)))
  (blank? [p] (zero? (len p)))
  (graphemes [p]
    (weak-cache-val p
      (loop [runs (:runs p)
             segment-start-offset 0
             segments []]
        (if-not runs
          segments
          (let [run (first runs)
                run-graphemes (graphemes run)
                offset-graphemes (map #(update % :offset (partial + segment-start-offset)) run-graphemes)]
            (recur (next runs)
                   (+ segment-start-offset (len run))
                   (concat segments offset-graphemes))))))))

(defrecord ParagraphFragment [runs]
  Fragment
  (items [fragment] (:runs fragment))
  (fragment-type [_] :paragraph)

  TextContainer
  (text [f] (reduce str (map text (items f))))
  (len [f] (reduce #(+ %1 (len %2)) 0 (items f)))
  (blank? [f] (zero? (len f))))

(defn fragment
  [run-or-runs]
  (if (sequential? run-or-runs)
    (->ParagraphFragment run-or-runs)
    (->ParagraphFragment [run-or-runs])))

(defn paragraph
  "Creates a new paragraph.

   Paragraph fields:
   - `runs`: vector of runs within the paragraph (never empty, always at least 1 empty run)
   - `type`: The type of paragraph -- either :h1, :h2, :ul, :ol or :body (default)."
  ([]
   (->Paragraph [(r/empty-run)] :body))
  ([runs]
   (->Paragraph (optimize-runs runs) :body))
  ([type runs]
   (->Paragraph (optimize-runs runs) type)))

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
                    (utils/assoc-last optimized (r/insert-end last-optimized-run (:text next-run)))
                    (conj optimized next-run))))
              (vector (first non-empty-runs))
              (subvec non-empty-runs 1)))))

;; These two functions are poorly named, really. Should probably be something like
;; run-idx-and-run-relative-offset-from-paragraph-offset, but I haven't come up with
;; anything that's descriptive enough without being mega-verbose.

;; There are a bit more complicated and loop-y than I would like them to be, but at
;; least the complexity is mostly contained here.

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
        (recur (inc run-idx)
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
(defn delete
  [para sel]
  (if (sel/single? sel)
    (if (zero? (sel/caret sel))
      para
      (let [[run-idx run-offset] (before-offset (:runs para) (sel/caret sel))
            new-runs (update (:runs para) run-idx r/delete run-offset)]
        (assoc para :runs (optimize-runs new-runs))))
    (let [runs (:runs para)
          [before _] (split-runs runs (-> sel :start :offset))
          [_ after] (split-runs runs (-> sel :end :offset))
          new-runs (optimize-runs (concat before after))]
      (assoc para :runs new-runs))))

(defn delete-after
  "Removes everything in paragraph `para` after the provided offset."
  [para offset]
  (let [para-len (len para)]
    (if (= offset para-len)
      para
      (delete para (selection [nil offset] [nil para-len])))))

(defn delete-before
  "Removes everything in paragraph `para` before the provided offset."
  [para offset]
  (delete para (selection [nil 0] [nil offset])))

(defmulti insert "Inserts into the paragraph."
  {:arglists '([paragraph selection content-to-insert])}
  (fn [& args] (type (last args))))

(defmethod insert
  ParagraphFragment
  [para sel {:keys [runs]}]
  (if (sel/range? sel)
    (let [selection-removed (delete para sel)]
      (insert selection-removed (sel/collapse-start sel) runs))
    (let [[before after] (split-runs (:runs para) (sel/caret sel))
          new-runs (concat before runs after)]
      (assoc para :runs (optimize-runs new-runs)))))

(defmethod insert
  r/Run
  [para sel run]
  (insert para sel (fragment run)))

(defmethod insert
  Paragraph
  [para sel para-to-insert]
  (let [new-type (if (zero? (-> sel :start :offset))
                   (:type para-to-insert)
                   (:type para))]
    (assoc (insert para sel (fragment (:runs para-to-insert)))
           :type new-type)))

(defmulti insert-start
  "Inserts at the start of the paragraph."
  (fn [& args] (type (last args))))

(defmulti insert-end
  "Inserts at the end of the paragraph."
  (fn [& args] (type (last args))))

(defmethod insert-start
  ParagraphFragment
  [para runs]
  (insert para (selection [nil 0]) runs))

(defmethod insert-start
  r/Run
  [para run]
  (insert para run))

(defmethod insert-start
  js/String
  [para text]
  (insert para (selection [nil 0]) (r/run text)))

(defmethod insert-start
  Paragraph
  [para para-to-insert]
  (insert para (selection [nil 0]) para-to-insert))

(defmethod insert-end
  ParagraphFragment
  [para fragment]
  (insert para (selection [nil (len para)]) fragment))

(defmethod insert-end
  r/Run
  [para run]
  (insert para (selection [nil (len para)]) run))

(defmethod insert-end
  Paragraph
  [para para-to-insert]
  (insert para (selection [nil (len para)]) para-to-insert))

(defn update-selected-runs
  "Returns a new paragraph with f applied to each run inside the selection."
  [para sel f]
  (let [[before selected after] (separate-selected (:runs para) sel)
        new-runs (-> [before (map f selected) after]
                     flatten vec optimize-runs)]
    (assoc para :runs new-runs)))

(defn formatting-at [para sel]
  (let [[run-idx _] (at-offset (:runs para) (sel/caret sel))]
    (:formats ((:runs para) run-idx))))

(defn formatting-before [para sel]
  (if (zero? (sel/caret sel))
    (throw (js/Error. "This should never happen, check formatting-before function."))
    (formatting-at para (sel/shift-single sel -1))))

(defn whole-paragraph-selected?
  "Returns true if the selection encompasses the whole paragraph."
  [paragraph sel]
  (and (= (-> sel :start :offset) 0)
       (= (-> sel :end :offset) (len paragraph))))

(defn indented?
  "Returns true if there is a tab at the start of the paragraph."
  [paragraph]
  (= "\t" (some-> paragraph :runs (get 0) :text (aget 0))))

(defn indent
  "Adds a tab to the start of the paragraph, if there is not already one present."
  [paragraph]
  (if (indented? paragraph)
    paragraph
    (assoc paragraph :runs (-> (:runs paragraph)
                               (vec)
                               (update-in [0 :text] #(str "\t" %))))))

(defn trim-start
  "Removes any leading whitespace from the paragraph (including tabs)."
  [paragraph]
  (update-in paragraph [:runs 0 :text] #(.trimStart %)))

(defn trim-end
  "Removes any leading whitespace from the paragraph (including tabs)."
  [paragraph]
  (update-in paragraph [:runs (dec (count (:runs paragraph))) :text] #(.trimEnd %)))

(defn apply-format
 ([p format]
  (update p :runs (partial mapv #(r/apply-format % format))))
 ([p sel format]
  (update-selected-runs p sel #(r/apply-format % format))))

(defn remove-format
 ([p format]
  (update p :runs (partial mapv #(r/remove-format % format))))
 ([p sel format]
  (update-selected-runs p sel #(r/remove-format % format))))

(defn toggle-format
 [para sel format]
 (let [[runs-before runs-in-selection runs-after] (separate-selected (:runs para) sel)
       common-formats (->> runs-in-selection
                           (map :formats)
                           (apply set/intersection))
       new-runs-in-selection (if (contains? common-formats format)
                               (mapv #(r/remove-format % format) runs-in-selection)
                               (mapv #(r/apply-format % format) runs-in-selection))
       new-runs (optimize-runs (concat runs-before new-runs-in-selection runs-after))]
   (assoc para :runs new-runs)))

(extend-type Paragraph
  Selectable
  (char-at [para sel]
    (if (>= (sel/caret sel) (len para))
      "" #_(throw (js/Error. "char-at given offset >= length of paragraph"))
      (let [[run-idx run-offset] (at-offset (:runs para) (sel/caret sel))
            run-text (:text ((:runs para) run-idx))]
        (nth run-text run-offset))))

  ;; TODO: graphemes
  (char-before [para sel]
    (if (zero? (sel/caret sel))
      "\n" ; TODO: Not sure if this is the right approach here
      (char-at para (sel/shift-single sel -1)))) ;; TODO: graphemes

  (selected-content [para sel]
    (fragment (second (separate-selected (:runs para) sel))))

  (formatting
    ([para] (formatting para (selection [nil 0]
                                        [nil (len para)])))
    ([para sel]
     (if (sel/single? sel)
       (if (zero? (sel/caret sel)) (formatting-at para sel) (formatting-before para sel))
       (let [runs (:runs para)
             [start-run-idx _] (at-offset runs (-> sel :start :offset))
             [end-run-idx _] (before-offset runs (-> sel :end :offset))
             selected-runs (subvec runs start-run-idx (inc end-run-idx))]
         (->> selected-runs
              (map :formats)
              (apply set/intersection)))))))

;; Operations on multiple paragraphs ;;
(defn merge-paragraphs
  "Merges the two paragraphs. New paragraph will have the type of the __first__ paragraph."
  [p1 p2]
  (paragraph (:type p1) (vec (concat (:runs p1) (:runs p2)))))
