(ns drop.editor.core
  (:require [clojure.set :as set])
  (:require [drop.editor.selection :as sel :refer [Selection selection]]))

;; TODO: spec all this out. Also learn spec :)

;;; Selection operations ;;;
;; This protocol could stand a better name if we're honest
(defprotocol TextContainer
  (text-len [this] "Returns the number of chars in container (run/paragraph)."))

(defn type-dispatch [& args] (mapv type args))

(defmulti insert "Inserts into a Run/Paragraph/Document." #'type-dispatch)
(defmulti insert-start "Shortcut for is at the start of a text container." #'type-dispatch)
(defmulti insert-end "Inserts at the end of a text container." #'type-dispatch)

(defmulti delete "Deletes from a Run/Paragraph/Document." #'type-dispatch)

;;; Run operations ;;;
(defrecord Run [text formats]
  TextContainer
  (text-len [r] (count (:text r))))

(defn run
  "Returns a new run. A run is defined as text with associating formatting."
  ([text formats]
   (->Run text formats))
  ([text]
   (->Run text #{}))
  ([]
   (->Run "text" #{})))

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
    (= offset (text-len r)) [r (empty-run)]
    :else (let [text-before (.slice (:text r) 0 offset)
                text-after (.slice (:text r) offset (text-len r))]
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

;; TODO: make these two multimethods and implement them for paragraphs as well
(defmethod insert-start [Run js/String]
  [run text]
  (insert run text 0))

(defmethod insert-end [Run js/String]
  [run text]
  (insert run text (text-len run)))

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
  (text-len [p] (reduce #(+ %1 (text-len %2)) 0 (:runs p))))

(declare optimize-runs) ;; Forward declare for use in `paragraph` function.

;; TODO: should we change these to ->paragraph and ->run to make more clear?
(defn paragraph
  "Creates a new paragraph."
  ([runs]
   (->Paragraph (optimize-runs runs)))
  ([]
   (->Paragraph [(run)])))

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
       (+ sum-prev-offsets (text-len (nth runs (inc run-idx))))))))

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
       (+ sum-prev-offsets (text-len (nth runs (inc run-idx))))))))

(defn- split-runs
  "Splits runs at offset, returning a vector of [runs before, runs after].
   Will break a run apart if `offset` is inside that run."
  [runs offset]
  (let [offset-fn (if (zero? offset)
                     at-offset
                     before-offset)

        ;; Split the run at the caret position
        [target-run-idx target-run-offset] (offset-fn runs offset)
        target-run (nth runs target-run-idx)
        [target-before target-after] (split target-run target-run-offset)

        ;; Get runs before and after the run that the caret is inside of
        runs-before (vec (take target-run-idx runs))
        runs-after (vec (drop (inc target-run-idx) runs))

        before (conj runs-before target-before)
        after (into [target-after] runs-after)]
    [before after]))

;; Main Paragraph operations
(defmethod insert [Paragraph Selection PersistentVector]
  [para sel runs]
  (if (sel/single? sel)
    (let [[before after] (split-runs (:runs para) (sel/caret sel))
          new-runs (concat before runs after)]
      (assoc para :runs (optimize-runs new-runs)))
    (let [selection-removed (delete para sel)]
      (insert selection-removed (sel/collapse-start sel) runs))))

(defmethod insert [Paragraph Selection Run]
  [para sel r]
  (insert para sel [r]))

(defn- single-delete-paragraph [para sel]
  (let [[run-idx run-offset] (before-offset (:runs para) (sel/caret sel))
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
  (if (sel/single? sel)
    (single-delete-paragraph para sel)
    (range-delete-paragraph para sel)))

(defn delete-after
  "Removes everything in paragraph `para` after the provided offset."
  [para offset]
  (delete para (selection [para offset] [para (text-len para)])))

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

(defn separate-selected
  "Splits the runs at the beginning and end of the selection and returns three vectors
   of runs: [runs before selection start, runs inside selection, runs after selection end].

   This may seem like an esoteric operation but it's a useful helper for some of the
   core functions (see below)."
  [runs sel]
  (let [[before-sel after-start] (split-runs runs (-> sel :start :offset))
        before-sel-len (reduce + (map text-len before-sel))
        adjusted-end-offset (- (-> sel :end :offset) before-sel-len)
        [within-sel after-sel] (split-runs after-start adjusted-end-offset)]
    [before-sel within-sel after-sel]))

(defn selected-content
  "Returns the content within the range-selection
   inside the paragraph, as a vector of runs."
  [para sel]
  (let [[_before within _after] (separate-selected (:runs para) sel)]
    within))

(defn toggle-format
  "Either applies the selected format to the selection (if the selected text
   does not already have that format) or removes it (if the selected text **does**
   have that format)."
  [para sel format]
  (let [[before in-selection after]
        (separate-selected (:runs para) sel)

        common-formats
        (->> in-selection
             (map :formats)
             (apply set/intersection))

        in-selection-updated
        (if (contains? common-formats format)
          (mapv #(remove-format % format) in-selection)
          (mapv #(apply-format % format) in-selection))

        new-runs
        (optimize-runs (concat before in-selection-updated after))]
    (assoc para :runs new-runs)))

;; TODO next: write Document stuff
;; - insert
;; - delete

;;; Document operations ;;;
(defrecord Document [children]
  TextContainer
  (text-len [doc] (reduce #(+ %1 (text-len %2)) 0 (:children doc))))

(defn document
  "Creates a new document."
  ([children]
   (->Document children))
  ([]
   (->Document [(paragraph)])))

(defn- insert-into-single-paragraph
  "Helper function. For document inserts where we only have to worry about a single paragraph,
   meaning we can basically just delegate to the paragraph insert function and replace the paragraph."
  [doc sel run]
  (let [target-idx (-> sel :start :paragraph)
        target-para (nth (:children doc) target-idx)
        new-para (insert target-para sel run)]
    (assoc-in doc [:children target-idx] new-para)))

(defn- insert-paragraphs-into-doc
  "Helper function. Inserts multiple paragraphs into the document."
  [doc sel paragraphs]
  (let [target-para-idx (-> sel :start :paragraph)
        target-para (nth (:children doc) target-para-idx)
        [before-caret after-caret] (split-runs (:runs target-para) (sel/caret sel))

        first-paragraph
        (paragraph (concat before-caret (:runs (first paragraphs))))
        ;; TODO: try and rewrite this a bit more elegantly using insert-end/insert-start.
        ;; Will need to implement those as multimethods?
        #_(-> target-para
              (delete-after (sel/caret sel))
              (insert-end (:runs (first paragraphs))))
        #_(insert-end (delete-after target-para (sel/caret sel)) (:runs (first paragraphs)))

        last-paragraph
        (paragraph (concat (:runs (peek paragraphs)) after-caret))

        in-between-paragraphs
        (subvec paragraphs 1 (-> paragraphs count dec))

        new-children
        (concat (subvec (:children doc) 0 target-para-idx)
                (flatten [first-paragraph in-between-paragraphs last-paragraph])
                (subvec (:children doc) (inc target-para-idx)))]
    (assoc doc :children new-children)))

(defmethod insert [Document Selection PersistentVector]
  [doc sel runs-or-paras]
  (if (sel/single? sel)
    (condp = (type (first runs-or-paras))
      Run (insert-into-single-paragraph doc sel runs-or-paras)
      Paragraph (insert-paragraphs-into-doc doc sel runs-or-paras))
    (insert (delete doc sel) (sel/collapse-start sel) runs-or-paras)))

(defmethod insert [Document Selection Paragraph]
  [doc sel para]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel (:runs para))
    (insert-into-single-paragraph (delete doc sel) (sel/collapse-start sel) (:runs para))))

(defmethod insert [Document Selection Run]
  [doc sel r]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel r)
    (insert-into-single-paragraph (delete doc sel) (sel/collapse-start sel) r)))

(defmethod insert [Document Selection js/String]
  [doc sel text]
  (insert-into-single-paragraph doc sel (run text)))

;; foobarbizzbuzz
(def p1 (paragraph [(run "foo" #{:italic})
                    (run "bar" #{:bold :italic})
                    (run "bizz" #{:italic})
                    (run "buzz" #{:bold})]))

(def p2 (paragraph [(run "aaa" #{})
                    (run "bbb" #{})
                    (run "ccc" #{})
                    (run "ddd" #{})]))

(def to-insert [(paragraph [(run "inserted paragraph 1")])
                (paragraph [(run "inserted paragraph 2")])
                (paragraph [(run "inserted paragraph 3")])])

(def doc (->Document [p1 p2]))

;; (insert doc
;;         (selection [0 3])
;;         [(run "Hello" #{:italic}) (run "Goodbye!")])

(insert doc (selection [0 10]) to-insert)
