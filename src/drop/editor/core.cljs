(ns drop.editor.core
  (:require [clojure.set :as set]
            [drop.editor.selection :as sel :refer [Selection selection]]
            [drop.editor.vec-utils :as vec-utils]))

;; TODO: spec all this out. Also learn spec :)

;; TODO: I think it might be beneficial to not return vectors of runs/paragraphs,
;; but instead to return paragraphs/documents (respectively). I think this might
;; give us better polymorphism bang-for-buck...

;; Some operations common to Runs, Paragraphs, and Documents

;; This protocol could stand a better name if we're honest
(defprotocol TextContainer
  (text-len [this] "Returns the number of chars in container (run/paragraph)."))

(defprotocol Formattable
  "Primitive operations for formatting text-containers (runs, paragraphs, documents)."
  (apply-format
   [this format]
   [this sel format]
   "Returns a new container with the format applied.
    Arity taking a selection is not implemented for runs.")
  (remove-format
   [this format]
   [this sel format]
   "Returns a new container with the format removed, if it is present.
    Arity taking a selection is not implemented for runs."))

(defprotocol Selectable
  "Any text container for which paragraph offset selections are valid.
   Note that this precludes runs, as they are contained inside paragraphs
   and therefore a 'paragraph offset into a run' would not make sense.
   Basically, this is a set of common operations on paragraphs and documents."

  (selected-content
   [container sel]
   "Returns the content within the range-selection inside the container, either as a vector
    of runs or a vector of paragraphs, depending which is appropriate.")
  (shared-formats
   [container]
   [container sel]
   "Returns the set of all the formats shared by each run that is inside (wholly or
    partially) the selection. Will return an empty set if there are no formats shared.
    If not passed a selection, will return shared formats for the whole container."))

(defn type-dispatch [& args] (mapv type args))

;; TODO: once all implementations of insert are done, there should be a HELLUVA
;; docstring explaining its use...the various forms it can take, etc. The goal is
;; for it to be "don't think about it, it just works, using this general form."
(defmulti insert "Inserts into a Run/Paragraph/Document." #'type-dispatch)
(defmulti insert-start "Shortcut for is at the start of a text container." #'type-dispatch)
(defmulti insert-end "Inserts at the end of a text container." #'type-dispatch)

(defmulti delete "Deletes from a Run/Paragraph/Document." #'type-dispatch)

;;; Run operations ;;;
(defrecord Run [text formats]
  TextContainer
  (text-len [r] (count (:text r)))

  Formattable
  (apply-format
   [r format]
   (update r :formats #(conj % format)))
  (remove-format
   [r format]
   (update r :formats #(disj % format))))

(defn run
  "Returns a new run. A run is defined as text with associating formatting."
  ([text formats]
   (->Run text formats))
  ([text]
   (->Run text #{}))
  ([]
   (->Run "" #{})))

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

(defmethod insert-start [Run js/String]
  [r text]
  (insert r text 0))

(defmethod insert-end [Run js/String]
  [r text]
  (insert r text (text-len r)))

;; Delete between start and end
(defmethod delete [Run js/Number js/Number]
  [run start end]
  (let [before (.slice (:text run) 0 start)
        after (.slice (:text run) end)]
    (assoc run :text (str before after))))

;; Delete at offset, acts like backspace
(defmethod delete [Run js/Number]
  [run caret]
  (delete run (dec caret) caret))

;; TODO: should these 2 be in the Formattable protocol?
(defn apply-formats
  "Returns a new run with the all the supplied formats applied."
  [r formats]
  (update r :formats #(apply conj % formats)))

(defn remove-formats
  "Returns a new run with all the supplied formats removed."
  [r formats]
  (update r :formats #(apply disj % formats)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;;  Paragraph operations  ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Paragraph [runs]
  TextContainer
  (text-len [p] (reduce #(+ %1 (text-len %2)) 0 (:runs p))))

(declare optimize-runs) ;; Forward declare for use in `paragraph` function.

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
;; run-idx-and-run-relative-offset-from-paragraph-offset, but I haven't come up with
;; anything that's descriptive enough and without being verbose.
;;
;; There are a bit more complicated than I would like them to be, but at least the
;; complexity is mostly contained here.

(defn at-offset
  "Returns the index of the run `offset` falls inside of, as well as the
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
         (+ sum-prev-offsets (text-len (nth runs (inc run-idx)))))))))

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
         (+ sum-prev-offsets (text-len (nth runs (inc run-idx)))))))))

;; TODO: I think split-runs and separate-selection could be merged into one
;; function that takes a SELECTION, and either splits AROUND the selection (if
;; single) and returns [before, after], or [before, inside, after] (if range).
;;
;; Performance implications and so forth will need to be considered.
(defn- split-runs
  "Splits runs at offset, returning a vector of [runs before, runs after].
   Will break a run apart if `offset` is inside that run."
  [runs offset]
  (let [runs-len (reduce + (map text-len runs))
        offset-fn (if (= runs-len offset)
                    before-offset
                    at-offset)

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

(defn separate-selected
  "Splits the runs at the beginning and end of the selection and returns three vectors
   of runs: [runs before selection start, runs inside selection, runs after selection end].

   This may seem like an esoteric operation but it's a useful helper for some of the
   core functions (see below)."
  [runs sel]
  (let [[runs-before runs-after] (split-runs runs (-> sel :start :offset))
        runs-before-len (reduce + (map text-len runs-before))
        adjusted-end-offset (- (-> sel :end :offset) runs-before-len)
        [within-sel after-sel] (split-runs runs-after adjusted-end-offset)]
    [runs-before within-sel after-sel]))

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

;; TODO: these might stand some mild testing
(defmethod insert-start [Paragraph PersistentVector]
  [para runs]
  (insert para (selection [-1 0]) runs))

(defmethod insert-start [Paragraph Run]
  [para run]
  (insert para (selection [-1 0]) run))

(defmethod insert-end [Paragraph PersistentVector]
  [para runs]
  (insert para (selection [-1 (text-len para)]) runs))

(defmethod insert-end [Paragraph Run]
  [para run]
  (insert para (selection [-1 (text-len para)]) run))

(defn- paragraph-single-delete [para sel]
  (if (zero? (sel/caret sel))
    para
    (let [[run-idx run-offset] (before-offset (:runs para) (sel/caret sel))
          new-run (-> (nth (:runs para) run-idx)
                      (delete run-offset))
          new-runs (assoc (:runs para) run-idx new-run)]
      (assoc para :runs (optimize-runs new-runs)))))

(defn- paragraph-range-delete [para sel]
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
    (paragraph-single-delete para sel)
    (paragraph-range-delete para sel)))

(defn delete-after
  "Removes everything in paragraph `para` after the provided offset."
  [para offset]
  (let [para-len (text-len para)]
    (if (= offset para-len)
      para
      (delete para (selection [-1 offset] [-1 para-len])))))

(defn delete-before
  "Removes everything in paragraph `para` before the provided offset."
  [para offset]
  (delete para (selection [-1 0] [-1 offset])))

(defn update-selected-runs
  "Returns a new paragraph with f applied to each run inside the selection."
  [para sel f]
  (let [[before selected after] (separate-selected (:runs para) sel)
        selected-updated (map f selected)]
    (paragraph (flatten [before selected-updated after]))))

;; TODO: write tests for Selectable functions
(extend-type Paragraph
  Selectable
  ;; TODO: should this return a paragraph instead of a list of runs?
  (selected-content [para sel] (second (separate-selected (:runs para) sel)))
  (shared-formats
   ([para] (shared-formats para (selection [-1 0] [-1 (text-len para)])))
   ([para sel]
    (let [runs (:runs para)
          [start-run-idx _] (at-offset runs (-> sel :start :offset))
          [end-run-idx _] (before-offset runs (-> sel :end :offset))
          selected-runs (subvec runs start-run-idx (inc end-run-idx))]
      (->> selected-runs
           (map :formats)
           (apply set/intersection)))))

  ;; TODO: test these
  Formattable
  (apply-format
   ([p format] (update p :runs (partial map #(apply-format % format))))
   ([p sel format] (update-selected-runs p sel #(apply-format % format))))
  (remove-format
   ([p format] (update p :runs (partial map #(remove-format % format))))
   ([p sel format] (update-selected-runs p sel #(remove-format % format)))))

#_(defn shared-formats
   "Returns the set of all the formats shared by each run that is inside (wholly or
   partially) the selection. Will return an empty set if there are no formats shared."
   ([para] (shared-formats para (selection [-1 0] [-1 (text-len para)])))
   ([para sel]
    (let [runs (:runs para)
          [start-run-idx _] (at-offset runs (-> sel :start :offset))
          [end-run-idx _] (before-offset runs (-> sel :end :offset))
          selected-runs (subvec runs start-run-idx (inc end-run-idx))]
      (->> selected-runs
           (map :formats)
           (apply set/intersection)))))

;; TODO: make part of selectable protocol
(defn toggle-format
  "Either applies the selected format to the selection (if the selected text
   does not already have that format) or removes it (if the selected text **does**
   have that format)."
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

;; Operations on multiple paragraphs ;;
(defn merge-paragraphs
  "Merges the two paragraphs."
  [p1 p2]
  (paragraph (concat (:runs p1) (:runs p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;;  Document operations  ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Document [children]
  TextContainer
  (text-len [doc] (reduce #(+ %1 (text-len %2)) 0 (:children doc))))

(defn document
  "Creates a new document."
  ([children]
   (->Document children))
  ([]
   (->Document [(paragraph)])))

;; Document helper functions
(defn- split-paragraph
  "Splits the selected paragraph at the (single) selection and returns the two halves."
  [doc sel]
  (map paragraph (-> (:children doc)
                     (get (-> sel :start :paragraph))
                     (get :runs)
                     (split-runs (sel/caret sel)))))

(defn- merge-paragraph-with-previous
  "Returns a new doc with the paragraph at `para-idx` merged into the one before it."
  [doc para-idx]
  (let [children (:children doc)
        para (nth children para-idx)
        prev (nth children (dec para-idx))
        merged (insert-end prev (:runs para))
        new-children (vec-utils/replace-range children (dec para-idx) para-idx merged)]
    (assoc doc :children new-children)))

(defn- replace-paragraph-with
  "Returns a new doc with the paragraph at `para-idx` replaced with
   `content`, which can be either a paragraph or a list of paragraphs."
  [doc para-idx content]
  (update doc :children #(vec-utils/replace-range % para-idx para-idx content)))

(defn- insert-into-single-paragraph
  "Helper function. For document inserts where we only have to worry about a single paragraph,
   meaning we can basically just delegate to the paragraph insert function and replace the paragraph."
  [doc sel run]
  (let [target-idx (-> sel :start :paragraph)
        target-para (nth (:children doc) target-idx)
        new-para (insert target-para sel run)]
    (assoc-in doc [:children target-idx] new-para)))

;; TODO: switch this to a faster concat using the rrb library
(defn- insert-paragraphs-into-doc
  "Helper function. Inserts multiple paragraphs into the document.
   The selection MUST be a single-selection. This is just a helper and
   it's assumed any deleting of a range selection has already been done."
  [doc sel paragraphs]
  (let [target-para-idx (-> sel :start :paragraph)
        target-para ((:children doc) target-para-idx)
        sel-caret (sel/caret sel)

        first-paragraph
        (-> target-para
            (delete-after sel-caret)
            (insert-end (:runs (first paragraphs))))

        last-paragraph
        (-> target-para
            (delete-before sel-caret)
            (insert-start (:runs (peek paragraphs))))

        in-between-paragraphs
        (subvec paragraphs 1 (dec (count paragraphs)))

        all-modified-paragraphs
        (flatten [first-paragraph in-between-paragraphs last-paragraph])

        new-children
        (vec-utils/replace-range (:children doc)
                                 target-para-idx
                                 target-para-idx
                                 all-modified-paragraphs)]
    (assoc doc :children new-children)))

;; Document main operations
;; TODO: could we make the delete-before-moving on logic here a little more elegant with a `cond->` form?
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

(defn insert-paragraph-before
  "Inserts an empty paragraph into the document immediately before the paragraph at position `index`."
  [doc index]
  #_(update doc :children #(vec-utils/replace-range % index index [(paragraph) (% index)]))
  (replace-paragraph-with doc index [(paragraph) ((:children doc) index)]))

(defn insert-paragraph-after
  "Inserts an empty paragraph into the document immediately after the paragraph at position `index`."
  [doc index]
  #_(update doc :children #(vec-utils/replace-range % index index [(% index) (paragraph)]))
  (replace-paragraph-with doc index [((:children doc) index) (paragraph)]))

(defn- doc-single-delete [doc sel]
  (if (zero? (sel/caret sel))
    (if (zero? (sel/start-para sel))
      doc
      (merge-paragraph-with-previous doc (-> sel :start :paragraph)))
    (update-in doc [:children (sel/start-para sel)] #(delete % sel))))

(defn- doc-range-delete [doc sel]
  (let [startp-idx (-> sel :start :paragraph)
        endp-idx (-> sel :end :paragraph)
        children (:children doc)
        ;; Replace one paragraph if start and end in the same paragraph, or all of them if not.
        new-para (if (= startp-idx endp-idx)
                   (delete ((:children doc) startp-idx) sel)
                   (merge-paragraphs
                    (delete-after (children startp-idx) (-> sel :start :offset))
                    (delete-before (children endp-idx) (-> sel :end :offset))))
        new-children (vec-utils/replace-range children startp-idx endp-idx new-para)]
    (assoc doc :children new-children)))

(defmethod delete [Document Selection]
  [doc sel]
  (if (sel/single? sel)
    (doc-single-delete doc sel)
    (doc-range-delete doc sel)))

;; TODO: implement
(defn enter
  "Equivalent to what happens when the user hits the enter button.
   Creates a new paragraph in the appropriate position in the doc."
  [doc sel]
  (let [caret (sel/caret sel)
        para-idx (-> sel :start :paragraph)
        para ((:children doc) para-idx)]
    (cond
      (= caret 0)
      (insert-paragraph-before doc (-> sel :start :paragraph))

      (= caret (text-len para))
      (insert-paragraph-after doc (-> sel :start :paragraph))

      :else
      (let [[para1 para2] (split-paragraph doc sel)]
        (replace-paragraph-with doc para-idx [para1 (paragraph) para2])))))

;; TODO: move to block below?
(defn doc-selected-content
  [doc sel]
  (let [start-para-idx (-> sel :start :paragraph)
        start-para ((:children doc) start-para-idx)
        end-para-idx (-> sel :end :paragraph)
        end-para ((:children doc) end-para-idx)]
    (if (sel/single-paragraph? sel)
      (selected-content ((:children doc) start-para-idx) sel)
      ((comp vec flatten) [(delete-before start-para (-> sel :start :offset))
                           (subvec (:children doc) (inc start-para-idx) end-para-idx)
                           (delete-after end-para (-> sel :end :offset))]))))

(defn doc-shared-formats [doc sel]
  (if (sel/single-paragraph? sel)
    (shared-formats ((:children doc) (-> sel :start :paragraph)) sel)
    (->> (selected-content doc sel)
         (map shared-formats)
         (apply set/intersection))))

(defn doc-toggle-format [doc sel format]
  (if (sel/single-paragraph? sel)
    (update-in doc [:children (sel/start-para sel)] #(toggle-format % sel format))
    (let [children (:children doc)
          common-formats (shared-formats doc sel)
          format-fn (if (common-formats format) remove-format apply-format)
          start-para-idx (-> sel :start :paragraph)
          end-para-idx (-> sel :end :paragraph)
          start-para (children start-para-idx)
          end-para (children end-para-idx)
          new-start-para (format-fn
                          start-para
                          (selection [-1 (-> sel :start :offset)] [-1 (text-len start-para)])
                          format)
          new-end-para (format-fn
                        end-para
                        (selection [-1 0] [-1 (-> sel :end :offset)])
                        format)
          inbetween-paras (subvec (selected-content doc sel) (inc start-para-idx) end-para-idx)
          inbetween-paras-new (map #(format-fn % format) inbetween-paras)
          new-children (-> children
                           (assoc start-para-idx new-start-para)
                           (assoc end-para-idx new-end-para)
                           (vec-utils/replace-range (inc start-para-idx) (dec end-para-idx) inbetween-paras-new))]
      (assoc doc :children new-children))))

;; TODO: should the functions be inlined here?
(extend-type Document
  Selectable
  (selected-content [doc sel] (doc-selected-content doc sel))
  (shared-formats [doc sel] (doc-shared-formats doc sel)))

;; TODO: implement toggle-formats (protocol?)
;; TODO: navigable functions for document
;; TODO: render document ;)
;; TODO: functions will need to be modified to return new selection
;;       (and possible the list of changes)

;; foobarbizzbuzz
;; aaabbbcccddd
(def p1 (paragraph [(run "foo" #{:italic})
                    (run "bar" #{:bold :italic})
                    (run "bizz" #{:italic})
                    (run "buzz" #{:bold :italic})]))

(def p2 (paragraph [(run "aaa" #{:bold :italic})
                    (run "bbb" #{})
                    (run "ccc" #{})
                    (run "ddd" #{})]))

(def to-insert [(paragraph [(run "inserted paragraph 1")])
                (paragraph [(run "inserted paragraph 2")])
                (paragraph [(run "inserted paragraph 3")])])

(def doc (->Document [p1 #_(paragraph [(run "111")]) p2]))

(def long-doc (document [(paragraph [(run "foo1" #{:italic})])
                         (paragraph [(run "foo2" #{:bold})])
                         (paragraph [(run "foo3" #{:underline})])
                         (paragraph [(run "foo4" #{:strike})])]))

;; (doc-toggle-format #p doc (selection [0 6] [1 3]) :bold)

;; TODO: identify bug and fix
(doc-toggle-format long-doc (selection [0 0] [2 4]) :italic)

;; (doc-selected-content doc (selection [0 3] [0 14]))
;; (selected-content doc (selection [0 3] [1 3]))

;; (doc-shared-formats doc (selection [0 0] [0 8]))
;; (doc-shared-formats doc (selection [0 0] [1 3]))

;; (selected-content long-doc (selection [0 0] [3 3]))

;; (enter doc (selection [0 10]))

;; (insert-paragraph-after doc 1)

;; (delete doc (selection [0 4]))
;; (delete doc (selection [1 0]))

;; (delete doc (selection [0 3] [1 3]))

;; (insert doc
;;         (selection [0 3] [1 9])
;;         [(run "Hello" #{:italic}) (run "Goodbye!")])

;; (insert doc (selection [0 10]) to-insert)

;; (insert doc (selection [0 14]) to-insert)
