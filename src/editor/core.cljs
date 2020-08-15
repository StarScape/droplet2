(ns editor.core)

;;; Selection operations ;;;

; ;; TODO: spec this all out. Also learn spec :)
(defn selection
  "Creates a new selection."
  ([[start-paragraph start-offset] [end-paragraph end-offset] backwards?]
   {:start {:paragraph start-paragraph
            :offset start-offset}
    :end {:paragraph end-paragraph
          :offset end-offset}
    :backwards? backwards?})
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
;; (defn collapse [sel]
;;   (if (single? sel)
;;     sel
;;     (if (:backwards sel)
;;       (collapse-start sel)
;;       (collapse-end sel))))

;;; Run operations ;;;
(defn run
  "Returns a new run. A run is defined as text with associating formatting."
  ([text formats]
   {:text text, :formats formats})
  ([text]
   {:text text, :formats #{}}))

(defn empty-run "Returns an empty run." [] (run ""))

(defn empty-run?
  "Returns true if the run is empty."
  [r]
  (or (= "" (:text r)) (= nil (:text r))))

(defn len
  "Returns the number of characters in the run."
  [r]
  (count (:text r)))

(defn split
  "Splits the run at `offset`, returning a vector of two new runs containing the text to either side."
  [r offset]
  (cond
    (zero? offset) [(empty-run) r]
    (= offset (len r)) [r (empty-run)]
    :else (let [text-before (.slice (:text r) 0 offset)
                text-after (.slice (:text r) offset (len r))]
            [(run text-before (:formats r)), (run text-after (:formats r))])))

(defn insert
  "Insert insert text into run at the given selection."
  ;; Range selection, remove block-selected text and then insert.
  ([r text start end]
   (let [before (.slice (:text r) 0 start)
         after (.slice (:text r) end)]
     (assoc r :text (str before text after))))
  ;; Single selection
  ([r text caret]
   (insert r text caret caret)))

(defn insert-start
  "Shortcut for inserting text at the start of a run."
  [run text]
  (insert run text 0))

(defn insert-end
  "Shortcut for inserting text at the end of a run."
  [run text]
  (insert run text (count (:text run))))

(defn delete
  "Remove the text between start and end. If passed a single position, acts like backspace."
  ([run start end]
   (let [before (.slice (:text run) 0 start)
         after (.slice (:text run) end)]
     (assoc run :text (str before after))))
  ([run caret]
   (delete run (dec caret) caret)))

(defn- toggle-set-entry [s v]
  (if (contains? s v)
    (disj s v)
    (conj s v)))

(defn toggle-format
  "Toggles the provided format on Run `r`. E.g. if `format` is not present it will be added, if
   it is it will be turned off. Providing a format that is not present in `r` will have no effect."
  [r format]
  (update r :formats #(toggle-set-entry % format)))

(defn toggle-formats
  "The same as `toggle-format`, but takes a coll of all formats to toggle."
  [r formats-coll]
  (assoc r :formats (reduce toggle-set-entry (:formats r) formats-coll)))

;; TODO: do we need apply-formats and remove-formats?
;; TODO: write tests for run


;;; Paragraph operations ;;;

;; Forward declare for use in `paragraph` function.
(declare optimize-runs)

(defn paragraph
  "Creates a new paragraph."
  [runs]
  {:runs (optimize-runs runs)})

(defn- optimize-runs
  "Merges adjacent runs that have the same formatting, and removes empty runs.
   Will return an empty run if one is passed in, or all runs have no content."
  [runs]
  (let [non-empty-runs (filterv (complement empty-run?) runs)]
    (reduce
     (fn [optimized next-run]
       (let [top (peek optimized)]
         (if (= (:formats next-run) (:formats top))
           (-> optimized
               pop
               (conj (insert-end top (:text next-run))))
           (conj optimized next-run))))
     (vector (first non-empty-runs))
     (subvec non-empty-runs 1))))

(def sum-runs [(run "pre" #{})
               (run "Foo" #{:italic})
               (run "" #{:somethin-else})
               (run "bar" #{:italic})
               (run "bizz" #{:bold})
               (run "buzz" #{:bold :underline})])

(optimize-runs sum-runs)

(paragraph [(run "Foo" #{:italic}) (run "bar" #{:bold})])

(def r1 (run "Foobar" #{:italic :bold}))

(toggle-format r1 :bold)
(toggle-formats r1 #{:underline})

(comment
  (empty-run? (run "Foo" #{:italic :bold :underline}))
  (empty-run? (empty-run))
  (def r (run "Foo"))
  (insert-end r "Bar")
  (insert-start r "oof")
  (delete r 1 3)
  (delete r 3))
