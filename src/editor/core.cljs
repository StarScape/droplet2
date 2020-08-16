(ns editor.core)

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

;; TODO: do we need apply-formats and remove-formats?
;; TODO: write tests for run

;;; Paragraph operations ;;;
(defrecord Paragraph [runs]
  TextContainer
  (len [p] (reduce #(+ %1 (len %2)) 0 (:runs p))))

(declare optimize-runs) ;; Forward declare for use in `paragraph` function.

(defn paragraph
  "Creates a new paragraph."
  [runs]
  (->Paragraph (optimize-runs runs)))

(defn- optimize-runs
  "Merges adjacent runs that have the same formatting, and removes empty runs.
   Will return an empty run if one is passed in, or all runs have no content."
  [runs]
  (let [non-empty-runs (filterv (complement empty-run?) runs)]
    (reduce (fn [optimized next-run]
              (let [top (peek optimized)]
                (if (= (:formats next-run) (:formats top))
                  (-> (pop optimized)
                      (conj (insert-end top (:text next-run))))
                  (conj optimized next-run))))
            (vector (first non-empty-runs))
            (subvec non-empty-runs 1))))

;; (defmethod insert [Paragraph PersistentVector Selection]
;;   [para runs sel]
;;   (if (single? sel)))

(comment
  (def sum-runs [(run "pre" #{})
                 (run "Foo" #{:italic})
                 (run "" #{:strike})
                 (run "bar" #{:italic})
                 (run "bizz" #{:bold})
                 (run "buzz" #{:bold :underline})])
  (paragraph sum-runs)
  (=
   {:text "foo" :formats #{:bar}}
   (into {} (run "foo" #{:bar})))
  (run "foo"))
