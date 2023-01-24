(ns slate.model.run
  "Runs are the basic building blocks of a document: a string with associated styling.
   This namespace contains the functions for dealing with Runs."
  (:require [slate.model.common :refer [TextContainer
                                        len
                                        graphemes]]
            [slate.utils :refer [weak-cache]]
            [slate.utils :refer-macros [weak-cache-val]]))

(defrecord Run [text formats]
  TextContainer
  (text [r] (:text r))
  (len [r] (count (:text r)))
  (blank? [r] (or (= "" (:text r)) (= nil (:text r))))
  (graphemes [r] (weak-cache-val r (graphemes (:text r)))))

(defn run
  "Constructor for a new run, a container for text with associated formatting."
  ([text formats]
   (->Run text formats))
  ([text]
   (->Run text #{}))
  ([]
   (->Run "" #{})))

(defn empty-run "Helper function, returns an empty run." [] (run ""))

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
  ;; Range selection, remove block-selected text and then insert.
  ([r text start end]
   (let [before (.slice (:text r) 0 start)
         after (.slice (:text r) end)]
     (assoc r :text (str before text after))))

  ;; Single selection insert, nothing removed.
  ([r text caret]
   (assert (and (number? caret) (string? text)))
   (insert r text caret caret)))

(defn insert-start
  "Inserts `text` at the start of the run."
  [r text]
  (insert r text 0))

(defn insert-end
  "Inserts `text` at the end of the run."
  [r text]
  (insert r text (len r)))

;; Delete between start and end
(defn delete
  ;; Delete in range
  ([run start-offset end-offset]
   (let [before (.slice (:text run) 0 start-offset)
         after (.slice (:text run) end-offset)]
     (assoc run :text (str before after))))
  ;; Delete at offset, acts like backspace
  ([run offset]
   (let [graphemes (graphemes run)
         prev-grapheme (first (reverse (filter #(< (:offset %) offset) graphemes)))]
     (delete run (:offset prev-grapheme) offset))))

(defn apply-format
 [r format]
 (update r :formats #(conj % format)))

(defn remove-format
 [r format]
 (update r :formats #(disj % format)))
