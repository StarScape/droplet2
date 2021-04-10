(ns slate.model.run
  "Runs are the basic building blocks of a document: a string with associated styling.
   This namespace contains the functions for dealing with Runs."
  (:require [slate.model.common :refer [TextContainer
                                        Formattable
                                        insert
                                        delete
                                        insert-start
                                        insert-end
                                        len]]))

(defrecord Run [text formats]
  TextContainer
  (len [r] (count (:text r)))
  (blank? [r] (or (= "" (:text r)) (= nil (:text r))))

  Formattable
  (apply-format
    [r format]
    (update r :formats #(conj % format)))
  (remove-format
    [r format]
    (update r :formats #(disj % format))))

(defn run
  "Constructor for a new run. A run is defined as text with associating formatting."
  ([text formats]
   (->Run text formats))
  ([text]
   (->Run text #{}))
  ([]
   (->Run "" #{})))

(defn empty-run "Helper function, returns an empty run." [] (run ""))

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

(defmethod insert-start [Run js/String]
  [r text]
  (insert r text 0))

(defmethod insert-end [Run js/String]
  [r text]
  (insert r text (len r)))

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

;; TODO: should these 2 be in the Formattable protocol? Are they needed at all?
(defn apply-formats
  "Returns a new run with the all the supplied formats applied."
  [r formats]
  (update r :formats #(apply conj % formats)))

(defn remove-formats
  "Returns a new run with all the supplied formats removed."
  [r formats]
  (update r :formats #(apply disj % formats)))
