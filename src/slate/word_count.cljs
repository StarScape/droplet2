(ns slate.word-count
  (:require [clojure.string :as str]
            [slate.model.common :as m]))

;; TODO: a lil caching?
(defn word-count [s]
  (.. s (trim) (split #"\s+") -length))

(comment
  (word-count "Hello world!")
  (word-count "Hello world ")
  (word-count " Hello  world there")
  )

(defn- words-in-paragraphs
  "Takes a list of paragraphs and returns the total number of words in them."
  [paragraphs]
  (reduce (fn [words, paragraph]
            (+ words (word-count (m/text paragraph))))
          0 paragraphs))

(defn full-count
  "Returns the total number of words in the document."
  [doc]
  (words-in-paragraphs (:children doc)))

(defn update-count
  [word-count prev-doc new-doc {:keys [inserted-uuids changed-uuids deleted-uuids]}]
  (let [deleted-paragraphs (map #(get (:children prev-doc) %) deleted-uuids)
        inserted-paragraphs (map #(get (:children new-doc) %) inserted-uuids)
        old-changed-paragraphs (map #(get (:children prev-doc) %) changed-uuids)
        new-changed-paragraphs (map #(get (:children new-doc) %) changed-uuids)]
    (-> word-count
        (+ (words-in-paragraphs inserted-paragraphs))
        (- (words-in-paragraphs deleted-paragraphs))
        (- (words-in-paragraphs old-changed-paragraphs))
        (+ (words-in-paragraphs new-changed-paragraphs)))))

