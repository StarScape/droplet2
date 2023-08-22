(ns slate.word-count
  (:require [clojure.string :as str]
            [slate.model.common :as m]
            [slate.model.paragraph :as p]
            [slate.model.selection :as sel])
  (:refer-clojure :exclude [update]))

;; TODO: a lil caching?
(defn str-word-count [s]
  (if (str/blank? s)
    0
    (.. s (trim) (split #"\s+") -length)))

(comment
  (str-word-count "")
  (str-word-count "Hello")
  (str-word-count "Hello world!")
  (str-word-count "Hello world ")
  (str-word-count " Hello  world there")
  )

(defn- paragraph-word-count [paragraph]
  (str-word-count (m/text paragraph)))

(defn- paragraphs-word-count
  "Takes a list of paragraphs and returns the total number of words in them."
  [paragraphs]
  (reduce (fn [words, paragraph]
            (+ words (paragraph-word-count paragraph)))
          0 paragraphs))

(defn update-total-count
  [total-word-count prev-doc new-doc {:keys [inserted-uuids changed-uuids deleted-uuids] :as _changelist}]
  (let [deleted-paragraphs (map #(get (:children prev-doc) %) deleted-uuids)
        inserted-paragraphs (map #(get (:children new-doc) %) inserted-uuids)
        old-changed-paragraphs (map #(get (:children prev-doc) %) changed-uuids)
        new-changed-paragraphs (map #(get (:children new-doc) %) changed-uuids)]
    (-> total-word-count
        (+ (paragraphs-word-count inserted-paragraphs))
        (- (paragraphs-word-count deleted-paragraphs))
        (- (paragraphs-word-count old-changed-paragraphs))
        (+ (paragraphs-word-count new-changed-paragraphs)))))

(defn total-count
  "Returns the total number of words in the document."
  [doc]
  (paragraphs-word-count (:children doc)))

(defn selection-count [{:keys [selection] :as editor-state}]
  (if (sel/single? selection)
    nil
    (let [selected-fragment (m/selected-content editor-state)]
      (case (m/fragment-type selected-fragment)
        :document (paragraphs-word-count (m/items selected-fragment))
        :paragraph (paragraph-word-count (p/paragraph (m/items selected-fragment)))))))

(defn update [word-count prev-state {:keys [editor-state changelist] :as _editor-update}]
  (-> word-count
      (clojure.core/update :total update-total-count (:doc prev-state) (:doc editor-state) changelist)
      (assoc :selection (selection-count editor-state))))

(defn init
  ([{:keys [doc] :as editor-state}]
   {:total (total-count doc)
    :selection (selection-count editor-state)})
  ([]
   {:total 0
    :selection nil}))

