(ns slate.model.find-and-replace
  (:require [slate.model.common :as m]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.run :as r]
            [slate.model.paragraph :as p]
            [slate.model.doc :as doc]
            [slate.model.editor-state :as es :refer [>>=]])
  (:refer-clojure :exclude [find replace]))

(defn- find-all-occurences
  "Returns the starting indices of all occurrences of `substr` within `str`."
  [str substr]
  ;; TODO: make return lazy seq
  (loop [start-idx 0
         results []]
    (let [idx (.indexOf str substr start-idx)]
      (if (not= idx -1)
        (recur (+ idx (.-length substr))
               (conj results idx))
        results))))

(comment
  ;; [3 9 15]
  (find-all-occurences "foobarfoobarfoobar" "bar")
  (find-all-occurences "foobarfoobarfoobar" "bizz")
  )

(defn paragraph-find [paragraph text]
  (let [uuid (:uuid paragraph)
        paragraph-text (m/text paragraph)
        offsets (find-all-occurences paragraph-text text)]
    (map #(selection [uuid %] [uuid (+ % (.-length text))]) offsets)))

(comment
  (def p (p/paragraph "p1" [(r/run "foo") (r/run "bar" #{:italic})
                            (r/run "goo") (r/run "bar" #{:bold})
                            (r/run "hoo") (r/run "bar" #{:underline})]))
  (paragraph-find p "bar")
  )

(defn find [editor-state text]
  (->> (-> editor-state :doc :children)
       (map #(paragraph-find % text))
       (flatten)))

(comment
  (def doc (document [(paragraph "p1" [(run "foo") (run "bar" #{:italic})
                                       (run "goo") (run "bar" #{:bold})
                                       (run "hoo") (run "bar" #{:underline})])
                      (paragraph "p2" [(run "one a one, and a foo, and a bar!")])]))
  (find (editor-state doc) "foo")
  (find (editor-state doc) "bar")
  )

(defn- adjust-selection [sel n]
  (-> sel
      (update-in [:start :offset] + n)
      (update-in [:end :offset] + n)))

(defn paragraph-replace [paragraph locations text]
  (loop [para paragraph
         offset-change 0
         [location & ls] locations]
    (if location
      (let [sel (adjust-selection location offset-change)
            prior-formatting (m/formatting paragraph sel)
            selected-length (- (-> sel :end :offset) (-> sel :start :offset))
          ; shift future offsets by difference between oldtext and newtext
            new-offset-change (- selected-length (.-length text))
            new-para (-> para
                         (m/delete sel)
                         (m/insert (sel/collapse-start sel) (r/run text prior-formatting)))]
        (recur new-para new-offset-change ls))
      para)))

(comment
  (def p (paragraph "p1" [(run "foo") (run "bar" #{:italic})
                          (run "goo") (run "bar" #{:bold})
                          (run "hoo") (run "bar" #{:underline})]))
  (paragraph-replace p [(selection ["p1" 3] ["p1" 6])
                        (selection ["p1" 9] ["p1" 12])
                        (selection ["p1" 15] ["p1" 18])] "FAR")
  )

(defn replace
  "Returns an EditorUpdate replacing the Selection `location` with `text`."
  [editor-state location text]
  (let [para-uuid (sel/caret-para location)
        para (get (-> editor-state :doc :children) para-uuid)
        new-para (paragraph-replace para [location] text)]
    (es/replace-paragraph editor-state para-uuid new-para)))

(defn replace-all
  "Returns an EditorUpdate replacing each Selection in `locations` with `text`."
  [editor-state locations text]
  (let [locations-by-paragraph (group-by sel/caret-para locations)]
    (reduce (fn [editor-update, [para-uuid, para-locations]]
              (let [doc (-> editor-update :editor-state :doc)
                    para (get (:children doc) para-uuid)
                    new-para (paragraph-replace para para-locations text)]
                (>>= editor-update es/replace-paragraph para-uuid new-para)))
            (es/identity-update editor-state) locations-by-paragraph)))

#_(defn find-and-replace-all [editor-state text]
  (let [locations-to-replace (find editor-state text)]
    (apply replace editor-state text locations-to-replace)
    #_(reduce (fn [editor-update, location]
              (>>= editor-update replace location))
            (return editor-state) locations-to-replace)))
