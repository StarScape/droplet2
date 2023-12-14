(ns slate.model.find-and-replace
  (:require [slate.model.dll :as dll]
            [slate.model.common :as m]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.run :as r]
            [slate.model.paragraph :as p]
            [slate.model.editor-state :as es])
  (:refer-clojure :exclude [find replace]))

(defn- find-all-occurrences
  "Returns the starting indices of all occurrences of `substr` within `str`."
  [str substr ignore-case?]
  (let [str (if ignore-case? (.toLowerCase str) str)
        substr (if ignore-case? (.toLowerCase substr) substr)]
    (loop [start-idx 0
           results []]
      (let [idx (.indexOf str substr start-idx)]
        (if (not= idx -1)
          (recur (+ idx (.-length substr))
                 (conj results idx))
          results)))))

(defn- adjust-selection [sel n]
  (-> sel
      (update-in [:start :offset] + n)
      (update-in [:end :offset] + n)))

(comment
  ;; [3 9 15]
  (find-all-occurrences "foobarfoobarfoobar" "bar")
  (find-all-occurrences "foobarfoobarfoobar" "bizz")
  )

(defn paragraph-find [paragraph paragraph-idx text ignore-case?]
  (let [paragraph-text (m/text paragraph)
        offsets (find-all-occurrences paragraph-text text ignore-case?)]
    (map #(selection [paragraph-idx %] [paragraph-idx (+ % (.-length text))]) offsets)))

(comment
  (def p (p/paragraph "p1" [(r/run "foo") (r/run "bar" #{:italic})
                            (r/run "goo") (r/run "bar" #{:bold})
                            (r/run "hoo") (r/run "bar" #{:underline})]))
  (paragraph-find p "bar")
  )

(defn find
  "Returns a list of locations where the text occurrences in the document."
  ([editor-state text ignore-case?]
   (let [children (-> editor-state :doc :children)]
     (->> (dll/all-indices children)
          (map (fn [idx] (paragraph-find (get children idx) idx text ignore-case?)))
          (flatten))))
  ([editor-state text] (find editor-state text false)))

(comment
  (def doc (document [(paragraph "p1" [(run "foo") (run "bar" #{:italic})
                                       (run "goo") (run "bar" #{:bold})
                                       (run "hoo") (run "bar" #{:underline})])
                      (paragraph "p2" [(run "one a one, and a foo, and a bar!")])]))
  (find (editor-state doc) "foo")
  (find (editor-state doc) "bar")
  )

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
                         (p/delete sel)
                         (p/insert (sel/collapse-start sel) (r/run text prior-formatting)))]
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
  "Returns a new EditorState replacing the current selection with `text`."
  [{:keys [doc selection] :as editor-state} text]
  {:pre [(sel/single-paragraph? selection)]}
  (let [para-idx (sel/caret-para selection)
        new-para (paragraph-replace (get (:children doc) para-idx) [selection] text)
        {new-selection :selection :as para-replaced-state} (es/replace-paragraph editor-state para-idx new-para)
        selection-length-diff (- (.-length text) (- (-> new-selection :end :offset) (-> new-selection :start :offset)))
        final-selection (sel/shift-end new-selection selection-length-diff)]
    (assoc para-replaced-state :selection final-selection)))

(defn replace-all
  "Returns a new EditorState replacing each Selection in `locations` with `text`."
  [editor-state locations text]
  (let [locations-by-paragraph (group-by sel/caret-para locations)]
    (reduce (fn [editor-state, [para-idx, para-locations]]
              (let [doc (:doc editor-state)
                    para (get (:children doc) para-idx)
                    new-para (paragraph-replace para para-locations text)]
                (es/replace-paragraph editor-state para-idx new-para)))
            editor-state locations-by-paragraph)))

(defn init
  "Initializes find and replace state map."
  []
  {:active? false
   :ignore-case? true
   :location-before nil
   :find-text ""
   :occurrences []
   :current-occurrence-idx 0})

(defn current-occurrence
  "If find is active, returns current found location, as a Selection."
  [{:keys [active? current-occurrence-idx occurrences] :as _find-and-replace-info}]
  (when active?
    (nth occurrences current-occurrence-idx)))

(defn next-occurrence
  [{:keys [occurrences current-occurrence-idx] :as find-and-replace-state}]
  (if (< current-occurrence-idx (dec (count occurrences)))
    (update find-and-replace-state :current-occurrence-idx inc)
    (assoc find-and-replace-state :current-occurrence-idx 0)))

(defn prev-occurrence
  [{:keys [occurrences current-occurrence-idx] :as find-and-replace-state}]
  (cond
    (empty? occurrences)
    find-and-replace-state

    (zero? current-occurrence-idx)
    (assoc find-and-replace-state :current-occurrence-idx (dec (count occurrences)))

    :else
    (update find-and-replace-state :current-occurrence-idx dec)))

(defn replace-current-selection
  "Returns an EditorState replacing the current occurrence with `replacement-text`."
  [editor-state replacement-text]
  (replace editor-state replacement-text))

(defn replace-all-occurrences
  "Returns an EditorState replacing all occurrences with `replacement-text`."
  [{:keys [occurrences] :as _find-and-replace-state} editor-state replacement-text]
  (replace-all editor-state occurrences replacement-text))

(defn activate-find [{:keys [active?] :as find-and-replace-state} editor-state]
  (if active?
    find-and-replace-state
    (merge find-and-replace-state {:active? true
                                   :location-before (:selection editor-state)})))

(defn find-occurrences
  [{:keys [find-text ignore-case? occurrences location-before] :as find-and-replace-state} editor-state]
  (let [occurences (find editor-state find-text ignore-case?)]
    (assoc find-and-replace-state
           :current-occurrence-idx 0
           :occurrences occurences
           :location-before (if (empty? occurrences)
                              (:selection editor-state)
                              location-before))))

(defn cancel-find [{:keys [active?] :as find-and-replace-state}]
  (if active?
    (merge find-and-replace-state {:active? false})
    find-and-replace-state))
