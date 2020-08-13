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

(defn insert
  "Insert insert text into run at the given selection."
  [run text start end]
  #_(uwu-uwu))

(comment
  (empty-run? (run "Foo" #{:italic :bold :underline}))
  (empty-run? (empty-run))

  )
