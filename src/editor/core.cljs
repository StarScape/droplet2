(ns drop.editor.core)

; ;;; Selection operations ;;;

; ;; TODO: make this a multi-method that can take maps OR arrays
; ;; TODO: spec this all out. Also learn spec :)
(def selection-dispatch (fn [& args] (prn args) (mapv type args)))

(defmulti mysel #'selection-dispatch)

(defmethod mysel
  [PersistentVector PersistentVector js/Boolean]
  [[start-paragraph start-offset] [end-paragraph end-offset] backwards]
  {:start {:paragraph start-paragraph
           :offset start-offset}
   :end {:paragraph end-paragraph
         :offset end-offset}
   :backwards false})

;; holy fuck it finally works
(mysel [:p1 1] [:p1 10] false)

; ; (defn selection
; ;   "Creates a new selection."
; ;   ([[start-paragraph start-offset] [end-paragraph end-offset] backwards]
; ;    {:start {:paragraph start-paragraph
; ;             :offset start-offset}
; ;     :end {:paragraph end-paragraph
; ;           :offset end-offset}
; ;     :backwards backwards})
; ;   ([start end]
; ;    (selection start end false))
; ;   ([start]
; ;    (selection start start false)))

; ; (defn single?
; ;   "Returns true if sel is a single selection."
; ;   [sel]
; ;    (= (:start sel) (:end sel)))

; ; (def range? (complement single?))

; ; (defn shift-single
; ;   "Shift a single-selection by n characters (can be positive or negative)."
; ;   [{{paragraph :paragraph offset :offset} :start} n]
; ;    (selection [paragraph (+ n offset)]))

; ; (defn set-single
; ;   "Sets a single-selection to a given offset."
; ;   [sel offset]
; ;    (-> sel 
; ;        (assoc-in [:start :offset] offset)
; ;        (assoc-in [:end :offset] offset)))

; ; ;; TODO: test all deez
; ; (defn collapse-start
; ;   "Returns a new single-selection at the start of the selection"
; ;   [sel]
; ;    (selection [(-> sel :start :paragraph) (-> sel :start :offset)]))

; ; (defn collapse-end
; ;   "Returns a new single-selection at the end of the selection"
; ;   [sel]
; ;    (selection [(-> sel :end :paragraph) (-> sel :end :offset)]))

; ; ;; TODO is this needed? see Paragraph.js
; ; (defn collapse [sel]
; ;   (if (single? sel)
; ;     sel
; ;     (if (:backwards sel)
; ;       (collapse-start sel)
; ;       (collapse-end sel))))


; ; ;; TODO: copy the test from Selection.test.js

; (def p1 :paragraph1)
; (def s1 (selection [p1 0]))

; (comment
;   (selection [p1 12] [p1 33] false)
;   (single? (selection [p1 12]))
;   s1
;   (shift-single s1 12)
;   (set-single s1 33))