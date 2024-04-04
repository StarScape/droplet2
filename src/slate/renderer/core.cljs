(ns slate.renderer.core
  (:require [shadow.cljs.modern :refer [defclass]]
            [slate.model.dll :as dll]
            [slate.renderer.measurement :refer [get-measure-fn]]
            [slate.renderer.viewmodel :as vm]
            [slate.renderer.bst :as bst]))

;; renderer operations:
;; insert
;; delete
;; at-y
;; at-index
;; first-viewmodel
;; last-viewmodel
;;
;; renderer state:
;; viewport-y
;; topmost-paragraph
;; document-height

(defn init-bst
  [doc width measure-fn]
  (let [paragraphs (:children doc)
        paragraphs-bst (bst/init-tree)]
    (loop [indices (dll/all-indices paragraphs)]
      (if (empty? indices)
        paragraphs-bst
        (let [idx (first indices)
              paragraph (get paragraphs idx)
              vm-paragraph (vm/from-para paragraph idx width measure-fn)]
          (bst/insert! paragraphs-bst idx vm-paragraph)
          (recur (rest indices)))))))

(defn set-canvas-dimensions!
  [ctx]
  (let [canvas (.-canvas ctx)
        dpr js/window.devicePixelRatio
        rect (.getBoundingClientRect canvas)]
    (doto canvas
      (aset "width" (* dpr (.-width rect)))
      (aset "height" (* dpr (.-height rect))))
    (.scale ctx dpr dpr)))

(defn font-str [font-size font-family]
  (str font-size "px " font-family))

(defn render-span!
  [ctx span font-family font-size line-y]
  (aset ctx "font" (font-str font-size font-family))
  (.fillText ctx (:text span) 0 line-y))

(defn get-line-height
  [ctx font-family font-size]
  (.save ctx)
  (let [metrics (-> (doto ctx
                      (aset "font" (font-str font-size font-family)))
                    (.measureText "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrztuv"))]
    (.restore ctx)
    (+ (.-actualBoundingBoxAscent metrics) (.-actualBoundingBoxDescent metrics))))

(defn render-vm!
  [ctx paragraph-vm paragraph-y font-family font-size]
  (loop [lines (:lines paragraph-vm)
         line-y paragraph-y]
    (when-let [line #p (first lines)]
      (doseq [span (:spans line)]
        (render-span! ctx span font-family font-size line-y))
      (recur (rest lines)
             (+ line-y (get-line-height ctx font-family font-size))))))

(defn init!
  "Initializes the Slate canvas renderer and does the initial render."
  [canvas doc font-family base-font-size tab-size-px]
  (let [#_#_#_#_state {:viewport-top-y 0}
            viewmodels-bst (init-bst (:doc init-bst) (.-width canvas) nil)
        ctx (.getContext canvas "2d")
        width (.-width canvas)
        measure-fn (get-measure-fn font-family base-font-size tab-size-px)
        line-height (get-line-height ctx font-family base-font-size)]
    (set-canvas-dimensions! ctx)
    (loop [idxs (dll/all-indices (:children doc))
           paragraph-y 50]
      (when-let [idx (first idxs)]
        (let [vm (vm/from-para (get (:children doc) idx) idx width measure-fn)]
          (render-vm! ctx vm paragraph-y font-family base-font-size)
          (recur (rest idxs) (+ paragraph-y (* (count (:lines vm)) line-height))))))))