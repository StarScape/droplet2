(ns slate.renderer.core
  (:require [clojure.string :as str]
            [shadow.cljs.modern :refer [defclass]]
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

(def tab-size-px 25) ;; TODO: move to single global constant, see viewmodel.cljs
(def dpr js/window.devicePixelRatio)

(defn font-str [font-size font-family]
  (str (* dpr font-size) "px " font-family))

(defn- split-on-tabs
  [str]
  (.split str #"(\t)"))

(defn get-line-height
  [ctx font-family font-size]
  (.save ctx)
  (let [metrics (-> (doto ctx
                      (aset "font" (font-str font-size font-family)))
                    (.measureText "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrztuv"))]
    (.restore ctx)
    (+ (.-actualBoundingBoxAscent metrics) (.-actualBoundingBoxDescent metrics))))

(defn tab-aware-fill-text!
  [ctx text x y]
  (loop [substrings (split-on-tabs text)
         current-x x]
    (when-let [substring (first substrings)]
      (if (= "\t" substring)
        (recur (rest substrings) (+ current-x tab-size-px))
        (let [text-metrics (.measureText ctx substring)]
          (.fillText ctx substring current-x y)
          (recur (rest substrings) (+ current-x (.-width text-metrics))))))))

(defn render-span!
  [ctx span font-family font-size line-y]
  (aset ctx "font" (font-str font-size font-family))
  (tab-aware-fill-text! ctx (:text span) 0 line-y))

(defn render-vm!
  [ctx paragraph-vm paragraph-y font-family font-size]
  (loop [lines (:lines paragraph-vm)
         line-y paragraph-y]
    (when-let [line (first lines)]
      (doseq [span (:spans line)]
        (render-span! ctx span font-family font-size line-y))
      (recur (rest lines)
             (+ line-y (get-line-height ctx font-family font-size))))))

(defn init-bst
  [doc width measure-fn line-heights]
  (let [paragraphs (:children doc)
        paragraphs-bst (bst/init-tree)]
    (loop [indices (dll/all-indices paragraphs)]
      (if (empty? indices)
        paragraphs-bst
        (let [idx (first indices)
              paragraph (get paragraphs idx)
              vm-paragraph (vm/from-para paragraph idx width measure-fn line-heights)]
          (bst/insert! paragraphs-bst idx vm-paragraph)
          (recur (rest indices)))))))

(defprotocol IRenderer
  (scroll! [this delta-y])
  (render! [this doc]))

(deftype Renderer [bst
                   scroll-y
                   viewport-width-px
                   viewport-height-px
                   line-heights
                   tab-size-px
                   font-family
                   base-font-size
                   text-layer-ctx]
  IRenderer
  (scroll! [this delta-y]
    (js/console.log "scroll"))

  ;; Renders only what's currently in the viewport
  (render! [this doc]
    (let [bottom-y (+ scroll-y viewport-height-px)
          first-visible-paragraph (bst/node-at-y (.-root bst) 0 0)
          last-visible-paragraph (bst/node-at-y (.-root bst) #p bottom-y 0)]
      (loop [idxs (dll/indices-range (:children doc)
                                     (.-index first-visible-paragraph)
                                     (if last-visible-paragraph
                                       (.-index last-visible-paragraph)
                                       (dll/last-index (:children doc))))
             paragraph-y (:body line-heights)]
        (when-let [idx (first idxs)]
          (let [vm (bst/search bst idx)]
            (render-vm! text-layer-ctx vm paragraph-y font-family base-font-size)
            (recur (rest idxs) (+ paragraph-y (* (count (:lines vm)) (:body line-heights))))))))))

#_(defn- set-canvas-dimensions!
    [ctx]
    (let [canvas (.-canvas ctx)
          dpr js/window.devicePixelRatio
          rect (.getBoundingClientRect canvas)]
      (doto canvas
        #_(aset "width" (* dpr (.-width rect)))
        #_(aset "height" (* dpr (.-height rect)))
        (aset "style" "border: 1px solid lightblue"))
      (.scale ctx dpr dpr)))

(defn init!
  "Initializes the Slate canvas renderer and does the initial render."
  [canvas doc font-family base-font-size tab-size-px]
  (let [dpr js/window.devicePixelRatio
        width (/ (.-width canvas) dpr) ;; get width and height before adjusting for DPR
        height (/ (.-height canvas) dpr)
        ctx (doto (.getContext canvas "2d")
              #_(set-canvas-dimensions!))
        body-line-height (get-line-height ctx font-family base-font-size)
        line-heights {:body body-line-height}
        measure-fn (get-measure-fn font-family base-font-size tab-size-px)
        bst (init-bst doc width measure-fn line-heights)
        renderer (Renderer. bst 0 width height line-heights tab-size-px font-family base-font-size ctx)]
    (.addEventListener js/document "wheel" (fn [e] (scroll! renderer (.-deltaY e))))

    ;; (.fillRect ctx 1100 700 100 100)
    (render! renderer doc)
    renderer))


