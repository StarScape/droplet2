(ns slate.renderer.core
  (:require [slate.model.dll :as dll]
            [slate.renderer.bst :as bst]
            [slate.renderer.measurement :refer [get-measure-fn]]
            [slate.renderer.utils :refer [font-str]]
            [slate.renderer.viewmodel :as vm]
            [slate.model.selection :as sel]))

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

(defn draw-caret! [ctx x y line-height]
  (set! (.-fillStyle ctx) "#0085f2")
  (.fillRect ctx x y 4 line-height))

(defn draw-span!
  [ctx span font-family font-size line-y]
  (aset ctx "font" (font-str font-size font-family))
  (tab-aware-fill-text! ctx (:text span) 0 line-y))

(defn draw-vm!
  [ctx paragraph-vm paragraph-y font-family font-size]
  (loop [lines (:lines paragraph-vm)
         line-y paragraph-y]
    (when-let [line (first lines)]
      (doseq [span (:spans line)]
        (draw-span! ctx span font-family font-size line-y))
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

(defn first-visible-viewmodel
  [bst scroll-y]
  (or (bst/vm-at-y bst scroll-y)
      (bst/first-vm bst)))

(defprotocol IRenderer
  (scroll! [this delta-y])
  (render-caret! [this editor-state])
  (render-doc! [this doc])
  (render! [this editor-state]))

(defn split-span
  "Splits the span into two at the paragraph offset, and return a vector of [before, after]."
  [span offset]
  (let [diff (- offset (:start-offset span))
        before (.substring (:text span) 0 diff)
        after (.substring (:text span) diff)]
    [(assoc span :text before), (assoc span :text after)]))

(defn spans-before-offset
  "Returns all spans in the line before the given paragraph offset."
  [line offset]
  (reduce (fn [spans-before, span]
            (let [span-end-offset (+ (count (:text span)) (:start-offset span))]
              (cond
                (<= span-end-offset offset)
                (conj spans-before span)

                (and (<= (:start-offset span) offset) (< offset span-end-offset))
                (conj spans-before (nth (split-span span offset) 0))

                :else
                (reduced spans-before))))
          [] (:spans line)))

(deftype Renderer [bst
                   scroll-y
                   viewport-width-px
                   viewport-height-px
                   line-heights
                   tab-size-px
                   font-family
                   base-font-size
                   measure-fn
                   text-layer-ctx
                   caret-layer-ctx]
  IRenderer
  (scroll! [this delta-y]
    (set! (.-scroll-y this) (min (max 0 (+ scroll-y delta-y)) (.-total-height-px bst))))

  (render-caret! [_ editor-state]
    (.clearRect caret-layer-ctx 0 0 viewport-width-px viewport-height-px)
    (let [vm (bst/search bst (sel/caret-para (:selection editor-state)))
          selection (:selection editor-state)
          lines (vec (:lines vm))
          caret-offset (sel/caret selection)
          line-idx (loop [i 0]
                     (let [{:keys [start-offset end-offset]} (nth lines i)]
                       (if (and (<= start-offset caret-offset)
                                (> end-offset caret-offset))
                         i
                         (recur (inc i)))))
          line (nth lines line-idx)
          line-height (get line-heights (:paragraph-type vm))
          screen-y (+ (- scroll-y (:y vm)) (* line-idx line-height))
          spans (spans-before-offset line caret-offset)
          screen-x (reduce (fn [x {:keys [text formats]}]
                             (+ x (measure-fn text formats (:paragraph-type vm))))
                           0 spans)]
      (draw-caret! caret-layer-ctx screen-x screen-y (:body line-heights))))

;; Renders only what's currently in the viewport
  (render-doc! [_ doc]
    (.clearRect text-layer-ctx 0 0 viewport-width-px viewport-height-px)
    (let [bottom-y (+ scroll-y viewport-height-px)
          first-visible-vm (first-visible-viewmodel bst scroll-y)
          first-visible-vm-offset (- scroll-y (:y first-visible-vm))
          last-visible-vm (bst/vm-at-y bst bottom-y)]
      (loop [idxs (dll/indices-range (:children doc)
                                     (:paragraph-index first-visible-vm)
                                     (if last-visible-vm
                                       (:paragraph-index last-visible-vm)
                                       (dll/last-index (:children doc))))
             paragraph-y (+ (get line-heights (:paragraph-type first-visible-vm)) first-visible-vm-offset)]
        (when-let [idx (first idxs)]
          (let [vm (bst/search bst idx)]
            (draw-vm! text-layer-ctx vm paragraph-y font-family base-font-size)
            (recur (rest idxs) (+ paragraph-y (* (count (:lines vm)) (:body line-heights)))))))))

  (render! [this editor-state]
    (render-doc! this (:doc editor-state))
    (render-caret! this editor-state)))

(defn init!
  "Initializes the Slate canvas renderer and does the initial render."
  [canvases editor-state font-family base-font-size tab-size-px]
  (let [text-canvas (:text canvases)
        caret-canvas (:caret canvases)
        dpr js/window.devicePixelRatio
        width (.-width text-canvas)
        height (* dpr (.-height text-canvas))
        text-ctx (.getContext text-canvas "2d")
        caret-ctx (.getContext caret-canvas "2d")
        body-line-height (get-line-height text-ctx font-family base-font-size)
        line-heights {:body body-line-height}
        measure-fn (get-measure-fn font-family base-font-size tab-size-px)
        bst (init-bst (:doc editor-state) width measure-fn line-heights)
        renderer (Renderer. bst
                            0
                            width
                            height
                            line-heights
                            tab-size-px
                            font-family
                            base-font-size
                            measure-fn
                            text-ctx
                            caret-ctx)]
    (.addEventListener js/document "wheel" (fn [e]
                                             (scroll! renderer (.-deltaY e))
                                             (render! renderer editor-state)))
    (render! renderer editor-state)
    renderer))


