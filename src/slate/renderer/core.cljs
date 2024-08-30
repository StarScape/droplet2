(ns slate.renderer.core
  (:require [clojure.set :as set]
            [slate.model.dll :as dll :refer [big-dec]]
            [slate.model.selection :as sel]
            [slate.renderer.bst :as bst]
            [slate.renderer.debug-tree :as debug-tree]
            [slate.renderer.measurement :refer [get-measure-fn]]
            [slate.renderer.utils :refer [font-str]]
            [slate.renderer.viewmodel :as vm]))

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

;; TODO: multi-line handling for this
(defn draw-selection-rect! [ctx start-x start-y end-x end-y line-height]
  (let [saved-global-alpha (.-globalAlpha ctx)]
    (set! (.-fillStyle ctx) "#0085f2")
    (set! (.-globalAlpha ctx) 0.5)
    (let [width (- end-x start-x)
          height (- end-y start-y)]
      (.fillRect ctx start-x start-y width line-height))
    (set! (.-globalAlpha ctx) saved-global-alpha)))

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

(defprotocol IRenderer
  (screen-coords-of [this single-selection])
  (scroll! [this delta-y])
  (render-selection! [this editor-state])
  (render-doc! [this doc])
  (render! [this editor-state])
  (update! [this new-editor-state changelist]))

(defn vms-in-selection
  ([renderer doc selection limit-to-visible?]
   (let [bottom-y (+ (.-scroll-y renderer) (.-viewport-height-px renderer))
         first-visible-idx (:paragraph-index (first-visible-viewmodel (.-bst renderer) (.-scroll-y renderer)))
         last-visible-idx (:paragraph-index (bst/vm-at-y (.-bst renderer) bottom-y))
         first-idx (if (and limit-to-visible? (.lt (sel/start-para selection) first-visible-idx))
                     first-visible-idx
                     (sel/caret-para selection))
         last-idx (if (and limit-to-visible? (.gt (sel/end-para selection) last-visible-idx))
                    last-visible-idx
                    (sel/end-para selection))
         idxs (dll/indices-range (:children doc) first-idx last-idx)]
     (map #(bst/search (.-bst renderer) %) idxs)))
  ([renderer doc selection]
   (vms-in-selection renderer doc selection true)))

(defn draw-selection-for-paragraph!
  [renderer vm selection]
  (let [[_, sel-start-y :as sel-start-coords] (screen-coords-of renderer (sel/collapse-start selection))
        [_, sel-end-y :as sel-end-coords] (screen-coords-of renderer (sel/collapse-end selection))
        line-height (get (.-line-heights renderer) (:paragraph-type vm))]
    (loop [lines (:lines vm), line-y 0]
      ;; Line must be within the selected area
      (when (and (<= sel-start-y line-y)
                 (>= sel-end-y line-y))
        (when-let [line (first lines)]
          (let [[block-start-x, block-start-y] (if (= line-y sel-start-y) ; this line is beginning of selection
                                                 sel-start-coords
                                                 [0, line-y])
                [block-end-x, block-end-y] (if (= sel-end-y line-y) ; this line is end of selection
                                             sel-end-coords
                                             (screen-coords-of renderer (sel/selection [(:paragraph-index vm), (dec (:end-offset line))])))]
            (draw-selection-rect! (.-caret-layer-ctx renderer)
                                  block-start-x
                                  block-start-y
                                  block-end-x
                                  block-end-y
                                  (:body (.-line-heights renderer)))
            (recur (next lines) (+ line-height line-y))))))))

;; TODO: make selection-to-bounding-boxes function
;; This would be useful also for doing things like drawing
;; underlines, or squiggly red lines for spellcheck, etc.

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
  
  (screen-coords-of [_ single-selection]
    (let [vm (bst/search bst (sel/caret-para single-selection))
          ;; this was previously a separate function called 'vm-line-number-of' but I'm not sure I need it separate?
          line-idx (let [caret-offset (sel/caret single-selection)]
                     (loop [i 0]
                       (let [{:keys [start-offset end-offset]} (nth (:lines vm) i)]
                         (if (and (<= start-offset caret-offset)
                                  (> end-offset caret-offset))
                           i
                           (recur (inc i))))))
          line (nth (:lines vm) line-idx)
          line-height (get line-heights (:paragraph-type vm))
          paragraph-y (- (:y vm) scroll-y)
          screen-y (+ paragraph-y (* line-idx line-height))
          spans (spans-before-offset line (sel/caret single-selection))
          screen-x (reduce (fn [x {:keys [text formats]}]
                             (+ x (measure-fn text formats (:paragraph-type vm))))
                           0 spans)]
      [screen-x, screen-y]))

  (render-selection! [this editor-state]
    (.clearRect caret-layer-ctx 0 0 viewport-width-px viewport-height-px)
    (let [selection (:selection editor-state)
          [screen-x, screen-y] (screen-coords-of this (sel/smart-collapse selection))]
      (when (sel/range? selection)
        (doseq [vm (vms-in-selection this (:doc editor-state) selection)]
          (draw-selection-for-paragraph! this vm selection)))
      (draw-caret! caret-layer-ctx screen-x screen-y (:body line-heights))))

  ;; Renders only what's currently in the viewport
  (render-doc! [_ doc]
    (.clearRect text-layer-ctx 0 0 viewport-width-px viewport-height-px)
    (let [bottom-y (+ scroll-y viewport-height-px)
          first-visible-vm (first-visible-viewmodel bst scroll-y)
          first-visible-vm-offset (- (:y first-visible-vm) scroll-y )
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
    (render-selection! this editor-state))

  (update! [this editor-state changelist]
    (let [{:keys [deleted-indices changed-indices inserted-indices]} changelist]
      (doseq [idx (set/union deleted-indices changed-indices)]
        (bst/delete! bst idx))
      (doseq [idx (set/union inserted-indices changed-indices)]
        (bst/insert! bst idx (vm/from-para (-> editor-state :doc :children (get idx))
                                           idx
                                           viewport-width-px
                                           measure-fn
                                           line-heights)))

      (render! this editor-state))))

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
    ;; TODO: this should probably be in the event handling code, it's not getting the latest editor state
    (.addEventListener js/document "wheel" (fn [e]
                                             (scroll! renderer (.-deltaY e))
                                             (render! renderer editor-state)))
    (render! renderer editor-state)
    renderer))


