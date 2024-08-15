(ns slate.renderer.debug-tree
  (:require [reagent.dom :as rdom]
            [slate.renderer.bst :as bst]
            ["react-svg-pan-zoom" :refer [UncontrolledReactSVGPanZoom]]))

;; L = length of line between nodes
;; θ = angle between child and parent nodes
;; X_child_left = X_parent - L * sin(θ)
;; X_child_right = X_parent + L * sin(θ)
;; Y_child = Y_parent + L * sin(θ)

(def r 80)
(def L (* 10 r))
(def pi js/Math.PI)
(def theta (* 45 (/ pi 180)))

(def sin js/Math.sin)
(def cos js/Math.cos)
(def tan js/Math.tan)

(defn node-render
  [n x y circles-vec level]
  (if (nil? n)
    circles-vec
    (let [circle-svg [:circle {:cx x :cy y :r r :fill "green"}]
          text-svg [:<>
                    [:text {:x x :y (- y 15) :font-size 12 :text-anchor "middle" :fill "black"}
                     (str "index: " (.-index n) ", ")]
                    [:text {:x x :y y :font-size 12 :text-anchor "middle" :fill "black"}
                     (str "height-px: " (:height-px (.-viewmodel n)) ", ")]
                    [:text {:x x :y (+ y 15) :font-size 12 :text-anchor "middle" :fill "black"}
                     (str "left-height-px: " (.-left-height-px n))]]
          child-y (+ y (* L (cos theta)))
          delta-x (/ (* L (sin theta)) (js/Math.pow 2 level))
          left-x (- x delta-x)
          right-x (+ x delta-x)
          left-children-svg (node-render (.-left n) left-x child-y circles-vec (inc level))
          left-line-svg [:line {:x1 x :y1 y :x2 left-x :y2 child-y :stroke "black"}]
          right-children-svg (node-render (.-right n) right-x child-y circles-vec (inc level))
          right-line-svg [:line {:x1 x :y1 y :x2 right-x :y2 child-y :stroke "black"}]]
      (concat (conj circles-vec circle-svg text-svg left-line-svg right-line-svg )
              left-children-svg
              right-children-svg))))

(defn svg [root-node width height]
  [:> UncontrolledReactSVGPanZoom
   {:width width
    :height height}
   [:svg {:version "1.1"
          :width width
          :height height
          :xmlns "http://www.w3.org/2000/svg"
          :style {:border "1px solid blue"}}
    (-> (node-render root-node (/ width 2) (/ height 2) [] 0)
        (conj :<>)
        (vec))]])

(defn debug [root-node]
  (let [width 1500
        height 1000
        new-window (.open js/window "" "" (str "width=" width ",height=" height))
        document (doto (.. new-window -document)
                   (.open)
                   (.write "<div id='svg-main'></div>")
                   (.close))
        elem (.getElementById document "svg-main")]
    (rdom/render [svg root-node width height] elem)))

(comment
  (debug)
  (+ 1 1))
