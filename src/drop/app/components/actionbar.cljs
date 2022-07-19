(ns drop.app.components.actionbar
  (:require [reagent.core :as r :refer [with-let]]))

;; (defn update-formats-elem
;;   [_key _atom _old-state new-state]
;;   (let [sel (:selection (history/current-state (:history new-state)))
;;         formats-str (str "Formats: " (str/join \, (:formats sel)))
;;         elem (js/document.getElementById "formats")]
;;     (set! (.-innerHTML elem) formats-str)))

(defn spacer []
  [:div {:class "w-1.5"}])

(defn button [img-url active? on-click]
  [:button {:on-click on-click
            ;; TODO: add light blue color to tailwind sheet
            :class [(if active? "bg-light-blue" "bg-white")
                    "m-0.5" "p-2" "rounded-md"]
            #_#_:style {:background-color (if active? "lightblue" "white")}}
   [:img {:src img-url
          :style {:width "15px"}}]])

(defn actionbar [{:keys [class active-formats on-format-toggle]
                  :or {class []}}]
  [:div {:class class}
   [button "icons/italic.svg" (active-formats :italic) #(on-format-toggle :italic)]
   [button "icons/bold.svg" (active-formats :bold) #(on-format-toggle :bold)]
   [button "icons/strikethrough.svg" (active-formats :strikethrough) #(on-format-toggle :strikethrough)]
   [spacer]
   [button "icons/h1.svg" (active-formats :h1) #(on-format-toggle :h1)]
   [button "icons/h2.svg" (active-formats :h2) #(on-format-toggle :h2)]
   [spacer]
   [button "icons/numbered.svg" (active-formats :ol) #(on-format-toggle :ol)]
   [button "icons/bulleted.svg" (active-formats :ol) #(on-format-toggle :ol)]])