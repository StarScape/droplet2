(ns drop.app.components.actionbar
  (:require [reagent.core :as r :refer [with-let]]))

;; (defn update-formats-elem
;;   [_key _atom _old-state new-state]
;;   (let [sel (:selection (history/current-state (:history new-state)))
;;         formats-str (str "Formats: " (str/join \, (:formats sel)))
;;         elem (js/document.getElementById "formats")]
;;     (set! (.-innerHTML elem) formats-str)))

(defn button [text active? on-click]
  [:button {:on-click on-click
            :style {:background-color (if active? "lightblue" "white")}}
   text])

(defn actionbar [active-formats on-format-toggle]
  [:div
   [button "i" (active-formats :italic) #(on-format-toggle :italic)]
   [button "b" (active-formats :bold) #(on-format-toggle :bold)]
   [button "s" (active-formats :strikethrough) #(on-format-toggle :strikethrough)]
   [button "h1" (active-formats :h1) #(on-format-toggle :h1)]
   [button "h2" (active-formats :h2) #(on-format-toggle :h2)]
   [button "ol" (active-formats :ol) #(on-format-toggle :ol)]
   [button "ul" (active-formats :ul) #(on-format-toggle :ul)]])
