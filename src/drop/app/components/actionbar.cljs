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
            :class [(if active? "bg-blue-300" "bg-white") "p-2"]
            #_#_:style {:background-color (if active? "lightblue" "white")}}
   text])

(defn actionbar [{:keys [class active-formats on-format-toggle]
                  :or {class []}}]
  [:div {:class class}
   [button [:span.italic "I"] (active-formats :italic) #(on-format-toggle :italic)]
   [button [:span.font-bold "B"] (active-formats :bold) #(on-format-toggle :bold)]
   [button [:span.line-through "S"] (active-formats :strikethrough) #(on-format-toggle :strikethrough)]
   [button [:span "H" [:sub.font-semibold "1"]] (active-formats :h1) #(on-format-toggle :h1)]
   [button [:span "H" [:sub.font-semibold "2"]] (active-formats :h2) #(on-format-toggle :h2)]
   [button "#" (active-formats :ol) #(on-format-toggle :ol)]
   [button "•" (active-formats :ul) #(on-format-toggle :ul)]])
