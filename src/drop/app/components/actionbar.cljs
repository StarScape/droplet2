(ns drop.app.components.actionbar
  (:require [drop.utils :refer [debounce]]
            [reagent.core :as r :refer-macros [with-let]]
            ["@headlessui/react" :refer [Transition]]))

(def actionbar-fade-out-ms 2500)

(defn spacer []
  [:div {:class "w-1.5"}])

(defn button [img-url active? transparent-mode? on-click]
  [:> Transition {:show (boolean (or active? (not transparent-mode?)))
                  :enter "transition-opacity duration-150"
                  :enterFrom "opacity-0"
                  :enterTo "opacity-100"
                  :leave "transition-opacity duration-150"
                  :leaveFrom "opacity-100"
                  :leaveTo "opacity-0"}
   [:button {:on-mouse-down #(.preventDefault %) ; prevent losing focus
             :on-click on-click
             :class ["m-0.5" "p-2" "rounded-md"
                     (if active? "bg-light-blue" "bg-white")]}
    [:img {:src img-url
           :style {:width "15px"}}]]])

(defn- invisible-button []
  [:div.invisible [button "icons/italic.svg" false false #()]])

(defn actionbar [{:keys [active-formats on-format-toggle *full-screen?]}]
  (r/with-let [*transparent-mode? (r/atom false)
               set-transparent-debounced! (debounce actionbar-fade-out-ms #(when @*full-screen?
                                                                             (reset! *transparent-mode? true)))
               move-handler (fn [_]
                              (reset! *transparent-mode? false)
                              (set-transparent-debounced!))
               _ (add-watch *full-screen? :fs-event-handler
                            (fn [_ _ _ fs?]
                              (if fs?
                                (do
                                  (set-transparent-debounced!)
                                  (.addEventListener js/window "mousemove" move-handler))
                                (do
                                  (reset! *transparent-mode? false)
                                  (.removeEventListener js/window "mousemove" move-handler)))))]
    (let [transparent? @*transparent-mode?
          base-classes "fixed bottom-0 w-screen px-1 py-1 flex transition-all duration-150 "]
      [:div {:class (str base-classes (if transparent?
                                        "bg-transparent"
                                        "bg-white border-t border-gray-200"))}
       [button "icons/italic.svg" (active-formats :italic) transparent? #(on-format-toggle :italic)]
       [button "icons/bold.svg" (active-formats :bold) transparent? #(on-format-toggle :bold)]
       [button "icons/strikethrough.svg" (active-formats :strikethrough) transparent? #(on-format-toggle :strikethrough)]
       [spacer]
       [button "icons/h1.svg" (active-formats :h1) transparent? #(on-format-toggle :h1)]
       [button "icons/h2.svg" (active-formats :h2) transparent? #(on-format-toggle :h2)]
       [spacer]
       [button "icons/numbered.svg" (active-formats :ol) transparent? #(on-format-toggle :ol)]
       [button "icons/bulleted.svg" (active-formats :ul) transparent? #(on-format-toggle :ul)]

       ;; Invisible button so that element maintains its height
       ;; Even when all the others are hidden in fullscreen mode
       [invisible-button]])))
