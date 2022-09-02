(ns drop.app.components.find-and-replace
  (:require [drop.app.components.layout :refer [v-spacer-m]]
            [drop.utils :refer [debounce]]
            [reagent.core :as r :refer-macros [with-let]]))

(defn- input-row
  [{:keys [placeholder tab-index buttons on-change] :or {on-change #()}}]
  (with-let [*value (r/atom "")]
    [:div {:class "flex flex-row"}
     [:input {:class "p-1 mr-1 border bg-slate-50 border-gray-100
                    outline-none focus:border focus:border-dark-blue"
              :type "text"
              :tabIndex tab-index
              :spellCheck "false"
              :size "25"
              :placeholder placeholder
              :autoComplete "off"
              :value @*value
              :on-change (fn [e]
                           (reset! *value (.. e -target -value))
                           (on-change (.. e -target -value)))}]
     buttons]))

(defn img-button
  [{:keys [src on-click shortcut-text]}]
  [:button {:on-mouse-down #(.preventDefault %)
            :title shortcut-text
            :on-click on-click
            :class "m-0.5 px-1 rounded-sm hover:bg-slate-300 active:bg-slate-400 color-red"}
   [:img {:src src
          :style {:width "15px"}}]])

(defn text-button
  [{:keys [text tab-index on-click]}]
  [:button {:on-mouse-down #(.preventDefault %)
            :on-click on-click
            :tabIndex tab-index
            :class "m-0.5 p-2 rounded-sm text-xs hover:bg-slate-300 active:bg-slate-400 outline-dark-blue"}
   text])

(defn find-and-replace-popup
  [{:keys [activated? on-find on-replace on-replace-all on-click-next on-click-prev on-click-exit]}]
  (when activated?
    [:div {:class "fixed top-0 right-5 px-2.5 py-2.5 bg-white
                   border-l border-r border-b drop-shadow-sm rounded-b-sm
                   flex flex-col"
           :on-key-down #(when (= "Escape" (.-code %)) (on-click-exit))}
     [input-row {:placeholder "Find"
                 :tab-index "1"
                 :buttons [:<>
                           [img-button {:src "icons/down_arrow.svg"
                                        :shortcut-text "Enter"
                                        :on-click on-click-next}]
                           [img-button {:src "icons/up_arrow.svg"
                                        :shortcut-text "⇧Enter"
                                        :on-click on-click-prev}]
                           [img-button {:src "icons/x.svg"
                                        :shortcut-text "⎋"
                                        :on-click on-click-exit}]]
                 :on-change (debounce 500 on-find)}]
     [v-spacer-m]
     [input-row {:placeholder "Replace"
                 :tab-index "2"
                 :buttons [:<>
                           [text-button {:text "Replace"
                                         :tab-index "3"
                                         :on-click on-replace}]
                           [text-button {:text "Replace All"
                                         :on-click on-replace-all}]]}]]))
