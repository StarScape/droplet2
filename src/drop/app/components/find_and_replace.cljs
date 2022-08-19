(ns drop.app.components.find-and-replace
  (:require [drop.app.components.layout :refer [v-spacer-m]]))

(defn- input-row
  [{:keys [placeholder tab-index buttons]}]
  [:div {:class "flex flex-row"}
   [:input {:class "p-1 mr-1 border bg-slate-50 border-gray-100 outline-dark-blue"
            :type "text"
            :tabIndex tab-index
            :spellCheck "false"
            :size "25"
            :placeholder placeholder
            :autoComplete "off"}]
   buttons])

(defn img-button
  [{:keys [src on-click]}]
  [:button {:on-mouse-down #(.preventDefault %)
            :on-click on-click
            :class "m-0.5 px-1 rounded-sm hover:bg-slate-300 active:bg-slate-400 color-red"}
   [:img {:src src
          :style {:width "15px"}}]])

(defn text-button
  [{:keys [text tab-index on-click]}]
  [:button {:on-mouse-down #(.preventDefault %)
            :on-click on-click
            :tabIndex tab-index
            :class "m-0.5 p-2 rounded-sm hover:bg-slate-300 active:bg-slate-400 text-xs color-red outline-dark-blue"}
   text])

(defn find-and-replace-popup
  [{:keys [activated? on-click-exit on-click-next on-click-prev]}]
  (when activated?
    [:div {:class "fixed top-0 right-5 px-2.5 py-2.5 bg-white
                   border-l border-r border-b drop-shadow-sm rounded-b-sm
                   flex flex-col"}
     [input-row {:placeholder "Find"
                 :tab-index "1"
                 :buttons [:<>
                           [img-button {:src "icons/down_arrow.svg"
                                        :on-click on-click-next}]
                           [img-button {:src "icons/up_arrow.svg"
                                        :on-click on-click-prev}]
                           [img-button {:src "icons/x.svg"
                                        :on-click on-click-exit}]]}]
     [v-spacer-m]
     [input-row {:placeholder "Replace"
                 :tab-index "2"
                 :buttons [:<>
                               [text-button {:text "Replace"
                                             :tab-index "3"
                                             :on-click #(js/console.log "click REPLACE")}]
                               #_[text-button {:text "Replace All"
                                             :on-click #(js/console.log "click REPLACE ALL")}]]}]
     #_[:div {:class "flex flex-row mt-3"}
      #_[text-button {:text "Replace"
                    :on-click #(js/console.log "click REPLACE")}]
      [text-button {:text "Replace All"
                    :on-click #(js/console.log "click REPLACE ALL")}]]]))
