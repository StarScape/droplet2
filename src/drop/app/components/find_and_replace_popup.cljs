(ns drop.app.components.find-and-replace-popup
  (:require [clojure.string :as str]
            [drop.app.components.layout :refer [v-spacer-m]]
            [drop.utils :refer [debounce]]
            [reagent.core :as r :refer-macros [with-let]]))

(defn- input-row
  [{:keys [placeholder tab-index buttons input-tray on-key-down on-change ref *value-atom autofocus?]
    :or {on-key-down #() on-change #() ref #() *value-atom (r/atom "")}}]
  (with-let [*value *value-atom]
    [:div {:class "flex flex-row"
           :on-key-down #(on-key-down % @*value)}
     [:div {:class "p-1 mr-1 border bg-slate-50 border-gray-100 flex flex-row
                    focus-within:border focus-within:border-dark-blue"}
      [:input {:class "outline-none bg-transparent"
               :type "text"
               :ref #(when ref (ref %))
               :tabIndex tab-index
               :spellCheck "false"
               :size "20"
               :placeholder placeholder
               :autoComplete "off"
               :autoFocus autofocus?
               :value @*value
               :on-change (fn [e]
                            (reset! *value (.. e -target -value))
                            (on-change (.. e -target -value)))}]
      [:div {:class "w-16 flex flex-row justify-end items-center"}
       input-tray]]
     ;; TODO: add tray
     buttons]))

(defn img-button
  [{:keys [src on-click hover-text toggled?] :or {toggled? false}}]
  [:button {:on-mouse-down #(.preventDefault %)
            :title hover-text
            :on-click on-click
            :class ["m-0.5" "px-1" "rounded-sm" "hover:bg-slate-300" "active:bg-slate-400"
                    (when toggled? "bg-slate-300")]}
   [:img {:src src
          :style {:width "15px"}}]])

(defn text-button
  [{:keys [text tab-index on-click]}]
  [:button {:on-mouse-down #(.preventDefault %)
            :on-click on-click
            :tabIndex tab-index
            :class "m-0.5 p-2 rounded-sm text-xs hover:bg-slate-300 active:bg-slate-400 outline-light-blue"}
   text])

(defn find-and-replace-popup
  [{:keys [activated?
           ignore-case-toggled?
           current-occurence
           total-occurences
           on-find
           on-replace
           on-replace-all
           on-click-next
           on-click-prev
           on-click-exit
           on-toggle-ignore-case
           search-input-ref]}]
  (r/with-let [*replace-text (r/atom "")
               *find-text (r/atom "")]
    (when activated?
      [:div {:class "fixed top-0 right-5 px-2.5 py-2.5 bg-white
                   border-l border-r border-b rounded-b-sm shadow-sm
                   flex flex-col"
             :on-key-down #(cond
                             (= "Escape" (.-code %))
                             (on-click-exit))}
       [input-row {:placeholder "Find"
                   :autofocus? true
                   :tab-index "1"
                   :*value-atom *find-text
                   :ref search-input-ref
                   :on-key-down (fn [e value]
                                  (when (= "Enter" (.-code e))
                                    (if (.-shiftKey e)
                                      (on-click-prev)
                                      (on-find value))))
                   :buttons [:<>
                             [text-button {:text "Search"
                                           :tab-index "3"
                                           :on-click #(on-find @*find-text)}]
                             [img-button {:src "icons/down_arrow.svg"
                                          :hover-text "Enter"
                                          :on-click on-click-next}]
                             [img-button {:src "icons/up_arrow.svg"
                                          :hover-text "⇧Enter"
                                          :on-click on-click-prev}]
                             [img-button {:src "icons/case_sensitivity.svg"
                                          :hover-text "Toggle case-sensitivity"
                                          :toggled? ignore-case-toggled?
                                          :on-click on-toggle-ignore-case}]
                             [img-button {:src "icons/x.svg"
                                          :hover-text "Esc"
                                          :on-click on-click-exit}]]
                   :input-tray [:div {:class "text-xs text-slate-500"}
                                (str (if (pos? total-occurences)
                                       (inc current-occurence)
                                       current-occurence)
                                     "/"
                                     total-occurences)]
                   :on-change (debounce 500 on-find)}]
       [v-spacer-m]
       [input-row {:placeholder "Replace"
                   :tab-index "2"
                   :*value-atom *replace-text
                   :buttons [:<>
                             [text-button {:text "Replace"
                                           :tab-index "4"
                                           :on-click #(when-not (str/blank? @*replace-text) (on-replace @*replace-text))}]
                             [text-button {:text "Replace All"
                                           :tab-index "5"
                                           :on-click #(when-not (str/blank? @*replace-text) (on-replace-all @*replace-text))}]]}]])))