(ns drop.app.components.find-and-replace-popup
  (:require [clojure.string :as str]
            [drop.app.components.library :as components]
            [drop.app.components.layout :refer [v-spacer-m]]
            [drop.utils :refer [debounce]]
            [reagent.core :as r :refer-macros [with-let]]
            [slate.utils :as slate-utils :refer [is-mac?]]))

(def element-id "find-and-replace-popup")

(defn img-button
  [{:keys [src on-click hover-text toggled?]}]
  [components/toggleable-button {:title hover-text
                                 :on-click on-click
                                 :toggled? toggled?}
   [:img {:src src
          :style {:width "15px"}}]])

(defn text-button
  [{:keys [text tab-index hover-text on-click]}]
  [components/button {:title hover-text
                      :on-click on-click
                      :tabIndex tab-index
                      :class "text-xs"}
   text])

(defn- input-row
  [{:keys [placeholder tab-index buttons input-tray on-key-down on-change ref value autofocus?]
    :or {on-key-down #() on-change #() ref #() value (r/atom "")}}]
  [:div {:class "flex flex-row"
         :on-key-down #(on-key-down % value)}
   [:div {:class "p-1 mr-1 border bg-slate-50 border-gray-100 flex flex-row
                  focus-within:border focus-within:border-dark-blue"}
    [:input {:class "outline-none bg-transparent"
             :type "text"
             :ref #(when ref (ref %))
             :tabIndex tab-index
             :spellCheck "false"
             :size "30"
             :placeholder placeholder
             :autoComplete "off"
             :autoFocus autofocus?
             :value value
             :on-change (fn [e] (on-change (.. e -target -value)))}]
    [:div {:class "w-16 flex flex-row justify-end items-center"}
     input-tray]]
   buttons])

(defn find-and-replace-popup
  [{:keys [activated?
           ignore-case-toggled?
           current-occurrence
           total-occurrences
           find-text
           on-find-text-changed
           on-find
           on-replace
           on-replace-all
           on-click-next
           on-click-prev
           on-click-exit
           on-toggle-ignore-case
           on-key-down
           on-focus
           on-blur
           search-input-ref]}]
  (r/with-let [debounced-on-find (debounce 500 on-find)
               *replace-text (r/atom "")
               replace! #(when-not (str/blank? @*replace-text) (on-replace @*replace-text))
               replace-all! #(when-not (str/blank? @*replace-text) (on-replace-all @*replace-text))]
    (when activated?
      [:div {:id element-id
             :class "fixed top-3 px-2.5 py-2.5 bg-white rounded-md border border-gray-300 shadow-lg flex flex-col"
             :style {:z-index 2}
             :on-focus on-focus
             :on-blur on-blur
             :on-key-down (fn [e]
                            (on-key-down ^js/Event (.-nativeEvent e))
                            (cond
                              (= "Escape" (.-code e))
                              (on-click-exit)

                              ;; cmd/ctrl+alt+c = toggle case sensitivity
                              (and (= "KeyC" (.-code e))
                                   (.-altKey e)
                                   (slate-utils/cmd-or-ctrl-key e))
                              (do
                                (.preventDefault e)
                                (on-toggle-ignore-case))

                              ;; cmd/ctrl+shift+r = replace all
                              (and (= "KeyR" (.-code e))
                                   (.-shiftKey e)
                                   (slate-utils/cmd-or-ctrl-key e))
                              (do
                                (.preventDefault e)
                                (replace-all!))

                              ;; cmd/ctrl+r = replace
                              (and (= "KeyR" (.-code e))
                                   (slate-utils/cmd-or-ctrl-key e))
                              (do
                                (.preventDefault e)
                                (replace!))))}
       [input-row {:placeholder "Find"
                   :autofocus? true
                   :tab-index "1"
                   :value find-text
                   :on-change (fn [new-text]
                                (on-find-text-changed new-text)
                                (debounced-on-find))
                   :ref search-input-ref
                   :on-key-down (fn [e value]
                                  (when (= "Enter" (.-code e))
                                    (if (.-shiftKey e)
                                      ; Shift+Enter = goto previous find
                                      (on-click-prev)
                                      ; Enter = find/goto next find
                                      (on-click-next value))))
                   :buttons [:<>
                             [img-button {:src "icons/down_arrow.svg"
                                          :hover-text "Enter"
                                          :on-click on-click-next}]
                             [img-button {:src "icons/up_arrow.svg"
                                          :hover-text (if (is-mac?) "⇧Enter" "Shift+Enter")
                                          :on-click on-click-prev}]
                             [img-button {:src "icons/case_sensitivity.svg"
                                          :hover-text (str "Toggle case-sensitivity " (if (is-mac?) "(⌘⎇C)" "(Ctrl+Alt+C)"))
                                          :toggled? ignore-case-toggled?
                                          :on-click on-toggle-ignore-case}]
                             [img-button {:src "icons/x.svg"
                                          :hover-text "Esc"
                                          :on-click on-click-exit}]]
                   :input-tray [:div {:class "text-xs text-slate-500"}
                                (str (if (pos? total-occurrences)
                                       (inc current-occurrence)
                                       current-occurrence)
                                     "/"
                                     total-occurrences)]}]
       [v-spacer-m]
       [input-row {:placeholder "Replace"
                   :tab-index "2"
                   :value @*replace-text
                   :on-change #(reset! *replace-text %)
                   :buttons [:<>
                             [text-button {:text "Replace One"
                                           :tab-index "3"
                                           :hover-text (if (is-mac?) "⌘R" "Ctrl+R")
                                           :on-click replace!}]
                             [text-button {:text "Replace All"
                                           :tab-index "4"
                                           :hover-text (if (is-mac?) "⌘⇧R" "Ctrl+Shift+R")
                                           :on-click replace-all!}]]}]])))
