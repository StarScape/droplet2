(ns drop.app.components.actionbar
  (:require [drop.app.consts :as consts]
            [drop.app.components.library :as components]
            [drop.app.components.layout :refer [h-spacer-sm]]
            [drop.utils :refer [debounce]]
            [reagent.core :as r :refer-macros [with-let]]
            [re-frame.core :as rf :refer [dispatch subscribe]]
            [slate.utils :as slate-utils]
            [re-frame.db :as db]
            ["@headlessui/react" :refer [Transition]]))

;; (def bg-color "bg-[rgb(246,247,249)]")
;; (def bg-color "bg-gray-100")
(def bg-color "bg-gray-50")

(defn- shortcut-for [formatting-command]
  (if slate-utils/is-mac?
    (case formatting-command
      :italic "⌘I"
      :bold "⌘B"
      :strikethrough "⌘T"
      :h1 "⌘1"
      :h2 "⌘2"
      :ol "⌘⇧O"
      :ul "⌘⇧U")
    (case formatting-command
      :italic "Ctrl+I"
      :bold "Ctrl+B"
      :strikethrough "Ctrl+T"
      :h1 "Ctrl+1"
      :h2 "Ctrl+2"
      :ol "Ctrl+Shift+O"
      :ul "Ctrl+Shift+U")))


;; solution here: https://jsfiddle.net/7o6q5c1v/1/
(defn format-button [{:keys [img-url active? transparent-mode? on-click mouseover-text]}]
  [:> Transition {:show (boolean (or active? (not transparent-mode?)))
                  :enterFrom "opacity-0"
                  :enterTo "opacity-100"
                  :leaveFrom "opacity-100"
                  :leaveTo "opacity-0 w-0"}
   [components/toggleable-button {:on-click on-click
                                  :toggled? active?
                                  :class "rounded-md"
                                  :title mouseover-text}
    [:img {:src img-url
           :style {:width "15px"}}]]])

(defn- invisible-button []
  [:div.invisible [format-button "icons/italic.svg" false false #()]])

(defn word-count-display [num-words]
  [:span {:class "flex text-slate-800 items-center text-sm mr-2"}
   num-words
   [:span {:class "text-xs text-slate-600 ml-1"} (if (= 1 num-words) "word" "words")]])

(defn actionbar [{:keys [active-formats word-count on-format-toggle]}]
  (r/with-let [move-handler (fn [e]
                              (when (:fullscreen? @re-frame.db/app-db)
                                (let [mouse-percent-y (/ (.-clientY e) js/window.innerHeight)]
                                  (when (>= mouse-percent-y consts/actionbar-fullscreen-wakeup-threshold)
                                    (dispatch [:actionbar-woken])))))
               _ (.addEventListener js/window "mousemove" move-handler)]
    (let [transparent? @(subscribe [:actionbar-transparent?])
          base-classes "fixed px-1 py-1 flex place-content-between transition-all duration-150 "
          visible-classes (str bg-color " bottom-2.5 rounded-md inset-x-10 border border-light-blue drop-shadow-[0_10px_10px_rgba(0,0,0,0.1)]")
          transparent-classes "inset-x-0 bottom-0 bg-transparent"]
      [:div {:class (str base-classes (if transparent? transparent-classes visible-classes))}
       [:div.flex
        [format-button {:img-url "icons/italic.svg"
                        :active? (active-formats :italic)
                        :transparent-mode? transparent?
                        :on-click #(on-format-toggle :italic)
                        :mouseover-text (str "Italic (" (shortcut-for :italic) ")")}]
        [format-button {:img-url "icons/bold.svg"
                        :active? (active-formats :bold)
                        :transparent-mode? transparent?
                        :on-click #(on-format-toggle :bold)
                        :mouseover-text (str "Bold (" (shortcut-for :bold) ")")}]
        [format-button {:img-url "icons/strikethrough.svg"
                        :active? (active-formats :strikethrough)
                        :transparent-mode? transparent?
                        :on-click #(on-format-toggle :strikethrough)
                        :mouseover-text (str "Strikethrough (" (shortcut-for :strikethrough) ")")}]

        [h-spacer-sm]

        [format-button {:img-url "icons/h1.svg"
                        :active? (active-formats :h1)
                        :transparent-mode? transparent?
                        :on-click #(on-format-toggle :h1)
                        :mouseover-text (str "Heading 1 (" (shortcut-for :h1) ")")}]
        [format-button {:img-url "icons/h2.svg"
                        :active? (active-formats :h2)
                        :transparent-mode? transparent?
                        :on-click #(on-format-toggle :h2)
                        :mouseover-text (str "Heading 2 (" (shortcut-for :h2) ")")}]

        [h-spacer-sm]

        [format-button {:img-url "icons/numbered.svg"
                        :active? (active-formats :ol)
                        :transparent-mode? transparent?
                        :on-click #(on-format-toggle :ol)
                        :mouseover-text (str "Ordered List (" (shortcut-for :ol) ")")}]
        [format-button {:img-url "icons/bulleted.svg"
                        :active? (active-formats :ul)
                        :transparent-mode? transparent?
                        :on-click #(on-format-toggle :ul)
                        :mouseover-text (str "Unordered List (" (shortcut-for :ul) ")")}]

        ;; Invisible button so that element maintains its height
        ;; Even when all the others are hidden in fullscreen mode
        [invisible-button]]
       #_[:span {:class "flex items-center text-sm mr-2"} word-count " words"]
       [word-count-display (or (:selection word-count) (:total word-count))]])))
