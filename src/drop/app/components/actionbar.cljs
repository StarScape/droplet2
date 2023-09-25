(ns drop.app.components.actionbar
  (:require [clojure.string :as str]
            [drop.app.consts :as consts]
            [drop.app.components.library :as components]
            [drop.app.components.layout :refer [h-spacer-sm]]
            [drop.utils :refer [debounce]]
            [reagent.core :as r :refer-macros [with-let]]
            [re-frame.core :as rf :refer [dispatch subscribe]]
            [slate.utils :as slate-utils]
            [re-frame.db :as db]
            ["tailwind-merge" :refer [twMerge]]))

(defn- find-first-index
  "Returns the index of the first item matching `pred`."
  [pred coll]
  (ffirst (filter (comp pred second) (map-indexed list coll))))

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

;; DONE: Buttons to left of first active should animate opacity, then width, when going into transparent mode
;; DONE: Buttons to left of first active should animate WIDTH, THEN opacity, when *leaving* transparent mode
;; TODO: After transition INTO transparent mode, kill all transitions on buttons
;; TODO: When transparent mode is disengaged, re-enable all transitions
;; TODO: Put spacers back in (can just use margin instead, but remember to animate it)

(defn format-button [{:keys [img-url active? transparent-mode? left-of-first-active? duration-ms on-click mouseover-text]}]
  (r/with-let [#_#_is-hidden? (fn [transparent-mode? active?] (and transparent-mode? (not active?)))
               #_#_get-transition (fn [transparent-mode? active? left-of-first-active?]
                                (if left-of-first-active?
                                  (if (is-hidden? transparent-mode? active?)
                                    "[transition:max-width_1s,padding_1s,opacity_1s_1s]"
                                    "[transition:opacity_1s,max-width_1s_1s,padding_1s_1s]")
                                  "[transition:opacity_1s"))
               ;; When *entering* transparent mode, first transition opacity to zero and then width/padding to zero
               into-hidden "[transition:opacity_100ms,max-width_100ms_100ms,padding_100ms_100ms]"
               ;; When *leave* transparent mode, first transition width/padding back to normal and then opacity to 1
               from-hidden "[transition:max-width_100ms,padding_100ms,opacity_100ms_100ms]"
               *transition (r/atom into-hidden)]
    (let [hidden? (and transparent-mode? (not active?))]
      [components/toggleable-button {:on-click on-click
                                     :toggled? active?
                                     :on-transition-end (fn [e]
                                                          (reset! *transition (if hidden? from-hidden into-hidden))
                                                          #_ (get-transition transparent-mode? active? left-of-first-active?)
                                                          #_(js/console.log (.-propertyName e)))
                                     :class (twMerge "rounded-md"
                                                     @*transition
                                                     (if hidden?
                                                       (twMerge "opacity-0"
                                                                (when left-of-first-active? "max-w-0 px-0"))
                                                       "opacity-1 max-w-[35px]"))
                                     :title mouseover-text}
       [:img {:src img-url
              :style {:width "15px"}}]])))

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
          base-classes "fixed px-1 py-1 flex place-content-between transition-all duration-200 " ;;transition-all duration-150
          visible-classes (str bg-color " bottom-2.5 rounded-md inset-x-10 border border-light-blue drop-shadow-[0_10px_10px_rgba(0,0,0,0.1)]")
          transparent-classes "inset-x-0 bottom-0 bg-transparent"
          buttons-info [{:img-url "icons/italic.svg"
                         :active? (contains? active-formats :italic)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :italic)
                         :mouseover-text (str "Italic (" (shortcut-for :italic) ")")}
                        {:img-url "icons/bold.svg"
                         :active? (contains? active-formats :bold)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :bold)
                         :mouseover-text (str "Bold (" (shortcut-for :bold) ")")}
                        {:img-url "icons/strikethrough.svg"
                         :active? (contains? active-formats :strikethrough)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :strikethrough)
                         :mouseover-text (str "Strikethrough (" (shortcut-for :strikethrough) ")")}

                        ;; :spacer

                        {:img-url "icons/h1.svg"
                         :active? (contains? active-formats :h1)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :h1)
                         :mouseover-text (str "Heading 1 (" (shortcut-for :h1) ")")}
                        {:img-url "icons/h2.svg"
                         :active? (contains? active-formats :h2)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :h2)
                         :mouseover-text (str "Heading 2 (" (shortcut-for :h2) ")")}

                        ;; :spacer

                        {:img-url "icons/numbered.svg"
                         :active? (contains? active-formats :ol)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :ol)
                         :mouseover-text (str "Ordered List (" (shortcut-for :ol) ")")}
                        {:img-url "icons/bulleted.svg"
                         :active? (contains? active-formats :ul)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :ul)
                         :mouseover-text (str "Unordered List (" (shortcut-for :ul) ")")}]
          idx-first-active (find-first-index :active? buttons-info)
          buttons-info (if idx-first-active
                         (map-indexed (fn [idx info]
                                        (if (< idx idx-first-active)
                                          (assoc info :left-of-first-active? true)
                                          info))
                                      buttons-info)
                         buttons-info)
          buttons-info (into [] buttons-info)]
      [:div {:class (twMerge base-classes (if transparent? transparent-classes visible-classes))}
       [:div.flex
        (for [info buttons-info]
            ^{:key (:img-url info)} [format-button info])

        ;; Invisible button so that element maintains its height
        ;; Even when all the others are hidden in fullscreen mode
        #_[invisible-button]]
       #_[:span {:class "flex items-center text-sm mr-2"} word-count " words"]
       [word-count-display (or (:selection word-count) (:total word-count))]])))
