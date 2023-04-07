(ns drop.app.components.actionbar
  (:require [drop.app.components.layout :refer [h-spacer-sm]]
            [drop.utils :refer [debounce]]
            [reagent.core :as r :refer-macros [with-let]]
            [slate.utils :as slate-utils]
            ["@headlessui/react" :refer [Transition]]))

(def actionbar-fade-out-ms 2500)
(def actionbar-fullscreen-wakeup-threshold
  "Threshold at which the actionbar will reappear when it has auto-hid itself in fullscreen mode,
   expressed as a percentage of the vertical distance the mouse is from the top of the window."
  0.85)

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

(defn button [{:keys [img-url active? transparent-mode? on-click mouseover-text]}]
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
                     (if active? "bg-light-blue" "bg-white")]
             :title mouseover-text}
    [:img {:src img-url
           :style {:width "15px"}}]]])

(defn- invisible-button []
  [:div.invisible [button "icons/italic.svg" false false #()]])

(defn word-count-display [num-words]
  [:span {:class "flex text-slate-800 items-center text-sm mr-2"}
   num-words
   [:span {:class "text-xs text-slate-500 ml-1"} (if (= 1 num-words) "word" "words")]])

(defn actionbar [{:keys [active-formats word-count on-format-toggle *full-screen?]}]
  (r/with-let [*transparent-mode? (r/atom false)
               set-transparent-debounced! (debounce actionbar-fade-out-ms #(when @*full-screen?
                                                                             (reset! *transparent-mode? true)))
               move-handler (fn [e]
                              (let [mouse-percent-y (/ (.-clientY e) js/window.innerHeight)]
                                (when (>= mouse-percent-y actionbar-fullscreen-wakeup-threshold)
                                  (reset! *transparent-mode? false)
                                  (set-transparent-debounced!))))
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
          base-classes "fixed bottom-0 w-screen px-1 py-1 flex place-content-between transition-all duration-150 "]
      [:div {:class (str base-classes (if transparent?
                                        "bg-transparent"
                                        "bg-white border-t border-gray-200"))}
       [:div.flex
        [button {:img-url "icons/italic.svg"
                 :active? (active-formats :italic)
                 :transparent-mode? transparent?
                 :on-click #(on-format-toggle :italic)
                 :mouseover-text (str "Italic (" (shortcut-for :italic) ")")}]
        [button {:img-url "icons/bold.svg"
                 :active? (active-formats :bold)
                 :transparent-mode? transparent?
                 :on-click #(on-format-toggle :bold)
                 :mouseover-text (str "Bold (" (shortcut-for :bold) ")")}]
        [button {:img-url "icons/strikethrough.svg"
                 :active? (active-formats :strikethrough)
                 :transparent-mode? transparent?
                 :on-click #(on-format-toggle :strikethrough)
                 :mouseover-text (str "Strikethrough (" (shortcut-for :strikethrough) ")")}]

        [h-spacer-sm]

        [button {:img-url "icons/h1.svg"
                 :active? (active-formats :h1)
                 :transparent-mode? transparent?
                 :on-click #(on-format-toggle :h1)
                 :mouseover-text (str "Heading 1 (" (shortcut-for :h1) ")")}]
        [button {:img-url "icons/h2.svg"
                 :active? (active-formats :h2)
                 :transparent-mode? transparent?
                 :on-click #(on-format-toggle :h2)
                 :mouseover-text (str "Heading 2 (" (shortcut-for :h2) ")")}]

        [h-spacer-sm]

        [button {:img-url "icons/numbered.svg"
                 :active? (active-formats :ol)
                 :transparent-mode? transparent?
                 :on-click #(on-format-toggle :ol)
                 :mouseover-text (str "Ordered List (" (shortcut-for :ol) ")")}]
        [button {:img-url "icons/bulleted.svg"
                 :active? (active-formats :ul)
                 :transparent-mode? transparent?
                 :on-click #(on-format-toggle :ul)
                 :mouseover-text (str "Unordered List (" (shortcut-for :ul) ")")}]

        ;; Invisible button so that element maintains its height
        ;; Even when all the others are hidden in fullscreen mode
        [invisible-button]]
       #_[:span {:class "flex items-center text-sm mr-2"} word-count " words"]
       [word-count-display word-count]])))
