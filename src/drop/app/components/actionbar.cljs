(ns drop.app.components.actionbar
  (:require [clojure.string :as str]
            [goog.string :as gstring :refer [format]]
            [goog.string.format]
            [drop.app.consts :as consts]
            [drop.app.components.library :as components]
            [drop.app.components.layout :refer [h-spacer-sm]]
            [drop.utils :refer [debounce]]
            [reagent.core :as r :refer-macros [with-let]]
            [re-frame.core :as rf :refer [dispatch subscribe]]
            [slate.utils :as slate-utils]
            [re-frame.db :as db]
            ["tailwind-merge" :refer [twMerge]]))

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

(defn format-button [{:keys [duration-ms] :as props}]
  (let [half-duration (/ duration-ms 2)
        ;; When *entering* transparent mode, first transition opacity to zero and then width/padding to zero
        transition-hide (format "[transition:opacity_%ims,max-width_%ims_%ims,padding_%ims_%ims]"
                                half-duration half-duration half-duration half-duration half-duration)
        ;; When *leaving* transparent mode, first transition width/padding back to normal and then opacity to 1
        transition-show (format "[transition:max-width_%ims,margin_%ims,padding_%ims,opacity_%ims_%ims]"
                                half-duration half-duration half-duration half-duration half-duration)
        transition-none "transition-none"
        *transition (r/atom transition-hide)
        *transparent-mode? (subscribe [:actionbar-transparent?])]
    
    ;; TODO: Is this comment out of date or still relevant?
    ;; Transition logic:
    ;; =================
    ;; 1. When `transparent-mode?` false -> true && `hidden?`: enable HIDE transition
    ;; 2. When `transparent-mode?` already true && `hidden?` _or_ visible: disable ALL transitions [1]
    ;; 3. When `transparent-mode?` true -> false && `hidden?`: enable SHOW transition
    ;;
    ;; [1] This is to prevent jerky movement when transitioning from e.g. an italic word to a bold word
    ;; via a click--we want the italic button to _immediately_ be replaced by the bold one, no animation.
    ;;
    ;; It would be cool to eventually figure out a way to have an enter animation still play when toggling a format
    ;; via the keyboard (and only then), but that's a bit fiddly for now and far from strictly necessary.
    (add-watch *transparent-mode? (keyword (str (:img-url props) "-watch"))
               (fn [_ _ old new]
                 (cond
                   (and (= old false) (= new true))
                   (do
                     (reset! *transition transition-hide)
                     ;; Hack. Situation: transition to transparent-mode with some button active (bold, say).
                     ;; The below :on-transition-end event is never called, because active buttons have nothing to transition
                     ;; Therefore, the active button gets set to transition-hide, and when we then go and click on some text
                     ;; with a different format, the active button has to transition out rather than just immediately show the
                     ;; new one
                     (js/setTimeout #(reset! *transition transition-none) duration-ms))

                   (and (= old true) (= new false))
                   (reset! *transition transition-show))))

    (fn [{:keys [img-url active? transparent-mode? space-after? on-click mouseover-text]}]
      (let [hidden? (and transparent-mode? (not active?))
            transition @*transition]
        [components/toggleable-button {:on-click on-click
                                       :toggled? active?
                                       :on-transition-end (fn [e]
                                                            (when (and transparent-mode?
                                                                       (or (and hidden? (= "max-width" (.-propertyName e)))
                                                                           (and (not hidden?) (= "opacity" (.-propertyName e)))))
                                                              (reset! *transition transition-none)))
                                       :class (twMerge "rounded-md" transition (when space-after? "mr-1.5")
                                                       (if hidden?
                                                         "opacity-0 mx-0 max-w-0 px-0"
                                                         "opacity-1 max-w-[35px]"))
                                       :title mouseover-text}
         [:img {:src img-url
                :style {:width "15px"}}]]))))

#_(defn- invisible-button []
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
          show-hide-duration 200
          base-classes (twMerge "fixed px-1 py-1 flex place-content-between transition-all" (str "duration-[" show-hide-duration "]"))
          visible-classes (str bg-color " bottom-2.5 rounded-md inset-x-10 border border-light-blue drop-shadow-[0_10px_10px_rgba(0,0,0,0.1)]")
          transparent-classes "inset-x-0 bottom-0 bg-transparent"
          buttons-info [{:img-url "icons/italic.svg"
                         :active? (contains? active-formats :italic)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :italic)
                         :mouseover-text (str "Italic (" (shortcut-for :italic) ")")
                         :duration-ms show-hide-duration}
                        {:img-url "icons/bold.svg"
                         :active? (contains? active-formats :bold)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :bold)
                         :mouseover-text (str "Bold (" (shortcut-for :bold) ")")
                         :duration-ms show-hide-duration}
                        {:img-url "icons/strikethrough.svg"
                         :active? (contains? active-formats :strikethrough)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :strikethrough)
                         :mouseover-text (str "Strikethrough (" (shortcut-for :strikethrough) ")")
                         :duration-ms show-hide-duration
                         :space-after? true}

                        {:img-url "icons/h1.svg"
                         :active? (contains? active-formats :h1)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :h1)
                         :mouseover-text (str "Heading 1 (" (shortcut-for :h1) ")")
                         :duration-ms show-hide-duration}
                        {:img-url "icons/h2.svg"
                         :active? (contains? active-formats :h2)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :h2)
                         :mouseover-text (str "Heading 2 (" (shortcut-for :h2) ")")
                         :duration-ms show-hide-duration
                         :space-after? true}

                        {:img-url "icons/numbered.svg"
                         :active? (contains? active-formats :ol)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :ol)
                         :mouseover-text (str "Ordered List (" (shortcut-for :ol) ")")
                         :duration-ms show-hide-duration}
                        {:img-url "icons/bulleted.svg"
                         :active? (contains? active-formats :ul)
                         :transparent-mode? transparent?
                         :left-of-first-active? false
                         :on-click #(on-format-toggle :ul)
                         :mouseover-text (str "Unordered List (" (shortcut-for :ul) ")")
                         :duration-ms show-hide-duration}]
          #_#_#_#_#_#_idx-first-active (find-first-index :active? buttons-info)
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
