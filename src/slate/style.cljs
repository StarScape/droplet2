(ns slate.style
  (:require-macros [garden.def :refer [defkeyframes]])
  (:require [garden.core :refer [css]]))

(defkeyframes blink-anim
  [:50%
   {:opacity 0}])

(def shadow-elem-style
  [blink-anim
   [(keyword ":host") {:--range-selection-color "#b4ddff"
                       :--text-caret-color "#008cff"}]
   [:body {:font-kerning "none !important"}]
   [:.hidden-input {:opacity 0
                    :position "absolute"
                    :right "10000px"
                    :z-index -10000
                    :width "0px"
                    :height "0px"
                    :pointer-events "none"}]
   [:.slate-editor {:white-space "pre"
                    :box-sizing "border-box"
                    :margin 0
                    :padding-top "20px"
                    :padding-bottom "70vh"
                    :font-size "16px"
                    :font-family "Merriweather, serif"
                    :user-select "none"
                    :color "#202124"}
    [:&:hover {:cursor "text"}]]
   [:.paragraph {:margin "0px"
                 :padding 0
                 :min-height "1em"}]
   ;; When there is an empty paragraph with just a caret in it, we need to render a _bit_
   ;; of text, so that the height of the paragraph is set to the same as any other ('font height' is
   ;; not possible to get programmatically) . This solves that.
   [:.line::after {:content "\" \""}]
   [:.slate-text-caret {:position "relative"}
    [:&::after {:position "absolute"
                :content "\" \""
                :width "2px"
                :border-radius "2px"
                :background-color "var(--text-caret-color)"
                :animation "blink-anim 1.2s infinite"
                :animation-delay "0.5s"}]]
   [:.slate-range-selection {:background-color "var(--range-selection-color)"
                             :border-radius "3px"
                             :z-index 1000}]
   ;; Selected elements whose previous sibling is a .slate-range-selection (all but first)
   [(keyword ".slate-range-selection + .slate-range-selection") {:border-top-left-radius 0
                                                                 :border-bottom-left-radius 0}]
   ;; Selected elements whose next sibling is a .slate-range-selection (all but last)
   [(keyword ".slate-range-selection:has(+ .slate-range-selection)") {:border-top-right-radius 0
                                                                      :border-bottom-right-radius 0}]

   ;; TODO: this is the wrong approach. Use a CSS variable for this and change it when the program isn't focused
   [:.slate-range-selection-blurred {:background-color "#e0e1e2"}]

   [:ul :ol {:padding 0
             :margin 0}]
   [:.ul-format :.ol-format {:display "list-item"
                             :margin-left "1.25em"}]
   [:.ul-format {:list-style-type "disc"}]
   [:.ol-format {:list-style-type "decimal"}]
   [:.h1-format {:font-size "2em"}]
   [:.h2-format {:font-size "1.25em"}]
   [:.italic-format {:font-style "italic !important"}]
   [:.strikethrough-format {:text-decoration "line-through"}]
   [:.bold-format {:font-weight "bold !important"}]
   [:.underline-format {:text-decoration "line-through !important"}]
   [:.highlight-format {:background-color "#d1d5db"}]])

(comment
  (println (apply css shadow-elem-style)))

(def shadow-elem-css-rendered (apply css shadow-elem-style))

(defn set-css-prop! [shadow-root prop val]
  (.. shadow-root -host -style (setProperty prop val)))

(defn lose-focus! [shadow-root]
  (set-css-prop! shadow-root "--range-selection-color", "#bdcddb")
  (set-css-prop! shadow-root "--text-caret-color", "#4496da"))

(defn gain-focus! [shadow-root]
  (set-css-prop! shadow-root "--range-selection-color" "#b4ddff")
  (set-css-prop! shadow-root "--text-caret-color", "#008cff"))
