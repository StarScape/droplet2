(ns slate.style
  (:require-macros [garden.def :refer [defkeyframes]])
  (:require [garden.core :refer [css]]))

(def global-style
  ;; Slate uses a global, hidden iframe for parsing and examining foreign HTML
  ;; This is because parts of the native HTML+CSS parsing capabilities of the
  ;; browser only work if the elements are actually in a document.
  [[:.slate-iframe {:position "absolute"
                    :left 0
                    :top 0
                    :width 0
                    :height 0
                    :pointer-events "none"
                    :z-index -1000
                    :opacity 0}]])

(def global-<style>
  (let [elem (js/document.createElement "style")]
    (set! (.-textContent elem) (apply css global-style))
    elem))

(defn install-global-styles!
  "Adds the Slate global stylesheet to the global document, if it has not already been added."
  []
  (when-not (.-isConnected global-<style>)
    (js/document.head.append global-<style>)))

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
                    :tab-size "25px"
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
                 :padding-bottom "5px"
                 :min-height "1em"}]
   ;; When there is an empty paragraph with just a caret in it, we need to render a _bit_
   ;; of text, so that the height of the paragraph is set to the same as any other ('font height' is
   ;; not possible to get programmatically) . This solves that.
   [:.line::after {:content "\" \""}]
   [:.span {:display "inline-block"}]
   [:.slate-range-selection {:background-color "var(--range-selection-color)"
                             ;;  :border-radius "3px"
                             :z-index 1000}]
   ;; Selected elements whose previous sibling is a .slate-range-selection (all but first)
  ;;  [(keyword ".slate-range-selection + .slate-range-selection") {:border-top-left-radius 0
  ;;                                                                :border-bottom-left-radius 0}]
  ;;  ;; Selected elements whose next sibling is a .slate-range-selection (all but last)
  ;;  [(keyword ".slate-range-selection:has(+ .slate-range-selection)") {:border-top-right-radius 0
  ;;                                                                     :border-bottom-right-radius 0}]
   [:.slate-text-caret {:position "relative"}
    [:&::after {:position "absolute"
                :content "\" \""
                :width "2px"
                :border-radius "2px"
                :background-color "var(--text-caret-color)"
                :animation "blink-anim 1.2s infinite"
                :animation-delay "0.5s"}]]
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
   #_[:.underline-format {:text-decoration "line-through !important"}]
   [:.highlight-format {:background-color "#d1d5db"}]])

(comment
  (println (apply css shadow-elem-style))
  )

(def shadow-elem-css-rendered (apply css shadow-elem-style))

(defn set-css-prop! [shadow-root prop val]
  (.. shadow-root -host -style (setProperty prop val)))

(defn lose-focus! [shadow-root]
  (set-css-prop! shadow-root "--range-selection-color", "#bdcddb")
  (set-css-prop! shadow-root "--text-caret-color", "#4496da"))

(defn gain-focus! [shadow-root]
  (set-css-prop! shadow-root "--range-selection-color" "#b4ddff")
  (set-css-prop! shadow-root "--text-caret-color", "#008cff"))
