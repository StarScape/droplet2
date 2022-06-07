(ns drop.stories.actionbar-story
  (:require [drop.app.components.actionbar :refer [actionbar]]
            [reagent.core :as r]))

(def ^:export default
  #js {:title "Actionbar"
       :component (r/reactify-component actionbar)})

(defn ^:export Basic []
  (r/as-element [:<>
                 [:p "Bold and italic active:"]
                 [actionbar #{:italic :bold}]]))
