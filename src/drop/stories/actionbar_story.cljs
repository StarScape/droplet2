(ns drop.stories.actionbar-story
  (:require [drop.app.components.actionbar :refer [actionbar]]
            [drop.stories.helper :as helper]
            [reagent.core :as r]))

(def ^:export default
  (helper/->default
   {:title "Actionbar"
    :component actionbar}))

(defn ^:export Basic []
  (r/as-element [:<>
                 [:p "Bold and italic active:"]
                 [actionbar #{:italic :bold}]]))
