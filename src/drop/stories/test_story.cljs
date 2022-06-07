(ns drop.stories.test-story
  (:require [drop.app.components.core :refer [test-component]]
            [drop.stories.helper :as helper]
            [reagent.core :as r]))

(def ^:export default
  (helper/->default {:title "Test component"
                     :component test-component}))

(defn ^:export JackTestComponent []
  (r/as-element [test-component "Jack"]))

(defn ^:export JohnTestComponent []
  (r/as-element [test-component "John"]))
