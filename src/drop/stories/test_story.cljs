(ns drop.stories.test-story
  (:require [drop.app.components.core :refer [test-component]]
            [reagent.core :as r]))

(def ^:export default
  #js {:title "Test component"
       :component (r/reactify-component test-component)})

(defn ^:export JackTestComponent []
  (r/as-element [test-component "Jack"]))

(defn ^:export JohnTestComponent []
  (r/as-element [test-component "John"]))
