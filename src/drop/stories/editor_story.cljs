(ns drop.stories.editor-story
  (:require [drop.app.components.slate-editor :refer [slate-editor]]
            [drop.stories.helper :as helper]
            [reagent.core :as r]))

(def ^:export default
  (helper/->default
   {:title "Editor"
    :component (r/reactify-component slate-editor)}))

(defn ^:export Basic []
  (r/as-element [slate-editor]))