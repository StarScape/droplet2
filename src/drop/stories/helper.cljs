(ns drop.stories.helper
  (:require [reagent.core :as r]))

(defn ->params [^js args]
  (js->clj args :keywordize-keys true))

(defn ->reactified [options path]
  (if (get-in options path)
    (update-in options path r/reactify-component)
    options))

(defn ->default
  "Automatically reactifies :component and [:parameters :docs :page], if they are present."
  [options]
  (-> options
      (->reactified [:component])
      (->reactified [:parameters :docs :page])
      clj->js))