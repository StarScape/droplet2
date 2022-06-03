(ns drop.app.components.core
  (:require [reagent.core :as r :refer-macros [with-let]]
            [drop.app.components.slate-editor :refer [slate-editor]]))

(defn app
  "Main app component."
  []
  [:div
   [slate-editor]])
