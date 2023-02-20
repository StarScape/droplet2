(ns drop.app.components.core
  (:require [reagent.core :as r :refer-macros [with-let]]
            [re-frame.core :as rf]
            [drop.app.components.slate-editor :refer [main-editor]]
            [drop.app.components.actionbar :refer [actionbar]]))

(defn test-component [name]
  [:h1 "Hello, " name "!"])

(defn app
  "Main app component."
  []
  [:div
   #_[:h1 {:on-click #(rf/dispatch [:set-open-file (str "file-" (rand-int 100))])}
    @(rf/subscribe [:open-file])]
   [main-editor]])
