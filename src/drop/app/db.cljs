(ns drop.app.db
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [re-frame.core :as re-frame]
            [reagent.core :as r]))

(defn get-ls-key [key] (str "ls-" key))

(def default-db
  {:open-file {:path nil,
               :loading? true}
   :*slate-instance (r/atom nil)
   :fullscreen? false
   :actionbar-transparent? false
   :active-formats #{}
   :word-count 0
   :find-and-replace-focused? false})
