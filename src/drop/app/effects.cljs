(ns drop.app.effects
  (:require [re-frame.core :as rf]
            [drop.app.db :refer [get-ls-key]]))

(rf/reg-fx
 :local-store-write
 (fn [[key val]]
   (.setItem js/localStorage (get-ls-key key) (prn-str val))))
