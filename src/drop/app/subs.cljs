(ns drop.app.subs
  (:require [re-frame.core :as rf]))

(rf/reg-sub
 :open-file
 (fn [db _query-vector]
   (:open-file db)))