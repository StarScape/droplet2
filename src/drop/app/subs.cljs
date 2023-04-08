(ns drop.app.subs
  (:require [re-frame.core :as rf]))

(rf/reg-sub
 :open-file-path
 (fn [db _]
   (-> db :open-file-path)))
