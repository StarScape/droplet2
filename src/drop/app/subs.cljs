(ns drop.app.subs
  (:require [re-frame.core :as rf]))

(rf/reg-sub
 :open-file-path
 (fn [db _]
   (-> db :open-file-path)))

(rf/reg-sub
 :slate-instance
 (fn [db _]
   (-> db :*slate-instance)))

(rf/reg-sub
 :fullscreen?
 (fn [db _]
   (-> db :fullscreen?)))

(rf/reg-sub
 :actionbar-transparent?
 (fn [db _]
   (-> db :actionbar-transparent?)))

(rf/reg-sub
 :active-formats
 (fn [db _]
   (-> db :active-formats)))

(rf/reg-sub
 :word-count
 (fn [db _]
   (-> db :word-count)))

(rf/reg-sub
 :find-and-replace-focused?
 (fn [db _] (-> db :find-and-replace-focused?)))
