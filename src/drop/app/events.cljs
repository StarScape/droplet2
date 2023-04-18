(ns drop.app.events
  (:require [clojure.spec.alpha :as s]
            [re-frame.core :refer [reg-event-db reg-event-fx inject-cofx path after]]
            [drop.app.db :refer [default-db]]
            [slate.serialization :refer [slate-types-readers]]
            [slate.editor-ui-state :as ui-state]))

(def ls-key-open-file "open-file-path")
(s/def ::open-file-path string?)

(reg-event-fx
 :boot
 (fn [_ _]
   {:db default-db
    :fx [[:check-for-opened-file]]}))

(reg-event-fx
 :reopen-last-file
 [(inject-cofx :local-store-read [ls-key-open-file slate-types-readers ::open-file-path])]
 (fn [{:keys [db ls-open-file-path]} _]
   (let [{:keys [*slate-instance]} db]
     {:db (if ls-open-file-path
            (assoc db :open-file {:path ls-open-file-path
                                  :loading? false}) ; read below is sync, set loading done
            db)
      :fx [(when ls-open-file-path
             [:read-and-open-file [*slate-instance ls-open-file-path]])
           [:local-store-write [ls-key-open-file ls-open-file-path]]
           [:set-title [ls-open-file-path true]]]})))

(reg-event-fx
 :opened-file-from-os
 (fn [{:keys [db]} [_ file-path file-contents]]
   (let [{:keys [*slate-instance]} db]
     {:db (assoc db :open-file {:path file-path
                                :loading? false})
      :fx [[:open-file [*slate-instance file-contents]]
           [:local-store-write [ls-key-open-file file-path]]
           [:set-title [file-path true]]]})))

(reg-event-fx
 :set-open-file-path
 (fn [{:keys [db]} [_ file-path]]
   {:db (assoc-in db [:open-file :path] file-path)
    :fx [[:local-store-write [ls-key-open-file file-path]]
         [:set-title [file-path true]]]}))

(reg-event-fx
 :doc-changed
 (fn [{:keys [db]} [_]]
   {:set-title [(-> db :open-file :path) false]}))

(reg-event-fx
 :doc-saved
 (fn [{:keys [db]} [_]]
   {:set-title [(-> db :open-file :path) true]}))
