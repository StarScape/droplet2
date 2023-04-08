(ns drop.app.events
  (:require [re-frame.core :refer [reg-event-db reg-event-fx inject-cofx path after]]
            [drop.app.db :refer [default-db]]
            [slate.editor-ui-state :as ui-state]))

(def ls-key-open-file "open-file")

(reg-event-fx
 :initialise-db
 [(inject-cofx :local-store-read [ls-key-open-file ui-state/slate-types-readers])]
 (fn [{:keys [ls-open-file] :as _cofx}]
   (let [initial-db (cond-> default-db ;; place in DB
                      (some? ls-open-file) (assoc :open-file-path ls-open-file))]
     {:db initial-db
      :set-title [(:open-file-path initial-db) true]})))

(reg-event-fx
 :set-open-file-path
 (fn [{:keys [db]} [_ file-path]]
   {:db (assoc db :open-file-path file-path)
    :fx [[:local-store-write [ls-key-open-file file-path]]
         [:set-title [file-path true]]]}))

(reg-event-fx
 :doc-changed
 (fn [{:keys [db]} [_]]
   {:set-title [(:open-file-path db) false]}))

(reg-event-fx
 :doc-saved
 (fn [{:keys [db]} [_]]
   {:set-title [(:open-file-path db) true]}))
