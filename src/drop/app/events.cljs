(ns drop.app.events
  (:require [re-frame.core :refer [reg-event-db reg-event-fx inject-cofx path after]]
            [drop.app.db :refer [default-db]]
            [slate.editor-ui-state :as ui-state]))

(def ls-key-open-file "open-file")

(reg-event-fx
 :initialise-db
 [(inject-cofx :local-store-read [ls-key-open-file ui-state/slate-types-readers])]
 (fn [{:keys [ls-open-file] :as cofx}]
   {:db (cond-> default-db ;; place in DB
          (some? ls-open-file) (assoc :open-file ls-open-file))}))

(reg-event-fx
 :set-open-file
 (fn [{:keys [db]} [_ file-name]]
   {:db (assoc db :open-file file-name)
    :fx [[:local-store-write [ls-key-open-file file-name]]]}))

(reg-event-db
 :set-open-file-path
 (fn [db [_ new-path]]
   (assoc-in db [:open-file :path] new-path)))

(reg-event-db
 :set-last-saved-doc
 (fn [db [_ new-last-saved]]
   (assoc-in db [:open-file :last-saved-doc] new-last-saved)))
