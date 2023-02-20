(ns drop.app.events
  (:require [re-frame.core :refer [reg-event-db reg-event-fx inject-cofx path after]]
            [drop.app.db :refer [default-db]]))

(reg-event-fx
 :initialise-db
 []
 (fn [_db _]
   {:db default-db}))

(reg-event-db
 :set-open-file
 (fn [db [_ file-name]]
   (assoc db :open-file file-name)))