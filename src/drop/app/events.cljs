(ns drop.app.events
  (:require [clojure.spec.alpha :as s]
            [re-frame.core :refer [reg-event-db reg-event-fx inject-cofx path after]]
            [drop.app.db :refer [default-db]]
            [slate.serialization :refer [slate-types-readers]]
            [slate.editor-ui-state :as ui-state]
            [drop.app.consts :as consts]))

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

(reg-event-fx
 :set-full-screen
 (fn [{:keys [db]} [_ fullscreen?]]
   {:db (cond-> (assoc db :fullscreen? fullscreen?)
                (not fullscreen?) (assoc :actionbar-transparent? false))
    :fx [(when fullscreen?
           ; Make actionbar transparent after n seconds
           [:dispatch-debounce [::actionbar [:set-actionbar-transparent true] consts/actionbar-fade-out-ms]])]}))

(reg-event-db
 :set-actionbar-transparent
 (fn [db [_ transparent?]]
   (assoc db :actionbar-transparent? (if (and transparent? (not (:fullscreen? db))) ; this can happen after a debounce
                                       false
                                       transparent?))))

(reg-event-fx
 :actionbar-woken
 (fn [{:keys [db]} _]
   {:db (assoc db :actionbar-transparent? false)
    :fx [[:dispatch-debounce [::actionbar [:set-actionbar-transparent true] consts/actionbar-fade-out-ms]]]}))

(reg-event-db
 :set-word-count
 (fn [db [_ new-word-count]]
   (assoc db :word-count new-word-count)))

(reg-event-db
 :set-active-formats
 (fn [db [_ new-active-formats]]
   (assoc db :active-formats new-active-formats)))
