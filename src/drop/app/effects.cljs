(ns drop.app.effects
  (:require [re-frame.core :as rf]
            [drop.app.db :refer [get-ls-key]]
            [slate.editor-ui-state :as ui-state]
            [slate.api :as slate-api]
            ["electron" :refer [ipcRenderer]]
            ["path" :as path]))

(rf/reg-fx
 :local-store-write
 (fn [[key val]]
   (.setItem js/localStorage (get-ls-key key) (prn-str val))))

(defonce debounce-timeouts
  (atom {}))

(rf/reg-fx :dispatch-debounce
        (fn [[id event-vec n]]
          (js/clearTimeout (@debounce-timeouts id))
          (swap! debounce-timeouts assoc id
                 (js/setTimeout (fn []
                                  (rf/dispatch event-vec)
                                  (swap! debounce-timeouts dissoc id))
                                n))))

(rf/reg-fx :stop-debounce
        (fn [id]
          (js/clearTimeout (@debounce-timeouts id))
          (swap! debounce-timeouts dissoc id)))

(rf/reg-fx
 :set-title
 (fn [[open-file-path saved?]]
   (let [file-name (some-> open-file-path (path/basename))
         title (if-not file-name
                 "Droplet"
                 (cond-> file-name
                   (not saved?) (str "*")))]
     (set! js/document.title title))))

(rf/reg-fx
 :check-for-opened-file
 (fn []
   (-> (.invoke ipcRenderer "file-opened-from-os?")
       (.then (fn [[opened? file-path contents]]
                (if opened?
                  (rf/dispatch [:opened-file-from-os file-path contents])
                  (rf/dispatch [:reopen-last-file]))))
       (.catch (fn [err]
                 (js/console.log err)
                 (rf/dispatch [:reopen-last-file]))))))

(rf/reg-fx
 ;; Read file from file system and open it in current slate instance.
 :read-and-open-file
 (fn [[*slate-instance file-path]]
   (let [file-contents (let [[error?, file-text] (.sendSync ipcRenderer "read-file" file-path)]
                         (if error?
                           ;; Slate will default to an empty document when receiving nil.
                           nil
                           file-text))]
     (slate-api/when-ready *slate-instance #(ui-state/load-file! *slate-instance file-contents)))))

(rf/reg-fx
 :open-file
 (fn [[*slate-instance file-contents]]
   (slate-api/when-ready *slate-instance #(ui-state/load-file! *slate-instance file-contents))))

;; (rf/reg-fx
;;  :open-new-document
;;  (fn [[*slate-instance]]
;;    (ui-state/new-document! #p *slate-instance)))
