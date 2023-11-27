(ns drop.app.effects
  (:require [re-frame.core :as rf]
            [drop.app.db :refer [get-ls-key]]
            [slate.editor-ui-state :as ui-state]
            [slate.api :as slate-api]
            ["electron" :refer [ipcRenderer]]
            ["path" :as path]))

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
 :open-file
 (fn [[*slate-instance file-contents]]
   ;; TODO: remove when ready to go back to loading document from FS
   (slate-api/when-ready *slate-instance #() #_#(ui-state/load-file! *slate-instance file-contents))))
