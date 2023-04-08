(ns drop.app.effects
  (:require [re-frame.core :as rf]
            [drop.app.db :refer [get-ls-key]]
            ["path" :as path]))

(rf/reg-fx
 :local-store-write
 (fn [[key val]]
   (.setItem js/localStorage (get-ls-key key) (prn-str val))))

(rf/reg-fx
 :set-title
 (fn [[open-file-path saved?]]
   (let [file-name (some-> open-file-path (path/basename))
         title (if-not file-name
                 "Droplet"
                 (cond-> file-name
                   (not saved?) (str "*")))]
     (set! js/document.title title))))
