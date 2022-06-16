(ns drop.app.components.slate-editor
  (:require [slate.editor-ui-state :as ui-state]
            [slate.core :as sl]
            [slate.utils :as utils]
            ["electron" :refer [ipcRenderer]]))

;; TODO: persist dis bih
(def *open-file (atom nil))

(defn slate-editor [file-deserialized]
  [:div.slate-editor
   {:ref (fn [elem]
           (when elem
             (let [on-save-as (fn [serialized]
                                (-> (.invoke ipcRenderer "save-file-as" serialized)
                                    (.then #(reset! *open-file %))
                                    (.catch #())))
                   *ui-state (sl/init! :history file-deserialized
                                       :dom-elem elem
                                       :on-save (fn [serialized]
                                                  (if @*open-file
                                                    (.send ipcRenderer "save-file" @*open-file serialized)
                                                    (on-save-as serialized)))
                                       :on-save-as on-save-as
                                       :on-open (fn [*ui-state]
                                                  (-> (.invoke ipcRenderer "choose-file")
                                                      (.then (fn [[file-path contents]]
                                                               (reset! *open-file file-path)
                                                               (sl/load-file! *ui-state contents)))
                                                      (.catch #(js/console.log %)))))]
               ;; Utility for viewing editor history from console
               (set! js/dumpHistory #(js/console.log (utils/pretty-history-stack (:history @*ui-state)))))))}])

(defn main-editor []
  (let [current-file @*open-file]
    (if current-file
      (let [file-contents (.sendSync ipcRenderer "read-file" current-file)
            deserialized (ui-state/deserialize file-contents)]
        (slate-editor deserialized))
      (slate-editor nil))))
