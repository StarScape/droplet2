(ns drop.app.file-handling
  (:require [slate.editor-ui-state :as ui-state]
            [slate.filetypes.core :as filetypes]
            [slate.model.history :as history]
            [re-frame.core :as rf :refer [dispatch]]
            [re-frame.db]
            ["electron" :refer [ipcRenderer]]
            ["path" :as path]))

(defn- spawn-new-file-confirmation-dialog!
  "Spawns a dialog asking the user to confirm that they'd like to create a new file.
   Will return `true` if the user confirms, false otherwise."
  []
  (.sendSync ipcRenderer "new-file-confirmation-dialog"))


(defn open-doc!
  [*ui-state doc]
  (ui-state/load-document! *ui-state doc)
  (dispatch [:set-open-file-path nil]))

(defn open-file!
  [*ui-state file-path contents]
  (ui-state/load-file! *ui-state contents)
  (dispatch [:set-open-file-path file-path])
  #_(dispatch [:set-open-file-loading false]))

(defn on-new!
  "Spawns a confirmation dialog, and if confirmed, resets the editor
   surface to a new file, and resets the open-file information."
  [*slate-instance]
  (when (spawn-new-file-confirmation-dialog!)
    (ui-state/new-document! *slate-instance)
    (dispatch [:set-open-file-path nil])))

(defn on-open! [*ui-state]
  (-> (.invoke ipcRenderer "choose-file")
      (.then (fn [[file-path contents]]
               (open-file! *ui-state file-path contents)))
      (.catch #(js/console.log %))))

(defn on-save-as!
  [serialized-history]
  (-> (.invoke ipcRenderer "save-file-as" serialized-history)
      (.then #(dispatch [:set-open-file-path %]))
      (.catch #(js/console.log %))))

(defn on-save! [serialized-history]
  (let [open-file-path (-> @re-frame.db/app-db :open-file :path)]
    (if open-file-path
      (do
        (.send ipcRenderer "save-file" open-file-path serialized-history)
        (dispatch [:doc-saved]))
      (on-save-as! serialized-history))))

(defn on-export! [ui-state export-type]
  (let [{:keys [history]} ui-state
        doc (:doc (history/current-state history))
        exported (filetypes/export export-type doc)
        open-file-path (-> @re-frame.db/app-db :open-file :path)
        suggested-file-name (if open-file-path
                              (.basename path open-file-path ".drop")
                              "untitled")]
    (.send ipcRenderer "save-exported-file-as" exported export-type suggested-file-name)))