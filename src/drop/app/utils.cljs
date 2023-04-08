(ns drop.app.utils
  (:require ["electron" :refer [ipcRenderer]]))

(defn show-error-dialog!
  "Shows a system error dialog. This sends an IPC message to the main process."
  [title dialog-text]
  (.send ipcRenderer "show-error-dialog" title dialog-text))
