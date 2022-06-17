(ns drop.electron.utils
  (:require ["electron" :refer [ipcMain]]))

(defn on-ipc [channel handler]
  (.on ipcMain channel handler))

(defn handle-ipc [channel handler]
  (.handle ipcMain channel handler))
