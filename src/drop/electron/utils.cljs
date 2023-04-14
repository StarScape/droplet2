(ns drop.electron.utils
  (:require ["electron" :refer [ipcMain]]))

(defn on-ipc [channel handler]
  (.on ipcMain channel handler))

(defn on-ipc-once [channel handler]
  (.once ipcMain channel handler))

(defn handle-ipc [channel handler]
  (.handle ipcMain channel handler))

;; Thoughts for an API I won't constantly forget:
;;
;; Electron Side:
;; ipc-send
;; ipc-send-sync
;; ipc-on
;; ipc-on-sync
;;
;; Renderer side:
;; ipc-send
;; ipc-send-syn
;; ipc-on
;; ipc-on-sync
;;
;; More thoughts needed on this. Write down each of the flows and think it through.
