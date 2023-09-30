(ns drop.electron.utils
  (:require ["electron" :refer [ipcMain app]]
            ["path" :as path]
            ["fs" :as fs]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]))

(defn log [& msg]
  (js/console.log (str "> " (apply str msg))))

;; Thoughts for an IPC API I won't constantly forget:
;;
;; Electron Side:
;; ipc-send
;; ipc-send-sync
;; ipc-on
;; ipc-on-sync
;;
;; Renderer side:
;; ipc-send
;; ipc-send-sync
;; ipc-on
;; ipc-on-sync
;;
;; More thoughts needed on this. Write down each of the flows and think it through.
(defn on-ipc [channel handler]
  (.on ipcMain channel handler))

(defn on-ipc-once [channel handler]
  (.once ipcMain channel handler))

(defn handle-ipc [channel handler]
  (.handle ipcMain channel handler))
