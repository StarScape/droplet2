(ns drop.electron.utils
  (:require ["electron" :refer [ipcMain app]]
            ["path" :as path]
            ["fs" :as fs]
            [clojure.edn :as edn]))

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
;; ipc-send-sync
;; ipc-on
;; ipc-on-sync
;;
;; More thoughts needed on this. Write down each of the flows and think it through.

(defn- persisted-file-path
  [file-name]
  (path/join (.getPath app "appData") (str file-name ".edn")))

(defn write-persisted!
  [file-name content]
  (fs/writeFile (persisted-file-path file-name) (prn-str content) "utf8" #()))

(defn read-persisted!
  [file-name default-val callback]
  (let [file-path (persisted-file-path file-name)]
    (fs/readFile file-path "utf8" (fn [err, contents]
                               (if err
                                 (do
                                   (write-persisted! file-name default-val)
                                   (callback default-val))
                                 (callback (edn/read-string contents)))))))

(defn log [& msg]
  (js/console.log (str "> " (apply str msg))))
