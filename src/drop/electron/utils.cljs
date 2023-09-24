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


(defn- persisted-file-path
  [file-name]
  (path/join (.getPath app "userData") (if (.-isPackaged app)
                                         (str file-name ".edn")
                                         ;; We don't want to mess with real files from our
                                         ;; main Droplet install while playing around during
                                         ;; development, potentionally changing schemas, etc.
                                         (str file-name "-dev" ".edn"))))

(defn write-persisted!
  [file-name content]
  (let [path (persisted-file-path file-name)]
    (log "Writing persisted file at path '" path "'")
    (fs/writeFile path (prn-str content) "utf8" (fn [err]
                                                  (when err
                                                    (log "Error writing to persisted file at path '" path "'")
                                                    (js/console.log err))))))

(defn read-persisted!
  [file-name default-val callback]
  (let [file-path (persisted-file-path file-name)]
    (fs/readFile file-path "utf8" (fn [err, contents]
                               (if err
                                 (do
                                   (write-persisted! file-name default-val)
                                   (callback default-val))
                                 (callback (edn/read-string contents)))))))

(comment
  (persisted/declare-file ::current-file
    :default {:path nil}
    :spec (s/or 1 2))
  (persisted/write! ::current-file {:path "~/foo.drop"})
  (persisted/read! ::current-file (fn [read-val]
                                    ...)))

