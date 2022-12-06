(ns drop.app.utils
  (:require ["electron" :refer [ipcRenderer]]
            ["path" :as path]))

(defn set-title!
  [{:keys [path last-saved-doc] :as _open-file-info} current-doc]
  (let [file-name (when path (path/basename path))
        title (or file-name "Untitled")
        title (if (and path (not= last-saved-doc current-doc))
                (str title "*")
                title)]
    (set! js/document.title title)))

(defn show-error-dialog!
  "Shows a system error dialog. This sends an IPC message to the main process."
  [title dialog-text]
  (.send ipcRenderer "show-error-dialog" title dialog-text))
