(ns drop.app.utils
  (:require ["path" :as path]))

(defn set-title!
  [{:keys [path last-saved-doc] :as _open-file-info} current-doc]
  (let [file-name (when path (path/basename path))
        title (or file-name "Droplet")
        title (if (and path (not= last-saved-doc current-doc))
                (str title "*")
                title)]
    (set! js/document.title title)))
