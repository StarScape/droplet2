(ns drop.app.utils
  (:require ["path" :as path]))

(defn set-title!
  [{:keys [path saved?]}]
  (let [file-name (when path (path/basename path))
        title (or file-name "Droplet")
        title (if (and path (not saved?))
                (str title "*")
                title)]
    (set! js/document.title title)))
