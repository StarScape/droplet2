(ns drop.app.persistent-atom
  (:require [clojure.edn :as edn]
            ["electron" :refer [ipcRenderer]]))

(defn persistent-atom
  "Creates a new atom whose value will be persisted between sessions.
   The `atom-name` parameter exists to give the atom a unique filename,
   and must be a fully-qualified keyword, such as ::last-file-open. There is
   no need to add file extensions."
  [atom-name default-val]
  {:pre [(qualified-keyword? atom-name)]}
  (let [persist! (fn [state]
                   (.send ipcRenderer "persist-atom" (str atom-name) (prn-str state)))
        [file-exists? file-contents] (.sendSync ipcRenderer "get-atom-value" (str atom-name))
        *atom (atom (if file-exists?
                      (edn/read-string file-contents)
                      default-val))]
    (when-not file-exists?
      (persist! @*atom))

    (add-watch *atom :persist (fn [_key _atom _old-state new-state]
                                (persist! new-state)))
    *atom))