(ns drop.app.persistent-atom
  (:require [clojure.edn :as edn]))

(defn atom-name->local-storage-key [atom-name]
  (str "prst-atom-" (str atom-name)))

(defn persistent-atom
  "Creates a new atom whose value will be persisted between sessions.
   The `atom-name` parameter exists to give the atom a globally unique name,
   and must be a fully-qualified keyword, such as ::last-file-open. There is
   no need to add file extensions.

   Takes the optional named parameters :readers to specific additional EDN readers needed."
  [atom-name default-val & {:keys [readers], :or {readers {}}}]
  {:pre [(qualified-keyword? atom-name)]}
  (let [key (atom-name->local-storage-key atom-name)
        persist! (fn [state]
                   (js/localStorage.setItem key (prn-str state)))
        persisted-val (js/localStorage.getItem key)
        *atom (atom (if persisted-val
                      (edn/read-string {:readers readers} persisted-val)
                      default-val))]
    (when-not persisted-val
      (persist! @*atom))

    (add-watch *atom :persist (fn [_key _atom _old-state new-state]
                                (persist! new-state)))
    *atom))

#_(defn persistent-atom
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
