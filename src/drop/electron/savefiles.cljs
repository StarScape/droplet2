(ns drop.electron.savefiles
  "Code for writing to persisted savefiles.
   The contents of these files will be persisted between sessions."
  (:require ["electron" :refer [app]]
            ["path" :as path]
            ["fs" :as fs]
            ["node:fs/promises" :refer [readFile writeFile]]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [promesa.core :as p]
            [drop.electron.utils :refer [log]])
  (:require-macros [promesa.core]))

(defn- persisted-file-path
  [name]
  {:pre [(keyword? name)]}
  (let [file-path (clojure.core/name name)]
    (path/join (.getPath app "userData") (if (.-isPackaged app)
                                           (str file-path ".edn")
                                           ;; We don't want to mess with real files from our
                                           ;; main Droplet install while playing around during
                                           ;; development, potentionally changing schemas, etc.
                                           (str file-path "-dev" ".edn")))))

(def ^:private *saved-files (atom {}))

(defn- get-or-throw [name]
  (if-let [savefile-info (get @*saved-files name)]
    savefile-info
    (throw (js/Error. (str "No savefile registered for " name)))))

(defn declare!
  [& {:keys [default spec name]}]
  {:pre [(keyword? name)]}
  (if (and (contains? @*saved-files name) (.-isPackaged app)) ; only in prod because in dev file will be reevaluated
    (throw (js/Error. (str "Savefile already registered for " name)))
    (swap! *saved-files assoc name {:default-val default
                                    :spec spec
                                    :file-path (persisted-file-path name)})))

(defn write!
  [name value]
  (let [{:keys [default-val file-path]} (get-or-throw name)]
    (log "Writing persisted file with name " name " to file at path '" file-path "'")

    (-> (writeFile file-path (prn-str value) "utf8")
        (p/catch (fn [err]
                   (log "Error writing to persisted file at path '" file-path "'")
                   (js/console.log err)
                   default-val)))))

(defn read!
  "Reads the savefile (previously registered with `declare!`) and returns its value in a Promise."
  [name]
  (let [{:keys [default-val spec file-path]} (get-or-throw name)]
    (-> (readFile file-path "utf8")
        (p/then (fn [contents]
                  (let [deserialized (edn/read-string contents)]
                    (if-not (s/valid? spec deserialized)
                      (do
                        (write! name default-val)
                        default-val)
                      deserialized))))
        (p/catch (fn [_err]
                   (write! name default-val)
                   default-val)))))
