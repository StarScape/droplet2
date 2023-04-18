(ns drop.app.db
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [re-frame.core :as re-frame]
            [reagent.core :as r]))

(defn get-ls-key [key] (str "ls-" key))

(def default-db
  {:open-file {:path nil,
               :loading? true}
   :*slate-instance (r/atom nil)
   :fullscreen? false})

(defn- check-matches-spec
  "Returns the localstore-value if it matches the spec,
   else clears it from localstorage and returns nil."
  [spec ls-key localstore-value]
  (if (s/valid? spec localstore-value)
    localstore-value
    (do
      (js/console.log (str "LocalStorage key " ls-key " does not match its spec, removing."))
      (.removeItem js/localStorage ls-key)
      nil)))

(re-frame/reg-cofx
 :local-store-read
 (fn [cofx [key readers spec]]
   (let [ls-key (get-ls-key key)]
     (assoc cofx (keyword ls-key)
            (some->> (.getItem js/localStorage ls-key)
                     (edn/read-string {:readers readers})
                     (check-matches-spec spec ls-key))))))
