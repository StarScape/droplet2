(ns drop.app.db
  (:require [re-frame.core :as re-frame]
            [clojure.edn :as edn]))

(defn get-ls-key [key] (str "ls-" key))

(def default-db
  {:open-file {:path nil, :last-saved-doc nil}})

(re-frame/reg-cofx
 :local-store-read
 (fn [cofx [key readers]]
   (let [ls-key (get-ls-key key)]
     (assoc cofx (keyword ls-key)
            (some->> (.getItem js/localStorage ls-key)
                     (edn/read-string {:readers readers}))))))
