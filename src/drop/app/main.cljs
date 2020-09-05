(ns drop.app.main)

(defn ^:dev/after-load reload
  "Called after hot-reloading."
  [] (println "sum"))

(defn main []
  (println (str "Main class x3!")))
