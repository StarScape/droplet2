(ns user)

(defn stack []
  (println (str "Last error thrown:\n\n\"\n" (.-stack *e) "\n\""))
  (js/console.error *e))
