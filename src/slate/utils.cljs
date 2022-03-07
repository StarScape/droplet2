(ns slate.utils
  "General purpose utilities, such as high-order or collection-
   manipulation functions not found in the standard library.")

(defn debounce [ms f]
  (let [timer (atom nil)]
    (fn [& xs]
      (js/clearTimeout @timer)
      (reset! timer (js/setTimeout #(apply f xs) ms)))))

(defn is-mac? [] true) ;; just return true for now in development

(defn remove-nil-vals-from-map [m]
  (reduce-kv (fn [new-map key val]
               (if (nil? val)
                 (dissoc new-map key)
                 new-map))
             m m))
