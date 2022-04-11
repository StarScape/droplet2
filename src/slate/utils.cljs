(ns slate.utils
  "General purpose utilities, such as high-order or collection-
   manipulation functions not found in the standard library."
  (:require [clojure.string :as str]))

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

(defn set-toggle
  "Adds `val` to set if not present, removes it if present."
  [s val]
  {:pre [(set? s)]}
  (if (contains? s val) (disj s val) (conj s val)))

(defn some!
  "Returns x if x is non-nil, otherwise throws error. (For use in dev, not production.)"
  [x]
  (if (some? x) x (throw (js/Error. (str "val " x " cannot be nil!")))))

(defn pretty-paragraph [para]
  (str "|(" (-> (:uuid para) (str) (.substr 0 5)) ", " (:type para) "), \""
       (->> (:runs para)
            (map :text)
            (flatten)
            (apply str)) "\"|"))

(defn pretty-editor-update [{:keys [editor-state changelist]}]
  (let [{:keys [doc]} editor-state
        changed-paras (map #(get (:children doc) %) (:changed-uuids changelist))
        inserted-paras (map #(get (:children doc) %) (:inserted-uuids changelist))
        deleted-paras (map #(get (:children doc) %) (:deleted-uuids changelist))
        pretty-paras (fn [paras] (str/join ", " (map pretty-paragraph #_(fn [p] (pretty-paragraph p)) paras)))
        render-paras (fn [title paras]
                       (if (seq paras)
                         (str title " (" (count paras) "): " (pretty-paras paras) "\n")
                         ""))]
    (str "----------------------------------------\n"
         (render-paras "changed" changed-paras)
         (render-paras "inserted" inserted-paras)
         (render-paras "deleted" deleted-paras)
         "----------------------------------------\n")))

(defn pretty-history-stack
  [history]
  (let [current-state-index (:current-state-index history)
        backstack-strs (map-indexed (fn [idx, editor-update]
                                      (str (if (= idx current-state-index)
                                             (str "> ## Backstack " idx ":\n")
                                             (str "## Backstack " idx ":\n"))
                                           (pretty-editor-update editor-update) "\n"))
                                    (:backstack history))
        pretty-backstack (str/join "\n" backstack-strs)]
    (str "# Backstack:\n"
         pretty-backstack "\n"

         (if (= current-state-index (count (:backstack history)))
           "> # Tip:\n"
           "# Tip:\n")
         (if-let [tip (:tip history)]
           (pretty-editor-update tip)
           "nil"))))
