(ns slate.utils
  "General purpose utilities, such as high-order or collection-
   manipulation functions not found in the standard library."
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn is-mac? [] true) ;; just return true for now in development

(defn cmd-or-ctrl-key
  "Takes a KeyboardEvent, returns true if the CMD key is pressed
   (on macOS), or if the ctrlKey is pressed (on Windows and Linux)."
  [keyboard-event]
  (if (is-mac?)
    (.-metaKey keyboard-event)
    (.-ctrlKey keyboard-event)))

(defn common-elements [& colls]
  (let [freqs (map frequencies colls)]
    (mapcat (fn [e] (repeat (apply min (map #(% e) freqs)) e))
            (apply set/intersection (map (comp set keys) freqs)))))

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
  ([history n]
   (let [current-state-index (:current-state-index history)
         backstack-strs (map-indexed (fn [idx, editor-update]
                                       (let [original-idx (dec (- (count (:backstack history)) idx))]
                                         (str (if (= original-idx current-state-index)
                                                (str "%c>>%c ## Backstack " original-idx ":\n")
                                                (str "## Backstack " original-idx ":\n"))
                                              (pretty-editor-update editor-update) "\n")))
                                     (take n (reverse (:backstack history))))
         pretty-backstack (str/join "\n" backstack-strs)
         fmt-str (str (if (= current-state-index (count (:backstack history)))
                      "%c>>%c # Tip:\n"
                      "# Tip:\n")
                    (if-let [tip (:tip history)]
                      (pretty-editor-update tip)
                      "No tip\n")
                    "\n"

                    "# Backstack:\n"
                    pretty-backstack)]
     (js/console.log fmt-str "color: red;", "color: black;")))
  ([history] (pretty-history-stack history (count (:backstack history)))))
