(ns drop.utils
  "Universal utilties (not specific to any submodule)."
  (:import [goog.async Throttle]))

(goog-define DEV false)

(def no-op "No-op function" #())

(defn debounce [ms f]
  (let [timer (atom nil)]
    (fn [& xs]
      (js/clearTimeout @timer)
      (reset! timer (js/setTimeout #(apply f xs) ms)))))

(defn disposable->function [disposable listener interval]
  (let [disposable-instance (disposable. listener interval)]
    (fn [& args]
      (.apply (.-fire disposable-instance) disposable-instance (to-array args)))))

;; TODO: this is used inside Slate and ought to be in its utils instead
(defn throttle [interval listener]
  (disposable->function Throttle listener interval))

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
