(ns slate.utils
  "General purpose utilities, such as high-order or collection-
   manipulation functions not found in the standard library.")

(deftype CancellableDebounce [timer, function]
  IFn
  (-invoke [this] (function this))
  (-invoke [this a] (function this a))
  (-invoke [this a b] (function this a b))
  (-invoke [this a b c] (function this a b c))
  (-invoke [this a b c d] (function this a b c d))
  (-invoke [this a b c d e] (function this a b c d e))
  (-invoke [this a b c d e f] (function this a b c d e f))
  (-invoke [this a b c d e f g] (function this a b c d e f g))
  (-invoke [this a b c d e f g h] (function this a b c d e f g h))
  (-invoke [this a b c d e f g h i] (function this a b c d e f g h i))
  (-invoke [this a b c d e f g h i j] (function this a b c d e f g h i j))
  (-invoke [this a b c d e f g h i j k] (function this a b c d e f g h i j k))
  (-invoke [this a b c d e f g h i j k l] (function this a b c d e f g h i j k l))
  (-invoke [this a b c d e f g h i j k l m] (function this a b c d e f g h i j k l m))
  (-invoke [this a b c d e f g h i j k l m n] (function this a b c d e f g h i j k l m n))
  (-invoke [this a b c d e f g h i j k l m n o] (function this a b c d e f g h i j k l m n o))
  (-invoke [this a b c d e f g h i j k l m n o p] (function this a b c d e f g h i j k l m n o p))
  (-invoke [this a b c d e f g h i j k l m n o p q] (function this a b c d e f g h i j k l m n o p q))
  (-invoke [this a b c d e f g h i j k l m n o p q r] (function this a b c d e f g h i j k l m n o p q r))
  (-invoke [this a b c d e f g h i j k l m n o p q r s] (function this a b c d e f g h i j k l m n o p q r s))
  (-invoke [this a b c d e f g h i j k l m n o p q r s t] (function this a b c d e f g h i j k l m n o p q r s t))
  (-invoke [this a b c d e f g h i j k l m n o p q r s t rest] (apply function this a b c d e f g h i j k l m n o p q r s t rest))) ;; just return true for now in development

(defn cancellable-debounce
  "Returns a debounced function that will only execute `f` after `ms` milliseconds have elapsed since the last call to it,
   with the _additional_ ability to cancel the timeout altogether, using the `(cancel-debounced!)` function."
  [ms f]
  (let [timer (atom nil)]
    (->CancellableDebounce timer (fn [& xs]
                                   (js/clearTimeout @timer)
                                   (reset! timer (js/setTimeout (fn [] #p "timeout elapsed!" (apply f xs)) ms))))))

(defn cancel-debounced!
  "Cancels a debounced function created with `(cancellable-debounce)`."
  [debounced-fn]
  (js/clearTimeout (deref ^IAtom (.-timer debounced-fn))))

(comment
  (defn test-fn [a b c]
    (println #p (str "Args are: " a b c))
    nil)

  (def debounced-fn (cancellable-debounce 3000 test-fn))

  (debounced-fn 1 :a ["apples"])

  (comment (cancel-debounced! debounced-fn))
  )

(defn debounce [ms f]
  (let [timer (atom nil)]
    (fn [& xs]
      (js/clearTimeout @timer)
      (reset! timer (js/setTimeout #(apply f xs) ms)))))

(defn remove-nil-vals-from-map [m]
  (reduce-kv (fn [new-map key val]
               (if (nil? val)
                 (dissoc new-map key)
                 new-map))
             m m))
