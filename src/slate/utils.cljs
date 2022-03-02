(ns slate.utils
  "General purpose utilities, such as high-order or collection-
   manipulation functions not found in the standard library.")

(deftype CancellableDebounce [timer, function]
  IFn
  (-invoke [_this] (function))
  (-invoke [_this a] (function a))
  (-invoke [_this a b] (function a b))
  (-invoke [_this a b c] (function a b c))
  (-invoke [_this a b c d] (function a b c d))
  (-invoke [_this a b c d e] (function a b c d e))
  (-invoke [_this a b c d e f] (function a b c d e f))
  (-invoke [_this a b c d e f g] (function a b c d e f g))
  (-invoke [_this a b c d e f g h] (function a b c d e f g h))
  (-invoke [_this a b c d e f g h i] (function a b c d e f g h i))
  (-invoke [_this a b c d e f g h i j] (function a b c d e f g h i j))
  (-invoke [_this a b c d e f g h i j k] (function a b c d e f g h i j k))
  (-invoke [_this a b c d e f g h i j k l] (function a b c d e f g h i j k l))
  (-invoke [_this a b c d e f g h i j k l m] (function a b c d e f g h i j k l m))
  (-invoke [_this a b c d e f g h i j k l m n] (function a b c d e f g h i j k l m n))
  (-invoke [_this a b c d e f g h i j k l m n o] (function a b c d e f g h i j k l m n o))
  (-invoke [_this a b c d e f g h i j k l m n o p] (function a b c d e f g h i j k l m n o p))
  (-invoke [_this a b c d e f g h i j k l m n o p q] (function a b c d e f g h i j k l m n o p q))
  (-invoke [_this a b c d e f g h i j k l m n o p q r] (function a b c d e f g h i j k l m n o p q r))
  (-invoke [_this a b c d e f g h i j k l m n o p q r s] (function a b c d e f g h i j k l m n o p q r s))
  (-invoke [_this a b c d e f g h i j k l m n o p q r s t] (function a b c d e f g h i j k l m n o p q r s t))
  (-invoke [_this a b c d e f g h i j k l m n o p q r s t rest] (apply function a b c d e f g h i j k l m n o p q r s t rest)))

(defn cancellable-debounce
  "Returns a debounced function that will only execute `f` after `ms` milliseconds have elapsed since the last call to it,
   with the _additional_ ability to cancel the timeout altogether, using the `(cancel-debounced!)` function."
  [ms f]
  (let [timer (atom nil)]
    (->CancellableDebounce timer (fn [& xs]
                                   (js/clearTimeout @timer)
                                   (reset! timer (js/setTimeout #(apply f xs) ms))))))

(defn cancel-debounced!
  "Cancels a debounced function created with `(cancellable-debounce)`."
  [debounced-fn]
  (js/clearTimeout (deref ^IAtom (.-timer debounced-fn))))

(comment
  (defn test-fn [a b c]
    (println (str "Args are: " a b c))
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

(defn is-mac? [] true) ;; just return true for now in development

(defn remove-nil-vals-from-map [m]
  (reduce-kv (fn [new-map key val]
               (if (nil? val)
                 (dissoc new-map key)
                 new-map))
             m m))
