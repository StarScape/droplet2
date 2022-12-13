(ns slate.weak-cache-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.utils :refer [weak-cache]]))

(deftest basic-test
  (testing "caching works"
    (let [weak-caching-fn (fn [] {:result 12})
          o1 #js {}
          o2 #js {}]
      (is (= {:result 12}
             (weak-cache o1 weak-caching-fn)
             (weak-cache o1 weak-caching-fn)))
      (is (identical? (weak-cache o1 weak-caching-fn)
                      (weak-cache o1 weak-caching-fn)))
      (is (not (identical? (weak-cache o1 weak-caching-fn)
                           (weak-cache o2 weak-caching-fn))))))
  (testing "prevents expensive computation"
    (let [*num-calls (atom 0)
          expensive-fn (fn []
                         (swap! *num-calls inc)
                         (for [x (range 1000)]
                           (* x x)))
          o1 #js {}
          call1 (weak-cache o1 expensive-fn)
          call2 (weak-cache o1 expensive-fn)
          call3 (weak-cache o1 expensive-fn)]
      (is (= call1 call2 call3))
      (is (= 1 @*num-calls)))))
