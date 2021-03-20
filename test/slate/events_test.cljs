(ns slate.events-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.refers :as events :refer [reg-interceptor
                                             parse-event
                                             add-key-to-history
                                             max-input-history]]))

(def dummy-op (fn []))

(deftest reg-interceptor-test
  (is (= (reg-interceptor {} :ctrl+shift+left dummy-op)
         {#{:ctrl :shift :left} dummy-op}))
  (is (= (reg-interceptor {} :ctrl+a dummy-op)
         {#{:ctrl :a} dummy-op}))
  (is (= (reg-interceptor {} :down dummy-op)
         {#{:down} dummy-op}))
  (is (thrown? js/Error (reg-interceptor {} :ctrk+shoft+left dummy-op))))

(deftest parse-event-test
  (is (= (parse-event #js {:ctrlKey true, :shiftKey true, :key "ArrowLeft"})
         #{:ctrl :shift :left})))

(deftest history-test
  (is (= (add-key-to-history [] "a")
         ["a"]))
  (is (= (add-key-to-history ["a" "b"] "c")
         ["a" "b" "c"]))
  (let [full-history (vec (repeat max-input-history "k"))]
    (is (= (add-key-to-history full-history "a")
           (-> (drop 1 full-history) (vec) (conj "a"))))))
