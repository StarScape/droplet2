(ns slate.events-browser-test
  (:require [cljs.test :include-macros true :refer [is deftest]]
            [slate.events :as e :refer [reg-interceptor
                                        parse-event
                                        add-key-to-history
                                        max-input-history]]))

(def dummy-op (fn []))
(def empty-int-map {:shortcuts {}, :completions {}})

(deftest history-test
  (is (= (add-key-to-history [] "a")
         ["a"]))
  (is (= (add-key-to-history ["a" "b"] "c")
         ["a" "b" "c"]))
  (let [full-history (vec (repeat max-input-history "k"))]
    (is (= (add-key-to-history full-history "a")
           (-> (drop 1 full-history) (vec) (conj "a"))))))

(deftest reg-interceptor-test
  (is (= (:shortcuts (reg-interceptor empty-int-map :ctrl+shift+left dummy-op))
         {#{:ctrl :shift :left} dummy-op}))
  (is (= (:shortcuts (reg-interceptor empty-int-map :ctrl+a dummy-op))
         {#{:ctrl :a} dummy-op}))
  (is (= (:shortcuts (reg-interceptor empty-int-map :down dummy-op))
         {#{:down} dummy-op}))
  (is (= (:completions (reg-interceptor empty-int-map "abc" dummy-op))
         {"c" {"b" {"a" dummy-op}}}))
  (is (thrown? js/Error (reg-interceptor empty-int-map :ctrk+shoft+left dummy-op))))

(deftest parse-event-test
  (is (= (parse-event #js {:ctrlKey true, :shiftKey true, :key "ArrowLeft"})
         #{:ctrl :shift :left})))

(deftest find-interceptor-test
  (let [sample-ints {:shortcuts {#{:ctrl :shift :a} :foo}
                     :completions {"c" {"b" {"a" :bar}}}}]
    (is (= :foo
           (e/find-interceptor sample-ints :ctrl+shift+a)
           (e/find-interceptor sample-ints :shift+ctrl+a)))
    (is (= :bar (e/find-interceptor sample-ints "abc")))))

(def comp1 (fn []))
(def comp2 (fn []))
(def completion-interceptors {:completions {"c" {"b" {"a" comp1, "1" comp2}}}
                              :shortcuts {}})

(deftest matching-completion-test
  ;; "abc" and "1bc"
  (is (= comp1 (e/matching-completion? "c" completion-interceptors ["a" "b"])))
  (is (= comp2 (e/matching-completion? "c" completion-interceptors ["1" "b"])))
  (is (= nil (e/matching-completion? "d" completion-interceptors ["a" "b"])))
  (let [comp3 (fn [])
        new-interceptors (reg-interceptor completion-interceptors "abd" comp3)]
    (is (= comp3 (e/matching-completion? "d" new-interceptors ["a" "b"])))))
