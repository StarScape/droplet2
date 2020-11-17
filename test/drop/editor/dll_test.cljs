(ns drop.editor.dll-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.dll :as dll :refer [dll]]))

(def val1 {:uuid "1" :content "foo"})
(def val2 {:uuid "2" :content "bar"})
(def val3 {:uuid "3" :content "bizz"})
(def val4 {:uuid "5" :content "bang"})
(def l (dll val1 val2 val3 val4))
(def l1 (dll/insert-before l "5" {:uuid "4" :content "bar"}))
(def l2 (dll/insert-before l "1" {:uuid "-1" :content "pre"}))

(deftest initialize-test
  (testing "empty constructor"
    (let [d (dll)]
      (is (= {} (.-entries-map d)))
      (is (= nil (.-first-uuid d)))
      (is (= nil (.-last-uuid d)))))

  (testing "adding one item"
    (let [d (dll {:uuid "a" :field :value})]
      (is (= {"a" (dll/Node. {:uuid "a" :field :value} nil nil)} (.-entries-map d)))
      (is (= "a" (.-first-uuid d)))
      (is (= "a" (.-last-uuid d)))))

  (testing "adding multiple items"
    (let [d (dll {:uuid "a" :field 1} {:uuid "b" :field 2} {:uuid "c" :field 3})]
      (is (= (.-entries-map d) {"a" (dll/Node. {:uuid "a" :field 1} nil "b")
                                "b" (dll/Node. {:uuid "b" :field 2} "a" "c")
                                "c" (dll/Node. {:uuid "c" :field 3} "b" nil)}))
      (is (= "a" (.-first-uuid d)))
      (is (= "c" (.-last-uuid d))))))

(deftest insert-before-test
  (is (= (dll/insert-before l "1" {:uuid "-1" :content "pre"})
         (dll {:uuid "-1" :content "pre"}
              {:uuid "1" :content "foo"}
              {:uuid "2" :content "bar"}
              {:uuid "3" :content "bizz"}
              {:uuid "5" :content "bang"})))

  (is (= (dll/insert-before l "5" {:uuid "4" :content "buzz"})
         (dll {:uuid "1" :content "foo"}
              {:uuid "2" :content "bar"}
              {:uuid "3" :content "bizz"}
              {:uuid "4" :content "buzz"}
              {:uuid "5" :content "bang"})))

  (is (thrown? js/Error (dll/insert-before l "-1" {:uuid "-2" :content "WUT"}))))

(deftest insert-after-test
  (is (= (dll/insert-after l "1" {:uuid "1.5" :content "inserted"})
         (dll {:uuid "1" :content "foo"}
              {:uuid "1.5" :content "inserted"}
              {:uuid "2" :content "bar"}
              {:uuid "3" :content "bizz"}
              {:uuid "5" :content "bang"})))

  (is (= (dll/insert-after l "5" {:uuid "6" :content "post"})
         (dll {:uuid "1" :content "foo"}
              {:uuid "2" :content "bar"}
              {:uuid "3" :content "bizz"}
              {:uuid "5" :content "bang"}
              {:uuid "6" :content "post"})))

  (is (thrown? js/Error (dll/insert-after l "100" {:uuid "101" :content "WUT"}))))

(deftest seq-test
  (testing "produces a seq correctly"
    (is (= (seq l) (seq [{:uuid "1" :content "foo"}
                         {:uuid "2" :content "bar"}
                         {:uuid "3" :content "bizz"}
                         {:uuid "5" :content "bang"}]))))

  (testing "can filter and map correctly"
    (is (= (map #(update % :content str " EDITED") l)
           (seq [{:uuid "1" :content "foo EDITED"}
                 {:uuid "2" :content "bar EDITED"}
                 {:uuid "3" :content "bizz EDITED"}
                 {:uuid "5" :content "bang EDITED"}])))
    (is (= (filter #(not= "1" (:uuid %)) l)
           (seq [{:uuid "2" :content "bar"}
                 {:uuid "3" :content "bizz"}
                 {:uuid "5" :content "bang"}])))))

(deftest count-test
  (is (= 4 (count l)))
  (is (= 5 (count l1)))
  (is (= 5 (count l2))))

(deftest equiv-test
  (testing "empty DLLs equal"
    (is (= (dll) (dll))))

  (testing "equivalent DLLs equal"
    (is (= (dll {:uuid "1" :content "foo"}
                {:uuid "2" :content "bar"}
                {:uuid "3" :content "bizz"})
           (dll {:uuid "1" :content "foo"}
                {:uuid "2" :content "bar"}
                {:uuid "3" :content "bizz"})))
    (is (= l1 (dll/insert-before l "5" {:uuid "4" :content "bar"})))))

(deftest next-test
  (testing "works with UUIDs"
    (is (= (dll/next l "1") val2))
    (is (= (dll/next l "2") val3))
    (is (= (dll/next l "3") val4))
    (is (= (dll/next l "5") nil))
    (is (thrown? js/Error (= (dll/next l "4") nil))))

  (testing "works with elements of list"
    (is (= val4 (->> (dll/next l val1)
                     (dll/next l)
                     (dll/next l))))))

(deftest prev-test
  (testing "works with UUIDs"
    (is (= (dll/prev l "1") nil))
    (is (= (dll/prev l "2") val1))
    (is (= (dll/prev l "3") val2))
    (is (= (dll/prev l "5") val3))
    (is (thrown? js/Error (= (dll/prev l "4") nil))))

  (testing "works with elements of list"
    (is (= val1 (->> (dll/prev l val4)
                     (dll/prev l)
                     (dll/prev l))))))

(deftest empty-test
  (is (empty? (dll)))
  (is #_:clj-kondo/ignore (not (empty? (dll {:uuid "1" :val "somethin"}))))
  (is (seq (dll {:uuid "1" :val "somethin"}))))

;; TODO: test (get) on DLL
;; TODO: test dissoc
;; TODO: test (empty?) on DLL
;; TODO: test conj and (into) on DLL
