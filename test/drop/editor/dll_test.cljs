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

(deftest get-test
  (is (= val1 (get l "1")))
  (is (= val2 (get l "2")))
  (is (= val3 (get l "3")))
  (is (= val4 (get l "5")))
  (is (= :default (get l "4" :default))))

(deftest dissoc-test
  (is (= (dissoc l "1") (dll val2 val3 val4)))
  (is (= (dissoc l "5") (dll val1 val2 val3)))
  (is (= (dissoc l "doesntexist") l))
  (is (= (dissoc (dll {:uuid "123" :content "foo"}) "123") (dll))))

(deftest conj-test
  (testing "conj works"
    (is (= (conj l {:uuid "6" :content "post"})
           (dll val1 val2 val3 val4 {:uuid "6" :content "post"})))
    (is (= (conj (dll) {:uuid "6" :content "post"})
           (dll {:uuid "6" :content "post"}))))

  (testing "calling (into) on DLL works"
    (is (= (into [] l) [val1 val2 val3 val4]))))

(deftest first-test
  (is (= (dll/first l) val1))
  (is (= (dll/first (dll)) nil)))

(deftest last-test
  (is (= (dll/last l) val4))
  (is (= (dll/last (dll)) nil)))

(deftest rest-test
  (is (= (rest l) (seq [val2 val3 val4])))
  (is (= (rest (dll)) ()))

  ;; Can we destructure it properly?
  (is (= [val2 val1] (let [[a b] l] [b a]))))

(deftest assoc-test
  (testing "assoc and update work correctly"
    (is (= (assoc l "2" {:uuid "2" :content "oyeah"})
           (dll val1 {:uuid "2" :content "oyeah"} val3 val4)))
    (is (= (update l "2" #(assoc % :content "baybee"))
           (dll val1 {:uuid "2" :content "baybee"} val3 val4))))

  (testing "fails when trying to change the uuid or access an element that doesn't exist"
    (is (thrown? js/Error (assoc l "2" {:uuid "3" :content "nope"})))
    (is (thrown? js/Error (assoc l "55" {:uuid "55" :content "nope"})))))

(deftest insert-all-after-test
  (is (= (dll/insert-all-after l "5" [{:uuid "6" :content "post1"} {:uuid "7" :content "post2"}])
         (dll val1 val2 val3 val4 {:uuid "6" :content "post1"} {:uuid "7" :content "post2"})))
  (is (= (dll/insert-all-after l "3" [{:uuid "6" :content "post1"} {:uuid "7" :content "post2"}])
         (dll val1 val2 val3 {:uuid "6" :content "post1"} {:uuid "7" :content "post2"} val4)))
  (is (= (dll/insert-all-after l "1" [{:uuid "6" :content "post1"} {:uuid "7" :content "post2"}])
         (dll val1 {:uuid "6" :content "post1"} {:uuid "7" :content "post2"} val2 val3 val4))))

#_(deftest insert-all-before-test
  (is (= (dll/insert-all-after l "5" [{:uuid "6" :content "post1"} {:uuid "7" :content "post2"}])
         (dll val1 val2 val3 val4 {:uuid "6" :content "post1"} {:uuid "7" :content "post2"})))
  (is (= (dll/insert-all-after l "3" [{:uuid "6" :content "post1"} {:uuid "7" :content "post2"}])
         (dll val1 val2 val3 {:uuid "6" :content "post1"} {:uuid "7" :content "post2"} val4)))
  (is (= (dll/insert-all-after l "1" [{:uuid "6" :content "post1"} {:uuid "7" :content "post2"}])
         (dll val1 {:uuid "6" :content "post1"} {:uuid "7" :content "post2"} val2 val3 val4))))

(deftest remove-all-test
  (is (= (dll/remove-all l "1" "5") (dll)))
  (is (= (dll/remove-all l "1" "3") (dll val4)))
  (is (= (dll/remove-all l "2" "3") (dll val1 val4)))
  (is (= (dll/remove-all l "3" "3") (dll val1 val2 val4)))
  (is (= (dll/remove-all l "1" "2") (dll val3 val4))))

(deftest replace-range-test
  (testing "replacing everything"
    (is (= (dll/replace-range l "1" "5" [{:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"}])
           (dll {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"})))
    (is (= (dll/replace-range l "1" "5" (dll {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"}))
           (dll {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"})))
    (is (= (dll/replace-range l "1" "5" {:uuid "#1" :content "CHANGED"})
           (dll {:uuid "#1" :content "CHANGED"}))))

  (testing "replacing in middle"
    (is (= (dll/replace-range l "2" "3" [{:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"}])
           (dll val1 {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"} val4)))
    (is (= (dll/replace-range l "2" "3" (dll {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"}))
           (dll val1 {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"} val4)))
    (is (= (dll/replace-range l "2" "3" {:uuid "#1" :content "CHANGED"})
           (dll val1 {:uuid "#1" :content "CHANGED"} val4))))

  (testing "replacing from start to middle"
    (is (= (dll/replace-range l "1" "3" [{:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"}])
           (dll {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"} val4)))
    (is (= (dll/replace-range l "1" "3" (dll {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"}))
           (dll {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"} val4)))
    (is (= (dll/replace-range l "1" "3" {:uuid "#1" :content "CHANGED"})
           (dll {:uuid "#1" :content "CHANGED"} val4))))

  (testing "replacing from middle to end"
    (is (= (dll/replace-range l "2" "5" [{:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"}])
           (dll val1 {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"})))
    (is (= (dll/replace-range l "2" "5" (dll {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"}))
           (dll val1 {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"})))
    (is (= (dll/replace-range l "2" "5" {:uuid "#1" :content "CHANGED"})
           (dll val1 {:uuid "#1" :content "CHANGED"})))))

(deftest ifn-test
  (testing "invoking a DLL as a function does lookup, same as vector or map"
    (is (= (l "1") val1))
    (is (identical? (l "1") val1))
    (is (= (l "2") val2))
    (is (identical? (l "2") val2))))
