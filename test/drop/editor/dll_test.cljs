(ns drop.editor.dll-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.dll :as dll :refer [dll]]))

(def val1 {:uuid "1" :content "foo"})
(def l (dll val1 {:uuid "2" :content "bar"} {:uuid "3" :content "bizz"} {:uuid "5" :content "bang"}))
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

;; TODO: test equiv
;; TODO: test (count) on DLL
;; TODO: test (get) on DLL
;; TODO: test next and prev
;; TODO: test dissoc
;; TODO: test (empty?) on DLL
;; TODO: test conj and (into) on DLL
