(ns slate.model.run-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.common :as sl]
            [slate.model.run :as r]))

(def run1 (r/run "foobar"))
(def run2 (r/run "foo"))

(deftest insert-test
  (testing "with range selection"
    (let [r (r/insert run1 "a" 1 3)]
      (is (= "fabar" (:text r)))))

  (testing "with range that is whole run"
    (let [r (r/insert run1 "fizzbuzz" 0 6)]
      (is (= "fizzbuzz" (:text r)))
      (is (= 8 (sl/len r)))))

  (testing "single-selection at the start of a run"
    (let [r (r/insert run1 "a" 0)]
      (is (= "afoobar" (:text r)))))

  (testing "single-selection in the middle of a run"
    (let [r (r/insert run1 "a" 2)]
      (is (= "foaobar" (:text r)))))

  (testing "single-selection at the end of run"
    (let [r (r/insert run1 "a" 6)]
      (is (= "foobara" (:text r)))))

  ;; TODO: illegal insert before and after run
  )

(deftest delete-test
  (testing "range delete from beginning"
    (let [r (sl/delete run1 0 1)]
      (is (= "oobar" (:text r)))))

  (testing "range delete from middle"
    (let [r (sl/delete run1 1 3)]
      (is (= "fbar" (:text r)))))

  (testing "range delete of entire run"
    (let [r (sl/delete run1 0 6)]
      (is (= "" (:text r)))))

  (testing "backspace at start"
    (comment "TODO"))

  (testing "backspace in the middle"
    (let [r (sl/delete run1 5)]
      (is (= "foobr" (:text r)))))

  (testing "backspace at the end"
    (let [r (sl/delete run1 6)]
      (is (= "fooba" (:text r)))))

  (testing "backspace off end of range"
    (comment "TODO"))

  (testing "backspace before beginning"
    (comment "TODO")))

(deftest graphemes-test
  (is (= (sl/graphemes (r/run "foo"))
         [{:index 0, :segment "f"} {:index 1, :segment "o"} {:index 2, :segment "o"}]))
  (is (= (sl/graphemes (r/run "建前"))
         [{:index 0, :segment "建"} {:index 1, :segment "前"}]))
  (is (= (sl/graphemes (r/run "🏳️‍🌈🦎🤦🏽ñ"))
         [{:index 0, :segment "🏳️‍🌈"}
          {:index 6, :segment "🦎"}
          {:index 8, :segment "🤦🏽"}
          {:index 12, :segment "ñ"}])))
