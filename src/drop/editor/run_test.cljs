(ns drop.editor.run-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.core :as c]))

(def run1 (c/run "foobar"))
(def run2 (c/run "foo"))

(deftest insert-test
  (testing "with range selection"
    (let [r (c/insert run1 "a" 1 3)]
      (is (= "fabar" (:text r)))))

  (testing "with range that is whole run"
    (let [r (c/insert run1 "fizzbuzz" 0 6)]
      (is (= "fizzbuzz" (:text r)))
      (is (= 8 (c/len r)))))

  (testing "single-selection at the start of a run"
    (let [r (c/insert run1 "a" 0)]
      (is (= "afoobar" (:text r)))))

  (testing "single-selection in the middle of a run"
    (let [r (c/insert run1 "a" 2)]
      (is (= "foaobar" (:text r)))))

  (testing "single-selection at the end of run"
    (let [r (c/insert run1 "a" 6)]
      (is (= "foobara" (:text r)))))

  ;; TODO: illegal insert before and after run
  )

(deftest delete-test
  (testing "range delete from beginning"
    (let [r (c/delete run1 0 1)]
      (is (= "oobar" (:text r)))))

  (testing "range delete from middle"
    (let [r (c/delete run1 1 3)]
      (is (= "fbar" (:text r)))))

  (testing "range delete of entire run"
    (let [r (c/delete run1 0 6)]
      (is (= "" (:text r)))))

  (testing "backspace at start"
    (comment "TODO"))

  (testing "backspace in the middle"
    (let [r (c/delete run1 5)]
      (is (= "foobr" (:text r)))))

  (testing "backspace at the end"
    (let [r (c/delete run1 6)]
      (is (= "fooba" (:text r)))))

  (testing "backspace off end of range"
    (comment "TODO"))

  (testing "backspace before beginning"
    (comment "TODO")))

(deftest toggle-format-test
  (let [test-run (c/run "foobar" #{:italic :bold})
        toggled1 (c/toggle-format test-run :italic)
        toggled2 (c/toggle-format test-run :bold)
        toggled3 (c/toggle-format test-run :underline)]
    (is (= #{:bold} (:formats toggled1)))
    (is (= #{:italic} (:formats toggled2)))
    (is (= #{:italic :bold :underline} (:formats toggled3)))))
