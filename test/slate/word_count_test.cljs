(ns slate.word-count-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.word-count :as wc :refer [str-word-count]]))

(deftest word-count-test
  (is (= 0 (str-word-count "")))
  (is (= 1 (str-word-count "Hello")))
  (is (= 2 (str-word-count "Hello world")))
  (is (= 2 (str-word-count "Hello world!")))
  (is (= 3 (str-word-count "Hello there world!")))
  (is (= 3 (str-word-count "Hello    there world!")))
  (is (= 2 (str-word-count "A compound-word"))))
