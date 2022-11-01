(ns slate.word-count-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.word-count :as wc :refer [word-count]]))

(deftest word-count-test
  (is (= 0 (word-count "")))
  (is (= 1 (word-count "Hello")))
  (is (= 2 (word-count "Hello world")))
  (is (= 2 (word-count "Hello world!")))
  (is (= 3 (word-count "Hello there world!")))
  (is (= 3 (word-count "Hello    there world!")))
  (is (= 2 (word-count "A compound-word"))))
