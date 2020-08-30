(ns drop.editor.navigation-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.navigation :as nav :refer [next-word]]
            [drop.editor.core :as core]))

(def test-str "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")
(def para (core/paragraph [(core/run test-str)]))

(deftest next-word-test
  (testing "starting from word-char goes to end of that word"
    (is (= 5 (next-word para 0))))

  (testing "starting from last char in word goes only to end of word"
    (is (= 5 (next-word para 4))))

  (testing "starting from space goes to the end of the next word"
    (is (= 11 (next-word para 5))))

  (testing "starting from single separator goes forward 1 space"
    (is (= 12 (next-word para 11))))

  (testing "skips over as much whitespace as possible to get to end of next word"
    (is (= 27 (next-word para 18))))

  (testing "skips over multiple separators if starting from a separator"
    (is (= 47 (next-word para 44))))

  (testing "skips to end of word when starting from a single punctuation mark inside of a word (e.g. a dash)"
    (is (= 80 (next-word para 77))))

  (testing "goes to end of the paragraph if already in the last word/separator/whitespace-block"
    (is (= (count test-str) (next-word para 78)))))

; (deftest next-word-test
;   (let [p (paragraph [(run "Hello world. Hello world, my name is Jack")])]
;     (testing "beginning of word goes to end"
;       (is (= 5 (c/caret (c/next-word p (selection [p 0]))))))

;     (testing "middle of word goes to end"
;       (is (= 5 (c/caret (c/next-word p (selection [p 2]))))))

;     (testing "end of word goes to end of next word, skipping space"
;       (is (= 11 (c/caret (c/next-word p (selection [p 5]))))))

;     (testing "period counts as a separate word"
;       (is (= 12 (c/caret (c/next-word p (selection [p 11]))))))

;     (testing "gives end of paragraph when there is no space at end of last word"
;       (is (= 41 (c/caret (c/next-word p (selection [p 38]))))))))

; (deftest prev-word-test
;   (let [p (paragraph [(run "Hello world. Hello world, my name is Jack")])]
;     (testing "beginning of word goes to end"
;       (is (= 5 (c/caret (c/next-word p (selection [p 0]))))))

;     (testing "middle of word goes to end"
;       (is (= 5 (c/caret (c/next-word p (selection [p 2]))))))

;     (testing "end of word goes to end of next word, skipping space"
;       (is (= 11 (c/caret (c/next-word p (selection [p 5]))))))

;     (testing "period counts as a separate word"
;       (is (= 12 (c/caret (c/next-word p (selection [p 11]))))))

;     (testing "gives end of paragraph when there is no space at end of last word"
;       (is (= 41 (c/caret (c/next-word p (selection [p 38]))))))))

