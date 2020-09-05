(ns drop.editor.navigation-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.navigation :as nav :refer [next-word prev-word next-word-offset prev-word-offset]]
            [drop.editor.core :as core :refer [caret selection]]))

(def test-str "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")
(def para (core/paragraph [(core/run test-str)]))

(deftest next-word-test
  (testing "starting from word-char goes to end of that word"
    (is (= 5 (caret (next-word para (selection [para 0]))))))

  (testing "starting from last char in word goes only to end of word"
    (is (= 5 (caret (next-word para (selection [para 4]))))))

  (testing "starting from space goes to the end of the next word"
    (is (= 11 (caret (next-word para (selection [para 5]))))))

  (testing "starting from single separator goes forward 1 space"
    (is (= 12 (caret (next-word para (selection [para 11]))))))

  (testing "skips over as much whitespace as possible to get to end of next word"
    (is (= 27 (caret (next-word para (selection [para 18]))))))

  (testing "skips over multiple separators if starting from a separator"
    (is (= 47 (caret (next-word para (selection [para 44]))))))

  (testing "skips to end of word when starting from a single punctuation mark inside of a word (e.g. a dash)"
    (is (= 80 (caret (next-word para (selection [para 77]))))))

  (testing "goes to end of the paragraph if already in the last word/separator/whitespace-block"
    (is (= (count test-str) (caret (next-word para (selection [para 78]))))))

  (testing "collapses first"
    (is (= 11 (caret (next-word para (selection [para 0] [para 5])))))))

;; TODO: fix this
(deftest prev-word-test
  (testing "starting from within word goes to first char of the word"
    (is (= 6 (caret (prev-word para (selection [para 9]))))))

  (testing "starting from space directly after word goes to first char of word"
    (is (= 32 (caret (prev-word para (selection [para 36]))))))

  (testing "starting from after space goes to the beginning of the previous word"
    (is (= 13 (caret (prev-word para (selection [para 19]))))))

  (testing "starting from immediately after single separator with a word before it goes to word start"
    (is (= 11 (caret (prev-word para (selection [para 12]))))))

  (testing "in hyphenated word, starting from the end goes back to the hyphen"
    (is (= 78 (caret (prev-word para (selection [para 80]))))))

  (testing "in hyphenated word, starting from immediately after the hyphen goes back to beginning of previous word"
    (is (= 75 (caret (prev-word para (selection [para 78]))))))

  (testing "in hypenated word, starting from first hyphen goes to beginning of word"
    (is (= 75 (caret (prev-word para (selection [para 77]))))))

  (testing "skips over as much whitespace as possible to get to the start of the previous word"
    (is (= 13 (caret (prev-word para (selection [para 21]))))))

  (testing "skips over mutliple separators if there is a separator before the starting position"
    (is (= 44 (caret (prev-word para (selection [para 47]))))))

  (testing "goes to beginning of the paragraph if already in the last word/separator/whitespace-block"
    (is (= 0 (caret (prev-word para (selection [para 4]))))))

  (testing "collapses first"
    (is (= 0 (caret (prev-word para (selection [para 5] [para 55] true)))))))

(deftest hyphen-back-and-forth-test
  (let [text "word1 a-very-long-hyphenated-word word2"]
    (is (= 33 (->> 5
                   (next-word-offset text)
                   (next-word-offset text)
                   (next-word-offset text)
                   (next-word-offset text)
                   (next-word-offset text))))

    (is (= 6 (->> 33
                  (prev-word-offset text)
                  (prev-word-offset text)
                  (prev-word-offset text)
                  (prev-word-offset text)
                  (prev-word-offset text))))))
