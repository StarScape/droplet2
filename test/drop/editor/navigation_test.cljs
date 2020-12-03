(ns drop.editor.navigation-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.navigation :as nav :refer [next-word prev-word next-word-offset prev-word-offset]]
            [drop.editor.core :as core]
            [drop.editor.selection :as sel :refer [caret selection]]))

(def test-str "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")
(def para (core/paragraph "p1" [(core/run test-str)]))

(def doc (core/document [para, (core/paragraph "p2" [(core/run "foo bar?")])]))

(deftest next-word-paragraph-test
  (testing "starting from word-char goes to end of that word"
    (is (= 5 (caret (next-word para (selection ["p1" 0]))))))

  (testing "starting from last char in word goes only to end of word"
    (is (= 5 (caret (next-word para (selection ["-1" 4]))))))

  (testing "starting from space goes to the end of the next word"
    (is (= 11 (caret (next-word para (selection ["p1" 5]))))))

  (testing "starting from single separator goes forward 1 space"
    (is (= 12 (caret (next-word para (selection ["p1" 11]))))))

  (testing "skips over as much whitespace as possible to get to end of next word"
    (is (= 27 (caret (next-word para (selection ["p1" 18]))))))

  (testing "skips over multiple separators if starting from a separator"
    (is (= 47 (caret (next-word para (selection ["p1" 44]))))))

  (testing "skips to end of word when starting from a single punctuation mark inside of a word (e.g. a dash)"
    (is (= 80 (caret (next-word para (selection ["p1" 77]))))))

  (testing "goes to end of the paragraph if already in the last word/separator/whitespace-block"
    (is (= (count test-str) (caret (next-word para (selection ["p1" 78]))))))

  (testing "collapses first"
    (is (= 11 (caret (next-word para (selection ["p1" 0] ["p1" 5])))))))

(deftest next-word-document-test
  (testing "should return the same offset as the paragraph next-word for all offsets but the last"
    (is (->> (range (core/text-len para))
             (map (fn [offset]
                    (= (caret (next-word doc (selection ["p1" offset])))
                       (caret (next-word para (selection ["p1" offset]))))))
             (every? true?))))

  (testing "next-word from last offset in paragraph should go to first char of next paragraph"
    (is (= (selection ["p2" 0])
           (next-word doc (selection ["p1" (core/text-len para)])))))

  (testing "past end of paragraph should fail"
    (is (thrown? js/Error (next-word doc (selection ["p1" 200])))))

  (testing "next-word from last offset of last paragraph should return itself"
    (is (= (selection ["p2" 8]) (next-word doc (selection ["p2" 8]))))))

(deftest prev-word-paragraph-test
  (testing "starting from within word goes to first char of the word"
    (is (= 6 (caret (prev-word para (selection ["p1" 9]))))))

  (testing "starting from space directly after word goes to first char of word"
    (is (= 32 (caret (prev-word para (selection ["p1" 36]))))))

  (testing "starting from after space goes to the beginning of the previous word"
    (is (= 13 (caret (prev-word para (selection ["p1" 19]))))))

  (testing "starting from immediately after single separator with a word before it goes to word start"
    (is (= 11 (caret (prev-word para (selection ["p1" 12]))))))

  (testing "in hyphenated word, starting from the end goes back to the hyphen"
    (is (= 78 (caret (prev-word para (selection ["p1" 80]))))))

  (testing "in hyphenated word, starting from immediately after the hyphen goes back to beginning of previous word"
    (is (= 75 (caret (prev-word para (selection ["p1" 78]))))))

  (testing "in hypenated word, starting from first hyphen goes to beginning of word"
    (is (= 75 (caret (prev-word para (selection ["p1" 77]))))))

  (testing "skips over as much whitespace as possible to get to the start of the previous word"
    (is (= 13 (caret (prev-word para (selection ["p1" 21]))))))

  (testing "skips over mutliple separators if there is a separator before the starting position"
    (is (= 44 (caret (prev-word para (selection ["p1" 47]))))))

  (testing "goes to beginning of the paragraph if already in the last word/separator/whitespace-block"
    (is (= 0 (caret (prev-word para (selection ["p1" 4]))))))

  (testing "collapses first"
    (is (= 0 (caret (prev-word para (selection ["p1" 5] ["p1" 55] true)))))))

(deftest prev-word-document-test
  (testing "should return the same offset as the paragraph prev-word for all offsets but 0"
    (is (->> (range 1 (inc (core/text-len para)))
             (map (fn [offset]
                    (= (caret (prev-word doc (selection ["p1" offset])))
                       (caret (prev-word para (selection ["p1" offset]))))))
             (every? true?))))

  (testing "prev-word from first offset in paragraph should go to the last char of next paragraph"
    (is (= (selection ["p1" (core/text-len para)])
           (prev-word doc (selection ["p2" 0])))))

  (testing "past end of paragraph should fail"
    (is (thrown? js/Error (prev-word doc (selection ["p1" 200])))))

  (testing "prev-word from offset 0 of first paragraph should return itself"
    (is (= (selection ["p1" 0]) (prev-word doc (selection ["p1" 0]))))))

(deftest hyphen-back-and-forth-test
  (let [text "word1 a-very-long-hyphenated-word word2"]
    ;; Start at offset 5, jump right 5 times
    (is (= 33 (->> 5
                   (next-word-offset text)
                   (next-word-offset text)
                   (next-word-offset text)
                   (next-word-offset text)
                   (next-word-offset text))))

    ;; Start at offset 33, jump left 5 times
    (is (= 6 (->> 33
                  (prev-word-offset text)
                  (prev-word-offset text)
                  (prev-word-offset text)
                  (prev-word-offset text)
                  (prev-word-offset text))))))
