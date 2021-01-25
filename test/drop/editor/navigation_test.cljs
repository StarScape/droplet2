(ns drop.editor.navigation-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.navigation :as nav :refer [next-char prev-char next-word prev-word next-word-offset prev-word-offset]]
            [drop.editor.core :as core]
            [drop.editor.selection :as sel :refer [caret selection]]))

(def test-str "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")
(def para (core/paragraph "p1" [(core/run test-str)]))
(def para2 (core/paragraph "p2" [(core/run "foo bar?")]))
(def doc (core/document [para, para2]))

(deftest start-test
  (testing "works for paragraphs"
    (is (= (selection ["p1" 0]) (nav/start para))))
  (testing "works for documents"
    (is (= (selection ["p1" 0]) (nav/start doc)))))

(deftest end-test
  (testing "works for paragraphs"
    (is (= (selection ["p1" (core/text-len para)]) (nav/end para))))
  (testing "works for documents"
    (is (= (selection ["p2" 8]) (nav/end doc)))))

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

(deftest next-char-paragraph-test
  (testing "works under normal circumstances"
    (is (= 1 (caret (next-char para (selection ["p1" 0])))))
    (is (= 80 (caret (next-char para (selection ["p1" 79]))))))

  (testing "returns original selection when at end of paragraph"
    (is (= 80 (caret (next-char para (selection ["p1" 80]))))))

  (testing "collapses to end if passed a range selection"
    (is (= 33 (caret (next-char para (selection ["p1" 1] ["p1" 33])))))))

(deftest prev-char-paragraph-test
  (testing "works under normal circumstances"
    (is (= 4 (caret (prev-char para (selection ["p1" 5])))))
    (is (= 0 (caret (prev-char para (selection ["p1" 1]))))))

  (testing "returns original selection when at start of paragraph"
    (is (= 0 (caret (prev-char para (selection ["p1" 0]))))))

  (testing "collapses to start if passed a range selection"
    (is (= 1 (caret (prev-char para (selection ["p1" 1] ["p1" 33])))))))

(deftest next-char-doc-test
  (testing "works under normal circumstances"
    (is (= 1 (caret (next-char doc (selection ["p1" 0]))))))

  (testing "returns start of next paragraph when at end of paragraph"
    (is (= (selection ["p2" 0]) (next-char doc (selection ["p1" (core/text-len para)])))))

  (testing "returns same selection when at end of LAST paragraph in document"
    (is (= (selection ["p2" 8]) (next-char doc (selection ["p2" 8])))))

  (testing "collapses to end when passed a range selection"
    (is (= (selection ["p1" 7]) (next-char doc (selection ["p1" 0] ["p1" 7]))))))

(deftest prev-char-doc-test
  (testing "works under normal circumstances"
    (is (= 1 (caret (prev-char doc (selection ["p1" 2]))))))

  (testing "returns end of previous paragraph when at start of paragraph"
    (is (= (selection ["p1" (core/text-len para)]) (prev-char doc (selection ["p2" 0])))))

  (testing "returns same selection when at start of FIRST paragraph in document"
    (is (= (selection ["p1" 0]) (prev-char doc (selection ["p1" 0])))))

  (testing "collapses to start when passed a range selection"
    (is (= (selection ["p1" 1]) (prev-char doc (selection ["p1" 1] ["p1" 7]))))))

(deftest shift+right
  ;; Forward selection
  (testing "works with single selection"
    (is (= (nav/shift+right doc (selection ["p1" 0]))
           (selection ["p1" 0] ["p1" 1] false))))

  (testing "works with forwards range selection"
    (is (= (nav/shift+right doc (selection ["p1" 1] ["p1" 2]))
           (selection ["p1" 1] ["p1" 3] false))))

  (testing "works with forwards range selection across paragraphs"
    (is (= (nav/shift+right doc (selection ["p1" 10] ["p1" (core/text-len para)]))
           (selection ["p1" 10] ["p2" 0]))))

  (testing "won't let you go past end of last paragraph"
    (is (= (nav/shift+right doc (selection ["p1" 10] ["p2" (core/text-len para2)]))
           (selection ["p1" 10] ["p2" (core/text-len para2)])))
    (is (= (nav/shift+right doc (selection ["p2" 0] ["p2" (core/text-len para2)]))
           (selection ["p2" 0] ["p2" (core/text-len para2)])))
    (is (= (nav/shift+right doc (selection ["p2" (core/text-len para2)]))
           (selection ["p2" (core/text-len para2)]))))

  (testing "works with selecting to end of para with forwards range selection"
    (is (= (nav/shift+right doc (selection ["p1" 0] ["p1" (dec (core/text-len para))]))
           (selection ["p1" 0] ["p1" (core/text-len para)] false))))

  ;; Backwards selection
  (testing "works with backwards range"
    (is (= (nav/shift+right doc (selection ["p1" 1] ["p1" 10] true))
           (selection ["p1" 2] ["p1" 10] true))))

  (testing "works with collapsing a backwards range selection once it becomes single again"
    (is (= (nav/shift+right doc (selection ["p1" 1] ["p1" 2] true))
           (selection ["p1" 2] ["p1" 2] false))))

  (testing "works across paragraphs"
    (is (= (nav/shift+right doc (selection ["p1" (core/text-len para)] ["p2" 5] true))
           (selection ["p2" 0] ["p2" 5] true)))
    (is (= (nav/shift+right doc (selection ["p1" 9] ["p2" 5] true))
           (selection ["p1" 10] ["p2" 5] true)))))

(deftest shift+left
  ;; Forward selection
  (testing "works with single selection"
    (is (= (nav/shift+left doc (selection ["p1" 2]))
           (selection ["p1" 1] ["p1" 2] true)))
    (is (= (nav/shift+left doc (selection ["p2" 0]))
           (selection ["p1" (core/text-len para)] ["p2" 0] true))))

  (testing "works with forwards range selection"
    (is (= (nav/shift+left doc (selection ["p1" 1] ["p1" 5]))
           (selection ["p1" 1] ["p1" 4]))))

  (testing "collapses a forward range selection correctly when it has a length of 1"
    (is (= (nav/shift+left doc (selection ["p1" 1] ["p1" 2]))
           (selection ["p1" 1]))))

  (testing "won't let you go past beginning of first paragraph"
    (is (= (nav/shift+left doc (selection ["p1" 0]))
           (selection ["p1" 0])))
    (is (= (nav/shift+left doc (selection ["p1" 0] ["p1" 10] true))
           (selection ["p1" 0] ["p1" 10] true))))

  (testing "works across paragraphs with forwards selection"
    (is (= (nav/shift+left doc (selection ["p1" 10] ["p2" 0]))
           (selection ["p1" 10] ["p1" (core/text-len para)])))
    (is (= (nav/shift+left doc (selection ["p1" 10] ["p2" 5]))
           (selection ["p1" 10] ["p2" 4]))))

  ;; Backwards selection
  (testing "works with backwards range selection"
    (is (= (nav/shift+left doc (selection ["p1" 1] ["p1" 10] true))
           (selection ["p1" 0] ["p1" 10] true))))

  (testing "works with backwards range selection across paragraphs"
    (is (= (nav/shift+left doc (selection ["p2" 0] ["p2" 5] true))
           (selection ["p1" (core/text-len para)] ["p2" 5] true)))
    (is (= (nav/shift+left doc (selection ["p1" 10] ["p2" 5] true))
           (selection ["p1" 9] ["p2" 5] true)))))

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
