(ns slate.model.navigation-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.dll :as dll :refer [big-dec]]
            [slate.model.navigation :as nav :refer [next-char prev-char
                                                    next-word prev-word
                                                    next-word-offset prev-word-offset
                                                    next-clause-offset prev-clause-offset]]
            [slate.model.common :as m]
            [slate.model.run :as r]
            [slate.model.paragraph :as p]
            [slate.model.doc :as doc]
            [slate.model.selection :as sel :refer [caret selection]]))

(def test-str "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")
(def para (p/paragraph [(r/run test-str)]))
(def para2 (p/paragraph [(r/run "foo bar?")]))
(def para3 (p/paragraph [(r/run "bizz buzz")]))

(def doc (doc/document [para, para2]))
(def doc2 (doc/document [para, para2, para3]))

(deftest start-test
  (testing "works for documents"
    (is (= (selection [(big-dec 1) 0]) (nav/start doc)))))

(deftest end-test
  (testing "works for documents"
    (is (= (selection [(big-dec 2) 8]) (nav/end doc)))))

(deftest next-word-single-paragraph-test
  (testing "starting from word-char goes to end of that word"
    (is (= 5 (caret (next-word doc (selection [(big-dec 1) 0]))))))

  (testing "starting from last char in word goes only to end of word"
    (is (= 5 (caret (next-word doc (selection [(big-dec 1) 4]))))))

  (testing "starting from space goes to the end of the next word"
    (is (= 11 (caret (next-word doc (selection [(big-dec 1) 5]))))))

  (testing "starting from single separator goes forward 1 space"
    (is (= 12 (caret (next-word doc (selection [(big-dec 1) 11]))))))

  (testing "skips over as much whitespace as possible to get to end of next word"
    (is (= 27 (caret (next-word doc (selection [(big-dec 1) 18]))))))

  (testing "skips over multiple separators if starting from a separator"
    (is (= 47 (caret (next-word doc (selection [(big-dec 1) 44]))))))

  (testing "skips to end of word when starting from a single punctuation mark inside of a word (e.g. a dash)"
    (is (= 80 (caret (next-word doc (selection [(big-dec 1) 77]))))))

  (testing "goes to end of the paragraph if already in the last word/separator/whitespace-block"
    (is (= (count test-str) (caret (next-word doc (selection [(big-dec 1) 78]))))))

  (testing "collapses first"
    (is (= 11 (caret (next-word doc (selection [(big-dec 1) 0] [(big-dec 1) 5])))))))

(deftest next-word-document-test
  (testing "next-word from last offset in paragraph should go to first char of next paragraph"
    (is (= (selection [(big-dec 2) 0])
           (next-word doc (selection [(big-dec 1) (m/len para)])))))

  (testing "past end of paragraph should fail"
    (is (thrown? js/Error (next-word doc (selection [(big-dec 1) 200])))))

  (testing "next-word from last offset of last paragraph should return itself"
    (is (= (selection [(big-dec 2) 8]) (next-word doc (selection [(big-dec 2) 8]))))))

(deftest prev-word-test
  (testing "works when start of string is space"
    (is (= 0 (prev-word-offset " Foo bar " 1)))))

(deftest next-word-test
  (testing "works when end of string is space"
    (is (= 9 (next-word-offset " Foo bar " 8)))))

(deftest prev-word-single-paragraph-test
  (testing "starting from within word goes to first char of the word"
    (is (= 6 (caret (prev-word doc (selection [(big-dec 1) 9]))))))

  (testing "starting from space directly after word goes to first char of word"
    (is (= 32 (caret (prev-word doc (selection [(big-dec 1) 36]))))))

  (testing "starting from after space goes to the beginning of the previous word"
    (is (= 13 (caret (prev-word doc (selection [(big-dec 1) 19]))))))

  (testing "starting from immediately after single separator with a word before it goes to word start"
    (is (= 11 (caret (prev-word doc (selection [(big-dec 1) 12]))))))

  (testing "in hyphenated word, starting from the end goes back to the hyphen"
    (is (= 78 (caret (prev-word doc (selection [(big-dec 1) 80]))))))

  (testing "in hyphenated word, starting from immediately after the hyphen goes back to beginning of previous word"
    (is (= 75 (caret (prev-word doc (selection [(big-dec 1) 78]))))))

  (testing "in hypenated word, starting from first hyphen goes to beginning of word"
    (is (= 75 (caret (prev-word doc (selection [(big-dec 1) 77]))))))

  (testing "skips over as much whitespace as possible to get to the start of the previous word"
    (is (= 13 (caret (prev-word doc (selection [(big-dec 1) 21]))))))

  (testing "skips over mutliple separators if there is a separator before the starting position"
    (is (= 44 (caret (prev-word doc (selection [(big-dec 1) 47]))))))

  (testing "goes to beginning of the paragraph if already in the last word/separator/whitespace-block"
    (is (= 0 (caret (prev-word doc (selection [(big-dec 1) 4]))))))

  (testing "collapses first"
    (is (= 0 (caret (prev-word doc (selection [(big-dec 1) 5] [(big-dec 1) 55] :backwards? true)))))))

(deftest prev-word-document-test
  (testing "prev-word from first offset in paragraph should go to the last char of next paragraph"
    (is (= (selection [(big-dec 1) (m/len para)])
           (prev-word doc (selection [(big-dec 2) 0])))))

  (testing "past end of paragraph should fail"
    (is (thrown? js/Error (prev-word doc (selection [(big-dec 1) 200])))))

  (testing "prev-word from offset 0 of first paragraph should return itself"
    (is (= (selection [(big-dec 1) 0]) (prev-word doc (selection [(big-dec 1) 0]))))))

(deftest next-char-paragraph-test
  (testing "works under normal circumstances"
    (is (= 1 (caret (next-char doc (selection [(big-dec 1) 0])))))
    (is (= 80 (caret (next-char doc (selection [(big-dec 1) 79]))))))

  (testing "returns original selection when at end of last paragraph"
    (is (= 8 (caret (next-char doc (selection [(big-dec 2) 8]))))))

  (testing "collapses to end if passed a range selection"
    (is (= 33 (caret (next-char doc (selection [(big-dec 1) 1] [(big-dec 1) 33])))))))

(deftest prev-char-paragraph-test
  (testing "works under normal circumstances"
    (is (= 4 (caret (prev-char doc (selection [(big-dec 1) 5])))))
    (is (= 0 (caret (prev-char doc (selection [(big-dec 1) 1]))))))

  (testing "returns original selection when at start of 1st paragraph"
    (is (= 0 (caret (prev-char doc (selection [(big-dec 1) 0]))))))

  (testing "collapses to start if passed a range selection"
    (is (= 1 (caret (prev-char doc (selection [(big-dec 1) 1] [(big-dec 1) 33])))))))

(deftest next-char-doc-test
  (testing "works under normal circumstances"
    (is (= 1 (caret (next-char doc (selection [(big-dec 1) 0]))))))

  (testing "returns start of next paragraph when at end of paragraph"
    (is (= (selection [(big-dec 2) 0]) (next-char doc (selection [(big-dec 1) (m/len para)])))))

  (testing "returns same selection when at end of LAST paragraph in document"
    (is (= (selection [(big-dec 2) 8]) (next-char doc (selection [(big-dec 2) 8])))))

  (testing "collapses to end when passed a range selection"
    (is (= (selection [(big-dec 1) 7]) (next-char doc (selection [(big-dec 1) 0] [(big-dec 1) 7]))))))

(deftest prev-char-doc-test
  (testing "works under normal circumstances"
    (is (= 1 (caret (prev-char doc (selection [(big-dec 1) 2]))))))

  (testing "returns end of previous paragraph when at start of paragraph"
    (is (= (selection [(big-dec 1) (m/len para)]) (prev-char doc (selection [(big-dec 2) 0])))))

  (testing "returns same selection when at start of FIRST paragraph in document"
    (is (= (selection [(big-dec 1) 0]) (prev-char doc (selection [(big-dec 1) 0])))))

  (testing "collapses to start when passed a range selection"
    (is (= (selection [(big-dec 1) 1]) (prev-char doc (selection [(big-dec 1) 1] [(big-dec 1) 7]))))))

(deftest shift+right
  ;; Forward selection
  (testing "works with single selection"
    (is (= (nav/shift+right doc (selection [(big-dec 1) 0]))
           (selection [(big-dec 1) 0] [(big-dec 1) 1] :backwards? false))))

  (testing "works with forwards range selection"
    (is (= (nav/shift+right doc (selection [(big-dec 1) 1] [(big-dec 1) 2]))
           (selection [(big-dec 1) 1] [(big-dec 1) 3] :backwards? false))))

  (testing "works with forwards range selection across paragraphs"
    (is (= (nav/shift+right doc (selection [(big-dec 1) 10] [(big-dec 1) (m/len para)]))
           (selection [(big-dec 1) 10] [(big-dec 2) 0])))
    (is (= (nav/shift+right doc2 (selection [(big-dec 1) 10] [(big-dec 2) (m/len para2)]))
           (selection [(big-dec 1) 10] [(big-dec 3) 0]))))

  (testing "won't let you go past end of last paragraph"
    (is (= (nav/shift+right doc (selection [(big-dec 1) 10] [(big-dec 2) (m/len para2)]))
           (selection [(big-dec 1) 10] [(big-dec 2) (m/len para2)])))
    (is (= (nav/shift+right doc (selection [(big-dec 2) 0] [(big-dec 2) (m/len para2)]))
           (selection [(big-dec 2) 0] [(big-dec 2) (m/len para2)])))
    (is (= (nav/shift+right doc (selection [(big-dec 2) (m/len para2)]))
           (selection [(big-dec 2) (m/len para2)]))))

  (testing "works with selecting to end of para with forwards range selection"
    (is (= (nav/shift+right doc (selection [(big-dec 1) 0] [(big-dec 1) (dec (m/len para))]))
           (selection [(big-dec 1) 0] [(big-dec 1) (m/len para)] :backwards? false))))

  ;; Backwards selection
  (testing "works with backwards range"
    (is (= (nav/shift+right doc (selection [(big-dec 1) 1] [(big-dec 1) 10] :backwards? true))
           (selection [(big-dec 1) 2] [(big-dec 1) 10] :backwards? true))))

  (testing "works with collapsing a backwards range selection once it becomes single again"
    (is (= (nav/shift+right doc (selection [(big-dec 1) 1] [(big-dec 1) 2] :backwards? true))
           (selection [(big-dec 1) 2] [(big-dec 1) 2] :backwards? false))))

  (testing "works with backwards range selections across paragraphs"
    (is (= (nav/shift+right doc (selection [(big-dec 1) (m/len para)] [(big-dec 2) 5] :backwards? true))
           (selection [(big-dec 2) 0] [(big-dec 2) 5] :backwards? true)))
    (is (= (nav/shift+right doc2 (selection [(big-dec 1) (m/len para)] [(big-dec 3) 5], :backwards? true))
           (selection [(big-dec 2) 0] [(big-dec 3) 5] :backwards? true)))
    (is (= (nav/shift+right doc (selection [(big-dec 1) 9] [(big-dec 2) 5] :backwards? true))
           (selection [(big-dec 1) 10] [(big-dec 2) 5] :backwards? true)))))

(deftest ctrl+shift+right
  (testing "works forwards"
    (is (= (nav/ctrl+shift+right doc2 (selection [(big-dec 1) 0]))
           (selection [(big-dec 1) 0] [(big-dec 1) 5])))
    (is (= (nav/ctrl+shift+right doc2 (selection [(big-dec 1) 0] [(big-dec 2) (m/len para2)]))
           (selection [(big-dec 1) 0] [(big-dec 3) 0]))))
  (testing "works backwards"
    (is (= (nav/ctrl+shift+right doc2 (selection [(big-dec 1) 0] [(big-dec 3) 0]
                                                 :backwards? true))
           (selection [(big-dec 1) 5] [(big-dec 3) 0]
                      :backwards? true)))
    (is (= (nav/ctrl+shift+right doc2 (selection [(big-dec 1) (m/len para)] [(big-dec 3) 0]
                                                 :backwards? true))
           (selection [(big-dec 2) 0] [(big-dec 3) 0] :backwards? true)))
    (is (= (nav/ctrl+shift+right doc2 (selection [(big-dec 1) 0] [(big-dec 1) 2] :backwards? true))
           (selection [(big-dec 1) 2] [(big-dec 1) 5] :backwards? false)))))

(deftest shift+left
  ;; Forward selection
  (testing "works with single selection"
    (is (= (nav/shift+left doc (selection [(big-dec 1) 2]))
           (selection [(big-dec 1) 1] [(big-dec 1) 2] :backwards? true)))
    (is (= (nav/shift+left doc (selection [(big-dec 2) 0]))
           (selection [(big-dec 1) (m/len para)] [(big-dec 2) 0] :backwards? true))))

  (testing "works with forwards range selection"
    (is (= (nav/shift+left doc (selection [(big-dec 1) 1] [(big-dec 1) 5]))
           (selection [(big-dec 1) 1] [(big-dec 1) 4]))))

  (testing "collapses a forward range selection correctly when it has a length of 1"
    (is (= (nav/shift+left doc (selection [(big-dec 1) 1] [(big-dec 1) 2]))
           (selection [(big-dec 1) 1]))))

  (testing "won't let you go past beginning of first paragraph"
    (is (= (nav/shift+left doc (selection [(big-dec 1) 0]))
           (selection [(big-dec 1) 0])))
    (is (= (nav/shift+left doc (selection [(big-dec 1) 0] [(big-dec 1) 10] :backwards? true))
           (selection [(big-dec 1) 0] [(big-dec 1) 10] :backwards? true))))

  (testing "works across paragraphs with forwards selection"
    (is (= (nav/shift+left doc (selection [(big-dec 1) 10] [(big-dec 2) 0]))
           (selection [(big-dec 1) 10] [(big-dec 1) (m/len para)])))
    (is (= (nav/shift+left doc (selection [(big-dec 1) 10] [(big-dec 2) 5]))
           (selection [(big-dec 1) 10] [(big-dec 2) 4])))
    (is (= (nav/shift+left doc2 (selection [(big-dec 1) 0] [(big-dec 3) 0]))
           (selection [(big-dec 1) 0] [(big-dec 2) (m/len para2)]))))

  ;; Backwards selection
  (testing "works with backwards range selection"
    (is (= (nav/shift+left doc (selection [(big-dec 1) 1] [(big-dec 1) 10] :backwards? true))
           (selection [(big-dec 1) 0] [(big-dec 1) 10] :backwards? true))))

  (testing "works with backwards range selection across paragraphs"
    (is (= (nav/shift+left doc (selection [(big-dec 2) 0] [(big-dec 2) 5] :backwards? true))
           (selection [(big-dec 1) (m/len para)] [(big-dec 2) 5] :backwards? true)))
    (is (= (nav/shift+left doc (selection [(big-dec 1) 10] [(big-dec 2) 5] :backwards? true))
           (selection [(big-dec 1) 9] [(big-dec 2) 5] :backwards? true)))
    (is (= (nav/shift+left doc2 (selection [(big-dec 2) 0] [(big-dec 3) 5] :backwards? true))
           (selection [(big-dec 1) 80] [(big-dec 3) 5] :backwards? true)))))

(deftest ctrl+shift+left
  (testing "works forwards"
    (is (= (nav/ctrl+shift+left doc2 (selection [(big-dec 1) 0] [(big-dec 3) 0]))
           (selection [(big-dec 1) 0] [(big-dec 2) (m/len para2)])))
    (is (= (nav/ctrl+shift+left doc2 (selection [(big-dec 1) 1] [(big-dec 1) 5]))
           (selection [(big-dec 1) 0] [(big-dec 1) 1] :backwards? true))))
  (testing "works backwards or as single selection"
    (is (= (nav/ctrl+shift+left doc2 (selection [(big-dec 1) 5]))
           (selection [(big-dec 1) 0] [(big-dec 1) 5] :backwards? true)))
    (is (= (nav/ctrl+shift+left doc2 (selection [(big-dec 1) 5] [(big-dec 3) 0]
                                                :backwards? true))
           (selection [(big-dec 1) 0] [(big-dec 3) 0]
                      :backwards? true)))
    (is (= (nav/ctrl+shift+left doc2 (selection [(big-dec 2) 0] [(big-dec 3) 5]
                                                :backwards? true))
           (selection [(big-dec 1) (m/len para)] [(big-dec 3) 5]
                      :backwards? true)))
    (is (= (nav/ctrl+shift+left doc2 (selection [(big-dec 2) 2] [(big-dec 3) 5] :backwards? true))
           (selection [(big-dec 2) 0] [(big-dec 3) 5] :backwards? true)))))

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


(def clause-offset-test-string "Hello! This is a sentence, which you, being a person of dignity and grace, might logically ask, 'What kind of sentence?' Any kind, really")
(def clause-offset-test-string2 " Hello...what seems to be the issue here?")

(deftest next-clause-test
  (testing "works for all edge cases"
    (is (= 6 (next-clause-offset clause-offset-test-string 0))) ; |Hello! -> Hello!|
    (is (= 6 (next-clause-offset clause-offset-test-string 1))) ; H|ello! -> Hello!|
    (is (= 6 (next-clause-offset clause-offset-test-string 5))) ; Hello|! -> Hello!|
    (is (= 26 (next-clause-offset clause-offset-test-string 6))) ; Hello!| -> Hello! This is a sentence,|
    (is (= 37 (next-clause-offset clause-offset-test-string 26))) ; Hello! This is a sentence,| -> This is a sentence, which you,|
    (is (= 137 (next-clause-offset clause-offset-test-string 130))) ; Any kind,| really -> Any kid, really|
    (is (= 137 (next-clause-offset clause-offset-test-string 131))) ; Any kind, |really -> Any kid, really|
    (is (= 137 (next-clause-offset clause-offset-test-string 137))) ; Any kind, really| -> Any kind, really|

    (is (= 9 (next-clause-offset clause-offset-test-string2 0))) ; | Hello... ->  Hello...|
    (is (= 9 (next-clause-offset clause-offset-test-string2 1))) ;  |Hello... ->  Hello...|
    (is (= 9 (next-clause-offset clause-offset-test-string2 2))) ;  H|ello... ->  Hello...|
    (is (= 9 (next-clause-offset clause-offset-test-string2 6))) ;  Hello|... ->  Hello...|
    (is (= 9 (next-clause-offset clause-offset-test-string2 7))) ;  Hello|... ->  Hello...|
    (is (= 41 (next-clause-offset clause-offset-test-string2 9))) ;  Hello...| ->  Hello...what seems to be the issue here?|
    ))

(deftest prev-clause-test
  (testing "works for all edge cases"
  ;; testing with -offset version of function for simplicity's sake
    (is (= 0 (prev-clause-offset clause-offset-test-string 0))) ; |Hello! -> |Hello!
    (is (= 0 (prev-clause-offset clause-offset-test-string 3))) ; Hel|llo! -> |Hello!
    (is (= 0 (prev-clause-offset clause-offset-test-string 4))) ; Hell|o! -> |Hello!
    (is (= 0 (prev-clause-offset clause-offset-test-string 5))) ; Hello|! -> |Hello!
    (is (= 0 (prev-clause-offset clause-offset-test-string 6))) ; Hello!| -> |Hello!
    (is (= 6 (prev-clause-offset clause-offset-test-string 7))) ; Hello! | -> Hello!|
    (is (= 7 (prev-clause-offset clause-offset-test-string 15))) ; Hello! This is |a sentence, -> Hello! |This is a sentence,
    (is (= 27 (prev-clause-offset clause-offset-test-string 33))) ; This is a sentence, which |you, -> This is a sentence, |which you,
    (is (= 96 (prev-clause-offset clause-offset-test-string 103))) ; might logically ask, 'What k|ind of sentence?' -> might logically ask, |'What kind of sentence?'
    (is (= 131 (prev-clause-offset clause-offset-test-string 137))) ; Any kind, really| -> Any kind, |really

    (is (= 9 (prev-clause-offset clause-offset-test-string2 41))) ;  Hello...what seems to be the issue here?| ->  Hello...|what seems to be the issue here?
    (is (= 9 (prev-clause-offset clause-offset-test-string2 40))) ;  Hello...what seems to be the issue here|? ->  Hello...|what seems to be the issue here?
    (is (= 9 (prev-clause-offset clause-offset-test-string2 30))) ;  Hello...what seems to be the |issue here? ->  Hello...|what seems to be the issue here?
    (is (= 9 (prev-clause-offset clause-offset-test-string2 10))) ;  Hello...w|hat seems to be the |issue here? ->  Hello...|what seems to be the issue here?
    (is (= 0 (prev-clause-offset clause-offset-test-string2 9))) ;  Hello...|what seems to be the |issue here? ->  |Hello...what seems to be the issue here?
    (is (= 0 (prev-clause-offset clause-offset-test-string2 8))) ;  Hello..|.what seems to be the |issue here? ->  |Hello...what seems to be the issue here?
    (is (= 0 (prev-clause-offset clause-offset-test-string2 4))) ;  Hel|lo...what seems to be the |issue here? ->  |Hello...what seems to be the issue here?
  ))
