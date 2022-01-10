(ns slate.model.document-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.common :as sl]
            [slate.model.run :as r :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]
            [slate.dll :as dll :refer [dll]]))

(def p1 (paragraph "p1" [(run "foo" #{:italic})
                         (run "bar" #{:bold :italic})
                         (run "bizz" #{:italic})
                         (run "buzz" #{:bold})]))

(def p2 (paragraph "p2" [(run "aaa" #{})
                         (run "bbb" #{})
                         (run "ccc" #{})
                         (run "ddd" #{})]))

(def to-insert [(paragraph "i1" [(run "inserted paragraph 1")])
                (paragraph "i2" [(run "inserted paragraph 2")])
                (paragraph "i3" [(run "inserted paragraph 3")])])

(def doc (document [p1 p2]))

(def long-doc (document [(paragraph "d1" [(run "foo1" #{:italic})])
                         (paragraph "d2" [(run "foo2" #{:bold})])
                         (paragraph "d3" [(run "foo3" #{:underline})])
                         (paragraph "d4" [(run "foo4" #{:strike})])]))

(deftest insert-test
  (testing "insert 2 runs in middle of a paragraph"
    (is (= (sl/insert doc (selection ["p1" 3]) [(run "Hello" #{:italic}) (run "Goodbye!")])
           (document [(paragraph "p1" [(run "fooHello" #{:italic})
                                       (run "Goodbye!" #{})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "insert single run in middle of a paragraph"
    (is (= (sl/insert doc (selection ["p1" 3]) (run "Goodbye!"))
           (document [(paragraph "p1" [(run "foo" #{:italic})
                                       (run "Goodbye!" #{})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "insert run at start of paragraph"
    (is (= (sl/insert doc (selection ["p1" 0]) (run "Hello!"))
           (document [(paragraph "p1" [(run "Hello!" #{})
                                       (run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "insert run at end of paragraph"
    (is (= (sl/insert doc (selection ["p2" 12]) (run "Goodbye!" #{:italic}))
           (document [p1, (paragraph "p2" [(run "aaabbbcccddd") (run "Goodbye!" #{:italic})])]))))

  (testing "multi-paragraph insert in the middle of a single paragraph"
    (is (= (sl/insert doc (selection ["p1" 10]) to-insert)
           (sl/insert doc (selection ["p1" 10]) (into (dll) to-insert))
           (document [(paragraph "p1" [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "inserted paragraph 1")])
                      (paragraph "i2" [(run "inserted paragraph 2")])
                      (paragraph "i3" [(run "inserted paragraph 3")
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "multi-paragraph insert at the start of a paragraph"
    (is (= (sl/insert doc (selection ["p2" 0]) to-insert)
           (sl/insert doc (selection ["p2" 0]) (into (dll) to-insert))
           (document [p1
                      (paragraph "p2" [(run "inserted paragraph 1")])
                      (paragraph "i2" [(run "inserted paragraph 2")])
                      (paragraph "i3" [(run "inserted paragraph 3aaabbbcccddd")])]))))

  (testing "multi-paragraph insert at the end of a paragraph"
    (is (= (sl/insert doc (selection ["p1" 14]) to-insert)
           (sl/insert doc (selection ["p1" 14]) (into (dll) to-insert))
           (document [(paragraph "p1" [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})
                                       (run "inserted paragraph 1")])
                      (paragraph "i2" [(run "inserted paragraph 2")])
                      (paragraph "i3" [(run "inserted paragraph 3")])
                      p2]))))

  (testing "inserting a plain string"
    (is (= (sl/insert doc (selection ["p1" 3]) "inserted")
           (document [(paragraph "p1" [(run "foo" #{:italic})
                                       (run "inserted")
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "when given a range-selection, deletes before inserting"
    (is (= (sl/insert doc (selection ["p1" 1] ["p2" 11]) (run "(inserted!)" #{}))
           (document [(paragraph "p1" [(run "f" #{:italic}), (run "(inserted!)d")])]))))

  (testing "throws when out of range of paragraph"
    (is (thrown?
         js/Error
         (sl/insert doc (selection ["p1" 55]) (run "Goodbye!" #{:italic}))))))

(deftest delete-single-test
  (testing "does nothing at beginning of doc"
    (is (= (sl/delete doc (selection ["p1" 0])) doc)))

  (testing "deletes single char in middle of paragraph"
    (is (= (sl/delete doc (selection ["p1" 1]))
           (document [(paragraph "p1" [(run "oo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "deletes single char at end of paragraph"
    (is (= (sl/delete doc (selection ["p1" 14]))
           (document [(paragraph "p1" [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buz" #{:bold})])
                      p2]))))

  (testing "merges paragraphs when backspacing from start of paragraph that is not first"
    (is (= (sl/delete doc (selection ["p2" 0]))
           (document [(paragraph "p1" (concat (:runs p1) (:runs p2)))]))))

  (testing "deletes single char as normal at end of the paragraph"
    (is (= (sl/delete doc (selection ["p2" 12]))
           (document [p1, (paragraph "p2" [(run "aaabbbcccdd")])]))))

  (testing "does nothing when backspacing at start of first paragraph"
    (is (= (sl/delete doc (selection ["p1" 0])) doc))))


;; TODO: weirdly, a test seems to be failing below because delete is returning a plain map instead of a Document
;; {:children #DoublyLinkedList [(#slate.model.paragraph.Paragraph{:uuid "p1", :runs [#slate.model.run.Run{:text "bar", :formats #{:bold :italic}} #slate.model.run.Run{:text "bizz", :formats #{:italic}} #slate.model.run.Run{:text "buzz", :formats #{:bold}}]} #slate.model.paragraph.Paragraph{:uuid "p2", :runs [#slate.model.run.Run{:text "aaabbbcccddd", :formats #{}}]})]}
;; {:children #DoublyLinkedList [(#slate.model.paragraph.Paragraph{:uuid "p1", :runs [#slate.model.run.Run{:text "bar", :formats #{:bold :italic}} #slate.model.run.Run{:text "bizz", :formats #{:italic}} #slate.model.run.Run{:text "buzz", :formats #{:bold}}]} #slate.model.paragraph.Paragraph{:uuid "p2", :runs [#slate.model.run.Run{:text "aaabbbcccddd", :formats #{}}]})]}

(deftest delete-range-test
  (testing "deletes from start of paragraph"
    (is (= (sl/delete doc (selection ["p1" 0] ["p1" 3]))
           (document [(paragraph "p1" [(run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "deletes from start of paragraph backwards"
    (is (= (sl/delete doc (selection ["p1" 0] ["p1" 3] :backwards? true))
           (document [(paragraph "p1" [(run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "deletes up to end of paragraph"
    (is (= (sl/delete doc (selection ["p1" 3] ["p1" 14]))
           (document [(paragraph "p1" [(run "foo" #{:italic})]), p2]))))

  (testing "deletes whole paragraph"
    ;; This is an odd edge case, but handling it this way makes the code simpler.
    ;; The reason it's like this is because the code merges the paragraph at the end
    ;; of the range selection with the paragraph at the beginning of the range selection,
    ;; and gives it the UUID of the first.
    (is (= (sl/delete doc (selection ["p1" 0] ["p2" 0]))
           (document [(assoc p2 :uuid "p1")]))))

  (testing "merges start and ending paragraphs when deleting across paragraphs"
    (is (= (sl/delete doc (selection ["p1" 3] ["p2" 3]))
           (document [(paragraph "p1" [(run "foo" #{:italic}), (run "bbbcccddd")])]))))

  (testing "merges start and ending paragraphs when deleting across more than 2 paragraphs"
    (is (= (sl/delete long-doc (selection ["d1" 4] ["d4" 0]))
           (document [(paragraph "d1" [(run "foo1" #{:italic}), (run "foo4" #{:strike})])]))))

  (testing "deletes whole document"
    (is (= (sl/delete doc (selection ["p1" 0] ["p2" 12]))
           (document [(paragraph "p1" [(run)])])))))

(deftest enter-test
  (testing "works at start of paragraph"
    (is (= (doc/enter doc (selection ["p1" 0]) "e1")
           (document [(paragraph "e1" [(run)]), p1, p2]))))

  (testing "works at end of paragraph"
    (is (= (doc/enter doc (selection ["p1" 14]) "e1")
           (document [p1, (paragraph "e1" [(run)]), p2]))))

  (testing "works in middle of paragraph"
    (is (= (doc/enter doc (selection ["p1" 3]) "e1")
           (document [(paragraph "p1" [(run "foo" #{:italic})])
                      (paragraph "e1" [(run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "works at end of doc"
    (is (= (doc/enter doc (selection ["p2" 12]) "e1")
           (document [p1, p2, (paragraph "e1" [(run)])]))))

  (testing "works with range selection"
    (is (= (doc/enter doc (selection ["p2" 0] ["p2" 12]) "e1")
           (document [p1, (p/empty-paragraph "e1"), (p/empty-paragraph "p2")])))))

(deftest selected-content-test
  (testing "returns list of runs when passed selection within one paragraph"
    (is (= [(run "bar" #{:bold :italic})
            (run "bizz" #{:italic})
            (run "buzz" #{:bold})]
           (sl/selected-content doc (selection ["p1" 3] ["p1" 14])))))

  (testing "returns list of paragraphs when passed selection across multiple paragraphs"
    (is (= [(paragraph "p1" [(run "bar" #{:bold :italic})
                             (run "bizz" #{:italic})
                             (run "buzz" #{:bold})])
            (paragraph "p2" [(run "aaa")])]
           (sl/selected-content doc (selection ["p1" 3] ["p2" 3])))))

  (testing "returns list of paragraphs when passed selection across multiple (> 3) paragraphs"
    (is (= [(paragraph "d1" [(run "foo1" #{:italic})])
            (paragraph "d2" [(run "foo2" #{:bold})])
            (paragraph "d3" [(run "foo3" #{:underline})])
            (paragraph "d4" [(run "foo" #{:strike})])]
           (sl/selected-content long-doc (selection ["d1" 0] ["d4" 3])))))

  ;; TODO: I **think** this is the correct implementation here...could be wrong though...
  (testing "returns one paragraph and empty next paragraph when going from start of paragraph 1 to start of paragraph 2"
    (is (= [(paragraph "d1" [(run "foo1" #{:italic})]) (paragraph "d2" [])]
           (sl/selected-content long-doc (selection ["d1" 0] ["d2" 0]))))))

(deftest shared-formats-test
  (let [formats-doc (document [(paragraph "f1" [(run "foo1" #{:italic})
                                                (run "foo2" #{:italic :bold})
                                                (run "foo3" #{:bold})])
                               (paragraph "f2" [(run "bar1" #{:italic :bold :underline})])])]
    (testing "works inside same paragraph"
      (is (= #{:italic} (sl/shared-formats formats-doc (selection ["f1" 0] ["f1" 8]))))
      (is (= #{:italic :bold} (sl/shared-formats formats-doc (selection ["f1" 4] ["f1" 8]))))
      (is (= #{:bold} (sl/shared-formats formats-doc (selection ["f1" 4] ["f1" 12])))))

    (testing "works across paragraphs"
      (is (= #{:bold} (sl/shared-formats formats-doc (selection ["f1" 8] ["f2" 3])))))))

;; TODO NEXT: fix toggle-format tests
;; TODO after that: get new transaction system rendering properly/fix any errors it's caused with view and event handling layers.

(deftest toggle-format-test
  (testing "toggling single run"
    (is (= (sl/toggle-format doc (selection ["p1" 0] ["p1" 3]) :italic)
           (document [(paragraph "p1" [(run "foo")
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "toggling across runs WITH shared format"
    (is (= (sl/toggle-format doc (selection ["p1" 0] ["p1" 10]) :italic)
           (document [(paragraph "p1" [(run "foo")
                                       (run "bar" #{:bold})
                                       (run "bizz" #{})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "toggling across runs WITH shared format, not on run boundaries"
    (is (= (sl/toggle-format doc (selection ["p1" 1] ["p1" 8]) :italic)
           (document [(paragraph "p1" [(run "f" #{:italic})
                                       (run "oo")
                                       (run "bar" #{:bold})
                                       (run "bi" #{})
                                       (run "zz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "toggling across runs WITHOUT shared format"
    (is (= (sl/toggle-format doc (selection ["p1" 0] ["p1" 14]) :italic)
           (document [(paragraph "p1" [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold :italic})])
                      p2]))))

  (testing "toggling across paragraphs WITHOUT shared format"
    (is (= (sl/toggle-format doc (selection ["p1" 0] ["p2" 12]) :italic)
           (document [(paragraph "p1" [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold :italic})])
                      (paragraph "p2" [(run "aaabbbcccddd" #{:italic})])]))))

  (testing "toggling across paragraphs WITHOUT shared format, and not landing on run boundaries"
    (is (= (sl/toggle-format doc (selection ["p1" 1] ["p2" 3]) :italic)
           (document [(paragraph "p1" [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold :italic})])
                      (paragraph "p2" [(run "aaa" #{:italic})
                                       (run "bbbcccddd")])]))))

  (testing "toggling across paragraphs WITH shared format"
    (let [modified (-> doc
                       (update-in [:children "p1" :runs 3 :formats] conj :italic)
                       (update-in [:children "p2" :runs 0 :formats] conj :italic))]
      (is (= (sl/toggle-format modified (selection ["p1" 10] ["p2" 12]) :italic)
             (document [(paragraph "p1" [(run "foo" #{:italic})
                                         (run "bar" #{:bold :italic})
                                         (run "bizz" #{:italic})
                                         (run "buzz" #{:bold})])
                        (paragraph "p2" [(run "aaabbbcccddd")])]))))))

(deftest char-at-test
  (testing "works in 1st paragraph"
    (is (= "f" (sl/char-at doc (selection ["p1" 0]))))
    (is (= "o" (sl/char-at doc (selection ["p1" 1]))))
    (is (= "z" (sl/char-at doc (selection ["p1" 13]))))
    (is (thrown? js/Error (sl/char-at doc (selection ["p1" 14])))))

  (testing "works in other paragraphs"
    (is (= "a" (sl/char-at doc (selection ["p2" 0]))))
    (is (= "b" (sl/char-at doc (selection ["p2" 3]))))
    (is (= "c" (sl/char-at doc (selection ["p2" 7]))))
    (is (= "d" (sl/char-at doc (selection ["p2" 11]))))
    (is (thrown? js/Error (sl/char-at doc (selection ["p2" 12]))))))

(deftest char-before-test
  (testing "works in 1st paragraph"
    (is (= "\n" (sl/char-before doc (selection ["p1" 0]))))
    (is (= "f" (sl/char-before doc (selection ["p1" 1]))))
    (is (= "o" (sl/char-before doc (selection ["p1" 2]))))
    (is (= "z" (sl/char-before doc (selection ["p1" 13]))))
    (is (= "z" (sl/char-before doc (selection ["p1" 14])))))

  (testing "works in other paragraphs"
    (is (= "\n" (sl/char-before doc (selection ["p2" 0]))))
    (is (= "a" (sl/char-before doc (selection ["p2" 1]))))
    (is (= "a" (sl/char-before doc (selection ["p2" 3]))))
    (is (= "b" (sl/char-before doc (selection ["p2" 4]))))
    (is (= "c" (sl/char-before doc (selection ["p2" 7]))))
    (is (= "d" (sl/char-before doc (selection ["p2" 11]))))
    (is (= "d" (sl/char-before doc (selection ["p2" 12]))))
    (is (thrown? js/Error (sl/char-before doc (selection ["[2]" 13]))))))
