(ns slate.model.doc-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.dll :as dll :refer [dll big-dec]]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.common :as sl]
            [slate.model.run :as r :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]))

(def p1 (paragraph [(run "foo" #{:italic})
                    (run "bar" #{:bold :italic})
                    (run "bizz" #{:italic})
                    (run "buzz" #{:bold})]))

(def p2 (paragraph [(run "aaa" #{})
                    (run "bbb" #{})
                    (run "ccc" #{})
                    (run "ddd" #{})]))

(def to-insert (doc/fragment [(paragraph [(run "inserted paragraph 1")])
                              (paragraph [(run "inserted paragraph 2")])
                              (paragraph [(run "inserted paragraph 3")])]))

(def test-doc (document [p1 p2]))

(def long-doc (document [(paragraph [(run "foo1" #{:italic})])
                         (paragraph [(run "foo2" #{:bold})])
                         (paragraph [(run "foo3" #{:underline})])
                         (paragraph [(run "foo4" #{:strike})])]))

(deftest insert-test
  (testing "insert 2 runs in middle of a paragraph"
    (is (= (doc/insert test-doc (selection [(big-dec 1) 3]) (p/fragment [(run "Hello" #{:italic}) (run "Goodbye!")]))
           (document [(paragraph [(run "fooHello" #{:italic})
                                  (run "Goodbye!" #{})
                                  (run "bar" #{:bold :italic})
                                  (run "bizz" #{:italic})
                                  (run "buzz" #{:bold})])
                      p2]))))

  (testing "insert single run in middle of a paragraph"
    (is (= (doc/insert test-doc (selection [(big-dec 1) 3]) (run "Goodbye!"))
           (document [(paragraph [(run "foo" #{:italic})
                                  (run "Goodbye!" #{})
                                  (run "bar" #{:bold :italic})
                                  (run "bizz" #{:italic})
                                  (run "buzz" #{:bold})])
                      p2]))))

  (testing "insert run at start of paragraph"
    (is (= (doc/insert test-doc (selection [(big-dec 1) 0]) (run "Hello!"))
           (document [(paragraph [(run "Hello!" #{})
                                  (run "foo" #{:italic})
                                  (run "bar" #{:bold :italic})
                                  (run "bizz" #{:italic})
                                  (run "buzz" #{:bold})])
                      p2]))))

  (testing "insert run at end of paragraph"
    (is (= (doc/insert test-doc (selection [(big-dec 2) 12]) (run "Goodbye!" #{:italic}))
           (document [p1, (paragraph [(run "aaabbbcccddd") (run "Goodbye!" #{:italic})])]))))

  (testing "multi-paragraph insert in the middle of a single paragraph"
    (is (= (doc/insert test-doc (selection [(big-dec 1) 10]) to-insert)
           (document [(paragraph [(run "foo" #{:italic})
                                  (run "bar" #{:bold :italic})
                                  (run "bizz" #{:italic})
                                  (run "inserted paragraph 1")])
                      (paragraph [(run "inserted paragraph 2")])
                      (paragraph [(run "inserted paragraph 3")
                                  (run "buzz" #{:bold})])
                      p2]))))

  (testing "multi-paragraph insert at the start of a paragraph"
    (is (= (doc/insert test-doc (selection [(big-dec 2) 0]) to-insert)
           (document [p1
                      (paragraph [(run "inserted paragraph 1")])
                      (paragraph [(run "inserted paragraph 2")])
                      (paragraph [(run "inserted paragraph 3aaabbbcccddd")])]))))

  (testing "multi-paragraph insert"
    (let [result (doc/insert test-doc (selection [(big-dec 2) 0]) (doc/fragment [p1 p2]))
          children (:children result)]
      (is (= 3 (count children)))
      (is (= p1 (first children)))
      (is (= (:runs (second children)) (:runs p1)))
      (is (= (:runs (nth children 2)) [(run "aaabbbcccdddaaabbbcccddd")]))))

  (testing "multi-paragraph insert at the end of a paragraph"
    (is (= (doc/insert test-doc (selection [(big-dec 1) 14]) to-insert)
           (document [(paragraph [(run "foo" #{:italic})
                                  (run "bar" #{:bold :italic})
                                  (run "bizz" #{:italic})
                                  (run "buzz" #{:bold})
                                  (run "inserted paragraph 1")])
                      (paragraph [(run "inserted paragraph 2")])
                      (paragraph [(run "inserted paragraph 3")])
                      p2]))))

  (testing "inserting a plain string"
    (is (= (doc/insert test-doc (selection [(big-dec 1) 3]) "inserted")
           (document [(paragraph [(run "foo" #{:italic})
                                       (run "inserted")
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "inserting a plain string when selection has a format"
    (is (= (doc/insert test-doc (selection [(big-dec 1) 3] [(big-dec 1) 3] :formats #{:underline}) "inserted")
           (document [(paragraph [(run "foo" #{:italic})
                                       (run "inserted" #{:underline})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "when given a range selection, deletes before inserting"
    (is (= (doc/insert test-doc (selection [(big-dec 1) 1] [(big-dec 2) 11]) (run "(inserted!)" #{}))
           (document [(paragraph [(run "f" #{:italic}), (run "(inserted!)d")])]))))

  (testing "throws when out of range of paragraph"
    (is (thrown?
         js/Error
         (doc/insert test-doc (selection [(big-dec 1) 55]) (run "Goodbye!" #{:italic}))))))

(deftest delete-single-test
  (testing "does nothing at beginning of doc"
    (is (= (doc/delete test-doc (selection [(big-dec 1) 0])) test-doc)))

  (testing "deletes single char in middle of paragraph"
    (is (= (doc/delete test-doc (selection [(big-dec 1) 1]))
           (document [(paragraph [(run "oo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "deletes single char at end of paragraph"
    (is (= (doc/delete test-doc (selection [(big-dec 1) 14]))
           (document [(paragraph [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buz" #{:bold})])
                      p2]))))

  (testing "merges paragraphs when backspacing from start of paragraph that is not first"
    (is (= (doc/delete test-doc (selection [(big-dec 2) 0]))
           (document [(paragraph (concat (:runs p1) (:runs p2)))]))))

  (testing "deletes single char as normal at end of the paragraph"
    (is (= (doc/delete test-doc (selection [(big-dec 2) 12]))
           (document [p1, (paragraph [(run "aaabbbcccdd")])]))))

  (testing "does nothing when backspacing at start of first paragraph"
    (is (= (doc/delete test-doc (selection [(big-dec 1) 0])) test-doc))))

(deftest delete-range-test
  (testing "deletes from start of paragraph"
    (is (= (doc/delete test-doc (selection [(big-dec 1) 0] [(big-dec 1) 3]))
           (document [(paragraph [(run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "deletes from start of paragraph backwards"
    (is (= (doc/delete test-doc (selection [(big-dec 1) 0] [(big-dec 1) 3] :backwards? true))
           (document [(paragraph [(run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "deletes up to end of paragraph"
    (is (= (doc/delete test-doc (selection [(big-dec 1) 3] [(big-dec 1) 14]))
           (document [(paragraph [(run "foo" #{:italic})]), p2]))))

  (testing "deletes whole paragraph"
    ;; This is an odd edge case, but handling it this way makes the code simpler.
    ;; The reason it's like this is because the code merges the paragraph at the end
    ;; of the range selection with the paragraph at the beginning of the range selection,
    ;; and gives it the UUID of the first.
    (is (= (doc/delete test-doc (selection [(big-dec 1) 0] [(big-dec 2) 0]))
           (document [p2])))) ;; FIXME

  (testing "merges start and ending paragraphs when deleting across paragraphs"
    (is (= (doc/delete test-doc (selection [(big-dec 1) 3] [(big-dec 2) 3]))
           (document [(paragraph [(run "foo" #{:italic}), (run "bbbcccddd")])]))))

  (testing "merges start and ending paragraphs when deleting across more than 2 paragraphs"
    (is (= (doc/delete long-doc (selection [(big-dec 1) 4] [(big-dec 4) 0]))
           (document [(paragraph [(run "foo1" #{:italic}), (run "foo4" #{:strike})])]))))

  (testing "deletes whole document"
    (is (= (doc/delete test-doc (selection [(big-dec 1) 0] [(big-dec 2) 12]))
           (document [(paragraph [(run)])])))))

#_(deftest enter-test
  (testing "works at start of paragraph"
    (is (= (doc/enter test-doc (selection [(big-dec 1) 0]))
           (document [(paragraph), p1, p2]))))

  (testing "works at end of paragraph"
    (is (= (doc/enter test-doc (selection [(big-dec 1) 14]))
           (document [p1, (paragraph [(run)]), p2]))))

  (testing "works in middle of paragraph"
    (is (= (doc/enter test-doc (selection [(big-dec 1) 3]))
           (document [(paragraph [(run "foo" #{:italic})])
                      (paragraph [(run "bar" #{:bold :italic})
                                  (run "bizz" #{:italic})
                                  (run "buzz" #{:bold})])
                      p2]))))

  (testing "works at end of doc"
    (is (= (doc/enter test-doc (selection [(big-dec 2) 12]))
           (document [p1 p2 (p/paragraph)])))))

(deftest selected-content-test
  (testing "returns paragraph fragment when passed selection within one paragraph"
    (is (= (p/fragment [(run "bar" #{:bold :italic})
                        (run "bizz" #{:italic})
                        (run "buzz" #{:bold})])
           (sl/selected-content test-doc (selection [(big-dec 1) 3] [(big-dec 1) 14])))))

  (testing "returns document fragment when passed selection across multiple paragraphs"
    (is (= (doc/fragment [(paragraph [(run "bar" #{:bold :italic})
                                           (run "bizz" #{:italic})
                                           (run "buzz" #{:bold})])
                          (paragraph [(run "aaa")])])
           (sl/selected-content test-doc (selection [(big-dec 1) 3] [(big-dec 2) 3])))))

  (testing "returns document fragment when passed selection across multiple (> 3) paragraphs"
    (is (= (doc/fragment [(paragraph [(run "foo1" #{:italic})])
                          (paragraph [(run "foo2" #{:bold})])
                          (paragraph [(run "foo3" #{:underline})])
                          (paragraph [(run "foo" #{:strike})])])
           (sl/selected-content long-doc (selection [(big-dec 1) 0] [(big-dec 4) 3])))))

  (testing "returns just one paragraph (no empty next paragraph) when going from start of paragraph 1 to start of paragraph 2"
    (is (= (doc/fragment [(paragraph [(run "foo1" #{:italic})]) (paragraph [])])
           (sl/selected-content long-doc (selection [(big-dec 1) 0] [(big-dec 2) 0]))))))

(deftest formatting-test
  (let [formats-doc (document [(paragraph [(run "foo1" #{:italic})
                                           (run "foo2" #{:italic :bold})
                                           (run "foo3" #{:bold})])
                               (paragraph [(run "bar1" #{:italic :bold :underline})])])]
    (testing "works inside same paragraph"
      (is (= #{:italic} (sl/formatting formats-doc (selection [(big-dec 1) 0] [(big-dec 1) 8]))))
      (is (= #{:italic :bold} (sl/formatting formats-doc (selection [(big-dec 1) 4] [(big-dec 1) 8]))))
      (is (= #{:bold} (sl/formatting formats-doc (selection [(big-dec 1) 4] [(big-dec 1) 12])))))

    (testing "works across paragraphs"
      (is (= #{:bold} (sl/formatting formats-doc (selection [(big-dec 1) 8] [(big-dec 2) 3])))))))

(deftest toggle-format-test
  (testing "toggling single run"
    (is (= (doc/toggle-format test-doc (selection [(big-dec 1) 0] [(big-dec 1) 3]) :italic)
           (document [(paragraph [(run "foo")
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "toggling across runs WITH shared format"
    (is (= (doc/toggle-format test-doc (selection [(big-dec 1) 0] [(big-dec 1) 10]) :italic)
           (document [(paragraph [(run "foo")
                                       (run "bar" #{:bold})
                                       (run "bizz" #{})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "toggling across runs WITH shared format, not on run boundaries"
    (is (= (doc/toggle-format test-doc (selection [(big-dec 1) 1] [(big-dec 1) 8]) :italic)
           (document [(paragraph [(run "f" #{:italic})
                                       (run "oo")
                                       (run "bar" #{:bold})
                                       (run "bi" #{})
                                       (run "zz" #{:italic})
                                       (run "buzz" #{:bold})])
                      p2]))))

  (testing "toggling across runs WITHOUT shared format"
    (is (= (doc/toggle-format test-doc (selection [(big-dec 1) 0] [(big-dec 1) 14]) :italic)
           (document [(paragraph [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold :italic})])
                      p2]))))

  (testing "toggling across paragraphs WITHOUT shared format"
    (is (= (doc/toggle-format test-doc (selection [(big-dec 1) 0] [(big-dec 2) 12]) :italic)
           (document [(paragraph [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold :italic})])
                      (paragraph [(run "aaabbbcccddd" #{:italic})])]))))

  (testing "toggling across paragraphs WITHOUT shared format, and not landing on run boundaries"
    (is (= (doc/toggle-format test-doc (selection [(big-dec 1) 1] [(big-dec 2) 3]) :italic)
           (document [(paragraph [(run "foo" #{:italic})
                                       (run "bar" #{:bold :italic})
                                       (run "bizz" #{:italic})
                                       (run "buzz" #{:bold :italic})])
                      (paragraph [(run "aaa" #{:italic})
                                       (run "bbbcccddd")])]))))

  (testing "toggling across paragraphs WITH shared format"
    (let [modified (-> test-doc
                       (update-in [:children (big-dec 1) :runs 3 :formats] conj :italic)
                       (update-in [:children (big-dec 2) :runs 0 :formats] conj :italic))]
      (is (= (doc/toggle-format modified (selection [(big-dec 1) 10] [(big-dec 2) 12]) :italic)
             (document [(paragraph [(run "foo" #{:italic})
                                         (run "bar" #{:bold :italic})
                                         (run "bizz" #{:italic})
                                         (run "buzz" #{:bold})])
                        (paragraph [(run "aaabbbcccddd")])])))))

  (testing "toggling with selection end on beginning of paragraph"
    (let [d (document [(paragraph [(run "foo" #{:italic})])
                       (paragraph [(run "bar")])])]
      (is (= (doc/toggle-format d (selection [(big-dec 1) 0] [(big-dec 2) 0]) :italic)
             (document [(paragraph [(run "foo" #{})])
                        (paragraph [(run "bar" #{})])])))
      (is (= (-> d
                 (doc/toggle-format (selection [(big-dec 1) 0] [(big-dec 2) 0]) :italic)
                 (doc/toggle-format (selection [(big-dec 1) 0] [(big-dec 2) 0]) :italic))
             d))))

  (testing "toggling with selection start on end of paragraph"
    (let [d (document [(paragraph [(run "foo" #{:italic})])
                       (paragraph [(run "bar")])])]
      (is (= (doc/toggle-format d (selection [(big-dec 1) 3] [(big-dec 2) 3]) :bold)
             (document [(paragraph [(run "foo" #{:italic})])
                        (paragraph [(run "bar" #{:bold})])])))
      (is (= (-> d
                 (doc/toggle-format (selection [(big-dec 1) 3] [(big-dec 2) 3]) :bold)
                 (doc/toggle-format (selection [(big-dec 1) 3] [(big-dec 2) 3]) :bold))
             d)))))

(deftest char-at-test
  (testing "works in 1st paragraph"
    (is (= "f" (sl/char-at test-doc (selection [(big-dec 1) 0]))))
    (is (= "o" (sl/char-at test-doc (selection [(big-dec 1) 1]))))
    (is (= "z" (sl/char-at test-doc (selection [(big-dec 1) 13]))))
    (is (= "" (sl/char-at test-doc (selection [(big-dec 1) 14])))))

  (testing "works in other paragraphs"
    (is (= "a" (sl/char-at test-doc (selection [(big-dec 2) 0]))))
    (is (= "b" (sl/char-at test-doc (selection [(big-dec 2) 3]))))
    (is (= "c" (sl/char-at test-doc (selection [(big-dec 2) 7]))))
    (is (= "d" (sl/char-at test-doc (selection [(big-dec 2) 11]))))
    (is (= "" (sl/char-at test-doc (selection [(big-dec 2) 12]))))))

(deftest char-before-test
  (testing "works in 1st paragraph"
    (is (= "\n" (sl/char-before test-doc (selection [(big-dec 1) 0]))))
    (is (= "f" (sl/char-before test-doc (selection [(big-dec 1) 1]))))
    (is (= "o" (sl/char-before test-doc (selection [(big-dec 1) 2]))))
    (is (= "z" (sl/char-before test-doc (selection [(big-dec 1) 13]))))
    (is (= "z" (sl/char-before test-doc (selection [(big-dec 1) 14])))))

  (testing "works in other paragraphs"
    (is (= "\n" (sl/char-before test-doc (selection [(big-dec 2) 0]))))
    (is (= "a" (sl/char-before test-doc (selection [(big-dec 2) 1]))))
    (is (= "a" (sl/char-before test-doc (selection [(big-dec 2) 3]))))
    (is (= "b" (sl/char-before test-doc (selection [(big-dec 2) 4]))))
    (is (= "c" (sl/char-before test-doc (selection [(big-dec 2) 7]))))
    (is (= "d" (sl/char-before test-doc (selection [(big-dec 2) 11]))))
    (is (= "d" (sl/char-before test-doc (selection [(big-dec 2) 12]))))
    (is (thrown? js/Error (sl/char-before test-doc (selection ["[2]" 13]))))))
