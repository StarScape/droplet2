(ns slate.document-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.selection :as sel :refer [selection]]
            [slate.core :as sl :refer [run paragraph document]]
            [slate.dll :as dll :refer [dll]]))

;; Because checking equivalence on a bunch of nested records is a ROYAL pain in the ass,
;; I invented a sort of custom version of Hiccup[1] for represented documents with paragraphs
;; and runs nested inside of them and use the `convert-doc` function to check equivalence.
;;
;; For example, `(convert-doc doc)` called on `doc` defined below would evaluate to:
;;
;; [[:p
;;   [:run "foo" :italic]
;;   [:run "bar" :bold :italic]
;;   [:run "bizz" :italic]
;;   [:run "buzz" :bold]]
;;   [:p [:run "aaabbbcccddd"]]]
;;
;; [1] https://github.com/weavejester/hiccup

(defn- convert-run [r] (into [:run (:text r)] (:formats r)))
(defn- convert-paragraph [p] (into [:p] (map convert-run (:runs p))))
(defn- convert-doc [d] (mapv convert-paragraph (:children d)))

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
  (testing "runs"
    (is (= [[:p
             [:run "fooHello" :italic]
             [:run "Goodbye!"]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (sl/insert doc
                                  (selection ["p1" 3])
                                  [(run "Hello" #{:italic}) (run "Goodbye!")])))))

  (testing "single run"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "Goodbye!"]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (sl/insert doc (selection ["p1" 3]) (run "Goodbye!"))))))

  (testing "at start of paragraph"
    (is (= [[:p
             [:run "Hello!"]
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (sl/insert doc (selection ["p1" 0]) (run "Hello!"))))))

  (testing "at end of paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p
             [:run "aaabbbcccddd"]
             [:run "Goodbye!" :italic]]]
           (convert-doc (sl/insert doc (selection ["p2" 12]) (run "Goodbye!" #{:italic}))))))

  ;; TODO: write some cases for multi-paragraph insert
  (testing "multi-paragraph insert in the middle of a paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "inserted paragraph 1"]]
            [:p
             [:run "inserted paragraph 2"]]
            [:p
             [:run "inserted paragraph 3"]
             [:run "buzz" :bold]]
            [:p
             [:run "aaabbbcccddd"]]]
           (convert-doc (sl/insert doc (selection ["p1" 10]) to-insert))))
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "inserted paragraph 1"]]
            [:p
             [:run "inserted paragraph 2"]]
            [:p
             [:run "inserted paragraph 3"]
             [:run "buzz" :bold]]
            [:p
             [:run "aaabbbcccddd"]]]
           (convert-doc (sl/insert doc (selection ["p1" 10]) (into (dll) to-insert))))))

  (testing "multi-paragraph insert at the start of a paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "inserted paragraph 1"]]
            [:p [:run "inserted paragraph 2"]]
            [:p [:run "inserted paragraph 3aaabbbcccddd"]]]
           (convert-doc (sl/insert doc (selection ["p2" 0]) to-insert))))
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "inserted paragraph 1"]]
            [:p [:run "inserted paragraph 2"]]
            [:p [:run "inserted paragraph 3aaabbbcccddd"]]]
           (convert-doc (sl/insert doc (selection ["p2" 0]) (into (dll) to-insert))))))

  (testing "multi-paragraph insert at the end of a paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]
             [:run "inserted paragraph 1"]]
            [:p [:run "inserted paragraph 2"]]
            [:p [:run "inserted paragraph 3"]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (sl/insert doc (selection ["p1" 14]) to-insert))))
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]
             [:run "inserted paragraph 1"]]
            [:p [:run "inserted paragraph 2"]]
            [:p [:run "inserted paragraph 3"]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (sl/insert doc (selection ["p1" 14]) (into (dll) to-insert))))))

  (testing "inserting a plain string"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "inserted"]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (sl/insert doc (selection ["p1" 3]) "inserted")))))

  (testing "when given a range-selection, deletes before inserting"
    (is (= [[:p [:run "f" :italic] [:run "(inserted!)d"]]]
           (convert-doc (sl/insert
                         doc
                         (selection ["p1" 1] ["p2" 11])
                         (run "(inserted!)" #{}))))))

  (testing "throws when out of range of paragraph"
    (is (thrown?
         js/Error
         (convert-doc (sl/insert doc (selection ["p1" 55]) (run "Goodbye!" #{:italic})))))))

(deftest delete-single-test
  (testing "does nothing at beginning of doc"
    (is (= doc (first (sl/delete doc (selection ["p1" 0]))))))

  (testing "deletes single char in middle of paragraph"
    (is (= [[:p
             [:run "oo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/delete doc (selection ["p1" 1])))))))

  (testing "deletes single char at end of paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/delete doc (selection ["p1" 14])))))))

  (testing "merges paragraphs when backspacing from start of paragraph that is not first"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]
             [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/delete doc (selection ["p2" 0])))))))

  (testing "deletes single char as normal at end of the paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccdd"]]]
           (convert-doc (first (sl/delete doc (selection ["p2" 12]))))))))

(deftest delete-range-test
  (testing "deletes from start of paragraph"
    (is (= [[:p
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/delete doc (selection ["p1" 0] ["p1" 3])))))))

  (testing "deletes from start of paragraph backwards"
    (is (= [[:p
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/delete doc (selection ["p1" 0] ["p1" 3] true)))))))

  (testing "deletes up to end of paragraph"
    (is (= [[:p [:run "foo" :italic]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/delete doc (selection ["p1" 3] ["p1" 14])))))))

  (testing "deletes whole paragraph"
    (is (= [[:p [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/delete doc (selection ["p1" 0] ["p2" 0])))))))

  (testing "merges start and ending paragraphs when deleting across paragraphs"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bbbcccddd"]]]
           (convert-doc (first (sl/delete doc (selection ["p1" 3] ["p2" 3])))))))

  (testing "merges start and ending paragraphs when deleting across more than 2 paragraphs"
    (is (= [[:p
             [:run "foo1" :italic]
             [:run "foo4" :strike]]]
           (convert-doc (first (sl/delete long-doc (selection ["d1" 4] ["d4" 0])))))))

  (testing "deletes whole document"
    (is (= [[:p [:run ""]]]
           (convert-doc (first (sl/delete doc (selection ["p1" 0] ["p2" 12]))))))))

(deftest enter-test
  (testing "works at start of paragraph"
    (is (= [[:p [:run ""]]
            [:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/enter doc (selection ["p1" 0])))))))

  (testing "works at end of paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run ""]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/enter doc (selection ["p1" 14])))))))

  (testing "works in middle of paragraph"
    (is (= [[:p [:run "foo" :italic]]
            [:p
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (first (sl/enter doc (selection ["p1" 3])))))))

  (testing "works at end of doc"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]
            [:p [:run ""]]]
           (convert-doc (first (sl/enter doc (selection ["p2" 12])))))))

  (testing "works with range selection"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]
            [:p [:run ""]]]
           (convert-doc (first (sl/enter doc (selection ["p2" 12]))))))))

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

(deftest toggle-format-test
  (testing "toggling single run"
    (is (= [[:p
             [:run "foo"]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (sl/toggle-format doc (selection ["p1" 0] ["p1" 3]) :italic)))))

  (testing "toggling across runs WITH shared format"
    (is (= [[:p
             [:run "foo"]
             [:run "bar" :bold]
             [:run "bizz"]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (sl/toggle-format doc (selection ["p1" 0] ["p1" 10]) :italic)))))

  (testing "toggling across runs WITHOUT shared format"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold :italic]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (sl/toggle-format doc (selection ["p1" 0] ["p1" 14]) :italic)))))

  (testing "toggling across paragraphs WITHOUT shared format"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold :italic]]
            [:p [:run "aaabbbcccddd" :italic]]]
           (convert-doc (sl/toggle-format doc (selection ["p1" 0] ["p2" 12]) :italic)))))

  (testing "toggling across paragraphs WITH shared format"
    (let [modified (-> doc
                       (update-in [:children "p1" :runs 3 :formats] conj :italic)
                       (update-in [:children "p2" :runs 0 :formats] conj :italic))]
      (is (= [[:p
               [:run "foo" :italic]
               [:run "bar" :bold :italic]
               [:run "bizz" :italic]
               [:run "buzz" :bold]]
              [:p [:run "aaabbbcccddd"]]]
             (convert-doc (sl/toggle-format modified (selection ["p1" 10] ["p2" 12]) :italic)))))))

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
    (is (= "f" (sl/char-before doc (selection ["p1"1]))))
    (is (= "o" (sl/char-before doc (selection ["p1"2]))))
    (is (= "z" (sl/char-before doc (selection ["p1"13]))))
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
