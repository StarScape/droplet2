(ns drop.editor.document-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.selection :as sel :refer [selection]]
            [drop.editor.core :as c :refer [run paragraph document]]))

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

(def p1 (paragraph [(run "foo" #{:italic})
                    (run "bar" #{:bold :italic})
                    (run "bizz" #{:italic})
                    (run "buzz" #{:bold})]))

(def p2 (paragraph [(run "aaa" #{})
                    (run "bbb" #{})
                    (run "ccc" #{})
                    (run "ddd" #{})]))

(def to-insert [(paragraph [(run "inserted paragraph 1")])
                (paragraph [(run "inserted paragraph 2")])
                (paragraph [(run "inserted paragraph 3")])])

(def doc (document [p1 p2]))

(def long-doc (document [(paragraph [(run "foo1" #{:italic})])
                           (paragraph [(run "foo2" #{:bold})])
                           (paragraph [(run "foo3" #{:underline})])
                           (paragraph [(run "foo4" #{:strike})])]))

(deftest insert-test
  (testing "runs"
    (is (= [[:p
             [:run "fooHello" :italic]
             [:run "Goodbye!"]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/insert doc
                                  (selection [0 3])
                                  [(run "Hello" #{:italic}) (run "Goodbye!")])))))

  (testing "single run"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "Goodbye!"]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/insert doc (selection [0 3]) (run "Goodbye!"))))))

  (testing "at start of paragraph"
    (is (= [[:p
             [:run "Hello!"]
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/insert doc (selection [0 0]) (run "Hello!"))))))

  (testing "at end of paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p
             [:run "aaabbbcccddd"]
             [:run "Goodbye!" :italic]]]
           (convert-doc (c/insert doc (selection [1 12]) (run "Goodbye!" #{:italic}))))))

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
           (convert-doc (c/insert doc (selection [0 10]) to-insert)))))

  (testing "multi-paragraph insert at the start of a paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "inserted paragraph 1"]]
            [:p [:run "inserted paragraph 2"]]
            [:p [:run "inserted paragraph 3aaabbbcccddd"]]]
           (convert-doc (c/insert doc (selection [1 0]) to-insert)))))

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
           (convert-doc (c/insert doc (selection [0 14]) to-insert)))))

  (testing "inserting a plain string"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "inserted"]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/insert doc (selection [0 3]) "inserted")))))

  (testing "when given a range-selection, deletes before inserting"
    (is (= [[:p [:run "f" :italic] [:run "(inserted!)d"]]]
           (convert-doc (c/insert
                         doc
                         (selection [0 1] [1 11])
                         (run "(inserted!)" #{}))))))

  (testing "throws when out of range of paragraph"
    (is (thrown?
         js/Error
         (convert-doc (c/insert doc (selection [0 55]) (run "Goodbye!" #{:italic})))))))

(deftest delete-single-test
  (testing "does nothing at beginning of doc"
    (is (= doc (c/delete doc (selection [0 0])))))

  (testing "deletes single char in middle of paragraph"
    (is (= [[:p
             [:run "oo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/delete doc (selection [0 1]))))))

  (testing "deletes single char at end of paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/delete doc (selection [0 14]))))))

  (testing "merges paragraphs when backspacing from start of paragraph that is not first"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]
             [:run "aaabbbcccddd"]]]
           (convert-doc (c/delete doc (selection [1 0]))))))

  (testing "deletes single char as normal at end of the paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccdd"]]]
           (convert-doc (c/delete doc (selection [1 12])))))))

(deftest delete-range-test
  (testing "deletes from start of paragraph"
    (is (= [[:p
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/delete doc (selection [0 0] [0 3]))))))

  (testing "deletes up to end of paragraph"
    (is (= [[:p [:run "foo" :italic]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/delete doc (selection [0 3] [0 14]))))))

  (testing "deletes whole paragraph"
    (is (= [[:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/delete doc (selection [0 0] [1 0]))))))

  (testing "merges start and ending paragraphs when deleting across paragraphs"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bbbcccddd"]]]
           (convert-doc (c/delete doc (selection [0 3] [1 3]))))))

  (testing "merges start and ending paragraphs when deleting across more than 2 paragraphs"
    (is (= [[:p
             [:run "foo1" :italic]
             [:run "foo4" :strike]]]
           (convert-doc (c/delete long-doc (selection [0 4] [3 0]))))))

  (testing "deletes whole document"
    (is (= [[:p [:run ""]]]
           (convert-doc (c/delete doc (selection [0 0] [1 12])))))))

(deftest enter-test
  (testing "works at start of paragraph"
    (is (= [[:p [:run ""]]
            [:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/enter doc (selection [0 0]))))))

  (testing "works at end of paragraph"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run ""]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/enter doc (selection [0 14]))))))

  (testing "works in middle of paragraph"
    (is (= [[:p [:run "foo" :italic]]
            [:p [:run ""]]
            [:p
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/enter doc (selection [0 3]))))))

  (testing "works at end of doc"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]
            [:p [:run ""]]]
           (convert-doc (c/enter doc (selection [1 12])))))))

(deftest selected-content-test
  (testing "returns list of runs when passed selection within one paragraph"
    (is (= [(run "bar" #{:bold :italic})
            (run "bizz" #{:italic})
            (run "buzz" #{:bold})]
           (c/selected-content doc (selection [0 3] [0 14])))))

  (testing "returns list of paragraphs when passed selection across multiple paragraphs"
    (is (= [(paragraph [(run "bar" #{:bold :italic})
                        (run "bizz" #{:italic})
                        (run "buzz" #{:bold})])
            (paragraph [(run "aaa")])]
           (c/selected-content doc (selection [0 3] [1 3])))))

  (testing "returns list of paragraphs when passed selection across multiple (> 3) paragraphs"
    (is (= [(paragraph [(run "foo1" #{:italic})])
            (paragraph [(run "foo2" #{:bold})])
            (paragraph [(run "foo3" #{:underline})])
            (paragraph [(run "foo" #{:strike})])]
           (c/selected-content long-doc (selection [0 0] [3 3])))))

  ;; TODO: I **think** this is the correct implementation here...could be wrong though...
  (testing "returns one paragraph and empty next paragraph when going from start of paragraph 1 to start of paragraph 2"
    (is (= [(paragraph [(run "foo1" #{:italic})]) (paragraph)]
           (c/selected-content long-doc (selection [0 0] [1 0]))))))

(deftest shared-formats-test
  (let [formats-doc (document [(paragraph [(run "foo1" #{:italic})
                                             (run "foo2" #{:italic :bold})
                                             (run "foo3" #{:bold})])
                                 (paragraph [(run "bar1" #{:italic :bold :underline})])])]
    (testing "works inside same paragraph"
      (is (= #{:italic} (c/shared-formats formats-doc (selection [0 0] [0 8]))))
      (is (= #{:italic :bold} (c/shared-formats formats-doc (selection [0 4] [0 8]))))
      (is (= #{:bold} (c/shared-formats formats-doc (selection [0 4] [0 12])))))

    (testing "works across paragraphs"
      (is (= #{:bold} (c/shared-formats formats-doc (selection [0 8] [1 3])))))))

(deftest toggle-format-test
  (testing "toggling single run"
    (is (= [[:p
             [:run "foo"]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/toggle-format doc (selection [0 0] [0 3]) :italic)))))

  (testing "toggling across runs WITH shared format"
    (is (= [[:p
             [:run "foo"]
             [:run "bar" :bold]
             [:run "bizz"]
             [:run "buzz" :bold]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/toggle-format doc (selection [0 0] [0 10]) :italic)))))

  (testing "toggling across runs WITHOUT shared format"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold :italic]]
            [:p [:run "aaabbbcccddd"]]]
           (convert-doc (c/toggle-format doc (selection [0 0] [0 14]) :italic)))))

  (testing "toggling across paragraphs WITHOUT shared format"
    (is (= [[:p
             [:run "foo" :italic]
             [:run "bar" :bold :italic]
             [:run "bizz" :italic]
             [:run "buzz" :bold :italic]]
            [:p [:run "aaabbbcccddd" :italic]]]
           (convert-doc (c/toggle-format doc (selection [0 0] [1 12]) :italic)))))

  (testing "toggling across paragraphs WITH shared format"
    (let [modified (-> doc
                       (update-in [:children 0 :runs 3 :formats] conj :italic)
                       (update-in [:children 1 :runs 0 :formats] conj :italic))]
      (is (= [[:p
               [:run "foo" :italic]
               [:run "bar" :bold :italic]
               [:run "bizz" :italic]
               [:run "buzz" :bold]]
              [:p [:run "aaabbbcccddd"]]]
             (convert-doc (c/toggle-format modified (selection [0 10] [1 12]) :italic)))))))

(deftest char-at-test
  (testing "works in 1st paragraph"
    (is (= "f" (c/char-at doc (selection [0 0]))))
    (is (= "o" (c/char-at doc (selection [0 1]))))
    (is (= "z" (c/char-at doc (selection [0 13]))))
    (is (thrown? js/Error (c/char-at doc (selection [0 14])))))

  (testing "works in other paragraphs"
    (is (= "a" (c/char-at doc (selection [1 0]))))
    (is (= "b" (c/char-at doc (selection [1 3]))))
    (is (= "c" (c/char-at doc (selection [1 7]))))
    (is (= "d" (c/char-at doc (selection [1 11]))))
    (is (thrown? js/Error (c/char-at doc (selection [1 12]))))))

(deftest char-before-test
  (testing "works in 1st paragraph"
    (is (= "\n" (c/char-before doc (selection [0 0]))))
    (is (= "f" (c/char-before doc (selection [0 1]))))
    (is (= "o" (c/char-before doc (selection [0 2]))))
    (is (= "z" (c/char-before doc (selection [0 13]))))
    (is (= "z" (c/char-before doc (selection [0 14])))))

  (testing "works in other paragraphs"
    (is (= "\n" (c/char-before doc (selection [1 0]))))
    (is (= "a" (c/char-before doc (selection [1 1]))))
    (is (= "a" (c/char-before doc (selection [1 3]))))
    (is (= "b" (c/char-before doc (selection [1 4]))))
    (is (= "c" (c/char-before doc (selection [1 7]))))
    (is (= "d" (c/char-before doc (selection [1 11]))))
    (is (= "d" (c/char-before doc (selection [1 12]))))
    (is (thrown? js/Error (c/char-before doc (selection [1 13]))))))