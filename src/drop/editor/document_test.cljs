(ns drop.editor.document-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.selection :as sel :refer [selection]]
            [drop.editor.core :as c :refer [run paragraph]]))

;; Because checking equivalence on a bunch of nested records is a ROYAL pain in the ass,
;; I invented a sort of custom version of Hiccup[1] for represented documents with paragraphs
;; and runs nested inside of them and use the `convert-doc` function to check equivalence.
;;
;; For example, `(convert-doc doc)` called on `doc` defined below would evaluate to:
;;
;; [[["foo" :italic]
;;   ["bar" :bold :italic]
;;   ["bizz" :italic]
;;   ["buzz" :bold]]
;;   [["aaabbbcccddd"]]]
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

(def doc (c/document [p1 p2]))

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
