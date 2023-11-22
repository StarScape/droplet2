(ns slate.model.find-and-replace-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.run :as r :refer [run]]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.editor-state :as es :refer [editor-state ->EditorUpdate changelist]]
            [slate.model.find-and-replace :refer [find replace replace-all]]))

#_(def p (p/paragraph "p1" [(r/run "foo") (r/run "bar" #{:italic})
                          (r/run "goo") (r/run "bar" #{:bold})
                          (r/run "hoo") (r/run "bar" #{:underline})]))

(def doc (document [(paragraph "p1" [(run "foo") (run "bar" #{:italic})
                                     (run "goo") (run "bar" #{:bold})
                                     (run "hoo") (run "bar" #{:underline})])
                    (paragraph "p2" [(run "one a one, and a foo, and a bar!")])]))

(def doc2 (document [(paragraph "p1" [(run "foo") (run "bar" #{:italic})
                                      (run "goo") (run "Bar" #{:bold})
                                      (run "hoo") (run "BAR" #{:underline})])
                     (paragraph "p2" [(run "one a one, and a foo, and a bAr!")])]))


(deftest find-test
  (testing "basic functions"
    (is (= (find (editor-state doc) "foo")
           [(selection ["p1" 0] ["p1" 3])
            (selection ["p2" 17] ["p2" 20])]))
    (is (= (find (editor-state doc) "bar")
           [(selection ["p1" 3] ["p1" 6])
            (selection ["p1" 9] ["p1" 12])
            (selection ["p1" 15] ["p1" 18])
            (selection ["p2" 28] ["p2" 31])]))
    (is (= (find (editor-state doc) "goo")
           [(selection ["p1" 6] ["p1" 9])]))
    (is (= [] (find (editor-state doc) "byzantium"))))

  (testing "case sensitivity"
    (is (= (find (editor-state doc2) "bar" true)
           [(selection ["p1" 3] ["p1" 6])
            (selection ["p1" 9] ["p1" 12])
            (selection ["p1" 15] ["p1" 18])
            (selection ["p2" 28] ["p2" 31])]))
    (is (= (find (editor-state doc2) "bar" false)
           [(selection ["p1" 3] ["p1" 6])]))))

(deftest replace-test
  (testing "shifts selection appropriately when replacement text is of different length than initial selection"
    (is (= (replace (editor-state doc (selection ["p1" 0] ["p1" 3])) "a longer run of text")
           (->EditorUpdate (editor-state (document [(paragraph "p1" [(run "a longer run of text") (run "bar" #{:italic})
                                                                     (run "goo") (run "bar" #{:bold})
                                                                     (run "hoo") (run "bar" #{:underline})])
                                                    (paragraph "p2" [(run "one a one, and a foo, and a bar!")])])
                                         (selection ["p1" 0] ["p1" 20]))
                           (changelist :changed-indices #{"p1"}))))))

(deftest replace-all-test
  (testing "single location"
    (is (= (replace-all (editor-state doc)
                        [(selection ["p1" 0] ["p1" 3])]
                        "123")
           (->EditorUpdate (editor-state (document [(paragraph "p1" [(run "123") (run "bar" #{:italic})
                                                                     (run "goo") (run "bar" #{:bold})
                                                                     (run "hoo") (run "bar" #{:underline})])
                                                    (paragraph "p2" [(run "one a one, and a foo, and a bar!")])]))
                           (changelist :changed-indices #{"p1"})))))
  (testing "multiple locations"
    (is (= (replace-all (editor-state doc)
                        [(selection ["p1" 0] ["p1" 3])
                         (selection ["p1" 6] ["p1" 9])]
                        "123")
           (->EditorUpdate (editor-state (document [(paragraph "p1" [(run "123") (run "bar" #{:italic})
                                                                     (run "123") (run "bar" #{:bold})
                                                                     (run "hoo") (run "bar" #{:underline})])
                                                    (paragraph "p2" [(run "one a one, and a foo, and a bar!")])]))
                           (changelist :changed-indices #{"p1"})))))
  (testing "multiple locations across 2 paragraphs"
    (is (= (replace-all (editor-state doc)
                        [(selection ["p1" 0] ["p1" 3])
                         (selection ["p1" 6] ["p1" 9])
                         (selection ["p2" 1] ["p2" 3])]
                        "123")
           (->EditorUpdate (editor-state (document [(paragraph "p1" [(run "123") (run "bar" #{:italic})
                                                                     (run "123") (run "bar" #{:bold})
                                                                     (run "hoo") (run "bar" #{:underline})])
                                                    (paragraph "p2" [(run "o123 a one, and a foo, and a bar!")])]))
                           (changelist :changed-indices #{"p1" "p2"})))))
  (testing "shifts selection appropriately when needed"
    (is (= (replace-all (editor-state doc (selection ["p1" 17]))
                        [(selection ["p1" 12] ["p1" 18])]
                        "bizz")
           (->EditorUpdate (editor-state (document [(paragraph "p1" [(run "foo") (run "bar" #{:italic})
                                                                     (run "goo") (run "bar" #{:bold})
                                                                     (run "bizz")])
                                                    (paragraph "p2" [(run "one a one, and a foo, and a bar!")])])
                                         (selection ["p1" 16]))
                           (changelist :changed-indices #{"p1"}))))))
