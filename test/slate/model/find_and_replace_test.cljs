(ns slate.model.find-and-replace-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.dll :refer [big-dec]]
            [slate.model.run :as r :refer [run]]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.editor-state :as es :refer [editor-state ->EditorUpdate changelist]]
            [slate.model.find-and-replace :refer [find replace replace-all]]))

(def doc (document [(paragraph [(run "foo") (run "bar" #{:italic})
                                (run "goo") (run "bar" #{:bold})
                                (run "hoo") (run "bar" #{:underline})])
                    (paragraph [(run "one a one, and a foo, and a bar!")])]))

(def doc2 (document [(paragraph [(run "foo") (run "bar" #{:italic})
                                 (run "goo") (run "Bar" #{:bold})
                                 (run "hoo") (run "BAR" #{:underline})])
                     (paragraph [(run "one a one, and a foo, and a bAr!")])]))


(deftest find-test
  (testing "basic functions"
    (is (= (find (editor-state doc) "foo")
           [(selection [(big-dec 1) 0] [(big-dec 1) 3])
            (selection [(big-dec 2) 17] [(big-dec 2) 20])]))
    (is (= (find (editor-state doc) "bar")
           [(selection [(big-dec 1) 3] [(big-dec 1) 6])
            (selection [(big-dec 1) 9] [(big-dec 1) 12])
            (selection [(big-dec 1) 15] [(big-dec 1) 18])
            (selection [(big-dec 2) 28] [(big-dec 2) 31])]))
    (is (= (find (editor-state doc) "goo")
           [(selection [(big-dec 1) 6] [(big-dec 1) 9])]))
    (is (= [] (find (editor-state doc) "byzantium"))))

  (testing "case sensitivity"
    (is (= (find (editor-state doc2) "bar" true)
           [(selection [(big-dec 1) 3] [(big-dec 1) 6])
            (selection [(big-dec 1) 9] [(big-dec 1) 12])
            (selection [(big-dec 1) 15] [(big-dec 1) 18])
            (selection [(big-dec 2) 28] [(big-dec 2) 31])]))
    (is (= (find (editor-state doc2) "bar" false)
           [(selection [(big-dec 1) 3] [(big-dec 1) 6])]))))

(deftest replace-test
  (testing "shifts selection appropriately when replacement text is of different length than initial selection"
    (is (= (replace (editor-state doc (selection [(big-dec 1) 0] [(big-dec 1) 3])) "a longer run of text")
           (->EditorUpdate (editor-state (document [(paragraph [(run "a longer run of text") (run "bar" #{:italic})
                                                                (run "goo") (run "bar" #{:bold})
                                                                (run "hoo") (run "bar" #{:underline})])
                                                    (paragraph [(run "one a one, and a foo, and a bar!")])])
                                         (selection [(big-dec 1) 0] [(big-dec 1) 20]))
                           (changelist :changed-indices #{(big-dec 1)}))))))

(deftest replace-all-test
  (testing "single location"
    (is (= (replace-all (editor-state doc)
                        [(selection [(big-dec 1) 0] [(big-dec 1) 3])]
                        "123")
           (->EditorUpdate (editor-state (document [(paragraph [(run "123") (run "bar" #{:italic})
                                                                (run "goo") (run "bar" #{:bold})
                                                                (run "hoo") (run "bar" #{:underline})])
                                                    (paragraph [(run "one a one, and a foo, and a bar!")])]))
                           (changelist :changed-indices #{(big-dec 1)})))))
  (testing "multiple locations"
    (is (= (replace-all (editor-state doc)
                        [(selection [(big-dec 1) 0] [(big-dec 1) 3])
                         (selection [(big-dec 1) 6] [(big-dec 1) 9])]
                        "123")
           (->EditorUpdate (editor-state (document [(paragraph [(run "123") (run "bar" #{:italic})
                                                                (run "123") (run "bar" #{:bold})
                                                                (run "hoo") (run "bar" #{:underline})])
                                                    (paragraph [(run "one a one, and a foo, and a bar!")])]))
                           (changelist :changed-indices #{(big-dec 1)})))))
  (testing "multiple locations across 2 paragraphs"
    (is (= (replace-all (editor-state doc)
                        [(selection [(big-dec 1) 0] [(big-dec 1) 3])
                         (selection [(big-dec 1) 6] [(big-dec 1) 9])
                         (selection [(big-dec 2) 1] [(big-dec 2) 3])]
                        "123")
           (->EditorUpdate (editor-state (document [(paragraph [(run "123") (run "bar" #{:italic})
                                                                (run "123") (run "bar" #{:bold})
                                                                (run "hoo") (run "bar" #{:underline})])
                                                    (paragraph [(run "o123 a one, and a foo, and a bar!")])]))
                           (changelist :changed-indices #{(big-dec 1) (big-dec 2)})))))
  (testing "shifts selection appropriately when needed"
    (is (= (replace-all (editor-state doc (selection [(big-dec 1) 17]))
                        [(selection [(big-dec 1) 12] [(big-dec 1) 18])]
                        "bizz")
           (->EditorUpdate (editor-state (document [(paragraph [(run "foo") (run "bar" #{:italic})
                                                                (run "goo") (run "bar" #{:bold})
                                                                (run "bizz")])
                                                    (paragraph [(run "one a one, and a foo, and a bar!")])])
                                         (selection [(big-dec 1) 16]))
                           (changelist :changed-indices #{(big-dec 1)}))))))
