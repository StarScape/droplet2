(ns slate.model.find-and-replace-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.dll :refer [create-changelist big-dec dll]]
            [slate.model.run :as r :refer [run]]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.editor-state :as es :refer [editor-state get-changelist]]
            [slate.model.find-and-replace :refer [find paragraph-replace replace replace-all find-occurrences replace-all-occurrences]]))

(def doc (document false (dll (paragraph [(run "foo") (run "bar" #{:italic})
                                          (run "goo") (run "bar" #{:bold})
                                          (run "hoo") (run "bar" #{:underline})])
                              (paragraph [(run "one a one, and a foo, and a bar!")]))))

(def doc2 (document false [(paragraph [(run "foo") (run "bar" #{:italic})
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
    (let [es (replace (editor-state doc (selection [(big-dec 1) 0] [(big-dec 1) 3])) "a longer run of text")]
      (is (= es (editor-state (document [(paragraph [(run "a longer run of text") (run "bar" #{:italic})
                                                     (run "goo") (run "bar" #{:bold})
                                                     (run "hoo") (run "bar" #{:underline})])
                                         (paragraph [(run "one a one, and a foo, and a bar!")])])
                              (selection [(big-dec 1) 0] [(big-dec 1) 20]))))
      (is (= (get-changelist es) (create-changelist :changed-indices #{(big-dec 1)}))))))

(deftest replace-all-test
  (testing "single location"
    (let [es (replace-all (editor-state doc) [(selection [(big-dec 1) 0] [(big-dec 1) 3])] "123")]
      (is (= es (editor-state (document [(paragraph [(run "123") (run "bar" #{:italic})
                                                     (run "goo") (run "bar" #{:bold})
                                                     (run "hoo") (run "bar" #{:underline})])
                                         (paragraph [(run "one a one, and a foo, and a bar!")])]))))
      (is (= (get-changelist es)
             (create-changelist :changed-indices #{(big-dec 1)})))))

  (testing "multiple locations"
    (let [es (replace-all (editor-state doc)
                          [(selection [(big-dec 1) 0] [(big-dec 1) 3])
                           (selection [(big-dec 1) 6] [(big-dec 1) 9])]
                          "123")]
      (is (= (replace-all (editor-state doc)
                          [(selection [(big-dec 1) 0] [(big-dec 1) 3])
                           (selection [(big-dec 1) 6] [(big-dec 1) 9])]
                          "123")
             (editor-state (document [(paragraph [(run "123") (run "bar" #{:italic})
                                                  (run "123") (run "bar" #{:bold})
                                                  (run "hoo") (run "bar" #{:underline})])
                                      (paragraph [(run "one a one, and a foo, and a bar!")])]))))
      (is (= (get-changelist es) (create-changelist :changed-indices #{(big-dec 1)})))))

  (testing "multiple locations across 2 paragraphs"
    (let [es (replace-all (editor-state doc)
                          [(selection [(big-dec 1) 0] [(big-dec 1) 3])
                           (selection [(big-dec 1) 6] [(big-dec 1) 9])
                           (selection [(big-dec 2) 1] [(big-dec 2) 3])]
                          "123")]
      (is (= es (editor-state (document [(paragraph [(run "123") (run "bar" #{:italic})
                                                     (run "123") (run "bar" #{:bold})
                                                     (run "hoo") (run "bar" #{:underline})])
                                         (paragraph [(run "o123 a one, and a foo, and a bar!")])]))))
      (is (= (get-changelist es) (create-changelist :changed-indices #{(big-dec 1) (big-dec 2)})))))

  (testing "shifts selection appropriately when needed"
    (let [es (replace-all (editor-state doc (selection [(big-dec 1) 17]))
                          [(selection [(big-dec 1) 12] [(big-dec 1) 18])]
                          "bizz")]
      (is (= es (editor-state (document [(paragraph [(run "foo") (run "bar" #{:italic})
                                                     (run "goo") (run "bar" #{:bold})
                                                     (run "bizz")])
                                         (paragraph [(run "one a one, and a foo, and a bar!")])])
                              (selection [(big-dec 1) 16]))))
      (is (= (get-changelist es) (create-changelist :changed-indices #{(big-dec 1)}))))))


(deftest paragraph-replace-test
  (testing "works correctly replacing with replacement text of different lengths (shorter and longer)"
    (is (= (paragraph-replace (p/paragraph [(r/run "hello and hello and hello")])
                              [(selection [nil 0] [nil 5])
                               (selection [nil 10] [nil 15])
                               (selection [nil 20] [nil 25])]
                              "hello1")
           (p/paragraph [(r/run "hello1 and hello1 and hello1")])))
    (is (= (paragraph-replace (p/paragraph [(r/run "hello and hello and hello")])
                              [(selection [nil 0] [nil 5])
                               (selection [nil 10] [nil 15])
                               (selection [nil 20] [nil 25])]
                              "hello1")
           (p/paragraph [(r/run "hello1 and hello1 and hello1")])))
    (is (= (paragraph-replace (p/paragraph [(r/run "hello" #{:italic})
                                            (r/run " and ")
                                            (r/run "hello" #{:bold})
                                            (r/run " and ")
                                            (r/run "hello" #{:strikethrough})])
                              [(selection [nil 0] [nil 5])
                               (selection [nil 10] [nil 15])
                               (selection [nil 20] [nil 25])]
                              "goodbye")
           (p/paragraph [(r/run "goodbye" #{:italic})
                         (r/run " and ")
                         (r/run "goodbye" #{:bold})
                         (r/run " and ")
                         (r/run "goodbye" #{:strikethrough})])))))
