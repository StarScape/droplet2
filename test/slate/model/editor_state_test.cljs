(ns slate.model.editor-state-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.dll :as dll :refer [dll big-dec]]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.common :as sl]
            [slate.model.run :as r :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.editor-state :as es :refer [editor-state
                                                     map->EditorState
                                                     get-changelist]]
            [slate.model.navigation :as nav]))

(def p1 (paragraph [(run "foo" #{:italic})
                    (run "bar" #{:bold :italic})
                    (run "bizz" #{:italic})
                    (run "buzz" #{:bold})]))

(def p2 (paragraph [(run "aaa" #{})
                    (run "bbb" #{})
                    (run "ccc" #{})
                    (run "ddd" #{})]))

(def to-insert (document [(paragraph [(run "inserted paragraph 1")])
                          (paragraph [(run "inserted paragraph 2")])
                          (paragraph [(run "inserted paragraph 3")])]))

(def doc (document false (dll p1 p2)))

(def long-doc (document false (dll (paragraph [(run "foo1" #{:italic})])
                                   (paragraph [(run "foo2" #{:bold})])
                                   (paragraph [(run "foo3" #{:underline})])
                                   (paragraph [(run "foo4" #{:strike})]))))

(deftest insert-test
  (testing "insert 2 runs in middle of a paragraph"
    (let [es (es/insert (editor-state doc (selection [(big-dec 1) 3])) (paragraph [(run "Hello" #{:italic}) (run "Goodbye!")]))]
      (is (= es (map->EditorState {:doc (document [(paragraph [(run "fooHello" #{:italic})
                                                               (run "Goodbye!" #{})
                                                               (run "bar" #{:bold :italic})
                                                               (run "bizz" #{:italic})
                                                               (run "buzz" #{:bold})])
                                                   p2])
                                   :selection (selection [(big-dec 1) 16])})))
      (is (= (get-changelist es)
             {:changed-indices #{(big-dec 1)}
              :inserted-indices #{}
              :deleted-indices #{}}))))

  (testing "insert single run in middle of a paragraph"
    (let [es (es/insert (editor-state doc (selection [(big-dec 1) 3])) (run "Goodbye!"))]
      (is (= es (map->EditorState {:doc (document [(paragraph [(run "foo" #{:italic})
                                                               (run "Goodbye!" #{})
                                                               (run "bar" #{:bold :italic})
                                                               (run "bizz" #{:italic})
                                                               (run "buzz" #{:bold})])
                                                   p2])
                                   :selection (selection [(big-dec 1) 11])})))
      (is (= (get-changelist es)
             {:changed-indices #{(big-dec 1)}
              :inserted-indices #{}
              :deleted-indices #{}}))))

  (testing "insert run at start of paragraph"
    (let [es (es/insert (editor-state doc (selection [(big-dec 1) 0])) (run "Hello!"))]
      (is (= es (map->EditorState {:doc (document false [(paragraph [(run "Hello!" #{})
                                                                     (run "foo" #{:italic})
                                                                     (run "bar" #{:bold :italic})
                                                                     (run "bizz" #{:italic})
                                                                     (run "buzz" #{:bold})])
                                                         p2])
                                   :selection (selection [(big-dec 1) 6])})))
      (is (= (get-changelist es) {:changed-indices #{(big-dec 1)}
                                  :inserted-indices #{}
                                  :deleted-indices #{}}))))

  #_(testing "inserting runs at start of paragraph retains type"
    (let [result (-> (es/insert (editor-state (document false [(paragraph :h1 [(run "foo" #{:italic})
                                                                               (run "bar" #{:bold :italic})
                                                                               (run "bizz" #{:italic})
                                                                               (run "buzz" #{:bold})])
                                                               (paragraph [(run "aaa" #{})
                                                                           (run "bbb" #{})
                                                                           (run "ccc" #{})
                                                                           (run "ddd" #{})])])
                                              (selection [(big-dec 1) 0]))
                                (paragraph [(run "Hello!") (run "Goodbye!" #{:italic})])))]
      (is (= result (map->EditorState {:doc (document [(paragraph :h1 [(run "Hello!" #{})
                                                                       (run "Goodbye!foo" #{:italic})
                                                                       (run "bar" #{:bold :italic})
                                                                       (run "bizz" #{:italic})
                                                                       (run "buzz" #{:bold})])
                                                   p2])
                                   :selection (selection [(big-dec 1) 14] :formats #{:italic})})))
      (is (= (get-changelist result)
             {:changed-indices #{(big-dec 1)}
              :inserted-indices #{}
              :deleted-indices #{}}))))

  ;; I have decided against this behavior for now, as I think it's not always user intuitive and a bit fiddly.
  ;; Instead, the type is changed only when inserting into a blank paragraph. So if I insert an :h1 paragraph
  ;; into a blank para, then the whole para will be an :h1...but if I insert an :h1 into the end of a :body paragraph
  ;; with a bunch of text, it won't change the type. There's a test for this in the `paragraph_test.cljs` NS.
  #_(testing "inserting paragraph at start of paragraph changes type to inserted paragraph"
    (let [es (es/insert (editor-state (document false [(paragraph :h1 [(run "foo" #{:italic})
                                                                       (run "bar" #{:bold :italic})
                                                                       (run "bizz" #{:italic})
                                                                       (run "buzz" #{:bold})])
                                                       (paragraph [(run "aaa" #{})
                                                                   (run "bbb" #{})
                                                                   (run "ccc" #{})
                                                                   (run "ddd" #{})])])
                                      (selection [(big-dec 1) 0]))
                        (paragraph :ol [(run "Hello!") (run "Goodbye!" #{:italic})]))]
      (is (= es (map->EditorState {:doc (document [(paragraph :ol [(run "Hello!" #{})
                                                                   (run "Goodbye!foo" #{:italic})
                                                                   (run "bar" #{:bold :italic})
                                                                   (run "bizz" #{:italic})
                                                                   (run "buzz" #{:bold})])
                                                   p2])
                                   :selection (selection [(big-dec 1) 14] :formats #{:italic})})))
      (is (= (get-changelist es)
             {:changed-indices #{(big-dec 1)}
              :inserted-indices #{}
              :deleted-indices #{}}))))

  (testing "insert run at end of paragraph"
    (let [es (es/insert (editor-state doc (selection [(big-dec 2) 12])) (run "Goodbye!" #{:italic}))]
      (is (= es (map->EditorState {:doc (document [p1, (paragraph [(run "aaabbbcccddd") (run "Goodbye!" #{:italic})])])
                                   :selection (selection [(big-dec 2) 20] [(big-dec 2) 20] :formats #{:italic})})))
      (is (= (get-changelist es)
             {:changed-indices #{(big-dec 2)}
              :inserted-indices #{}
              :deleted-indices #{}}))))

  (testing "multi-paragraph insert in the middle of a single paragraph"
    (is (= (es/insert (editor-state doc (selection [(big-dec 1) 10])) to-insert)
           (map->EditorState {:doc (document [(paragraph [(run "foo" #{:italic})
                                                          (run "bar" #{:bold :italic})
                                                          (run "bizz" #{:italic})
                                                          (run "inserted paragraph 1")])
                                              (paragraph [(run "inserted paragraph 2")])
                                              (paragraph [(run "inserted paragraph 3")
                                                          (run "buzz" #{:bold})])
                                              p2])
                              :selection (selection [(big-dec 1.75) 20])})))
    (is (= (get-changelist (es/insert (editor-state doc (selection [(big-dec 1) 10])) to-insert))
           {:changed-indices #{(big-dec 1)}
            :inserted-indices #{(big-dec 1.5), (big-dec 1.75)}
            :deleted-indices #{}})))

  (testing "multi-paragraph insert at the start of a paragraph"
    (is (= (es/insert (editor-state doc (selection [(big-dec 2) 0])) to-insert)
           (map->EditorState {:doc (document [p1
                                              (paragraph [(run "inserted paragraph 1")])
                                              (paragraph [(run "inserted paragraph 2")])
                                              (paragraph [(run "inserted paragraph 3aaabbbcccddd")])])
                              :selection (selection [(big-dec 4) 20])})))
    (is (= (get-changelist (es/insert (editor-state doc (selection [(big-dec 2) 0])) to-insert))
           {:changed-indices #{(big-dec 2)}
            :inserted-indices #{(big-dec 3) (big-dec 4)}
            :deleted-indices #{}})))
  (testing "multi-paragraph insert at the end of a paragraph"
    (is (= (es/insert (editor-state doc (selection [(big-dec 1) 14])) to-insert)
           (map->EditorState {:doc (document [(paragraph [(run "foo" #{:italic})
                                                          (run "bar" #{:bold :italic})
                                                          (run "bizz" #{:italic})
                                                          (run "buzz" #{:bold})
                                                          (run "inserted paragraph 1")])
                                              (paragraph [(run "inserted paragraph 2")])
                                              (paragraph [(run "inserted paragraph 3")])
                                              p2])
                              :selection (selection [(big-dec 1.75) 20])})))
    (is (= (get-changelist (es/insert (editor-state doc (selection [(big-dec 1) 14])) to-insert))
           {:changed-indices #{(big-dec 1)}
            :inserted-indices #{(big-dec 1.5) (big-dec 1.75)}
            :deleted-indices #{}})))

  (testing "inserting a plain string"
    (is (= (es/insert (editor-state doc (selection [(big-dec 1) 3])) "inserted")
           (map->EditorState {:doc (document [(paragraph [(run "foo" #{:italic})
                                                          (run "inserted")
                                                          (run "bar" #{:bold :italic})
                                                          (run "bizz" #{:italic})
                                                          (run "buzz" #{:bold})])
                                              p2])
                              :selection (selection [(big-dec 1) 11])})))
    (is (= (get-changelist (es/insert (editor-state doc (selection [(big-dec 1) 3])) "inserted"))
           {:changed-indices #{(big-dec 1)}
            :inserted-indices #{}
            :deleted-indices #{}})))

  (testing "inserting a plain string with newlines produces new paragraphs"
    (let [es (es/insert (editor-state doc (selection [(big-dec 1) 3])) "inserted\ninserted2")
          children (-> es :doc :children)
          para1 (nth children 0)
          para2 (nth children 1)
          para3 (nth children 2)]
      (is (= (:runs para1) [(run "foo" #{:italic})
                            (run "inserted")]))
      (is (= (:runs para2) [(run "inserted2")
                            (run "bar" #{:bold :italic})
                            (run "bizz" #{:italic})
                            (run "buzz" #{:bold})]))
      (is (= para3 p2))
      (is (= (-> es :selection) (selection [(big-dec 1.5) 9])))
      (is (= (get-changelist es) {:changed-indices #{(big-dec 1)}
                                  :inserted-indices #{(big-dec 1.5)}
                                  :deleted-indices #{}}))))

  (testing "when given a range-selection, deletes before inserting"
    (is (= (es/insert (editor-state doc (selection [(big-dec 1) 1] [(big-dec 2) 11])) (run "(inserted!)" #{}))
           (map->EditorState {:doc (document [(paragraph [(run "f" #{:italic}), (run "(inserted!)d")])])
                              :selection (selection [(big-dec 1) 12] [(big-dec 1) 12])})))
    (is (= (get-changelist (es/insert (editor-state doc (selection [(big-dec 1) 1] [(big-dec 2) 11])) (run "(inserted!)" #{})))
           {:changed-indices #{(big-dec 1)}
            :deleted-indices #{(big-dec 2)}
            :inserted-indices #{}})))

  (testing "Inserting a Paragraph"
    (is (= (es/insert (editor-state doc (selection [(big-dec 1) 1] [(big-dec 2) 11])) (paragraph [(r/run "inserted")]))
           (map->EditorState {:doc (document [(paragraph [(run "f" #{:italic}), (run "insertedd")])])
                              :selection (selection [(big-dec 1) 9] [(big-dec 1) 9])})))
    (is (= (get-changelist (es/insert (editor-state doc (selection [(big-dec 1) 1] [(big-dec 2) 11])) (paragraph [(r/run "inserted")])))
           {:changed-indices #{(big-dec 1)}
            :deleted-indices #{(big-dec 2)}
            :inserted-indices #{}})))

  (testing "Inserting a Document"
    (let [es (es/insert (editor-state doc (selection [(big-dec 1) 1] [(big-dec 2) 11]))
                        (document [(paragraph [(r/run "inserted1")])
                                   (paragraph [(r/run "inserted2")])
                                   (paragraph [(r/run "inserted3")])]))]
      (is (= es (map->EditorState {:doc (document [(paragraph [(run "f" #{:italic}), (run "inserted1")])
                                                   (paragraph [(r/run "inserted2")])
                                                   (paragraph [(r/run "inserted3d")])])
                                   :selection (selection [(big-dec 3) 9])})))
      (is (= (get-changelist es)
             {:changed-indices #{(big-dec 1) (big-dec 2)}
              :inserted-indices #{(big-dec 3)}
              :deleted-indices #{}}))))

  (testing "throws when out of range of paragraph"
    (is (thrown?
         js/Error
         (es/insert (editor-state doc (selection [(big-dec 1) 55])) (run "Goodbye!" #{:italic}))))))

(deftest delete-single-test
  (testing "does nothing at beginning of doc"
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 0])))
           (map->EditorState {:doc doc
                              :selection (selection [(big-dec 1) 0])})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 0]))))
           {:changed-indices #{}
            :deleted-indices #{}
            :inserted-indices #{}})))

  (testing "deletes single char in middle of paragraph"
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 1])))
           (map->EditorState {:doc (document [(paragraph [(run "oo" #{:italic})
                                                          (run "bar" #{:bold :italic})
                                                          (run "bizz" #{:italic})
                                                          (run "buzz" #{:bold})])
                                              p2])
                              :selection (selection [(big-dec 1) 0] [(big-dec 1) 0] :formats #{:italic})})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 1]))))
           {:changed-indices #{(big-dec 1)}
            :inserted-indices #{}
            :deleted-indices #{}})))

  (testing "deletes single char at end of paragraph"
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 14])))
           (map->EditorState {:doc (document [(paragraph [(run "foo" #{:italic})
                                                          (run "bar" #{:bold :italic})
                                                          (run "bizz" #{:italic})
                                                          (run "buz" #{:bold})])
                                              p2])
                              :selection (selection [(big-dec 1) 13] [(big-dec 1) 13] :formats #{:bold})})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 14]))))
           {:changed-indices #{(big-dec 1)}
            :inserted-indices #{}
            :deleted-indices #{}})))

  (testing "merges paragraphs when backspacing from start of paragraph that is not first"
    (is (= (es/delete (editor-state doc (selection [(big-dec 2) 0])))
           (map->EditorState {:doc (document [(paragraph (concat (:runs p1) (:runs p2)))])
                              :selection (selection [(big-dec 1) 14] [(big-dec 1) 14] :formats #{:bold})})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 2) 0]))))
           {:changed-indices #{(big-dec 1)}
            :deleted-indices #{(big-dec 2)}
            :inserted-indices #{}})))

  (testing "merges paragraphs when backspacing from start of paragraph that is not first"
    (is (= (es/delete (editor-state doc (selection [(big-dec 2) 0])))
           (map->EditorState {:doc (document [(paragraph (concat (:runs p1) (:runs p2)))])
                              :selection (selection [(big-dec 1) 14] [(big-dec 1) 14] :formats #{:bold})})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 2) 0]))))
           {:changed-indices #{(big-dec 1)}
            :deleted-indices #{(big-dec 2)}
            :inserted-indices #{}})))

  (testing "deletes single char as normal at end of the paragraph"
    (is (= (es/delete (editor-state doc (selection [(big-dec 2) 12])))
           (map->EditorState {:doc (document [p1, (paragraph [(run "aaabbbcccdd")])])
                              :selection (selection [(big-dec 2) 11])})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 2) 12]))))
           {:changed-indices #{(big-dec 2)}
            :deleted-indices #{}
            :inserted-indices #{}})))

  (testing "deletes single char as normal at end of the paragraph"
    (is (= (es/delete (editor-state doc (selection [(big-dec 2) 12])))
           (map->EditorState {:doc (document [p1, (paragraph [(run "aaabbbcccdd")])])
                              :selection (selection [(big-dec 2) 11])})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 2) 12]))))
           {:changed-indices #{(big-dec 2)}
            :deleted-indices #{}
            :inserted-indices #{}})))

  (testing "does nothing when backspacing at start of first paragraph"
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 0])))
           (map->EditorState {:doc doc
                              :selection (selection [(big-dec 1) 0])})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 0]))))
           {:changed-indices #{}
            :inserted-indices #{}
            :deleted-indices #{}}))))

(deftest delete-range-test
  (testing "deletes from start of paragraph"
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 0] [(big-dec 1) 3])))
           (map->EditorState {:doc (document [(paragraph [(run "bar" #{:bold :italic})
                                                          (run "bizz" #{:italic})
                                                          (run "buzz" #{:bold})])
                                              p2])
                              :selection (selection [(big-dec 1) 0] [(big-dec 1) 0] :formats #{:bold :italic})})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 0] [(big-dec 1) 3]))))
           {:changed-indices #{(big-dec 1)}
            :inserted-indices #{}
            :deleted-indices #{}})))

  (testing "deletes from start of paragraph backwards"
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 0] [(big-dec 1) 3] :backwards? true)))
           (map->EditorState {:doc (document [(paragraph [(run "bar" #{:bold :italic})
                                                          (run "bizz" #{:italic})
                                                          (run "buzz" #{:bold})])
                                              p2])
                              :selection (selection [(big-dec 1) 0] [(big-dec 1) 0] :formats #{:bold :italic})})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 0] [(big-dec 1) 3] :backwards? true))))
           {:changed-indices #{(big-dec 1)}
            :inserted-indices #{}
            :deleted-indices #{}})))

  (testing "deletes up to end of paragraph"
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 3] [(big-dec 1) 14])))
           (map->EditorState {:doc (document [(paragraph [(run "foo" #{:italic})]), p2])
                              :selection (selection [(big-dec 1) 3] [(big-dec 1) 3] :formats #{:italic})})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 3] [(big-dec 1) 14]))))
           {:changed-indices #{(big-dec 1)}
            :inserted-indices #{}
            :deleted-indices #{}})))

  (testing "deletes whole paragraph"
    ;; This is an odd edge case, but handling it this way makes the code simpler.
    ;; The reason it's like this is because the code merges the paragraph at the end
    ;; of the range selection with the paragraph at the beginning of the range selection.
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 0] [(big-dec 2) 0])))
           (map->EditorState {:doc (document [p2])
                              :selection (selection [(big-dec 1) 0])})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 0] [(big-dec 2) 0]))))
           {:changed-indices #{(big-dec 1)}
            :deleted-indices #{(big-dec 2)}
            :inserted-indices #{}})))

  (testing "merges start and ending paragraphs when deleting across paragraphs"
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 3] [(big-dec 2) 3])))
           (map->EditorState {:doc (document [(paragraph [(run "foo" #{:italic}), (run "bbbcccddd")])])
                              :selection (selection [(big-dec 1) 3] [(big-dec 1) 3] :formats #{:italic})})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 3] [(big-dec 2) 3]))))
           {:changed-indices #{(big-dec 1)}
            :deleted-indices #{(big-dec 2)}
            :inserted-indices #{}})))

  (testing "merges start and ending paragraphs when deleting across more than 2 paragraphs"
    (is (= (es/delete (editor-state long-doc (selection [(big-dec 1) 4] [(big-dec 4) 0])))
           (map->EditorState {:doc (document [(paragraph [(run "foo1" #{:italic}), (run "foo4" #{:strike})])])
                              :selection (selection [(big-dec 1) 4] [(big-dec 1) 4] :formats #{:italic})})))
    (is (= (get-changelist (es/delete (editor-state long-doc (selection [(big-dec 1) 4] [(big-dec 4) 0]))))
           {:changed-indices #{(big-dec 1)}
            :deleted-indices #{(big-dec 2) (big-dec 3) (big-dec 4)}
            :inserted-indices #{}})))

  (testing "deletes whole document"
    (is (= (es/delete (editor-state doc (selection [(big-dec 1) 0] [(big-dec 2) 12])))
           (map->EditorState {:doc (document [(paragraph [(run)])])
                              :selection (selection [(big-dec 1) 0])})))
    (is (= (get-changelist (es/delete (editor-state doc (selection [(big-dec 1) 0] [(big-dec 2) 12]))))
           {:changed-indices #{(big-dec 1)}
            :deleted-indices #{(big-dec 2)}
            :inserted-indices #{}}))))

(deftest enter-test
  (testing "works at start of paragraph"
    (is (= (es/enter (editor-state doc (selection [(big-dec 1) 0])))
           (map->EditorState {:doc (document [(paragraph [(run)]), p1, p2])
                              :selection (selection [(big-dec 1) 0])})))
    (is (= (get-changelist (es/enter (editor-state doc (selection [(big-dec 1) 0]))))
           {:inserted-indices #{(big-dec 0.5)}
            :deleted-indices #{}
            :changed-indices #{}})))

  (testing "works at end of paragraph"
    (is (= (es/enter (editor-state doc (selection [(big-dec 1) 14])))
           (map->EditorState {:doc (document [p1, (paragraph [(run)]), p2])
                              :selection (selection [(big-dec 1.5) 0])})))
    (is (= (get-changelist (es/enter (editor-state doc (selection [(big-dec 1) 14]))))
           {:inserted-indices #{(big-dec 1.5)}
            :changed-indices #{}
            :deleted-indices #{}})))

  (testing "works in middle of paragraph"
    (is (= (es/enter (editor-state doc (selection [(big-dec 1) 3])))
           (map->EditorState {:doc (document [(paragraph [(run "foo" #{:italic})])
                                              (paragraph [(run "bar" #{:bold :italic})
                                                          (run "bizz" #{:italic})
                                                          (run "buzz" #{:bold})])
                                              p2])
                              :selection (selection [(big-dec 1.5) 0] [(big-dec 1.5) 0] :formats #{:bold :italic})})))
    (is (= (get-changelist (es/enter (editor-state doc (selection [(big-dec 1) 3]))))
           {:changed-indices #{(big-dec 1)}
            :inserted-indices #{(big-dec 1.5)}
            :deleted-indices #{}})))

  (testing "works at end of doc"
    (is (= (es/enter (editor-state doc (selection [(big-dec 2) 12])))
           (map->EditorState {:doc (document [p1, p2, (paragraph [(run)])])
                              :selection (selection [(big-dec 3) 0])})))
    (is (= (get-changelist (es/enter (editor-state doc (selection [(big-dec 2) 12]))))
           {:inserted-indices #{(big-dec 3)}
            :changed-indices #{}
            :deleted-indices #{}})))

  (testing "works with range selection"
    (is (= (es/enter (editor-state doc (selection [(big-dec 2) 0] [(big-dec 2) 12])))
           (map->EditorState {:doc (document [p1, (p/paragraph), (p/paragraph)])
                              :selection (selection [(big-dec 3) 0])})))
    (is (= (get-changelist (es/enter (editor-state doc (selection [(big-dec 2) 0] [(big-dec 2) 12]))))
           {:inserted-indices #{(big-dec 3)}
            :changed-indices #{(big-dec 2)}
            :deleted-indices #{}}))))

(deftest auto-surround-test
  (testing "wraps cursor in opening and closing for single selection"
    (is (= (es/auto-surround (editor-state doc (selection [(big-dec 2) 3])) "(" ")")
           (map->EditorState {:doc (document [p1, (p/paragraph [(r/run "aaa()bbbcccddd")])])
                              :selection (selection [(big-dec 2) 4])})))
    (is (= (get-changelist (es/auto-surround (editor-state doc (selection [(big-dec 2) 3])) "(" ")"))
           (dll/create-changelist :changed-indices #{(big-dec 2)}))))
  (testing "surrounds selection with opening and closing for range selection"
    (is (= (es/auto-surround (editor-state doc (selection [(big-dec 1) 0] [(big-dec 2) 3])) "(" ")")
           (map->EditorState {:doc (document [(paragraph [(run "(")
                                                          (run "foo" #{:italic})
                                                          (run "bar" #{:bold :italic})
                                                          (run "bizz" #{:italic})
                                                          (run "buzz" #{:bold})])
                                              (p/paragraph [(r/run "aaa)bbbcccddd")])])
                              :selection (selection [(big-dec 1) 1] [(big-dec 2) 3])})))
    (is (= (get-changelist (es/auto-surround (editor-state doc (selection [(big-dec 1) 0] [(big-dec 2) 3])) "(" ")"))
           (dll/create-changelist :changed-indices #{(big-dec 1) (big-dec 2)})))))

(deftest nav-functions-test
  (testing "start and end work"
    (is (= (nav/start (editor-state doc (selection [(big-dec 1) 3])))
           (editor-state doc (selection [(big-dec 1) 0] [(big-dec 1) 0] :formats #{:italic}))))
    (is (= (nav/start (editor-state doc (selection [(big-dec 1) 0] [(big-dec 2) 3])))
           (editor-state doc (selection [(big-dec 1) 0] [(big-dec 1) 0] :formats #{:italic}))))
    (is (= (nav/end (editor-state doc (selection [(big-dec 1) 3])))
           (editor-state doc (selection [(big-dec 2) (sl/len p2)]))))
    (is (= (nav/end (editor-state doc (selection [(big-dec 1) 0] [(big-dec 2) 3])))
           (editor-state doc (selection [(big-dec 2) (sl/len p2)]))))
    (is (= (nav/end (editor-state doc (selection [(big-dec 2) 3])))
           (editor-state doc (selection [(big-dec 2) (sl/len p2)])))))

  ;; The rest of these currently all use the same fallthrough function,
  ;; so testing one is basically the same as testing all of them.
  (testing "rest work"
    (is (= (nav/next-char (editor-state doc (selection [(big-dec 1) 0])))
           (editor-state doc (selection [(big-dec 1) 1] [(big-dec 1) 1] :formats #{:italic}))))
    (is (= (nav/next-char (editor-state doc (selection [(big-dec 1) 14] [(big-dec 1) 14] :formats #{:bold})))
           (editor-state doc (selection [(big-dec 2) 0]))))
    (is (= (nav/next-char (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 3) 4])))
           (editor-state long-doc (selection [(big-dec 3) 4] [(big-dec 3) 4] :formats #{:underline}))))))

(deftest selectable-functions-test
  (testing "shift+right works forwards (or single)"
    (is (= (nav/shift+right (editor-state long-doc (selection [(big-dec 1) 0])))
           (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 1) 1], :formats #{:italic}))))
    (is (= (nav/shift+right (editor-state long-doc (selection [(big-dec 1) 4])))
           (editor-state long-doc (selection [(big-dec 1) 4] [(big-dec 2) 0]))))

    (is (= (nav/shift+right (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 1) 4])))
           (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 2) 0] :formats #{:italic}))))
    (is (= (nav/shift+right (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 3) 4])))
           (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 4) 0])))))

  (testing "shift+right works backwards"
    (is (= (nav/shift+right (editor-state long-doc (selection [(big-dec 1) 4] [(big-dec 2) 4] :backwards? true)))
           (editor-state long-doc (selection [(big-dec 2) 0] [(big-dec 2) 4] :backwards? true, :formats #{:bold}))))
    (is (= (nav/shift+right (editor-state long-doc (selection [(big-dec 1) 4] [(big-dec 3) 4] :backwards? true)))
           (editor-state long-doc (selection [(big-dec 2) 0] [(big-dec 3) 4] :backwards? true)))))

  (testing "shift+left works forwards"
    (is (= (nav/shift+left (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 2) 0])))
           (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 1) 4], :formats #{:italic}))))
    (is (= (nav/shift+left (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 3) 0])))
           (editor-state long-doc (selection [(big-dec 1) 0] [(big-dec 2) 4])))))

  (testing "shift+left works backwards (or single)"
    (is (= (nav/shift+left (editor-state long-doc (selection [(big-dec 1) 4])))
           (editor-state long-doc (selection [(big-dec 1) 3] [(big-dec 1) 4], :backwards? true, :formats #{:italic}))))
    (is (= (nav/shift+left (editor-state long-doc (selection [(big-dec 2) 0])))
           (editor-state long-doc (selection [(big-dec 1) 4] [(big-dec 2) 0], :backwards? true))))

    (is (= (nav/shift+left (editor-state long-doc (selection [(big-dec 2) 0] [(big-dec 4) 4], :backwards? true)))
           (editor-state long-doc (selection [(big-dec 1) 4] [(big-dec 4) 4], :backwards? true))))
    (is (= (nav/shift+left (editor-state long-doc (selection [(big-dec 2) 0] [(big-dec 2) 4], :backwards? true)))
           (editor-state long-doc (selection [(big-dec 1) 4] [(big-dec 2) 4], :backwards? true, :formats #{:bold}))))))

(deftest select-whole-word-test
  (is (= (es/select-whole-word (editor-state (doc/document [(p/paragraph [(r/run "\t\"And so he said, like....hello world!\"")])])
                                             (selection [(big-dec 1) 2])))
         (editor-state (doc/document [(p/paragraph [(r/run "\t\"And so he said, like....hello world!\"")])])
                       (selection [(big-dec 1) 2] [(big-dec 1) 5]))))
  (is (= (es/select-whole-word (editor-state (doc/document [(p/paragraph [(r/run "\t\"And so he said, like....hello world!\"")])])
                                             (selection [(big-dec 1) 0])))
         (editor-state (doc/document [(p/paragraph [(r/run "\t\"And so he said, like....hello world!\"")])])
                       (selection [(big-dec 1) 0] [(big-dec 1) 1]))))
  (is (= (es/select-whole-word (editor-state (doc/document [(p/paragraph [(r/run "\t\"And so he said, like....hello world!\"")])])
                                             (selection [(big-dec 1) 12])))
         (editor-state (doc/document [(p/paragraph [(r/run "\t\"And so he said, like....hello world!\"")])])
                       (selection [(big-dec 1) 12] [(big-dec 1) 16])))))

(deftest next-paragraph-test
  (is (= (nav/next-paragraph (editor-state (document [(paragraph [(run "foo")])
                                                      (paragraph [(run "bar")])])
                                           (selection [(big-dec 1) 0])))
         (editor-state (document [(paragraph [(run "foo")])
                                  (paragraph [(run "bar")])])
                       (selection [(big-dec 1) 3]))))
  (is (= (nav/next-paragraph (editor-state (document [(paragraph [(run "foo")])
                                                      (paragraph [(run "bar")])])
                                           (selection [(big-dec 1) 3])))
         (editor-state (document [(paragraph [(run "foo")])
                                  (paragraph [(run "bar")])])
                       (selection [(big-dec 2) 0]))))
  (is (= (nav/next-paragraph (editor-state (document [(paragraph [(run "foo")])
                                                      (paragraph [(run "bar")])])
                                           (selection [(big-dec 2) 0])))
         (editor-state (document [(paragraph [(run "foo")])
                                  (paragraph [(run "bar")])])
                       (selection [(big-dec 2) 3]))))
  (is (= (nav/next-paragraph (editor-state (document [(paragraph [(run "foo")])
                                                      (paragraph [(run "bar")])])
                                           (selection [(big-dec 2) 3])))
         (editor-state (document [(paragraph [(run "foo")])
                                  (paragraph [(run "bar")])])
                       (selection [(big-dec 2) 3])))))

(deftest prev-paragraph-test
  (is (= (nav/prev-paragraph (editor-state (document [(paragraph [(run "foo")])
                                                      (paragraph [(run "bar")])])
                                           (selection [(big-dec 2) 3])))
         (editor-state (document [(paragraph [(run "foo")])
                                  (paragraph [(run "bar")])])
                       (selection [(big-dec 2) 0]))))
  (is (= (nav/prev-paragraph (editor-state (document [(paragraph [(run "foo")])
                                                      (paragraph [(run "bar")])])
                                           (selection [(big-dec 2) 0])))
         (editor-state (document [(paragraph [(run "foo")])
                                  (paragraph [(run "bar")])])
                       (selection [(big-dec 1) 3]))))
  (is (= (nav/prev-paragraph (editor-state (document [(paragraph [(run "foo")])
                                                      (paragraph [(run "bar")])])
                                           (selection [(big-dec 1) 3])))
         (editor-state (document [(paragraph [(run "foo")])
                                  (paragraph [(run "bar")])])
                       (selection [(big-dec 1) 0]))))
  (is (= (nav/prev-paragraph (editor-state (document [(paragraph [(run "foo")])
                                                      (paragraph [(run "bar")])])
                                           (selection [(big-dec 1) 0])))
         (editor-state (document [(paragraph [(run "foo")])
                                  (paragraph [(run "bar")])])
                       (selection [(big-dec 1) 0])))))

;; (deftest selected-content-test
;;   (testing "returns list of runs when passed selection within one paragraph"
;;     (is (= [(run "bar" #{:bold :italic})
;;             (run "bizz" #{:italic})
;;             (run "buzz" #{:bold})]
;;            (sl/selected-content doc (selection [(big-dec 1) 3] [(big-dec 1) 14])))))

;;   (testing "returns list of paragraphs when passed selection across multiple paragraphs"
;;     (is (= [(paragraph [(run "bar" #{:bold :italic})
;;                              (run "bizz" #{:italic})
;;                              (run "buzz" #{:bold})])
;;             (paragraph [(run "aaa")])]
;;            (sl/selected-content doc (selection [(big-dec 1) 3] [(big-dec 2) 3])))))

;;   (testing "returns list of paragraphs when passed selection across multiple (> 3) paragraphs"
;;     (is (= [(paragraph [(run "foo1" #{:italic})])
;;             (paragraph [(run "foo2" #{:bold})])
;;             (paragraph [(run "foo3" #{:underline})])
;;             (paragraph [(run "foo" #{:strike})])]
;;            (sl/selected-content long-doc (selection [(big-dec 1) 0] [(big-dec 4) 3])))))

;;   ;; TODO: I **think** this is the correct implementation here...could be wrong though...
;;   (testing "returns one paragraph and empty next paragraph when going from start of paragraph 1 to start of paragraph 2"
;;     (is (= [(paragraph [(run "foo1" #{:italic})]) (paragraph [])]
;;            (sl/selected-content long-doc (selection [(big-dec 1) 0] [(big-dec 2) 0]))))))

;; (deftest formatting-test
;;   (let [formats-doc (document [(paragraph [(run "foo1" #{:italic})
;;                                                 (run "foo2" #{:italic :bold})
;;                                                 (run "foo3" #{:bold})])
;;                                (paragraph [(run "bar1" #{:italic :bold :underline})])])]
;;     (testing "works inside same paragraph"
;;       (is (= #{:italic} (sl/formatting formats-doc (selection ["f1" 0] ["f1" 8]))))
;;       (is (= #{:italic :bold} (sl/formatting formats-doc (selection ["f1" 4] ["f1" 8]))))
;;       (is (= #{:bold} (sl/formatting formats-doc (selection ["f1" 4] ["f1" 12])))))

;;     (testing "works across paragraphs"
;;       (is (= #{:bold} (sl/formatting formats-doc (selection ["f1" 8] ["f2" 3])))))))

;; ;; TODO NEXT: fix toggle-format tests
;; ;; TODO after that: get new transaction system rendering properly/fix any errors it's caused with view and event handling layers.

;; (deftest toggle-format-test
;;   (testing "toggling single run"
;;     (is (= (sl/toggle-format doc (selection [(big-dec 1) 0] [(big-dec 1) 3]) :italic)
;;            {:doc (document [(paragraph [(run "foo")
;;                                              (run "bar" #{:bold :italic})
;;                                              (run "bizz" #{:italic})
;;                                              (run "buzz" #{:bold})])
;;                             p2])
;;             :selection (selection [(big-dec 1) 0] [(big-dec 1) 3])
;;             :changed-indices #{(big-dec 1)}})))

;;   (testing "toggling across runs WITH shared format"
;;     (is (= (sl/toggle-format doc (selection [(big-dec 1) 0] [(big-dec 1) 10]) :italic)
;;            {:doc (document [(paragraph [(run "foo")
;;                                              (run "bar" #{:bold})
;;                                              (run "bizz" #{})
;;                                              (run "buzz" #{:bold})])
;;                             p2])
;;             :selection (selection [(big-dec 1) 0] [(big-dec 1) 10])
;;             :changed-indices #{(big-dec 1)}})))

;;   (testing "toggling across runs WITH shared format, not on run boundaries"
;;     (is (= (sl/toggle-format doc (selection [(big-dec 1) 1] [(big-dec 1) 8]) :italic)
;;            {:doc (document [(paragraph [(run "f" #{:italic})
;;                                              (run "oo")
;;                                              (run "bar" #{:bold})
;;                                              (run "bi" #{})
;;                                              (run "zz" #{:italic})
;;                                              (run "buzz" #{:bold})])
;;                             p2])
;;             :selection (selection [(big-dec 1) 1] [(big-dec 1) 8])
;;             :changed-indices #{(big-dec 1)}})))

;;   (testing "toggling across runs WITHOUT shared format"
;;     (is (= (sl/toggle-format doc (selection [(big-dec 1) 0] [(big-dec 1) 14]) :italic)
;;            {:doc (document [(paragraph [(run "foo" #{:italic})
;;                                              (run "bar" #{:bold :italic})
;;                                              (run "bizz" #{:italic})
;;                                              (run "buzz" #{:bold :italic})])
;;                             p2])
;;             :selection (selection [(big-dec 1) 0] [(big-dec 1) 14])
;;             :changed-indices #{(big-dec 1)}})))

;;   (testing "toggling across paragraphs WITHOUT shared format"
;;     (is (= (sl/toggle-format doc (selection [(big-dec 1) 0] [(big-dec 2) 12]) :italic)
;;            {:doc (document [(paragraph [(run "foo" #{:italic})
;;                                              (run "bar" #{:bold :italic})
;;                                              (run "bizz" #{:italic})
;;                                              (run "buzz" #{:bold :italic})])
;;                             (paragraph [(run "aaabbbcccddd" #{:italic})])])
;;             :selection (selection [(big-dec 1) 0] [(big-dec 2) 12])
;;             :changed-indices #{(big-dec 1) (big-dec 2)}})))

;;   (testing "toggling across paragraphs WITHOUT shared format, and not landing on run boundaries"
;;     (is (= (sl/toggle-format doc (selection [(big-dec 1) 1] [(big-dec 2) 3]) :italic)
;;            {:doc (document [(paragraph [(run "foo" #{:italic})
;;                                              (run "bar" #{:bold :italic})
;;                                              (run "bizz" #{:italic})
;;                                              (run "buzz" #{:bold :italic})])
;;                             (paragraph [(run "aaa" #{:italic})
;;                                              (run "bbbcccddd")])])
;;             :selection (selection [(big-dec 1) 1] [(big-dec 2) 3])
;;             :changed-indices #{(big-dec 1) (big-dec 2)}})))

;;   (testing "toggling across paragraphs WITH shared format"
;;     (let [modified (-> doc
;;                        (update-in [:children (big-dec 1) :runs 3 :formats] conj :italic)
;;                        (update-in [:children (big-dec 2) :runs 0 :formats] conj :italic))]
;;       (is (= (sl/toggle-format modified (selection [(big-dec 1) 10] [(big-dec 2) 12]) :italic)
;;              {:doc (document [(paragraph [(run "foo" #{:italic})
;;                                                (run "bar" #{:bold :italic})
;;                                                (run "bizz" #{:italic})
;;                                                (run "buzz" #{:bold})])
;;                               (paragraph [(run "aaabbbcccddd")])])
;;               :selection (selection [(big-dec 1) 10] [(big-dec 2) 12])
;;               :changed-indices #{(big-dec 1) (big-dec 2)}})))))

;; (deftest char-at-test
;;   (testing "works in 1st paragraph"
;;     (is (= "f" (sl/char-at doc (selection [(big-dec 1) 0]))))
;;     (is (= "o" (sl/char-at doc (selection [(big-dec 1) 1]))))
;;     (is (= "z" (sl/char-at doc (selection [(big-dec 1) 13]))))
;;     (is (thrown? js/Error (sl/char-at doc (selection [(big-dec 1) 14])))))

;;   (testing "works in other paragraphs"
;;     (is (= "a" (sl/char-at doc (selection [(big-dec 2) 0]))))
;;     (is (= "b" (sl/char-at doc (selection [(big-dec 2) 3]))))
;;     (is (= "c" (sl/char-at doc (selection [(big-dec 2) 7]))))
;;     (is (= "d" (sl/char-at doc (selection [(big-dec 2) 11]))))
;;     (is (thrown? js/Error (sl/char-at doc (selection [(big-dec 2) 12]))))))

;; (deftest char-before-test
;;   (testing "works in 1st paragraph"
;;     (is (= "\n" (sl/char-before doc (selection [(big-dec 1) 0]))))
;;     (is (= "f" (sl/char-before doc (selection [(big-dec 1) 1]))))
;;     (is (= "o" (sl/char-before doc (selection [(big-dec 1) 2]))))
;;     (is (= "z" (sl/char-before doc (selection [(big-dec 1) 13]))))
;;     (is (= "z" (sl/char-before doc (selection [(big-dec 1) 14])))))

;;   (testing "works in other paragraphs"
;;     (is (= "\n" (sl/char-before doc (selection [(big-dec 2) 0]))))
;;     (is (= "a" (sl/char-before doc (selection [(big-dec 2) 1]))))
;;     (is (= "a" (sl/char-before doc (selection [(big-dec 2) 3]))))
;;     (is (= "b" (sl/char-before doc (selection [(big-dec 2) 4]))))
;;     (is (= "c" (sl/char-before doc (selection [(big-dec 2) 7]))))
;;     (is (= "d" (sl/char-before doc (selection [(big-dec 2) 11]))))
;;     (is (= "d" (sl/char-before doc (selection [(big-dec 2) 12]))))
;;     (is (thrown? js/Error (sl/char-before doc (selection ["[2]" 13]))))))
