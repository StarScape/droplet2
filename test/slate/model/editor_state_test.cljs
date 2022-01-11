(ns slate.model.editor-state-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.common :as sl]
            [slate.model.run :as r :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.editor-state :as state :refer [editor-state
                                                        changelist
                                                        ->EditorUpdate
                                                        map->EditorState]]
            [slate.model.navigation :as nav]
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
    (is (= (sl/insert (editor-state doc (selection ["p1" 3])) [(run "Hello" #{:italic}) (run "Goodbye!")])
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "fooHello" #{:italic})
                                                                (run "Goodbye!" #{})
                                                                (run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})])
                                               p2])
                               :selection (selection ["p1" 16])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "insert single run in middle of a paragraph"
    (is (= (sl/insert (editor-state doc (selection ["p1" 3])) (run "Goodbye!"))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "foo" #{:italic})
                                                                (run "Goodbye!" #{})
                                                                (run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})])
                                               p2])
                               :selection (selection ["p1" 11])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "insert run at start of paragraph"
    (is (= (sl/insert (editor-state doc (selection ["p1" 0])) (run "Hello!"))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "Hello!" #{})
                                                                (run "foo" #{:italic})
                                                                (run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})])
                                               p2])
                               :selection (selection ["p1" 6])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "insert run at end of paragraph"
    (is (= (sl/insert (editor-state doc (selection ["p2" 12])) (run "Goodbye!" #{:italic}))
           (->EditorUpdate
            (map->EditorState {:doc (document [p1, (paragraph "p2" [(run "aaabbbcccddd") (run "Goodbye!" #{:italic})])])
                               :selection (selection ["p2" 20])})
            {:changed-uuids #{"p2"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "multi-paragraph insert in the middle of a single paragraph"
    (is (= (sl/insert (editor-state doc (selection ["p1" 10])) to-insert)
           (sl/insert (editor-state doc (selection ["p1" 10])) (into (dll) to-insert))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "foo" #{:italic})
                                                                (run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "inserted paragraph 1")])
                                               (paragraph "i2" [(run "inserted paragraph 2")])
                                               (paragraph "i3" [(run "inserted paragraph 3")
                                                                (run "buzz" #{:bold})])
                                               p2])
                               :selection (selection ["i3" 20])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{"i2" "i3"}
             :deleted-uuids #{}}))))

  (testing "multi-paragraph insert at the start of a paragraph"
    (is (= (sl/insert (editor-state doc (selection ["p2" 0])) to-insert)
           (sl/insert (editor-state doc (selection ["p2" 0])) (into (dll) to-insert))
           (->EditorUpdate
            (map->EditorState {:doc (document [p1
                                               (paragraph "p2" [(run "inserted paragraph 1")])
                                               (paragraph "i2" [(run "inserted paragraph 2")])
                                               (paragraph "i3" [(run "inserted paragraph 3aaabbbcccddd")])])
                               :selection (selection ["i3" 20])})
            {:changed-uuids #{"p2"}
             :inserted-uuids #{"i2" "i3"}
             :deleted-uuids #{}}))))

  (testing "multi-paragraph insert at the end of a paragraph"
    (is (= (sl/insert (editor-state doc (selection ["p1" 14])) to-insert)
           (sl/insert (editor-state doc (selection ["p1" 14])) (into (dll) to-insert))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "foo" #{:italic})
                                                                (run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})
                                                                (run "inserted paragraph 1")])
                                               (paragraph "i2" [(run "inserted paragraph 2")])
                                               (paragraph "i3" [(run "inserted paragraph 3")])
                                               p2])
                               :selection (selection ["i3" 20])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{"i2" "i3"}
             :deleted-uuids #{}}))))

  (testing "inserting a plain string"
    (is (= (sl/insert (editor-state doc (selection ["p1" 3])) "inserted")
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "foo" #{:italic})
                                                                (run "inserted")
                                                                (run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})])
                                               p2])
                               :selection (selection ["p1" 11])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "when given a range-selection, deletes before inserting"
    (is (= (sl/insert (editor-state doc (selection ["p1" 1] ["p2" 11])) (run "(inserted!)" #{}))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "f" #{:italic}), (run "(inserted!)d")])])
                               :selection (selection ["p1" 12])})
            {:changed-uuids #{"p1"}
             :deleted-uuids #{"p2"}
             :inserted-uuids #{}}))))

  (testing "throws when out of range of paragraph"
    (is (thrown?
         js/Error
         (sl/insert (editor-state doc (selection ["p1" 55])) (run "Goodbye!" #{:italic}))))))

(deftest delete-single-test
  (testing "does nothing at beginning of doc"
    (is (= (sl/delete (editor-state doc (selection ["p1" 0])))
           (->EditorUpdate
            (map->EditorState {:doc doc
                               :selection (selection ["p1" 0])})
            {:changed-uuids #{}
             :deleted-uuids #{}
             :inserted-uuids #{}}))))

  (testing "deletes single char in middle of paragraph"
    (is (= (sl/delete (editor-state doc (selection ["p1" 1])))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "oo" #{:italic})
                                                                (run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})])
                                               p2])
                               :selection (selection ["p1" 0])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "deletes single char at end of paragraph"
    (is (= (sl/delete (editor-state doc (selection ["p1" 14])))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "foo" #{:italic})
                                                                (run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buz" #{:bold})])
                                               p2])
                               :selection (selection ["p1" 13])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "merges paragraphs when backspacing from start of paragraph that is not first"
    (is (= (sl/delete (editor-state doc (selection ["p2" 0])))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" (concat (:runs p1) (:runs p2)))])
                               :selection (selection ["p1" 14])})
            {:changed-uuids #{"p1"}
             :deleted-uuids #{"p2"}
             :inserted-uuids #{}}))))

  (testing "deletes single char as normal at end of the paragraph"
    (is (= (sl/delete (editor-state doc (selection ["p2" 12])))
           (->EditorUpdate
            (map->EditorState {:doc (document [p1, (paragraph "p2" [(run "aaabbbcccdd")])])
                               :selection (selection ["p2" 11])})
            {:changed-uuids #{"p2"}
             :deleted-uuids #{}
             :inserted-uuids #{}}))))

  (testing "does nothing when backspacing at start of first paragraph"
    (is (= (sl/delete (editor-state doc (selection ["p1" 0])))
           (->EditorUpdate
            (map->EditorState {:doc doc
                               :selection (selection ["p1" 0])})
            {:changed-uuids #{}
             :inserted-uuids #{}
             :deleted-uuids #{}})))))

(deftest delete-range-test
  (testing "deletes from start of paragraph"
    (is (= (sl/delete (editor-state doc (selection ["p1" 0] ["p1" 3])))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})])
                                               p2])
                               :selection (selection ["p1" 0])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "deletes from start of paragraph backwards"
    (is (= (sl/delete (editor-state doc (selection ["p1" 0] ["p1" 3] :backwards? true)))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})])
                                               p2])
                               :selection (selection ["p1" 0])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "deletes up to end of paragraph"
    (is (= (sl/delete (editor-state doc (selection ["p1" 3] ["p1" 14])))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "foo" #{:italic})]), p2])
                               :selection (selection ["p1" 3])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{}
             :deleted-uuids #{}}))))

  (testing "deletes whole paragraph"
    ;; This is an odd edge case, but handling it this way makes the code simpler.
    ;; The reason it's like this is because the code merges the paragraph at the end
    ;; of the range selection with the paragraph at the beginning of the range selection,
    ;; and gives it the UUID of the first.
    (is (= (sl/delete (editor-state doc (selection ["p1" 0] ["p2" 0])))
           (->EditorUpdate
            (map->EditorState {:doc (document [(assoc p2 :uuid "p1")])
                               :selection (selection ["p1" 0])})
            {:changed-uuids #{"p1"}
             :deleted-uuids #{"p2"}
             :inserted-uuids #{}}))))

  (testing "merges start and ending paragraphs when deleting across paragraphs"
    (is (= (sl/delete (editor-state doc (selection ["p1" 3] ["p2" 3])))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "foo" #{:italic}), (run "bbbcccddd")])])
                               :selection (selection ["p1" 3])})
            {:changed-uuids #{"p1"}
             :deleted-uuids #{"p2"}
             :inserted-uuids #{}}))))

  (testing "merges start and ending paragraphs when deleting across more than 2 paragraphs"
    (is (= (sl/delete (editor-state long-doc (selection ["d1" 4] ["d4" 0])))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "d1" [(run "foo1" #{:italic}), (run "foo4" #{:strike})])])
                               :selection (selection ["d1" 4])})
            {:changed-uuids #{"d1"}
             :deleted-uuids #{"d2" "d3" "d4"}
             :inserted-uuids #{}}))))

  (testing "deletes whole document"
    (is (= (sl/delete (editor-state doc (selection ["p1" 0] ["p2" 12])))
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run)])])
                               :selection (selection ["p1" 0])})
            {:changed-uuids #{"p1"}
             :deleted-uuids #{"p2"}
             :inserted-uuids #{}})))))

(deftest enter-test
  (testing "works at start of paragraph"
    (is (= (state/enter (editor-state doc (selection ["p1" 0])) "e1")
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "e1" [(run)]), p1, p2])
                               :selection (selection ["p1" 0])})
            {:inserted-uuids #{"e1"}
             :deleted-uuids #{}
             :changed-uuids #{"p1"}}))))

  (testing "works at end of paragraph"
    (is (= (state/enter (editor-state doc (selection ["p1" 14])) "e1")
           (->EditorUpdate
            (map->EditorState {:doc (document [p1, (paragraph "e1" [(run)]), p2])
                               :selection (selection ["e1" 0])})
            {:inserted-uuids #{"e1"}
             :changed-uuids #{"p1"}
             :deleted-uuids #{}}))))

  (testing "works in middle of paragraph"
    (is (= (state/enter (editor-state doc (selection ["p1" 3])) "e1")
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "foo" #{:italic})])
                                               (paragraph "e1" [(run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})])
                                               p2])
                               :selection (selection ["e1" 0])})
            {:changed-uuids #{"p1"}
             :inserted-uuids #{"e1"}
             :deleted-uuids #{}}))))

  (testing "works at end of doc"
    (is (= (state/enter (editor-state doc (selection ["p2" 12])) "e1")
           (->EditorUpdate
            (map->EditorState {:doc (document [p1, p2, (paragraph "e1" [(run)])])
                               :selection (selection ["e1" 0])})
            {:inserted-uuids #{"e1"}
             :changed-uuids #{"p2"}
             :deleted-uuids #{}}))))

  (testing "works with range selection"
    (is (= (state/enter (editor-state doc (selection ["p2" 0] ["p2" 12])) "e1")
           (->EditorUpdate
            (map->EditorState {:doc (document [p1, (p/empty-paragraph "e1"), (p/empty-paragraph "p2")])
                               :selection (selection ["p2" 0])})
            {:inserted-uuids #{"e1"}
             :changed-uuids #{"p2"}
             :deleted-uuids #{}})))))

(deftest auto-surround-test
  (testing "wraps cursor in opening and closing for single selection"
    (is (= (state/auto-surround (editor-state doc (selection ["p2" 3])) "(" ")")
           (->EditorUpdate
            (map->EditorState {:doc (document [p1, (p/paragraph "p2" [(r/run "aaa()bbbcccddd")])])
                               :selection (selection ["p2" 4])})
            (changelist :changed-uuids #{"p2"})))))
  (testing "surrounds selection with opening and closing for range selection"
    (is (= (state/auto-surround (editor-state doc (selection ["p1" 0] ["p2" 3])) "(" ")")
           (->EditorUpdate
            (map->EditorState {:doc (document [(paragraph "p1" [(run "(")
                                                                (run "foo" #{:italic})
                                                                (run "bar" #{:bold :italic})
                                                                (run "bizz" #{:italic})
                                                                (run "buzz" #{:bold})])
                                               (p/paragraph "p2" [(r/run "aaa)bbbcccddd")])])
                               :selection (selection ["p1" 1] ["p2" 3])})
            (changelist :changed-uuids #{"p1" "p2"}))))))

(deftest nav-functions-test
  (testing "start and end work"
    (is (= (nav/start (editor-state doc (selection ["p1" 3])))
           (->EditorUpdate (editor-state doc (selection ["p1" 0]))
                           (changelist :changed-uuids #{"p1"}))))
    (is (= (nav/start (editor-state doc (selection ["p1" 0] ["p2" 3])))
           (->EditorUpdate (editor-state doc (selection ["p1" 0]))
                           (changelist :changed-uuids #{"p1" "p2"}))))
    (is (= (nav/end (editor-state doc (selection ["p1" 3])))
           (->EditorUpdate (editor-state doc (selection ["p2" (sl/len p2)]))
                           (changelist :changed-uuids #{"p1" "p2"}))))
    (is (= (nav/end (editor-state doc (selection ["p1" 0] ["p2" 3])))
           (->EditorUpdate (editor-state doc (selection ["p2" (sl/len p2)]))
                           (changelist :changed-uuids #{"p1" "p2"}))))
    (is (= (nav/end (editor-state doc (selection ["p2" 3])))
           (->EditorUpdate (editor-state doc (selection ["p2" (sl/len p2)]))
                           (changelist :changed-uuids #{"p2"})))))

  ;; The rest of these currently all use the same fallthrough function,
  ;; so testing one is basically the same as testing all of them.
  (testing "rest work"
    (is (= (nav/next-char (editor-state doc (selection ["p1" 0])))
           (->EditorUpdate (editor-state doc (selection ["p1" 1]))
                           (changelist :changed-uuids #{"p1"}))))
    (is (= (nav/next-char (editor-state doc (selection ["p1" 14])))
           (->EditorUpdate (editor-state doc (selection ["p2" 0]))
                           (changelist :changed-uuids #{"p1" "p2"}))))
    (is (= (nav/next-char (editor-state long-doc (selection ["d1" 0] ["d3" 4] :between #{"d2"})))
           (->EditorUpdate (editor-state long-doc (selection ["d3" 4]))
                           (changelist :changed-uuids #{"d1" "d2" "d3"}))))))

(deftest selectable-functions-test
  (testing "shift+right works forwards (or single)"
    (is (= (nav/shift+right (editor-state long-doc (selection ["d1" 0])))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 0] ["d1" 1]))
                           (changelist :changed-uuids #{"d1"}))))
    (is (= (nav/shift+right (editor-state long-doc (selection ["d1" 4])))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 4] ["d2" 0]))
                           (changelist :changed-uuids #{"d1" "d2"}))))

    (is (= (nav/shift+right (editor-state long-doc (selection ["d1" 0] ["d1" 4])))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 0] ["d2" 0]))
                           (changelist :changed-uuids #{"d1" "d2"}))))
    (is (= (nav/shift+right (editor-state long-doc (selection ["d1" 0] ["d3" 4] :between #{"d2"})))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 0] ["d4" 0] :between #{"d2" "d3"}))
                           (changelist :changed-uuids #{"d3" "d4"})))))

  (testing "shift+right works backwards"
    (is (= (nav/shift+right (editor-state long-doc (selection ["d1" 4] ["d2" 4] :backwards? true)))
           (->EditorUpdate (editor-state long-doc (selection ["d2" 0] ["d2" 4] :backwards? true))
                           (changelist :changed-uuids #{"d1" "d2"}))))
    (is (= (nav/shift+right (editor-state long-doc (selection ["d1" 4] ["d3" 4] :backwards? true, :between #{"d2"})))
           (->EditorUpdate (editor-state long-doc (selection ["d2" 0] ["d3" 4] :backwards? true))
                           (changelist :changed-uuids #{"d1" "d2"})))))

  (testing "shift+left works forwards"
    (is (= (nav/shift+left (editor-state long-doc (selection ["d1" 0] ["d2" 0])))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 0] ["d1" 4]))
                           (changelist :changed-uuids #{"d1" "d2"}))))
    (is (= (nav/shift+left (editor-state long-doc (selection ["d1" 0] ["d3" 0] :between #{"d2"})))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 0] ["d2" 4]))
                           (changelist :changed-uuids #{"d3" "d2"})))))

  (testing "shift+left works backwards (or single)"
    (is (= (nav/shift+left (editor-state long-doc (selection ["d1" 4])))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 3] ["d1" 4], :backwards? true))
                           (changelist :changed-uuids #{"d1"}))))
    (is (= (nav/shift+left (editor-state long-doc (selection ["d2" 0])))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 4] ["d2" 0], :backwards? true))
                           (changelist :changed-uuids #{"d1" "d2"}))))

    (is (= (nav/shift+left (editor-state long-doc (selection ["d2" 0] ["d4" 4], :backwards? true, :between #{"d3"})))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 4] ["d4" 4], :backwards? true, :between #{"d2" "d3"}))
                           (changelist :changed-uuids #{"d1" "d2"}))))
    (is (= (nav/shift+left (editor-state long-doc (selection ["d2" 0] ["d2" 4], :backwards? true)))
           (->EditorUpdate (editor-state long-doc (selection ["d1" 4] ["d2" 4], :backwards? true))
                           (changelist :changed-uuids #{"d1" "d2"}))))))

;; (deftest selected-content-test
;;   (testing "returns list of runs when passed selection within one paragraph"
;;     (is (= [(run "bar" #{:bold :italic})
;;             (run "bizz" #{:italic})
;;             (run "buzz" #{:bold})]
;;            (sl/selected-content doc (selection ["p1" 3] ["p1" 14])))))

;;   (testing "returns list of paragraphs when passed selection across multiple paragraphs"
;;     (is (= [(paragraph "p1" [(run "bar" #{:bold :italic})
;;                              (run "bizz" #{:italic})
;;                              (run "buzz" #{:bold})])
;;             (paragraph "p2" [(run "aaa")])]
;;            (sl/selected-content doc (selection ["p1" 3] ["p2" 3])))))

;;   (testing "returns list of paragraphs when passed selection across multiple (> 3) paragraphs"
;;     (is (= [(paragraph "d1" [(run "foo1" #{:italic})])
;;             (paragraph "d2" [(run "foo2" #{:bold})])
;;             (paragraph "d3" [(run "foo3" #{:underline})])
;;             (paragraph "d4" [(run "foo" #{:strike})])]
;;            (sl/selected-content long-doc (selection ["d1" 0] ["d4" 3])))))

;;   ;; TODO: I **think** this is the correct implementation here...could be wrong though...
;;   (testing "returns one paragraph and empty next paragraph when going from start of paragraph 1 to start of paragraph 2"
;;     (is (= [(paragraph "d1" [(run "foo1" #{:italic})]) (paragraph "d2" [])]
;;            (sl/selected-content long-doc (selection ["d1" 0] ["d2" 0]))))))

;; (deftest shared-formats-test
;;   (let [formats-doc (document [(paragraph "f1" [(run "foo1" #{:italic})
;;                                                 (run "foo2" #{:italic :bold})
;;                                                 (run "foo3" #{:bold})])
;;                                (paragraph "f2" [(run "bar1" #{:italic :bold :underline})])])]
;;     (testing "works inside same paragraph"
;;       (is (= #{:italic} (sl/shared-formats formats-doc (selection ["f1" 0] ["f1" 8]))))
;;       (is (= #{:italic :bold} (sl/shared-formats formats-doc (selection ["f1" 4] ["f1" 8]))))
;;       (is (= #{:bold} (sl/shared-formats formats-doc (selection ["f1" 4] ["f1" 12])))))

;;     (testing "works across paragraphs"
;;       (is (= #{:bold} (sl/shared-formats formats-doc (selection ["f1" 8] ["f2" 3])))))))

;; ;; TODO NEXT: fix toggle-format tests
;; ;; TODO after that: get new transaction system rendering properly/fix any errors it's caused with view and event handling layers.

;; (deftest toggle-format-test
;;   (testing "toggling single run"
;;     (is (= (sl/toggle-format doc (selection ["p1" 0] ["p1" 3]) :italic)
;;            {:doc (document [(paragraph "p1" [(run "foo")
;;                                              (run "bar" #{:bold :italic})
;;                                              (run "bizz" #{:italic})
;;                                              (run "buzz" #{:bold})])
;;                             p2])
;;             :selection (selection ["p1" 0] ["p1" 3])
;;             :changed-uuids #{"p1"}})))

;;   (testing "toggling across runs WITH shared format"
;;     (is (= (sl/toggle-format doc (selection ["p1" 0] ["p1" 10]) :italic)
;;            {:doc (document [(paragraph "p1" [(run "foo")
;;                                              (run "bar" #{:bold})
;;                                              (run "bizz" #{})
;;                                              (run "buzz" #{:bold})])
;;                             p2])
;;             :selection (selection ["p1" 0] ["p1" 10])
;;             :changed-uuids #{"p1"}})))

;;   (testing "toggling across runs WITH shared format, not on run boundaries"
;;     (is (= (sl/toggle-format doc (selection ["p1" 1] ["p1" 8]) :italic)
;;            {:doc (document [(paragraph "p1" [(run "f" #{:italic})
;;                                              (run "oo")
;;                                              (run "bar" #{:bold})
;;                                              (run "bi" #{})
;;                                              (run "zz" #{:italic})
;;                                              (run "buzz" #{:bold})])
;;                             p2])
;;             :selection (selection ["p1" 1] ["p1" 8])
;;             :changed-uuids #{"p1"}})))

;;   (testing "toggling across runs WITHOUT shared format"
;;     (is (= (sl/toggle-format doc (selection ["p1" 0] ["p1" 14]) :italic)
;;            {:doc (document [(paragraph "p1" [(run "foo" #{:italic})
;;                                              (run "bar" #{:bold :italic})
;;                                              (run "bizz" #{:italic})
;;                                              (run "buzz" #{:bold :italic})])
;;                             p2])
;;             :selection (selection ["p1" 0] ["p1" 14])
;;             :changed-uuids #{"p1"}})))

;;   (testing "toggling across paragraphs WITHOUT shared format"
;;     (is (= (sl/toggle-format doc (selection ["p1" 0] ["p2" 12]) :italic)
;;            {:doc (document [(paragraph "p1" [(run "foo" #{:italic})
;;                                              (run "bar" #{:bold :italic})
;;                                              (run "bizz" #{:italic})
;;                                              (run "buzz" #{:bold :italic})])
;;                             (paragraph "p2" [(run "aaabbbcccddd" #{:italic})])])
;;             :selection (selection ["p1" 0] ["p2" 12])
;;             :changed-uuids #{"p1" "p2"}})))

;;   (testing "toggling across paragraphs WITHOUT shared format, and not landing on run boundaries"
;;     (is (= (sl/toggle-format doc (selection ["p1" 1] ["p2" 3]) :italic)
;;            {:doc (document [(paragraph "p1" [(run "foo" #{:italic})
;;                                              (run "bar" #{:bold :italic})
;;                                              (run "bizz" #{:italic})
;;                                              (run "buzz" #{:bold :italic})])
;;                             (paragraph "p2" [(run "aaa" #{:italic})
;;                                              (run "bbbcccddd")])])
;;             :selection (selection ["p1" 1] ["p2" 3])
;;             :changed-uuids #{"p1" "p2"}})))

;;   (testing "toggling across paragraphs WITH shared format"
;;     (let [modified (-> doc
;;                        (update-in [:children "p1" :runs 3 :formats] conj :italic)
;;                        (update-in [:children "p2" :runs 0 :formats] conj :italic))]
;;       (is (= (sl/toggle-format modified (selection ["p1" 10] ["p2" 12]) :italic)
;;              {:doc (document [(paragraph "p1" [(run "foo" #{:italic})
;;                                                (run "bar" #{:bold :italic})
;;                                                (run "bizz" #{:italic})
;;                                                (run "buzz" #{:bold})])
;;                               (paragraph "p2" [(run "aaabbbcccddd")])])
;;               :selection (selection ["p1" 10] ["p2" 12])
;;               :changed-uuids #{"p1" "p2"}})))))

;; (deftest char-at-test
;;   (testing "works in 1st paragraph"
;;     (is (= "f" (sl/char-at doc (selection ["p1" 0]))))
;;     (is (= "o" (sl/char-at doc (selection ["p1" 1]))))
;;     (is (= "z" (sl/char-at doc (selection ["p1" 13]))))
;;     (is (thrown? js/Error (sl/char-at doc (selection ["p1" 14])))))

;;   (testing "works in other paragraphs"
;;     (is (= "a" (sl/char-at doc (selection ["p2" 0]))))
;;     (is (= "b" (sl/char-at doc (selection ["p2" 3]))))
;;     (is (= "c" (sl/char-at doc (selection ["p2" 7]))))
;;     (is (= "d" (sl/char-at doc (selection ["p2" 11]))))
;;     (is (thrown? js/Error (sl/char-at doc (selection ["p2" 12]))))))

;; (deftest char-before-test
;;   (testing "works in 1st paragraph"
;;     (is (= "\n" (sl/char-before doc (selection ["p1" 0]))))
;;     (is (= "f" (sl/char-before doc (selection ["p1" 1]))))
;;     (is (= "o" (sl/char-before doc (selection ["p1" 2]))))
;;     (is (= "z" (sl/char-before doc (selection ["p1" 13]))))
;;     (is (= "z" (sl/char-before doc (selection ["p1" 14])))))

;;   (testing "works in other paragraphs"
;;     (is (= "\n" (sl/char-before doc (selection ["p2" 0]))))
;;     (is (= "a" (sl/char-before doc (selection ["p2" 1]))))
;;     (is (= "a" (sl/char-before doc (selection ["p2" 3]))))
;;     (is (= "b" (sl/char-before doc (selection ["p2" 4]))))
;;     (is (= "c" (sl/char-before doc (selection ["p2" 7]))))
;;     (is (= "d" (sl/char-before doc (selection ["p2" 11]))))
;;     (is (= "d" (sl/char-before doc (selection ["p2" 12]))))
;;     (is (thrown? js/Error (sl/char-before doc (selection ["[2]" 13]))))))

(deftest merge-changelists-test
  (testing "merge logic works as it should (merge-changelists doc for details)"
    (= (state/merge-changelists
        {:resolved? false
         :deleted-uuids #{"a" "b" "g"}
         :changed-uuids #{"c" "d" "h"}
         :inserted-uuids #{"e" "f" "i"}}
        {:resolved? false
         :deleted-uuids #{"c" "d" "e"}
         :inserted-uuids #{"a" "b" "f"}})
       {:resolved? false
        :deleted-uuids #{"c" "d" "g"}
        :changed-uuids #{"a" "b" "h"}
        :inserted-uuids #{"f" "i"}}))

  (testing "merging with a resolved list ignores merge and returns second changelist"
    (= (state/merge-changelists
        {:resolved? true
         :deleted-uuids #{"a" "b" "g"}
         :changed-uuids #{"c" "d" "h"}
         :inserted-uuids #{"e" "f" "i"}}
        {:resolved? false
         :deleted-uuids #{"c" "d" "e"}
         :inserted-uuids #{"a" "b" "f"}})
       {:resolved? false
        :deleted-uuids #{"c" "d" "e"}
        :inserted-uuids #{"a" "b" "f"}})))
