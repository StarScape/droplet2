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

(defn- convert-run [r] (into [(:text r)] (:formats r)))
(defn- convert-paragraph [p] (mapv convert-run (:runs p)))
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
    (is (= [[["fooHello" :italic]
             ["Goodbye!"]
             ["bar" :bold :italic]
             ["bizz" :italic]
             ["buzz" :bold]]
            [["aaabbbcccddd"]]]
           (convert-doc (c/insert doc
                                  (selection [0 3])
                                  [(run "Hello" #{:italic}) (run "Goodbye!")])))))

  (testing "single run"
    (is (= [[["foo" :italic]
             ["Goodbye!"]
             ["bar" :bold :italic]
             ["bizz" :italic]
             ["buzz" :bold]]
            [["aaabbbcccddd"]]]
           (convert-doc (c/insert doc (selection [0 3]) (run "Goodbye!"))))))

  (testing "at start of paragraph"
    (is (= [[["Hello!"]
             ["foo" :italic]
             ["bar" :bold :italic]
             ["bizz" :italic]
             ["buzz" :bold]]
            [["aaabbbcccddd"]]]
           (convert-doc (c/insert doc (selection [0 0]) (run "Hello!"))))))

  (testing "at end of paragraph"
    (is (= [[["foo" :italic]
             ["bar" :bold :italic]
             ["bizz" :italic]
             ["buzz" :bold]]
            [["aaabbbcccddd"]
             ["Goodbye!" :italic]]]
           (convert-doc (c/insert doc (selection [1 12]) (run "Goodbye!" #{:italic}))))))

  ;; TODO: write some cases for multi-paragraph insert

  (testing "throws when out of range of paragraph"
    (is (thrown? js/Error
                 [[["foo" :italic]
                   ["bar" :bold :italic]
                   ["bizz" :italic]
                   ["buzz" :bold]
                   ["Goodbye!" :italic]]
                  [["aaabbbcccddd"]]]
                 (convert-doc (c/insert doc (selection [0 55]) (run "Goodbye!" #{:italic})))))))
