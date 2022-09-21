(ns slate.filetypes.html-test
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.filetypes.html :as html]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]))

(def test-file (slurp-file "test_files/html/the_quiet_universe.html"))

(def test-file-expected
  (document [(paragraph [(run "The Quiet Universe - 2.235674301")])]))

(defn doc=
  "Tests if two docs are equal, disregarding paragraph UUIDs."
  [doc1 doc2]
  (letfn [(strip-uuids [doc]
            (update doc :children
                    (fn [children] (map #(dissoc % :uuid) children))))]
    (= (strip-uuids doc1) (strip-uuids doc2))))

(comment
  (doc= (document [(paragraph [(run "Hello!")])])
        (document [(paragraph [(run "Hello!")])]))
  )

(deftest html->droplet
  (testing "can import from google docs"
    (is (doc= (html/html->doc test-file) test-file-expected))))
