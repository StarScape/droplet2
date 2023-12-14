(ns slate.serialization-test
  (:require-macros [slate.utils :refer [slurp-file]])
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.serialization :as serialization :refer [serialize deserialize]]
            [slate.model.dll :as dll :refer [dll big-dec]]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.run :as r :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.editor-state :as es :refer [editor-state]]
            [slate.model.history :as history]))

(def simple-test-result
  {:version 3
   :editor-state (editor-state (document (dll (paragraph :h1 [(run "Heading 1")])
                                              (paragraph :h2 [(run "Heading 2")])
                                              (paragraph)
                                              (paragraph [(run "Simple paragraph with ")
                                                          (run "italics" #{:italic})
                                                          (run ", ")
                                                          (run "bolding" #{:bold})
                                                          (run ", and ")
                                                          (run "strikethrough" #{:strikethrough})
                                                          (run ".")])
                                              (paragraph)
                                              (paragraph :ol [(run "O1")])
                                              (paragraph :ol [(run "O2")])
                                              (paragraph :ol [(run "O3")])
                                              (paragraph)
                                              (paragraph :ul [(run "U1")])
                                              (paragraph :ul [(run "U2")])
                                              (paragraph :ul [(run "U3")])
                                              (paragraph :ul [(run "U4")])))
                               (selection [(big-dec 13) 2]))})

(deftest v2->v3-test
  (is (= (deserialize (slurp-file "test_files/serialization-test-files/v2/simple_test.drop"))
         simple-test-result)))

(deftest circular-test
  (testing "can serialize and deserialize and get the same result as initial EditorState"
    (is (= (-> {:history (history/init (:editor-state simple-test-result))}
               (serialize)
               (deserialize))
           simple-test-result))))

(defn deserialize-error? [x] (contains? x :error-message))

(deftest deserialize-test
  (testing "returns error when fed bad input: invalid EDN, wrong data structure, etc"
    (is (deserialize-error? (deserialize "{}")))
    (is (deserialize-error? (deserialize "{:some-bad-edn [12 3}")))
    (is (deserialize-error? (deserialize nil)))
    (is (deserialize-error? (deserialize (assoc simple-test-result :version 1000000))))
    (is (deserialize-error? (deserialize "{:version 3, :editor-state {}}")))
    (is (deserialize-error? (deserialize "{:version 3, :editor-state {:doc nil, :selection {:foo :bar, :start {:paragraph 1, :offset -33}}}}")))))
