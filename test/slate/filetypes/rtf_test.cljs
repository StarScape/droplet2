(ns slate.filetypes.rtf-test
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [cljs.test :refer-macros [is deftest testing]]
            [slate.filetypes.import.rtf :as rtf-import]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]
            [slate.test-utils :as test-utils :refer [doc= doc-frag=]]))

(deftest rtf-import
  (testing "can convert basic RTF documents"
    ;; {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b bold} text.\par
    ;; }
    (is (doc= (rtf-import/rtf->doc (slurp-file "test_files/rtf/basic1.rtf"))
              (document [(paragraph [(run "This is some ")
                                     (run "bold" #{:bold})
                                     (run " text.")])])))))
