(ns slate.filetypes.rtf-test
  (:require-macros [slate.utils :refer [slurp-file]])
  (:require [cljs.test :refer-macros [is deftest testing]]
            [slate.filetypes.import.rtf :as rtf-import]
            [slate.filetypes.export.rtf :as rtf-export]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]))

(deftest rtf-import
  (testing "can convert basic RTF documents"
    ;; {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b bold} text.\par
    ;; }
    (is (= (rtf-import/rtf->doc (slurp-file "test_files/rtf/basic1.rtf"))
           (document [(paragraph [(run "This is some ")
                                  (run "bold" #{:bold})
                                  (run " text.")])])))
    ;;{\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b bold and italic\i} text.\par
    ;; }
    (is (= (rtf-import/rtf->doc (slurp-file "test_files/rtf/basic2.rtf"))
           (document [(paragraph [(run "This is some ")
                                  (run "bold and italic" #{:bold :italic})
                                  (run " text.")])])))
    ;; {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b bold and italic\i} text. And some Japanese: \u24314\u21069\par
    ;; }
    (is (= (rtf-import/rtf->doc (slurp-file "test_files/rtf/basic3.rtf"))
           (document [(paragraph [(run "This is some ")
                                  (run "bold and italic" #{:bold :italic})
                                  (run " text. And some Japanese: 建前")])])))
    ;; {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b bold and italic\i} text. And some f\'E4ncy t\'EAxt.\par
    ;; }
    (is (= (rtf-import/rtf->doc (slurp-file "test_files/rtf/basic4.rtf"))
           (document [(paragraph [(run "This is some ")
                                  (run "bold and italic" #{:bold :italic})
                                  (run " text. And some fäncy têxt.")])])))
    ;; {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b1bold} text.\par
    ;; }
    (is (= (rtf-import/rtf->doc (slurp-file "test_files/rtf/basic5.rtf"))
           (document [(paragraph [(run "This is some ")
                                  (run "bold" #{:bold})
                                  (run " text.")])])))
    (is (= (rtf-import/rtf->doc (slurp-file "test_files/rtf/basic6.rtf"))
           (document [(paragraph [(run "Some emoji and special symbols: 🦎, 🏳️‍🌈, 🤦🏽, ñ, {, }, \\.")])]))))
  (testing "standard conversion test"
    (is (= (rtf-import/rtf->doc (slurp-file "test_files/rtf/conversion_test.rtf"))
           (document [(paragraph :h1 [(run "This is an H1")])
                      (paragraph :h2 [(run "This is an H2")])
                      (paragraph [(run "")])
                      (paragraph [(run "Normal paragraph with a sentence, some ")
                                  (run "italics" #{:italic})
                                  (run ", ")
                                  (run "bold" #{:bold})
                                  (run ", and ")
                                  (run "strikethrough" #{:strikethrough})
                                  (run ".")])
                      (paragraph [(run "")])
                      (paragraph :ol [(run "OL 1")])
                      (paragraph :ol [(run "OL 2")])
                      (paragraph :ol [(run "OL 3")])
                      (paragraph [(run "")])
                      (paragraph :ul [(run "UL 1")])
                      (paragraph :ul [(run "UL 2")])
                      (paragraph :ul [(run "UL 3")])
                      (paragraph [(run "")])
                      (paragraph [(run "\tAnd a longer indented paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])
                      (paragraph [(run "")])]))))
  (testing "errors"
    (is (thrown? js/Error (rtf-import/rtf->doc "This is not an RTF document")))
    (is (thrown? js/Error (rtf-import/rtf->doc "{\\rtf1}")))))

(deftest rtf-export
  (let [test-doc (document [(paragraph :h1 [(run "This is an H1")])
                            (paragraph :h2 [(run "This is an H2")])
                            (paragraph [(run "")])
                            (paragraph [(run "Normal paragraph with a sentence, some ")
                                        (run "italics" #{:italic})
                                        (run ", ")
                                        (run "bold" #{:bold})
                                        (run ", and ")
                                        (run "strikethrough" #{:strikethrough})
                                        (run ".")])
                            (paragraph [(run "")])
                            (paragraph :ol [(run "OL 1")])
                            (paragraph :ol [(run "OL 2")])
                            (paragraph :ol [(run "OL 3")])
                            (paragraph [(run "")])
                            (paragraph :ul [(run "UL 1")])
                            (paragraph :ul [(run "UL 2")])
                            (paragraph :ul [(run "UL 3")])
                            (paragraph [(run "")])
                            (paragraph [(run "\tAnd a longer indented paragraph after, with Unicode: 建前. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])
                            (paragraph [(run "")])])
        rtf (rtf-export/doc->rtf test-doc)
        reconverted-doc (rtf-import/rtf->doc rtf)]
    (is (= test-doc reconverted-doc))))
