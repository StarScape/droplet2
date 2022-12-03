(ns slate.filetypes.rtf-test
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [cljs.test :refer-macros [is deftest testing]]
            [slate.filetypes.import.rtf :as r-import]
            [slate.filetypes.export.rtf :as r-export]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]
            [slate.test-utils :as test-utils :refer [doc= doc-frag=]]))

(deftest rtf-import
  (testing "can convert basic RTF documents"
    ;; {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b bold} text.\par
    ;; }
    (is (doc= (r-import/rtf->doc (slurp-file "test_files/rtf/basic1.rtf"))
              (document [(paragraph [(run "This is some ")
                                     (run "bold" #{:bold})
                                     (run " text.")])])))
    ;;{\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b bold and italic\i} text.\par
    ;; }
    (is (doc= (r-import/rtf->doc (slurp-file "test_files/rtf/basic2.rtf"))
              (document [(paragraph [(run "This is some ")
                                     (run "bold and italic" #{:bold :italic})
                                     (run " text.")])])))
    ;; {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b bold and italic\i} text. And some Japanese: \u24314\u21069\par
    ;; }
    (is (doc= (r-import/rtf->doc (slurp-file "test_files/rtf/basic3.rtf"))
              (document [(paragraph [(run "This is some ")
                                     (run "bold and italic" #{:bold :italic})
                                     (run " text. And some Japanese: 建前")])])))
    ;; {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b bold and italic\i} text. And some f\'E4ncy t\'EAxt.\par
    ;; }
    (is (doc= (r-import/rtf->doc (slurp-file "test_files/rtf/basic4.rtf"))
              (document [(paragraph [(run "This is some ")
                                     (run "bold and italic" #{:bold :italic})
                                     (run " text. And some fäncy têxt.")])])))
    ;; {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
    ;; This is some {\b1bold} text.\par
    ;; }
    (is (doc= (r-import/rtf->doc (slurp-file "test_files/rtf/basic5.rtf"))
              (document [(paragraph [(run "This is some ")
                                     (run "bold" #{:bold})
                                     (run " text.")])])))
    (is (doc= (r-import/rtf->doc (slurp-file "test_files/rtf/basic6.rtf"))
              (document [(paragraph [(run "Some emoji and special symbols: 🦎, 🏳️‍🌈, 🤦🏽, ñ.")])]))))
  (testing "standard conversion test"
    (is (doc= (r-import/rtf->doc (slurp-file "test_files/rtf/conversion_test.rtf"))
              (document [(paragraph (random-uuid) :h1 [(run "This is an H1")])
                         (paragraph (random-uuid) :h2 [(run "This is an H2")])
                         (paragraph [(run "")])
                         (paragraph [(run "Normal paragraph with a sentence, some ")
                                     (run "italics" #{:italic})
                                     (run ", ")
                                     (run "bold" #{:bold})
                                     (run ", and ")
                                     (run "strikethrough" #{:strikethrough})
                                     (run ".")])
                         (paragraph [(run "")])
                         (paragraph (random-uuid) :ol [(run "OL 1")])
                         (paragraph (random-uuid) :ol [(run "OL 2")])
                         (paragraph (random-uuid) :ol [(run "OL 3")])
                         (paragraph [(run "")])
                         (paragraph (random-uuid) :ul [(run "UL 1")])
                         (paragraph (random-uuid) :ul [(run "UL 2")])
                         (paragraph (random-uuid) :ul [(run "UL 3")])
                         (paragraph [(run "")])
                         (paragraph [(run "\u2003And a longer indented paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])
                         (paragraph [(run "")])])))))

#_(deftest rtf-export
  (let [test-doc (document [(paragraph (random-uuid) :h1 [(run "This is an H1")])
                            (paragraph (random-uuid) :h2 [(run "This is an H2")])
                            (paragraph [(run "")])
                            (paragraph [(run "Normal paragraph with a sentence, some ")
                                        (run "italics" #{:italic})
                                        (run ", ")
                                        (run "bold" #{:bold})
                                        (run ", and ")
                                        (run "strikethrough" #{:strikethrough})
                                        (run ".")])
                            (paragraph [(run "")])
                            (paragraph (random-uuid) :ol [(run "OL 1")])
                            (paragraph (random-uuid) :ol [(run "OL 2")])
                            (paragraph (random-uuid) :ol [(run "OL 3")])
                            (paragraph [(run "")])
                            (paragraph (random-uuid) :ul [(run "UL 1")])
                            (paragraph (random-uuid) :ul [(run "UL 2")])
                            (paragraph (random-uuid) :ul [(run "UL 3")])
                            (paragraph [(run "")])
                            (paragraph [(run "\u2003And a longer indented paragraph after, with Unicode: 建前. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])
                            (paragraph [(run "")])])
        rtf (r-export/doc->rtf test-doc)
        reconverted-doc (r-import/rtf->doc rtf)]
    reconverted-doc))
