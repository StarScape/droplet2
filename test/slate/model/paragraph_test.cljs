(ns slate.model.paragraph-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.selection :as sel :refer [selection]]
            [slate.model.common :as sl]
            [slate.model.run :as r :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]))

;; These provide a simple way to render a paragraph as a string, thereby allowing
;; us to check output of `insert` in the test below without manually checking each
;; item in the vector every time, and all the glorious headache that would provide.
(defn basic-run-render [r]
  (let [style-tags (sort (:formats r))
        open-tags (apply str (map #(str "<" (name %) ">") style-tags))
        close-tags (apply str (map #(str "</" (name %) ">") (reverse style-tags)))]
    (str open-tags (:text r) close-tags)))

(defn basic-paragraph-render [p]
  (let [runs-rendered (apply str (map basic-run-render (:runs p)))]
    (str "<p>" runs-rendered "</p>")))

(def para (paragraph [(run "Foobar 1." #{:bold})
                      (run " Foobar 2.")
                      (run " Foobar 3." #{:italic})]))

(def simplep (paragraph [(run "foobar1" #{:bold})
                         (run "goobar2")
                         (run "hoobar3" #{:italic})]))

(deftest optimize-runs-test
  (testing "deletes and combines unnecessary runs"
    (let [runs [(run "a" #{:italic})
                (run "b" #{:italic})
                (run "c")
                (run "")
                (run "d" #{:bold})
                (run "e" #{:bold})
                (run "f" #{:bold})
                (run "g")
                (run "h")
                (run "i" #{:bold})
                (run "j")]
          optimized (p/optimize-runs runs)
          rendered (basic-paragraph-render (paragraph optimized))]
      (is (= rendered "<p><italic>ab</italic>c<bold>def</bold>gh<bold>i</bold>j</p>"))))

  (testing "works with one empty run"
    (let [runs [(run "" #{:bold})]
          optimized (p/optimize-runs runs)]
      (is (= 1 (count optimized)))
      (is (= "" (-> optimized first :text)))
      (is (= #{} (-> optimized first :formats))))))

(deftest insert-single-test
  (testing "at end of run with same formatting"
    (let [p (p/insert para (selection [nil 9]) (run " Foobar 1.5." #{:bold}))]
      (is (= (basic-paragraph-render p)
             "<p><bold>Foobar 1. Foobar 1.5.</bold> Foobar 2.<italic> Foobar 3.</italic></p>"))))

  (testing "at end of run with different formatting"
    (let [p (p/insert para (selection [nil 9]) (run " Foobar 1.5."))]
      (is (= (basic-paragraph-render p)
             "<p><bold>Foobar 1.</bold> Foobar 1.5. Foobar 2.<italic> Foobar 3.</italic></p>"))))

  (testing "in middle of run with different formatting"
    (let [p (p/insert simplep (selection [nil 13]) (run "bizzbuzz" #{:italic}))]
      (is (= p (paragraph [(run "foobar1" #{:bold})
                           (run "goobar" #{})
                           (run "bizzbuzz" #{:italic})
                           (run "2" #{})
                           (run "hoobar3" #{:italic})])))))

  (testing "at start of paragraph"
    (let [p (p/insert simplep (selection [nil 0]) (run "pre" #{:underline}))]
      (is (= p (paragraph [(run "pre" #{:underline})
                           (run "foobar1" #{:bold})
                           (run "goobar2")
                           (run "hoobar3" #{:italic})])))))

  (testing "at end of paragraph"
    (let [sel (selection [nil (sl/len simplep)])
          p (p/insert simplep sel (run "post"))]
      (is (= p (paragraph [(run "foobar1" #{:bold})
                           (run "goobar2")
                           (run "hoobar3" #{:italic})
                           (run "post" #{})]))))))

(deftest delete-single-test
  (testing "at beginning of paragraph"
    (let [p (p/delete simplep (selection [nil 0]))]
      (is (= p simplep))))

  (testing "in middle of paragraph"
    (let [p (p/delete simplep (selection [nil 11]))]
      (is (= p (paragraph [(run "foobar1" #{:bold})
                           (run "gooar2")
                           (run "hoobar3" #{:italic})])))))

  (testing "end of run"
    (let [p (p/delete simplep (selection [nil 14]))]
      (is (= p (paragraph [(run "foobar1" #{:bold})
                           (run "goobar")
                           (run "hoobar3" #{:italic})])))))

  (testing "end of paragraph"
    (let [sel (selection [nil (sl/len simplep)])
          p (p/delete simplep sel)]
      (is (= p (paragraph [(run "foobar1" #{:bold})
                           (run "goobar2")
                           (run "hoobar" #{:italic})])))))

  (testing "single character run"
    (let [custom (paragraph "c" [(run "aaa" #{:italic})
                                 (run "b")
                                 (run "ccc" #{:bold})])
          p (p/delete custom (selection [nil 4]))]
      (is (= p (paragraph "c" [(run "aaa" #{:italic})
                               (run "ccc" #{:bold})])))))

  (testing "errors"
    (comment "TODO")))

;; TODO: add autogenerated tests for insert w/ range/selection?
;; I.e. it should always == delete + collapse + insert for same selection.

(deftest delete-range-test
  (testing "whole paragraph"
    (let [sel (selection [nil 0] [simplep (sl/len simplep)])
          p (p/delete simplep sel)]
      (is (= p (paragraph [(run "")])))))

  (testing "whole first run"
    (let [sel (selection [nil 0] [nil 7])
          p (p/delete simplep sel)]
      (is (= p (paragraph [(run "goobar2")
                           (run "hoobar3" #{:italic})])))))

  (testing "partial first run, from beginning"
    (let [sel (selection [nil 0] [nil 6])
          p (p/delete simplep sel)]
      (is (= p (paragraph [(run "1" #{:bold})
                           (run "goobar2")
                           (run "hoobar3" #{:italic})])))))

  (testing "partial first run, from middle"
    (let [sel (selection [nil 1] [nil 6])
          p (p/delete simplep sel)]
      (is (= p (paragraph [(run "f1" #{:bold})
                           (run "goobar2")
                           (run "hoobar3" #{:italic})])))))

  (testing "whole run in middle of paragraph"
    (let [sel (selection [nil 7] [nil 14])
          p (p/delete simplep sel)]
      (is (= p (paragraph [(run "foobar1" #{:bold})
                           (run "hoobar3" #{:italic})])))))

  (testing "partial run in middle of paragraph"
    (let [sel (selection [nil 8] [nil 13])
          p (p/delete simplep sel)]
      (is (= p (paragraph [(run "foobar1" #{:bold})
                           (run "g2")
                           (run "hoobar3" #{:italic})])))))

  (testing "whole last run"
    (let [sel (selection [nil 14] [nil (sl/len simplep)])
          p (p/delete simplep sel)]
      (is (= p (paragraph [(run "foobar1" #{:bold})
                           (run "goobar2")]))))))

(deftest formatting-test
  (testing "with a shared format across many runs"
    (let [p (paragraph "123" [(run "a", #{:italic :bold})
                              (run "b", #{:italic :underline})
                              (run "c", #{:italic :strikethrough})
                              (run "d", #{:italic})])]
      (is (= #{:italic}
             (sl/formatting p (selection [nil 0] [nil (sl/len p)]))))))

  (testing "with a no shared formats"
    (let [p (paragraph "123" [(run "a", #{:italic :bold})
                              (run "b", #{:italic :underline})
                              (run "c", #{:strikethrough})
                              (run "d", #{:italic})])]
      (is (= #{}
             (sl/formatting p (selection [nil 0] [nil (sl/len p)]))))))

  (testing "single run"
    (let [p (paragraph "123" [(run "a", #{:italic :bold})
                              (run "b", #{:italic :underline})
                              (run "c", #{:strikethrough})
                              (run "d", #{:italic})])]
      (is (= #{:italic :bold}
             (sl/formatting p (selection [nil 0] [nil 1]))))))

  (testing "two runs"
    (let [p (paragraph "123" [(run "aa", #{:italic :bold})
                              (run "bb", #{:italic :underline})
                              (run "cc", #{:strikethrough})
                              (run "dd", #{:italic})])]
      (is (= #{:italic}
             (sl/formatting p (selection [nil 0] [nil 3]))
             (sl/formatting p (selection [nil 0] [nil 4]))))))

  (testing "single-arity version selections whole paragraph"
    (let [p (paragraph "123" [(run "aa", #{:italic :bold})
                              (run "bb", #{:italic :underline})
                              (run "cc", #{:italic :strikethrough})
                              (run "dd", #{:italic})])]
      (is (= #{:italic}
             (sl/formatting p (selection [nil 0] [nil 3]))
             (sl/formatting p (selection [nil 0] [nil 4])))))))

;; TODO: test for selected-content

(deftest delete-after-test
  (testing "beginning of paragraph"
    (let [p (p/delete-after simplep 0)]
      (is (= p (paragraph [(run "")])))))

  (testing "middle of paragraph"
    (let [p (p/delete-after simplep 7)]
      (is (= p (paragraph [(run "foobar1" #{:bold})])))))

  (testing "end of paragraph"
    (let [p (p/delete-after simplep 21)]
      (is (= p simplep)))))

(deftest delete-before-test
  (testing "beginning of paragraph"
    (let [p (p/delete-before simplep 0)]
      (is (= p simplep))))

  (testing "middle of paragraph"
    (let [p (p/delete-before simplep 7)]
      (is (= p (paragraph [(run "goobar2" #{})
                           (run "hoobar3" #{:italic})])))))

  (testing "end of paragraph"
    (let [p (p/delete-before simplep 21)]
      (is (= p (paragraph [(run "")]))))))

;; TODO: finish this test
;; (deftest toggle-format-test
;;   (let [p (paragraph [(run "aaa", #{:italic :bold})
;;                       (run "bbb", #{:italic :underline})
;;                       (run "ccc", #{:strikethrough})
;;                       (run "ddd", #{:italic})])]

;;     (testing "not activated on whole selection, should activate"
;;       (is (= #{:})))))

(deftest get-formatting-test
  (let [p (paragraph [(run "f" #{:italic}) (run "oo" #{:bold})])]
    (is (= #{:italic} (sl/formatting p (selection [nil 0]))))
    (is (= #{:italic} (sl/formatting p (selection [nil 1]))))
    (is (= #{:bold} (sl/formatting p (selection [nil 2]))))
    (is (= #{:bold} (sl/formatting p (selection [nil (sl/len p)]))))))

(deftest char-at-test
  (let [mypara (paragraph "123" [(run "foo")])]
    (is (= "f" (sl/char-at mypara (selection [nil 0]))))
    (is (= "o" (sl/char-at mypara (selection [nil 1]))))
    (is (= "o" (sl/char-at mypara (selection [nil 2]))))
    (is (= "" (sl/char-at mypara (selection [nil 3]))))
    #_(is (thrown? js/Error (sl/char-at mypara (selection [nil 3]))))))

(deftest char-before-test
  (let [mypara (paragraph "123" [(run "foo")])]
    (is (= "\n" (sl/char-before mypara (selection [nil 0]))))
    (is (= "f" (sl/char-before mypara (selection [nil 1]))))
    (is (= "o" (sl/char-before mypara (selection [nil 2]))))
    (is (= "o" (sl/char-before mypara (selection [nil 3]))))
    (is (= "" (sl/char-before mypara (selection [nil 4]))))
    #_(is (thrown? js/Error (sl/char-before mypara (selection [nil 4]))))))

(deftest trim-test
  (let [p (paragraph [(r/run " \u00a0\n\tHello ") (r/run "world! \u00a0\n\t" #{:italic})])]
    (is (= (p/trim-start p) (paragraph [(r/run "Hello ") (r/run "world! \u00a0\n\t" #{:italic})])))
    (is (= (p/trim-end p) (paragraph [(r/run " \u00a0\n\tHello ") (r/run "world!" #{:italic})])))))

(deftest graphemes-test
  (testing "handles multiple runs properly"
    (let [p (paragraph [(r/run "🏳️‍🌈🦎") (r/run "🤦🏽ñ" #{:bold})])]
      (is (= (sl/graphemes p) (sl/graphemes "🏳️‍🌈🦎🤦🏽ñ"))))))
