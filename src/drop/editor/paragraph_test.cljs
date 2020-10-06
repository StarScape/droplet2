(ns drop.editor.paragraph-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.selection :as sel :refer [selection]]
            [drop.editor.core :as c :refer [run paragraph]]))

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
          optimized (c/optimize-runs runs)
          rendered (basic-paragraph-render (paragraph optimized))]
      (is (= rendered "<p><italic>ab</italic>c<bold>def</bold>gh<bold>i</bold>j</p>"))))

  (testing "works with one empty run"
    (let [runs [(run "" #{:bold})]
          optimized (c/optimize-runs runs)]
      (is (= 1 (count optimized)))
      (is (= "" (-> optimized first :text)))
      (is (= #{} (-> optimized first :formats))))))

(deftest insert-single-test
  (testing "at end of run with same formatting"
    (let [p (c/insert para (selection [para 9]) (run " Foobar 1.5." #{:bold}))]
      (is (= (basic-paragraph-render p)
             "<p><bold>Foobar 1. Foobar 1.5.</bold> Foobar 2.<italic> Foobar 3.</italic></p>"))))

  (testing "at end of run with different formatting"
    (let [p (c/insert para (selection [para 9]) (run " Foobar 1.5."))]
      (is (= (basic-paragraph-render p)
             "<p><bold>Foobar 1.</bold> Foobar 1.5. Foobar 2.<italic> Foobar 3.</italic></p>"))))

  (testing "in middle of run with different formatting"
    (let [p (c/insert simplep (selection [simplep 13]) (run "bizzbuzz" #{:italic}))]
      (is (= p
             (paragraph [(run "foobar1" #{:bold})
                         (run "goobar" #{})
                         (run "bizzbuzz" #{:italic})
                         (run "2" #{})
                         (run "hoobar3" #{:italic})])))))

  (testing "at start of paragraph"
    (let [p (c/insert simplep (selection [simplep 0]) (run "pre" #{:underline}))]
      (is (= p
             (paragraph [(run "pre" #{:underline})
                         (run "foobar1" #{:bold})
                         (run "goobar2")
                         (run "hoobar3" #{:italic})])))))

  (testing "at end of paragraph"
    (let [sel (selection [simplep (c/text-len simplep)])
          p (c/insert simplep sel (run "post"))]
      (is (= p
             (paragraph [(run "foobar1" #{:bold})
                         (run "goobar2")
                         (run "hoobar3" #{:italic})
                         (run "post" #{})]))))))

(deftest delete-single-test
  (testing "at beginning of paragraph"
    (let [p (c/delete simplep (selection [simplep 0]))]
      (is (= p simplep))))

  (testing "in middle of paragraph"
    (let [p (c/delete simplep (selection [simplep 11]))]
      (is (= p
             (paragraph [(run "foobar1" #{:bold})
                         (run "gooar2")
                         (run "hoobar3" #{:italic})])))))

  (testing "end of run"
    (let [p (c/delete simplep (selection [simplep 14]))]
      (is (= p
             (paragraph [(run "foobar1" #{:bold})
                         (run "goobar")
                         (run "hoobar3" #{:italic})])))))

  (testing "end of paragraph"
    (let [sel (selection [simplep (c/text-len simplep)])
          p (c/delete simplep sel)]
      (is (= p
             (paragraph [(run "foobar1" #{:bold})
                         (run "goobar2")
                         (run "hoobar" #{:italic})])))))

  (testing "single character run"
    (let [custom (paragraph [(run "aaa" #{:italic})
                             (run "b")
                             (run "ccc" #{:bold})])
          p (c/delete custom (selection [custom 4]))]
      (is (= p
             (paragraph [(run "aaa" #{:italic})
                         (run "ccc" #{:bold})])))))

  (testing "errors"
    (comment "TODO")))

;; TODO: add autogenerated tests for insert w/ range/selection?
;; I.e. it should always == delete + collapse + insert for same selection.

(deftest delete-range-test
  (testing "whole paragraph"
    (let [sel (selection [simplep 0] [simplep (c/text-len simplep)])
          p (c/delete simplep sel)]
      (is (= p (paragraph [(run "")])))))

  (testing "whole first run"
    (let [sel (selection [simplep 0] [simplep 7])
          p (c/delete simplep sel)]
      (is (= p
             (paragraph [(run "goobar2")
                         (run "hoobar3" #{:italic})])))))

  (testing "partial first run, from beginning"
    (let [sel (selection [simplep 0] [simplep 6])
          p (c/delete simplep sel)]
      (is (= p
             (paragraph [(run "1" #{:bold})
                         (run "goobar2")
                         (run "hoobar3" #{:italic})])))))

  (testing "partial first run, from middle"
    (let [sel (selection [simplep 1] [simplep 6])
          p (c/delete simplep sel)]
      (is (= p
             (paragraph [(run "f1" #{:bold})
                         (run "goobar2")
                         (run "hoobar3" #{:italic})])))))
  (testing "whole run in middle of paragraph"
    (let [sel (selection [simplep 7] [simplep 14])
          p (c/delete simplep sel)]
      (is (= p
             (paragraph [(run "foobar1" #{:bold})
                         (run "hoobar3" #{:italic})])))))

  (testing "partial run in middle of paragraph"
    (let [sel (selection [simplep 8] [simplep 13])
          p (c/delete simplep sel)]
      (is (= p
             (paragraph [(run "foobar1" #{:bold})
                         (run "g2")
                         (run "hoobar3" #{:italic})])))))

  (testing "whole last run"
    (let [sel (selection [simplep 14] [simplep (c/text-len simplep)])
          p (c/delete simplep sel)]
      (is (= p
             (paragraph [(run "foobar1" #{:bold})
                         (run "goobar2")]))))))

(deftest shared-formats-test
  (testing "with a shared format across many runs"
    (let [p (paragraph [(run "a", #{:italic :bold})
                        (run "b", #{:italic :underline})
                        (run "c", #{:italic :strikethrough})
                        (run "d", #{:italic})])]
      (is (= #{:italic}
             (c/shared-formats p (selection [p 0] [p (c/text-len p)]))))))

  (testing "with a no shared formats"
    (let [p (paragraph [(run "a", #{:italic :bold})
                        (run "b", #{:italic :underline})
                        (run "c", #{:strikethrough})
                        (run "d", #{:italic})])]
      (is (= #{}
             (c/shared-formats p (selection [p 0] [p (c/text-len p)]))))))

  (testing "single run"
    (let [p (paragraph [(run "a", #{:italic :bold})
                        (run "b", #{:italic :underline})
                        (run "c", #{:strikethrough})
                        (run "d", #{:italic})])]
      (is (= #{:italic :bold}
             (c/shared-formats p (selection [p 0] [p 1]))))))

  (testing "two runs"
    (let [p (paragraph [(run "aa", #{:italic :bold})
                        (run "bb", #{:italic :underline})
                        (run "cc", #{:strikethrough})
                        (run "dd", #{:italic})])]
      (is (= #{:italic}
             (c/shared-formats p (selection [p 0] [p 3]))
             (c/shared-formats p (selection [p 0] [p 4]))))))

  (testing "single-arity version selections whole paragraph"
    (let [p (paragraph [(run "aa", #{:italic :bold})
                        (run "bb", #{:italic :underline})
                        (run "cc", #{:italic :strikethrough})
                        (run "dd", #{:italic})])]
      (is (= #{:italic}
             (c/shared-formats p (selection [p 0] [p 3]))
             (c/shared-formats p (selection [p 0] [p 4])))))))

;; TODO: test for selected-content

(deftest delete-after-test
  (testing "beginning of paragraph"
    (let [p (c/delete-after simplep 0)]
      (is (= p (paragraph [(run "")])))))

  (testing "middle of paragraph"
    (let [p (c/delete-after simplep 7)]
      (is (= p (paragraph [(run "foobar1" #{:bold})])))))

  (testing "end of paragraph"
    (let [p (c/delete-after simplep 21)]
      (is (= p simplep)))))

(deftest delete-before-test
  (testing "beginning of paragraph"
    (let [p (c/delete-before simplep 0)]
      (is (= p simplep))))

  (testing "middle of paragraph"
    (let [p (c/delete-before simplep 7)]
      (is (= p (paragraph [(run "goobar2" #{})
                           (run "hoobar3" #{:italic})])))))

  (testing "end of paragraph"
    (let [p (c/delete-before simplep 21)]
      (is (= p (paragraph [(run "")]))))))

;; TODO: finish this test
;; (deftest toggle-format-test
;;   (let [p (paragraph [(run "aaa", #{:italic :bold})
;;                       (run "bbb", #{:italic :underline})
;;                       (run "ccc", #{:strikethrough})
;;                       (run "ddd", #{:italic})])]

;;     (testing "not activated on whole selection, should activate"
;;       (is (= #{:})))))
