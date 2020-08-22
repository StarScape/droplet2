(ns drop.editor.paragraph-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.core :as c]))

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

(def run1 (c/run "Foobar 1." #{:bold}))
(def run2 (c/run " Foobar 2."))
(def run3 (c/run " Foobar 3." #{:italic}))

(def para (c/paragraph [run1 run2 run3]))
(def simplep (c/paragraph [(c/run "foobar1" #{:bold})
                           (c/run "goobar2")
                           (c/run "hoobar3" #{:italic})]))

(deftest optimize-runs-test
  (testing "deletes and combines unnecessary runs"
    (let [runs [(c/run "a" #{:italic})
                (c/run "b" #{:italic})
                (c/run "c")
                (c/run "")
                (c/run "d" #{:bold})
                (c/run "e" #{:bold})
                (c/run "f" #{:bold})
                (c/run "g")
                (c/run "h")
                (c/run "i" #{:bold})
                (c/run "j")]
          optimized (c/optimize-runs runs)
          rendered (basic-paragraph-render (c/paragraph optimized))]
      (is (= rendered "<p><italic>ab</italic>c<bold>def</bold>gh<bold>i</bold>j</p>"))))

  (testing "works with one empty run"
    (let [runs [(c/run "" #{:bold})]
          optimized (c/optimize-runs runs)]
      (is (= 1 (count optimized)))
      (is (= "" (-> optimized first :text)))
      (is (= #{} (-> optimized first :formats))))))

(deftest insert-single-test
  (testing "at end of run with same formatting"
    (let [p (c/insert para (c/selection [para 9]) (c/run " Foobar 1.5." #{:bold}))]
      (is (= (basic-paragraph-render p)
             "<p><bold>Foobar 1. Foobar 1.5.</bold> Foobar 2.<italic> Foobar 3.</italic></p>"))))

  (testing "at end of run with different formatting"
    (let [p (c/insert para (c/selection [para 9]) (c/run " Foobar 1.5."))]
      (is (= (basic-paragraph-render p)
             "<p><bold>Foobar 1.</bold> Foobar 1.5. Foobar 2.<italic> Foobar 3.</italic></p>"))))

  (testing "in middle of run with different formatting"
    (let [p (c/insert simplep (c/selection [simplep 13]) (c/run "bizzbuzz" #{:italic}))]
      (is (= p
             (c/paragraph [(c/run "foobar1" #{:bold})
                           (c/run "goobar" #{})
                           (c/run "bizzbuzz" #{:italic})
                           (c/run "2" #{})
                           (c/run "hoobar3" #{:italic})])))))

  (testing "at start of paragraph"
    (let [p (c/insert simplep (c/selection [simplep 0]) (c/run "pre" #{:underline}))]
      (is (= p
             (c/paragraph [(c/run "pre" #{:underline})
                           (c/run "foobar1" #{:bold})
                           (c/run "goobar2")
                           (c/run "hoobar3" #{:italic})])))))

  (testing "at end of paragraph"
    (let [sel (c/selection [simplep (c/len simplep)])
          p (c/insert simplep sel (c/run "post"))]
      (is (= p
             (c/paragraph [(c/run "foobar1" #{:bold})
                           (c/run "goobar2")
                           (c/run "hoobar3" #{:italic})
                           (c/run "post" #{})]))))))
