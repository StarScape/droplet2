(ns slate.model.dll-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.dll :as dll :refer [dll big-dec]]
            ["decimal.js" :refer [Decimal]]))

(def val1 {:content "foo"})
(def val2 {:content "bar"})
(def val3 {:content "bizz"})
(def val4 {:content "bang"})
(def before-val {:content "bar"})
(def l (dll val1 val2 val3 val4))
(def l1 (dll/insert-before l (big-dec 3) before-val)) ; foo bar bar bizz bang
(def l2 (dll/insert-before l (big-dec 1) {:content "pre"}))

(deftest initialize-test
  (testing "empty constructor"
    (let [d (dll)]
      (is (= {} (.-entries-map d)))
      (is (= nil (.-first-index d)))
      (is (= nil (.-last-index d)))))

  (testing "adding one item"
    (let [d (dll {:field :value})]
      (is (= (.-entries-map d)
             {(big-dec 1) (dll/Node. {:field :value} (big-dec 1) nil nil)}))
      (is (= (big-dec 1) (.-first-index d)))
      (is (= (big-dec 1) (.-last-index d)))))

  (testing "adding multiple items"
    (let [d (dll {:field 1} {:field 2} {:field 3})]
      (is (= (.-entries-map d)
             {(big-dec 1) (dll/Node. {:field 1} (big-dec 1) nil (big-dec 2))
              (big-dec 2) (dll/Node. {:field 2} (big-dec 2) (big-dec 1) (big-dec 3))
              (big-dec 3) (dll/Node. {:field 3} (big-dec 3) (big-dec 2) nil)}))
      (is (= (big-dec 1) (.-first-index d)))
      (is (= (big-dec 3) (.-last-index d)))))

  (testing "works with 10 elements or more"
    ;; This test is the result of a very specific bug
    (let [d (dll :1 :2 :3 :4 :5 :6 :7 :8 :9 :10 :11)]
      (is (= :1 (get d (big-dec 1))))
      (is (= :11 (get d (big-dec 11)))))))

(deftest insert-before-test
  (is (= (dll/insert-before l (big-dec 1) {:content "pre"})
         (dll {:content "pre"}
              {:content "foo"}
              {:content "bar"}
              {:content "bizz"}
              {:content "bang"})))

  (is (= (dll/insert-before l (big-dec 4) {:content "buzz"})
         (dll {:content "foo"}
              {:content "bar"}
              {:content "bizz"}
              {:content "buzz"}
              {:content "bang"})))

  (is (thrown? js/Error (dll/insert-before l (big-dec -1) {:content "WUT"})))
  (is (thrown? js/Error (dll/insert-before l (big-dec 1000) {:content "WUT"}))))

(deftest insert-after-test
  (is (= (dll/insert-after l (big-dec 1) {:content "inserted"})
         (dll {:content "foo"}
              {:content "inserted"}
              {:content "bar"}
              {:content "bizz"}
              {:content "bang"})))

  (is (= (dll/insert-after l (big-dec 4) {:content "post"})
         (dll {:content "foo"}
              {:content "bar"}
              {:content "bizz"}
              {:content "bang"}
              {:content "post"})))

  (is (thrown? js/Error (dll/insert-after l (big-dec 100) {:content "WUT"}))))

(deftest seq-test
  (testing "produces a seq correctly"
    (is (= (seq l) (seq [{:content "foo"}
                         {:content "bar"}
                         {:content "bizz"}
                         {:content "bang"}]))))

  (testing "can filter and map correctly"
    (is (= (map #(update % :content str "EDITED") l)
           (seq [{:content "fooEDITED"}
                 {:content "barEDITED"}
                 {:content "bizzEDITED"}
                 {:content "bangEDITED"}])))
    (is (= (filter #(not= "foo" (:content %)) l)
           (seq [{:content "bar"}
                 {:content "bizz"}
                 {:content "bang"}])))))

(deftest count-test
  (is (= 4 (count l)))
  (is (= 5 (count l1)))
  (is (= 5 (count l2))))

(deftest equiv-test
  (testing "empty DLLs equal"
    (is (= (dll) (dll))))

  (testing "equivalent DLLs equal"
    (is (= (dll {:content "foo"}
                {:content "bar"}
                {:content "bizz"})
           (dll {:content "foo"}
                {:content "bar"}
                {:content "bizz"})))
    (is (= l l))
    (is (= l1 (dll/insert-before l (big-dec 3) {:content "bar"})))))

(deftest next-test
  (is (= (dll/next l (big-dec 1)) val2))
  (is (= (dll/next l (big-dec 2)) val3))
  (is (= (dll/next l (big-dec 3)) val4))
  (is (= (dll/next l (big-dec 4)) nil))
  (is (thrown? js/Error (dll/next-index l (big-dec 5)))))

(deftest next-index-test
  (is (= val4 (->> (dll/next-index l (big-dec 1))
                   (dll/next-index l)
                   (dll/next-index l)
                   (get l)))))

(deftest prev-test
  (is (= (dll/prev l (big-dec 1)) nil))
  (is (= (dll/prev l (big-dec 2)) val1))
  (is (= (dll/prev l (big-dec 3)) val2))
  (is (= (dll/prev l (big-dec 4)) val3))
  (is (thrown? js/Error (dll/prev l (big-dec 5)))))

(deftest prev-index-test
  (is (= val1 (->> (dll/prev-index l (big-dec 4))
                   (dll/prev-index l)
                   (dll/prev-index l)
                   (get l)))))

(deftest empty-test
  (is (empty? (dll)))
  (is #_:clj-kondo/ignore (not (empty? (dll {:val "somethin"}))))
  (is (seq (dll {:val "somethin"}))))


(deftest get-test
  (is (= val1 (get l (big-dec 1))))
  (is (= val2 (get l (big-dec 2))))
  (is (= val3 (get l (big-dec 3))))
  (is (= val4 (get l (big-dec 4))))
  (is (= :default (get l :non-extant-key :default))))


(deftest dissoc-test
  (is (= (dissoc l (big-dec 1)) (dll val2 val3 val4)))
  (is (= (dissoc l (big-dec 4)) (dll val1 val2 val3)))
  (is (= (dissoc l :doesntexist) l))
  (is (= (dissoc (dll {:content "foo"}) (big-dec 1)) (dll))))

(deftest conj-test
  (testing "conj works"
    (is (= (conj l {:content "post"})
           (dll val1 val2 val3 val4 {:content "post"})))
    (is (= (conj (dll) {:content "post"})
           (dll {:content "post"}))))

  (testing "(into) works with DLL"
    (is (= (into [] l) [val1 val2 val3 val4]))
    (is (= (into (dll) [1, 2, 3])
           (dll 1, 2, 3)))))

(deftest prepend-test
  (testing "prepending works on a populated list"
    (is (= (dll/prepend l {:content "pre"}) (dll {:content "pre"} val1 val2 val3 val4))))

  (testing "works on an empty list"
    (is (= (dll/prepend (dll) val1) (dll val1)))))

(deftest first-test
  (is (= (dll/first l) val1))
  (is (= (dll/first (dll)) nil)))

(deftest last-test
  (is (= (dll/last l) val4))
  (is (= (dll/last (dll)) nil)))

(deftest rest-test
  (is (= (rest l) (seq [val2 val3 val4])))
  (is (= (rest (dll)) ()))

  ;; Can we destructure it properly?
  (is (= [val2 val1] (let [[a b] l] [b a]))))

(deftest assoc-test
  (testing "assoc and update work correctly"
    (is (= (assoc l (big-dec 2) {:content "oyeah"})
           (dll val1 {:content "oyeah"} val3 val4)))
    (is (= (update l (big-dec 2) #(assoc % :content "baybee"))
           (dll val1 {:content "baybee"} val3 val4))))

  (testing "fails when trying to assoc an element that doesn't exist"
    (is (thrown? js/Error (assoc l " 55 " {:content "nope"})))))

(deftest remove-range-test
  (is (= (dll/remove-range l (big-dec 1) (big-dec 4)) (dll)))
  (is (= (dll/remove-range l (big-dec 1) (big-dec 3)) (dll val4)))
  (is (= (dll/remove-range l (big-dec 2) (big-dec 3)) (dll val1 val4)))
  (is (= (dll/remove-range l (big-dec 3) (big-dec 3)) (dll val1 val2 val4)))
  (is (= (dll/remove-range l (big-dec 1) (big-dec 2)) (dll val3 val4))))

(deftest ifn-test
  (testing "invoking a DLL as a function does lookup, same as vector or map"
    (is (= (l (big-dec 1)) val1))
    (is (identical? (l (big-dec 1)) val1))
    (is (= (l (big-dec 2)) val2))
    (is (identical? (l (big-dec 2)) val2))))

(deftest replace-range-test
  (testing "replacing everything"
    (is (= (dll/replace-range l (big-dec 1) (big-dec 4) [{:content "CHANGED1"} {:content "CHANGED2"}])
           (dll {:content "CHANGED1"} {:content "CHANGED2"})))
    (is (= (dll/replace-range l (big-dec 1) (big-dec 4) (dll {:content "CHANGED1"} {:content "CHANGED2"}))
           (dll {:content "CHANGED1"} {:content "CHANGED2"})))
    (is (= (dll/replace-range l (big-dec 1) (big-dec 4) {:content "CHANGED"})
           (dll {:content "CHANGED"}))))

  (testing "replacing in middle"
    (is (= (dll/replace-range l (big-dec 2) (big-dec 3) [{:content "CHANGED1"} {:content "CHANGED2"}])
           (dll val1 {:content "CHANGED1"} {:content "CHANGED2"} val4)))
    (is (= (dll/replace-range l (big-dec 2) (big-dec 3) (dll {:content "CHANGED1"} {:content "CHANGED2"}))
           (dll val1 {:content "CHANGED1"} {:content "CHANGED2"} val4)))
    (is (= (dll/replace-range l (big-dec 2) (big-dec 3) {:content "CHANGED"})
           (dll val1 {:content "CHANGED"} val4))))

  (testing "replacing from start to middle"
    (is (= (dll/replace-range l (big-dec 1) (big-dec 3) [{:content "CHANGED1"} {:content "CHANGED2"}])
           (dll {:content "CHANGED1"} {:content "CHANGED2"} val4)))
    (is (= (dll/replace-range l (big-dec 1) (big-dec 3) (dll {:content "CHANGED1"} {:content "CHANGED2"}))
           (dll {:content "CHANGED1"} {:content "CHANGED2"} val4)))
    (is (= (dll/replace-range l (big-dec 1) (big-dec 3) {:content "CHANGED"})
           (dll {:content "CHANGED"} val4))))

  (testing "replacing from middle to end"
    (is (= (dll/replace-range l (big-dec 2) (big-dec 4) [{:content "CHANGED1"} {:content "CHANGED2"}])
           (dll val1 {:content "CHANGED1"} {:content "CHANGED2"})))
    (is (= (dll/replace-range l (big-dec 2) (big-dec 4) (dll {:content "CHANGED1"} {:content "CHANGED2"}))
           (dll val1 {:content "CHANGED1"} {:content "CHANGED2"})))
    (is (= (dll/replace-range l (big-dec 2) (big-dec 4) {:content "CHANGED"})
           (dll val1 {:content "CHANGED"}))))

  (testing "replacing just one node"
    (is (= (dll/replace-range l (big-dec 2) (big-dec 2) [{:content "a"}, {:content "b"}])
           (dll val1 {:content "a"} {:content "b"} val3 val4)))
    (is (= (dll/replace-range l (big-dec 1) (big-dec 1) [{:content "a"}, {:content "b"}])
           (dll {:content "a"} {:content "b"} val2 val3 val4)))
    (is (= (dll/replace-range l (big-dec 4) (big-dec 4) [{:content "a"}, {:content "b"}])
           (dll val1 val2 val3 {:content "a"} {:content "b"}))))

  (testing "edge cases and indices"
    (is (= [1] (dll/replace-range (dll :a :b) (big-dec 1) (big-dec 2) 1)))
    (is (= [:a 1 :d] (dll/replace-range (dll :a :b :c :d) (big-dec 2) (big-dec 3) 1)))

    (is (= [:a 1 2 :c :d :e]
           (dll/replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 2) [1 2])))
    (is (= [:a 1 2 3 :c :d :e]
           (dll/replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 2) [1 2 3])))

    (is (= [:a 1 2 3 :e]
           (dll/replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 4) [1 2 3])))
    (is (= [(big-dec 1) (big-dec 2) (big-dec 3) (big-dec 4) (big-dec 5)]
           (dll/all-indices (dll/replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 4) [1 2 3]))))

    (is (= [:a 1 2 :e]
           (dll/replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 4) [1 2])))
    (is (= [(big-dec 1) (big-dec 2) (big-dec 4) (big-dec 5)]
           (dll/all-indices (dll/replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 4) [1 2]))))

    (is (= [(big-dec 1) (big-dec 2) (big-dec 5)]
           (dll/all-indices (dll/replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 5) [1 2]))))))

(deftest replace-between-test
  (is (= [:a :b :c :d :e] (dll/remove-between (dll :a :b :c :d :e) (big-dec 1) (big-dec 1))))
  (is (= [:a :b :c :d :e] (dll/remove-between (dll :a :b :c :d :e) (big-dec 1) (big-dec 2))))
  (is (= [:a :c :d :e] (dll/remove-between (dll :a :b :c :d :e) (big-dec 1) (big-dec 3))))
  (is (= [:a :e] (dll/remove-between (dll :a :b :c :d :e) (big-dec 1) (big-dec 5))))
  (is (= [:a :b :c :d :e] (dll/remove-between (dll :a :b :c :d :e) (big-dec 4) (big-dec 5))))
  (is (= [:a :b :c :d :e] (dll/remove-between (dll :a :b :c :d :e) (big-dec 5) (big-dec 5))))
  (is (= [:a :b :c :e] (dll/remove-between (dll :a :b :c :d :e) (big-dec 3) (big-dec 5)))))

(deftest between-test
  (is (empty? (dll/between l (big-dec 1) (big-dec 2))))
  (is (empty? (dll/between l (big-dec 1) (big-dec 1))))
  (is (= (dll val2 val3) (dll/between l (big-dec 1) (big-dec 4))))
  (is (= (dll val2 before-val val3) (dll/between l1 (big-dec 1) (big-dec 4))))
  (is (= (dll val2 before-val) (dll/between l1 (big-dec 1) (big-dec 3))))
  (is (= (dll before-val val3) (dll/between l1 (big-dec 2) (big-dec 4)))))

(deftest range-test
  (is (= (dll val1) (dll/range l (big-dec 1) (big-dec 1))))
  (is (= (dll val1 val2 val3) (dll/range l (big-dec 1) (big-dec 3))))
  (is (= (dll val1 val2) (dll/range l (big-dec 1) (big-dec 2))))
  (is (= (dll val1 val2 before-val val3 val4) (dll/range l1 (big-dec 1) (big-dec 4))))
  (is (= (dll val2 before-val val3 val4) (dll/range l1 (big-dec 2) (big-dec 4)))))

(deftest from-map-test
  (is (= (dll/from-indexed-items [[(big-dec 1) :a]
                        [(big-dec 3) :b]
                        [(big-dec 3.5) :c]
                        [(big-dec 4) :d]
                        [(big-dec 4.1) :e]])
         (dll :a :b :c :d :e)))
  (is (= (dll/all-indices (dll/from-indexed-items [[(big-dec 1) :a]
                                         [(big-dec 3) :b]
                                         [(big-dec 3.5) :c]
                                         [(big-dec 4) :d]
                                         [(big-dec 4.1) :e]]))
         [(big-dec 1) (big-dec 3) (big-dec 3.5) (big-dec 4) (big-dec 4.1)])))

(deftest decimal-division-test
  ;; This is really just a sanity test -- making sure that the
  ;; Decimal object can withstand a ton of divisions without loss of
  ;; precision. 10,000 iterations is chosen because JS's builtin number
  ;; type will underflow after just over 1,000 divisions.
  (let [iters 10000
        lots-of-halving (loop [n (big-dec 1), i 0]
                          (if (>= i iters)
                            n
                            (recur (.div n 2) (inc i))))]
    (is (not (.eq lots-of-halving 0)))
    (is (.gt lots-of-halving 0))
    (is (dll/big-dec? lots-of-halving))
    #_(is (.eq (big-dec 1)
             (.mul lots-of-halving (.pow (big-dec 2) iters))))))
