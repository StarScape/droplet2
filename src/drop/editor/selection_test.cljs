(ns drop.editor.selection-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.core :as c]))

(def sel-single (c/selection [:p1 10]))
(def sel-range (c/selection [:p1 10] [:p1 20]))
(def sel-backwards (c/selection [:p1 10] [:p1 20] true))

;; Some mock paragraphs
(deftest selection-init-test
  (testing "Basic initialization"
    (let [sel (c/selection [:p1 0] [:p2 10] true)]
      (is (= (into {} sel) {:start {:paragraph :p1, :offset 0}
                            :end {:paragraph :p2, :offset 10}
                            :backwards? true}))))
  (testing "Different ways of initializing selection"
    (let [s1 (c/selection [:p1 0] [:p1 0] false)
          s2 (c/selection [:p1 0] [:p1 0])
          s3 (c/selection [:p1 0])]
      (is (= (into {} s1) {:start {:paragraph :p1, :offset 0}
                           :end {:paragraph :p1, :offset 0}
                           :backwards? false}))
      (is (= s1 s2 s3)))))

(deftest caret-test
  (is (= (c/caret sel-single) 10))
  (is (= (c/caret sel-range) 20))
  (is (= (c/caret sel-backwards) 10)))

(deftest single?-test
  (let [s1 (c/selection [:p1 1])
        s2 (c/selection [:p1 1] [:p1 10])
        s3 (c/selection [:p1 1] [:p1 1])]
    (is (c/single? s1))
    (is (not (c/single? s2)))
    (is (c/single? s3))))

(deftest shift-single-test
  (is (=
       (+ 5 (c/caret sel-single))
       (-> sel-single (c/shift-single 5) c/caret))))

(deftest set-single-test
  (is (= 255 (-> sel-single (c/set-single 255) c/caret))))

(deftest collapse-test
  (let [sel (c/selection [:p1 10] [:p1 20])
        collapsed-start (c/collapse-start sel)
        collapsed-end (c/collapse-end sel)]
    (= (c/caret collapsed-start) 10)
    (= (c/caret collapsed-end) 20)))

;; TODO: write tests for Paragraph and Run
