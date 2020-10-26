(ns drop.editor.selection-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.selection :as sel :refer [selection]]))

(def sel-single (selection [:p1 10]))
(def sel-range (selection [:p1 10] [:p1 20]))
(def sel-backwards (selection [:p1 10] [:p1 20] true))

;; Some mock paragraphs
(deftest selection-init-test
  (testing "Basic initialization"
    (let [sel (selection [:p1 0] [:p2 10] true)]
      (is (= (into {} sel) {:start {:paragraph :p1, :offset 0}
                            :end {:paragraph :p2, :offset 10}
                            :backwards? true}))))
  (testing "Different ways of initializing selection"
    (let [s1 (selection [:p1 0] [:p1 0] false)
          s2 (selection [:p1 0] [:p1 0])
          s3 (selection [:p1 0])]
      (is (= (into {} s1) {:start {:paragraph :p1, :offset 0}
                           :end {:paragraph :p1, :offset 0}
                           :backwards? false}))
      (is (= s1 s2 s3)))))

(deftest caret-test
  (is (= (sel/caret sel-single) 10))
  (is (= (sel/caret sel-range) 20))
  (is (= (sel/caret sel-backwards) 10)))

(deftest single?-test
  (let [s1 (selection [:p1 1])
        s2 (selection [:p1 1] [:p1 10])
        s3 (selection [:p1 1] [:p1 1])]
    (is (sel/single? s1))
    (is (not (sel/single? s2)))
    (is (sel/single? s3))))

(deftest shift-single-test
  (is (= (+ 5 (sel/caret sel-single))
         (sel/caret (sel/shift-single sel-single 5))))
  (is (thrown? js/Error (sel/shift-single sel-range 10)))
  (is (thrown? js/Error (sel/shift-single sel-single -10000))))

(deftest set-single-test
  (is (= 255 (-> sel-single (sel/set-single 255) sel/caret)))
  (is (thrown? js/Error (sel/set-single sel-range 10)))
  (is (thrown? js/Error (sel/set-single sel-single -1)))
  (is (= 0 (sel/caret (sel/set-single sel-single 0)))))

(deftest collapse-test
  (let [sel (selection [:p1 10] [:p1 20])
        collapsed-start (sel/collapse-start sel)
        collapsed-end (sel/collapse-end sel)]
    (= (sel/caret collapsed-start) 10)
    (= (sel/caret collapsed-end) 20)))
