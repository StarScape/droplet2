(ns slate.model.selection-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.selection :as sel :refer [selection map->Selection]]))

(def sel-single (selection [:p1 10]))
(def sel-range (selection [:p1 10] [:p1 20]))
(def sel-backwards (selection [:p1 10] [:p1 20] :backwards? true))

;; Some mock paragraphs
(deftest selection-init-test
  (testing "Basic initialization"
    (let [sel (selection [:p1 0] [:p2 10] :backwards? true)]
      (is (= sel (map->Selection {:start {:paragraph :p1, :offset 0}
                                  :end {:paragraph :p2, :offset 10}
                                  :backwards? true
                                  :between #{}
                                  :formats #{}})))))
  (testing "Different ways of initializing selection"
    (let [s1 (selection [:p1 0] [:p1 0] :backwards? false)
          s2 (selection [:p1 0] [:p1 0])
          s3 (selection [:p1 0])
          s4 (selection [:p1 0] :formats #{:italic})
          s4-alt1 (selection :start [:p1 0] :formats #{:italic})
          s4-alt2 (selection :start [:p1 0] :end [:p1 0] :formats #{:italic})
          s5 (selection :start [:p1 0] :end [:p2 3])
          s6 (selection :start [:p1 0] :end [:p2 3] :between #{:p1.5})]
      (is (= s1 (map->Selection {:start {:paragraph :p1, :offset 0}
                                 :end {:paragraph :p1, :offset 0}
                                 :backwards? false
                                 :between #{}
                                 :formats #{}})))
      (is (= s1 s2 s3))
      (is (= s4 (map->Selection {:start {:paragraph :p1, :offset 0}
                                 :end {:paragraph :p1, :offset 0}
                                 :backwards? false
                                 :between #{}
                                 :formats #{:italic}})))
      (is (= s4 s4-alt1 s4-alt2))
      (is (= s5 (map->Selection {:start {:paragraph :p1, :offset 0}
                                 :end {:paragraph :p2, :offset 3}
                                 :backwards? false
                                 :between #{}
                                 :formats #{}})))
      (is (= s6 (map->Selection {:start {:paragraph :p1, :offset 0}
                                 :end {:paragraph :p2, :offset 3}
                                 :backwards? false
                                 :between #{:p1.5}
                                 :formats #{}}))))))

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
  (is (= 0 (sel/caret (sel/set-single sel-single 0)))))

(deftest collapse-test
  (let [sel (selection [:p1 10] [:p1 20])
        collapsed-start (sel/collapse-start sel)
        collapsed-end (sel/collapse-end sel)]
    (is (= (sel/caret collapsed-start) 10))
    (is (= (sel/caret collapsed-end) 20))))

(def between-sel (selection [:p1 0] [:p4 10] :between #{:p2 :p3}))

(deftest between-test
  (is (= (sel/collapse-start between-sel)
         (map->Selection {:start {:paragraph :p1
                                  :offset 0}
                          :end {:paragraph :p1
                                :offset 0}
                          :backwards? false
                          :between #{}
                          :formats #{}})))
  (is (= (sel/collapse-end between-sel)
         (map->Selection {:start {:paragraph :p4
                                  :offset 10}
                          :end {:paragraph :p4
                                :offset 10}
                          :backwards? false
                          :between #{}
                          :formats #{}})))
  (is (= (sel/smart-collapse between-sel)
         (map->Selection {:start {:paragraph :p4
                                  :offset 10}
                          :end {:paragraph :p4
                                :offset 10}
                          :backwards? false
                          :between #{}
                          :formats #{}}))))

(deftest add-to-between
  (let [sel (selection ["p1" 0] ["p4" 0] :between #{"p2"})]
    (is (= (sel/add-to-between sel "p3")
           (selection ["p1" 0] ["p4" 0] :between #{"p2" "p3"})))
    (is (= sel (sel/add-to-between sel "p1")))))

(deftest all-uuids
  (is (= (sel/all-uuids (selection [:p1 0] [:p4 0] :between #{:p2 :p3}))
         #{:p1 :p2 :p3 :p4}))
  (is (= (sel/all-uuids (selection [:p1 0] [:p2 0]))
         #{:p1 :p2}))
  (is (= (sel/all-uuids (selection [:p1 0] [:p1 1]))
         #{:p1})))

(deftest toggle-format-test
  (let [s (selection [:p1 0] :formats #{:italic})]
    (is (= (-> s (sel/toggle-format :italic) :formats) #{}))
    (is (= (-> s (sel/toggle-format :bold) :formats) #{:italic :bold}))))
