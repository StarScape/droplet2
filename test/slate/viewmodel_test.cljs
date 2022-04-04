(ns slate.viewmodel-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.model.run :as r]
            [slate.measurement :refer [fake-measure-fn]]
            [slate.viewmodel :as vm]))

(defn unrecord
  "Recursively converts records to maps inside an arbitrary structure of maps and vectors."
  [structure]
  (cond
    (vector? structure) (mapv unrecord structure)
    (map? structure) (reduce-kv (fn [new-map k v]
                                     (assoc new-map k (unrecord v)))
                                   {} structure)
    :else structure))

(def runs [(r/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now.")])
(def runs-formatted [(r/run "foobar bizz " #{})
                     (r/run "buzz hello hello goodbye. And " #{:italic})
                     (r/run "this should" #{:bold})
                     (r/run " be " #{:bold :italic})
                     (r/run "on the second line " #{})
                     (r/run "now." #{:underline})])

(deftest lineify-test
  (testing "works with an empty run"
    (is (= (unrecord (vm/lineify [(r/run "")] :body 300 fake-measure-fn))
           [{:start-offset 0
             :end-offset 0
             :width 0
             :spans [{:text "", :formats #{}, :start-offset 0, :width 0}]}])))

  (testing "works with a run that fits on just one line"
    (is (= (unrecord (vm/lineify [(r/run "foobar")] :body 300 fake-measure-fn))
           [{:start-offset 0
             :end-offset 6
             :width 60
             :spans [{:text "foobar"
                      :formats #{}
                      :start-offset 0
                      :width 60}]}])))

  (testing "works with a big long run"
    (is (= (unrecord (vm/lineify runs :body 300 fake-measure-fn))
           [{:start-offset 0
             :end-offset 29
             :width 290
             :spans [{:text "foobar bizz buzz hello hello "
                      :formats #{}
                      :start-offset 0
                      :width 290}]}
            {:start-offset 29
             :end-offset 60
             :width 310
             :spans [{:text "goodbye. And this should be on "
                      :formats #{}
                      :start-offset 29
                      :width 310}]}
            {:start-offset 60
             :end-offset 80
             :width 200
             :spans [{:text "the second line now."
                      :formats #{}
                      :start-offset 60
                      :width 200}]}])))

  (testing "works with a bunch of small runs"
    (is (= (unrecord (vm/lineify runs-formatted :body 300 fake-measure-fn))
           [{:start-offset 0
             :end-offset 29
             :width 290
             :spans [{:text "foobar bizz "
                      :formats #{}
                      :start-offset 0
                      :width 120}
                     {:text "buzz hello hello "
                      :formats #{:italic}
                      :start-offset 12
                      :width 170}]}
            {:start-offset 29
             :end-offset 60
             :width 310
             :spans [{:text "goodbye. And "
                      :formats #{:italic}
                      :start-offset 29
                      :width 130}
                     {:text "this should"
                      :formats #{:bold}
                      :start-offset 42
                      :width 110}
                     {:text " be "
                      :formats #{:bold :italic}
                      :start-offset 53
                      :width 40}
                     {:text "on "
                      :formats #{}
                      :start-offset 57
                      :width 30}]}
            {:start-offset 60
             :end-offset 80
             :width 200
             :spans [{:text "the second line "
                      :formats #{}
                      :start-offset 60
                      :width 160}
                     {:text "now."
                      :formats #{:underline}
                      :start-offset 76
                      :width 40}]}])))

  (testing "works with a word too large to fit on a line"
    (is (= (unrecord (vm/lineify [(r/run "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")] :body 300 fake-measure-fn))
           [{:spans [{:text "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      :formats #{}
                      :start-offset 0
                      :width 300}]
             :start-offset 0
             :end-offset 30
             :width 300}
            {:spans [{:text "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      :formats #{}
                      :start-offset 30
                      :width 300}]
             :start-offset 30
             :end-offset 60, :width 300}
            {:spans [{:text "aaaaaaaaaa"
                      :formats #{}
                      :start-offset 60
                      :width 100}]
             :start-offset 60
             :end-offset 70
             :width 100}]))))

(deftest from-para-test
  )

#_[(r/run "foobar bizz " #{})
 (r/run "buzz hello hello goodbye. And " #{:italic})
 (r/run "this should" #{:bold})
 (r/run " be " #{:bold :italic})
 (r/run "on the second line " #{})
 (r/run "now." #{:underline})]