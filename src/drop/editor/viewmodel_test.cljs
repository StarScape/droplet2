(ns drop.editor.viewmodel-test
  (:require ["./CharRuler" :refer (fakeRuler)]
            [cljs.test :include-macros true :refer [is deftest testing]]
            [drop.editor.core :as c]
            [drop.editor.viewmodel :as vm]))

(defn unrecord
  "Recursively converts records to maps inside an arbitrary structure of maps and vectors."
  [structure]
  (cond
    (vector? structure) (mapv unrecord structure)
    (map? structure) (into {} structure)
    :else (throw (js/Error. (str "Unrecognized data type in unrecord: " (type structure))))))

(def runs [(c/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now.")])
(def runs-formatted [(c/run "foobar bizz " #{})
                     (c/run "buzz hello hello goodbye. And " #{:italic})
                     (c/run "this should" #{:bold})
                     (c/run " be " #{:bold :italic})
                     (c/run "on the second line " #{})
                     (c/run "now." #{:underline})])

(deftest lineify-test
  (testing "works with an empty run"
    (is (= (unrecord (vm/lineify [(c/run "")] 300 fakeRuler))
           [{:start-offset 0
             :end-offset 0
             :width 0
             :spans []}])))

  (testing "works with a run that fits on just one line"
    (is (= (unrecord (vm/lineify [(c/run "foobar")] 300 fakeRuler))
           [{:start-offset 0
             :end-offset 6
             :width 60
             :spans [{:text "foobar"
                      :start-offset 0
                      :width 60}]}])))

  (testing "works with a big long run"
    (is (= (unrecord (vm/lineify runs 300 fakeRuler))
           [{:start-offset 0
             :end-offset 29
             :width 290
             :spans [{:text "foobar bizz buzz hello hello "
                      ;;:formats #{}
                      :start-offset 0
                      :width 290}]}
            {:start-offset 29
             :end-offset 60
             :width 310
             :spans [{:text "goodbye. And this should be on "
                      ;;:formats #{}
                      :start-offset 29
                      :width 310}]}
            {:start-offset 60
             :end-offset 80
             :width 200
             :spans [{:text "the second line now."
                      ;;:formats #{}
                      :start-offset 60
                      :width 200}]}])))

  (testing "works with a bunch of small runs"
    (is (= (unrecord (vm/lineify runs-formatted 300 fakeRuler))
           [{:start-offset 0
             :end-offset 29
             :width 290
             :spans [{:text "foobar bizz "
                      ;;:formats #{}
                      :start-offset 0
                      :width 120}
                     {:text "buzz hello hello "
                      ;;:formats #{:italic}
                      :start-offset 12
                      :width 170}]}
            {:start-offset 29
             :end-offset 60
             :width 310
             :spans [{:text "goodbye. And "
                      ;;:formats #{:italic}
                      :start-offset 29
                      :width 130}
                     {:text "this should"
                      ;;:formats #{:bold}
                      :start-offset 42
                      :width 110}
                     {:text " be "
                      ;;:formats #{:bold :italic}
                      :start-offset 53
                      :width 40}
                     {:text "on "
                      ;;:formats #{}
                      :start-offset 57
                      :width 30}]}
            {:start-offset 60
             :end-offset 80
             :width 200
             :spans [{:text "the second line "
                      ;;:formats #{}
                      :start-offset 60
                      :width 160}
                     {:text "now."
                      ;;:formats #{:underline}
                      :start-offset 76
                      :width 40}]}]))))

#_[(c/run "foobar bizz " #{})
 (c/run "buzz hello hello goodbye. And " #{:italic})
 (c/run "this should" #{:bold})
 (c/run " be " #{:bold :italic})
 (c/run "on the second line " #{})
 (c/run "now." #{:underline})]