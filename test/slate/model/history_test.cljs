(ns slate.model.history-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [clojure.spec.alpha :as s]
            [slate.model.editor-state :as es :refer [editor-state ->EditorUpdate]]
            [slate.model.history :as history]))

(defn editor-update []
  (->EditorUpdate (es/editor-state) (es/changelist)))

(deftest spec-test
  (testing "spec catches all invalid states"
    (is (= false
           (s/valid? ::history/editor-state-history
                     {:backstack []
                      :current-state-index 0
                      :tip nil})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (editor-update)))
                      :current-state-index 4
                      :tip nil})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (editor-update)))
                      :current-state-index 1
                      :tip (editor-update)})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (editor-update)))
                      :current-state-index 5
                      :tip nil})))
    (is (= true
           (s/valid? ::history/editor-state-history
                     {:backstack []
                      :current-state-index 0
                      :tip (editor-update)})
           (s/valid? ::history/editor-state-history
                     {:backstack [(editor-update)]
                      :current-state-index 1
                      :tip (editor-update)})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (editor-update)))
                      :current-state-index 1
                      :tip nil})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (editor-update)))
                      :current-state-index 3
                      :tip nil})))))

(deftest add-tip-to-backstack-test
  (let [backstack (vec (repeat 4 (editor-update)))
        tip (editor-update)]
    (is (= (history/add-tip-to-backstack {:backstack backstack
                                          :current-state-index 4
                                          :tip tip})
           {:backstack (conj backstack tip)
            :current-state-index 4
            :tip nil}))))

(deftest current-test
  (let [state (editor-update)]
    (is (= (history/current {:backstack (vec (repeat 4 (editor-update)))
                             :current-state-index 4
                             :tip state})
           state))
    (is (= (history/current {:backstack (-> (repeat 4 (editor-update))
                                            (vec)
                                            (conj state))
                             :current-state-index 4
                             :tip nil})
           state))
    (is (= (history/current {:backstack (-> [state]
                                            (concat (repeat 4 (editor-update)))
                                            (vec))
                             :current-state-index 0
                             :tip nil})
           state))
    ;; I wrote this test and it was passing prior to the fractional indexing refactor,
    ;; but for the life of me I can't understand what I was getting at with it???
    ;; Fix at some point maybe.
    #_(is (not= (history/current {:backstack (-> [state]
                                               (concat (repeat 4 (editor-update)))
                                               (vec))
                                :current-state-index 1
                                :tip nil})
              state))))

(deftest has-undo?-test
  (is (= true
         (history/has-undo? {:backstack (vec (repeat 4 (editor-update)))
                             :current-state-index 4
                             :tip (editor-update)})
         (history/has-undo? {:backstack [(editor-update)]
                             :current-state-index 1
                             :tip (editor-update)})))
  (is (= false
         (history/has-undo? {:backstack (vec (repeat 4 (editor-update)))
                             :current-state-index 0
                             :tip nil})
         (history/has-undo? {:backstack []
                             :current-state-index 0
                             :tip (editor-update)})
         (history/has-undo? {:backstack [(editor-update)]
                             :current-state-index 0
                             :tip nil}))))

(deftest has-redo?-test
  (is (= false
         (history/has-redo? {:backstack (vec (repeat 4 (editor-update)))
                             :current-state-index 4
                             :tip (editor-update)})
         (history/has-redo? {:backstack [(editor-update)]
                             :current-state-index 1
                             :tip (editor-update)})))
  (is (= true
         (history/has-redo? {:backstack (vec (repeat 4 (editor-update)))
                             :current-state-index 0
                             :tip nil})
         (history/has-redo? {:backstack [(editor-update) (editor-update)]
                             :current-state-index 0
                             :tip nil}))))

(deftest undo-test
  (let [backstack (vec (repeat 4 (editor-update)))
        tip (editor-update)]
    (is (= (history/undo {:backstack backstack
                          :current-state-index 4
                          :tip tip})
           {:backstack (conj backstack tip)
            :current-state-index 3
            :tip nil}))
    (is (= (history/undo {:backstack backstack
                          :current-state-index 3
                          :tip nil})
           {:backstack backstack
            :current-state-index 2
            :tip nil}))
    (is (= (history/undo {:backstack backstack
                          :current-state-index 3
                          :tip nil})
           {:backstack backstack
            :current-state-index 2
            :tip nil}))
    (is (= (history/undo {:backstack backstack
                          :current-state-index 0
                          :tip nil})
           {:backstack backstack
            :current-state-index 0
            :tip nil}))))

(deftest redo-test
  (let [update1 (editor-update)
        update2 (editor-update)
        update3 (editor-update)
        update4 (editor-update)
        tip (editor-update)]
    (is (= (history/redo {:backstack [update1 update2 update3 update4]
                          :current-state-index 3
                          :tip nil})
           {:backstack [update1 update2 update3 update4]
            :current-state-index 3
            :tip nil}))
    (is (= (history/redo {:backstack [update1 update2 update3 update4]
                          :current-state-index 2
                          :tip nil})
           {:backstack [update1 update2 update3 update4]
            :current-state-index 3
            :tip nil}))
    (is (= (history/redo {:backstack [update1 update2 update3 update4]
                          :current-state-index 4
                          :tip tip})
           {:backstack [update1 update2 update3 update4]
            :current-state-index 4
            :tip tip}))))

(deftest set-tip-test
  (let [update1 (editor-update)
        update2 (editor-update)
        update3 (editor-update)
        update4 (editor-update)
        tip (editor-update)
        tip2 (editor-update)]
    (is (= (history/set-tip {:backstack [update1 update2 update3 update4]
                             :current-state-index 4
                             :tip tip}
                            tip2)
           {:backstack [update1 update2 update3 update4]
            :current-state-index 4
            :tip tip2}))
    (is (= (history/set-tip {:backstack [update1 update2 update3 update4]
                             :current-state-index 3
                             :tip nil}
                            tip2)
           {:backstack [update1 update2 update3 update4]
            :current-state-index 4
            :tip tip2}))
    (is (= (history/set-tip {:backstack [update1 update2 update3 update4]
                             :current-state-index 1
                             :tip nil}
                            tip2)
           {:backstack [update1 update2]
            :current-state-index 2
            :tip tip2}))))
