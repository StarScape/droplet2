(ns slate.model.editor-state-history-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [clojure.spec.alpha :as s]
            [slate.model.editor-state :as es :refer [editor-state]]
            [slate.model.editor-state-history :as history]))

(defn resolved-editor-state
  []
  (-> (es/editor-state)
      (assoc-in [:changelist :resolved?] true)))

(deftest spec-test
  (testing "spec catches all invalid states"
    (is (= false
           (s/valid? ::history/editor-state-history
                     {:backstack []
                      :current-state-index 0
                      :tip nil})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (resolved-editor-state)))
                      :current-state-index 4
                      :tip nil})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (resolved-editor-state)))
                      :current-state-index 1
                      :tip (resolved-editor-state)})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (resolved-editor-state)))
                      :current-state-index 5
                      :tip nil})))
    (is (= true
           (s/valid? ::history/editor-state-history
                     {:backstack []
                      :current-state-index 0
                      :tip (resolved-editor-state)})
           (s/valid? ::history/editor-state-history
                     {:backstack [(resolved-editor-state)]
                      :current-state-index 1
                      :tip (resolved-editor-state)})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (resolved-editor-state)))
                      :current-state-index 1
                      :tip nil})
           (s/valid? ::history/editor-state-history
                     {:backstack (vec (repeat 4 (resolved-editor-state)))
                      :current-state-index 3
                      :tip nil})))))

(deftest add-tip-to-backstack-test
  (let [backstack (vec (repeat 4 (resolved-editor-state)))
        tip (resolved-editor-state)]
    (is (= (history/add-tip-to-backstack {:backstack backstack
                                          :current-state-index 4
                                          :tip tip})
           {:backstack (conj backstack tip)
            :current-state-index 4
            :tip nil}))))

(deftest current-state-test
  (let [state (resolved-editor-state)]
    (is (= (history/current-state {:backstack (vec (repeat 4 (resolved-editor-state)))
                                   :current-state-index 4
                                   :tip state})
           state))
    (is (= (history/current-state {:backstack (-> (repeat 4 (resolved-editor-state))
                                                  (vec)
                                                  (conj state))
                                   :current-state-index 4
                                   :tip nil})
           state))
    (is (= (history/current-state {:backstack (-> [state]
                                                  (concat (repeat 4 (resolved-editor-state)))
                                                  (vec))
                                   :current-state-index 0
                                   :tip nil})
           state))
    (is (not= (history/current-state {:backstack (-> [state]
                                                     (concat (repeat 4 (resolved-editor-state)))
                                                     (vec))
                                      :current-state-index 1
                                      :tip nil})
              state))))

(deftest has-undo?-test
  (is (= true
         (history/has-undo? {:backstack (vec (repeat 4 (resolved-editor-state)))
                             :current-state-index 4
                             :tip (resolved-editor-state)})
         (history/has-undo? {:backstack [(resolved-editor-state)]
                             :current-state-index 1
                             :tip (resolved-editor-state)})))
  (is (= false
         (history/has-undo? {:backstack (vec (repeat 4 (resolved-editor-state)))
                             :current-state-index 0
                             :tip nil})
         (history/has-undo? {:backstack []
                             :current-state-index 0
                             :tip (resolved-editor-state)})
         (history/has-undo? {:backstack [(resolved-editor-state)]
                             :current-state-index 0
                             :tip nil}))))

(deftest has-redo?-test
  (is (= false
         (history/has-redo? {:backstack (vec (repeat 4 (resolved-editor-state)))
                             :current-state-index 4
                             :tip (resolved-editor-state)})
         (history/has-redo? {:backstack [(resolved-editor-state)]
                             :current-state-index 1
                             :tip (resolved-editor-state)})))
  (is (= true
         (history/has-redo? {:backstack (vec (repeat 4 (resolved-editor-state)))
                             :current-state-index 0
                             :tip nil})
         (history/has-redo? {:backstack [(resolved-editor-state) (resolved-editor-state)]
                             :current-state-index 0
                             :tip nil}))))


