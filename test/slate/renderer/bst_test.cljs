(ns slate.renderer.bst-test
  (:require [cljs.test :include-macros true :refer [is deftest testing]]
            [slate.renderer.bst :as bst]))

(deftest basic-test
  (let [my-tree (bst/init-tree)
        vm1 {:text "a" :px-height 10}
        vm2 {:text "b" :px-height 10}
        vm3 {:text "c" :px-height 10}]
    (bst/insert! my-tree 1 vm1)
    (bst/insert! my-tree 2 vm2)
    (bst/insert! my-tree 3 vm3)

    (is (= vm1 (.-viewmodel (bst/at-y my-tree 5))))
    (is (= vm2 (.-viewmodel (bst/at-y my-tree 15))))
    (is (= vm3 (.-viewmodel (bst/at-y my-tree 25))))

    (bst/delete! my-tree 2)
    (bst/insert! my-tree 2 {:text "b" :px-height 30})

    (is (= vm1 (.-viewmodel (bst/at-y my-tree 5))))
    (is (= {:text "b" :px-height 30} (.-viewmodel (bst/at-y my-tree 15))))
    (is (= {:text "b" :px-height 30} (.-viewmodel (bst/at-y my-tree 25))))
    (is (= {:text "b" :px-height 30} (.-viewmodel (bst/at-y my-tree 35))))
    (is (= vm3 (.-viewmodel (bst/at-y my-tree 45))))
    ))
