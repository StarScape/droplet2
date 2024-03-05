(ns slate.renderer.bst)

;; The astute observer will notice this is very un-Clojure-y ClojureScript.
;; More like JS with parens, in fact. The reason are various:
;;
;; (1) A complex, mutable data structure is needed for performance's sake.
;;
;; (2) Clojure is not exactly designed with manipulating complex mutable structures
;; in mind, and so dealing with a binary tree is less clear in Clojure than it might
;; be in JS or another imperative language.
;;
;; (3) It's still easier to write the code for the data structure in CLJS, rather
;; than writing it in JS and then dealing with having JS and CLJS in the same project,
;; interop, etc.
;;
;; For this reason I have chosen in some places to annotate the CLJS code with comments showing
;; the equivalent line in JS, for clarity's sake.

(set! *warn-on-infer* false)

(defn lt [a b] (< a b))
(defn gt [a b] (> a b))

(deftype AVLNode [^:mutable index
                  ^:mutable viewmodel
                  ^:mutable left
                  ^:mutable right
                  ^:mutable height
                  ^:mutable left-px-height]
  IPrintWithWriter
  (-pr-writer [this writer opts] (-write writer (js/JSON.stringify this nil 2))))

(defn init-node
  [index, viewmodel]
  (AVLNode. index viewmodel nil nil 1 0))

(defn height
  "Get the height of a node."
  [node]
  (if node (.-height node) 0))

(defn balance-factor
  "Get the balance factor of a node."
  [node]
  (if node
    (- (height (.-left node)) (height (.-right node)))
    0))

(defn px-height [node]
  (:px-height (.. node -viewmodel)))

(defn left-px-height [node]
  (if node (.-left-px-height node) 0))

(defn total-px-height
  [node]
  (if node
    (+ (.-left-px-height node) (.. node -viewmodel -px-height))
    0))

(defn find-min-node
  [node]
  (if-not (.-left node)
    node
    (recur (.-left node))))

(defn rotate-right!
  "Performs a right rotation on BST node."
  [node]
  (let [left-node (.-left node)]
    ;; node.left = leftNode.right;
    (set! (.-left node) (.-right left-node))
    ;; leftNode.right = node;
    (set! (.-right left-node) node)
    ;; node.leftPxHeight -= (leftPxHeight(leftNode) + heightPx(leftNode));
    (set! (.-left-px-height node) (- (.-left-px-height node) (left-px-height left-node) (px-height left-node)))
    ;; node.height = Math.max(height(node.left), height(node.right)) + 1;
    (set! (.-height node) (max (height (.-left node)) (inc (height (.-right node)))))
    ;; leftNode.height = Math.max(height(leftNode.left), height(leftNode.right)) + 1;
    (set! (.-height left-node) (inc (max (height (.-left left-node)) (height (.-right left-node)))))
    ;; return leftNode;
    left-node))

(defn rotate-left!
  "Performs a left rotation on BST node."
  [node]
  (let [right-node (.-right node)]
    ;; node.right = rightNode.left;
    (set! (.-right node) (.-left right-node))
    ;; rightNode.left = node;
    (set! (.-left right-node) node)
    ;; rightNode.leftPxHeight += (leftPxHeight(node) + pxHeight(node))
    (set! (.-left-px-height right-node) (+ (left-px-height right-node) (left-px-height node) (px-height node)))
    ;; node.height = Math.max(height(node.left), height(node.right)) + 1;
    (set! (.-height node) (inc (max (height (.-left node)) (height (.-right node)))))
    ;; rightNode.height = Math.max(height(rightNode.left), height(rightNode.right)) + 1;
    (set! (.-height right-node) (inc (max (height (.-left right-node)), (height (.-right right-node)))))
    ;; return rightNode;
    right-node))

(defn node-insert! [node index viewmodel]
  (cond
    (not node) (init-node index viewmodel)

    ;; duplicate indices are not allowed, return node unchanged
    (= node index) node

    :else
    (do
      (cond
        (lt index (.-index node))
        (do
          (set! (.-left node) (node-insert! (.-left node) index viewmodel))
          (set! (.-left-px-height node) (+ (left-px-height node) (:px-height viewmodel))))

        (gt index (.-index node))
        (set! (.-right node) (node-insert! (.-right node) index viewmodel)))
      (set! (.-height node) (inc (max (height (.-left node)) (height (.-right node)))))

      ;; Rebalance
      (let [balance (balance-factor node)]
        (cond
          (and (< 1 balance) (lt index (.. node -left -index)))
          (rotate-right! node)

          (and (< 1 balance) (gt index (.. node -left -index)))
          (do
            (set! (.-left node) (rotate-left! (.-left node)))
            (rotate-right! node))

          (and (< balance -1) (gt index (.. node -right -index)))
          (rotate-left! node)

          (and (< balance -1) (gt index (.. node -right -index)))
          (do
            (set! (.-right node) (rotate-right! (.-right node)))
            (rotate-left! node))

          :else node)))))

(defn node-search [node index]
  (when node
    (cond
      (lt index (.-index node)) (recur (.-left node) index)
      (gt index (.-index node)) (recur (.-right node) index)
      :else node)))

(defn node-delete! [node index]
  (when node
    (let [new-node (cond
                     (lt index (.-index node))
                     (doto node
                       (aset "left" (node-delete! (.-left node) index))
                       (aset "left-px-height" (left-px-height (.-left node)) (px-height (.-left node))))

                     (gt index (.-index node))
                     (doto node
                       (aset "right" (node-delete! (.-right node) index)))

                     :else
                     (cond
                       ;; Node to be deleted has two children
                       (and (.-left node) (.-right node))
                       (let [min-node (find-min-node (.-right node))]
                         (doto node
                           (aset "index" (.-index min-node))
                           (aset "viewmodel" (.-viewmodel min-node))
                           (aset "right" (node-delete! (.-right node) (.-index min-node)))))

                       ;; Node to be deleted has only left child
                       (.-left node) (.-left node)

                       ;; Node to be deleted has only right child
                       (.-right node) (.-right node)

                       ;; Node to be deleted has no children
                       :else nil))]
      (when new-node
        (let [balance (balance-factor (doto new-node
                                        (aset "height" (inc (max (height (.-left new-node)) (height (.-right new-node)))))))
              left-balance (balance-factor (.-left new-node))
              right-balance (balance-factor (.-right new-node))]
          (cond
            (and (< 1 balance) (>= left-balance 0))
            (rotate-right! new-node)

            (and (< 1 balance) (< left-balance 0))
            (doto new-node
              (aset "left" (rotate-left! (.-left new-node)))
              (rotate-right!))

            (and (< balance -1) (<= right-balance 0))
            (rotate-left! new-node)

            (and (< balance -1) (> right-balance 0))
            (doto new-node
              (aset "right" (rotate-right! (.-right new-node)))
              (rotate-left!))

            :else new-node))))))

(defn node-at-y
  [node target-y running-count]
  (let [height-above (+ (left-px-height node) running-count)]
    (cond
      ;; target-y overlaps with node -- hit
      (and (<= height-above target-y)
           (< target-y (+ height-above (px-height node))))
      node

      ;; target-y in node before this, ie left in BST
      (< target-y height-above)
      (recur (.-left node) target-y running-count)

      ;; node in node after this, ie right in BST
      (> target-y height-above)
      (recur (.-right node) target-y (+ running-count (left-px-height node) (px-height node))))))

(deftype AVLTree [^:mutable root])

(defn init-tree []
  (AVLTree. nil))

(defn insert!
  [tree index viewmodel]
  (set! (.-root tree) (node-insert! (.-root tree) index viewmodel))
  tree)

(defn search
  [tree index]
  (node-search (.-root tree) index))

(defn delete!
  [tree index]
  (set! (.-root tree) (node-delete! (.-root tree) index)))

(defn at-y
  [tree target-y-offset]
  (node-at-y (.-root tree) target-y-offset 0))

(comment
  (init-node 1 (js-obj :foo 1, :bar 2)))

(def my-tree (init-tree))

(insert! my-tree 1 {:text "a" :px-height 10})
(insert! my-tree 2 {:text "b" :px-height 10})
(insert! my-tree 3 {:text "c" :px-height 10})

(.-viewmodel (at-y my-tree 5))
(.-viewmodel (at-y my-tree 15))
(.-viewmodel (at-y my-tree 25))

(delete! my-tree 2)
(insert! my-tree 2 {:text "b" :px-height 30})
(.-viewmodel (at-y my-tree 5))
(.-viewmodel (at-y my-tree 15))
(.-viewmodel (at-y my-tree 25))
(.-viewmodel (at-y my-tree 35))
(.-viewmodel (at-y my-tree 45))

