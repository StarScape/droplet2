(ns drop.editor.dll
  ;; TODO: more detailed description
  "A fully-persistent, UUID-based doubly-linked list. Used for the list of paragraphs."
  (:refer-clojure :exclude [last next]))

;;(declare first)
;;(declare last)
;;(declare next)
;;(declare prev)
(declare insert-after)
(declare insert-before)

;; entries-map :: Map (UUID -> DLLEntry)
;; DLLEntry :: {uuid, prev-uuid, next-uuid, value}

(deftype Node [^obj value ^string prev-uuid ^string next-uuid]
  ;; Implement pretty-printing for easier debugging
  IPrintWithWriter
  (-pr-writer [n writer opts]
   (-write writer "#Node{value: ") (-write writer (.-value n))
   (-write writer ", prev-uuid: ") (-write writer (.-prev-uuid n))
   (-write writer ", next-uuid: ") (-write writer (.-next-uuid n))
   (-write writer "}")))

;; TODO: make ISeqable
;; TODO: make map and filterable - looks like (map) at least just uses first and rest
(deftype DoublyLinkedList [entries-map first-uuid last-uuid]
  ISeq
  (-first [^DoublyLinkedList dll] (get (.-entries-map dll) (.-first-uuid dll)))

  ICounted
  (-count [dll] (count (.-entries-map dll)))

  ICollection
  ;; TODO: rewrite as calling into a insert-after function (and implement that)
  (-conj [^DoublyLinkedList dll val]
    (if (empty? (.-entries-map dll))
      (let [uuid (:uuid val), node (Node. val nil nil)]
        (DoublyLinkedList. {(:uuid val) node} uuid uuid))
      (insert-after dll (.-last-uuid dll) val)))

  IMap
  (-dissoc [^DoublyLinkedList dll uuid]
    (let [new-entries (dissoc (.-entries-map dll) uuid)
          first ^Node (get (.-entries-map dll) (.-first-uuid dll))
          last ^Node (get (.-entries-map dll) (.-last-uuid dll))
          new-first (cond-> first
                      (= uuid (.-first-uuid dll)) (get new-entries (.-next-uuid first)))
          new-last (cond-> last
                     (= uuid (:uuid (.-value last))) (get new-entries (.-prev-uuid last)))]
      (DoublyLinkedList. new-entries new-first new-last))))

(defn- assoc-node [^Node node & kvs]
  (reduce (fn [^Node new-node [k v]]
            (case k
              :value (Node. v (.-prev-uuid new-node) (.-next-uuid new-node))
              :prev-uuid (Node. (.-value new-node) v (.-next-uuid new-node))
              :next-uuid (Node. (.-value new-node) (.-prev-uuid new-node) v)))
          node (partition 2 kvs)))

(defn- insert-between
  "Inserts `val` into the map of `entries` immediately between `prev-uuid` and `next-uuid`.
   If you call this with values of `prev-uuid` and `next-uuid` that are not adjacent, you
   will die a horrible death.

   Also note that this method operates on the entries-map, NOT the DLL itself."
  [entries {new-uuid :uuid :as val} prev-uuid next-uuid]
  (cond-> entries
    (some? prev-uuid) (update prev-uuid assoc-node :next-uuid new-uuid)
    (some? next-uuid) (update next-uuid assoc-node :prev-uuid new-uuid)
    :always (assoc new-uuid (Node. val prev-uuid next-uuid))))

;; TODO: add condition that dll cannot be empty.
(defn insert-after
  "Inserts `val` into the double-linked list `dll` immediately after the node with uuid = `prev-uuid`."
  [^DoublyLinkedList dll prev-uuid val]
  (let [next-uuid (.-next-uuid (get (.-entries-map dll) prev-uuid))
        new-entries (insert-between (.-entries-map dll) val prev-uuid next-uuid)
        new-last-uuid (if (= (.-last-uuid dll) prev-uuid)
                        (:uuid val)
                        (.-last-uuid dll))]
    (DoublyLinkedList. new-entries (.-first-uuid dll) new-last-uuid)))

(defn insert-before
  "Inserts `val` into the double-linked list `dll` immediately before the node with uuid = `prev-uuid`."
  [^DoublyLinkedList dll next-uuid val]
  (let [prev-uuid (.-prev-uuid (get (.-entries-map dll) next-uuid))
        new-entries (insert-between (.-entries-map dll) val prev-uuid next-uuid)
        new-first-uuid (if (= (.-first-uuid dll) next-uuid)
                         (:uuid val)
                         (.-first-uuid dll))]
    (DoublyLinkedList. new-entries new-first-uuid (.-last-uuid dll))))

(defn dll
  "Constructor for a new doubly-linked-list."
  ([]
   (DoublyLinkedList. {} nil nil))
  ([& xs]
   (reduce (fn [dll x] (conj dll x)) (dll) xs)))

(def l (dll {:uuid "1" :content "foo"} {:uuid "2" :content "bar"} {:uuid "3" :content "bizz"} {:uuid "5" :content "bang"}))
(def l1 (insert-before l "5" {:uuid "4" :content "bar"}))
(def l2 (insert-before l "1" {:uuid "-1" :content "pre"}))

(.-entries-map l1)
(.-last-uuid l1)
(.-first-uuid l1)

(.-entries-map l2)
(.-last-uuid l2)
(.-first-uuid l2)

(.log js/console (insert-before l "5" {:uuid "4" :content "bar"}))

;; (first (dll {:uuid 1, :content "foo"}))
;; (assoc (dll) 1 {:uuid 1, :content "foo"})

;; INext