(ns drop.editor.dll
  "A fully-persistent, UUID-based doubly-linked list. Used for the list of paragraphs."
  (:refer-clojure :exclude [last next])
  )

;;(declare first)
;;(declare last)
;;(declare next)
;;(declare prev)
(declare insert-after)
;;(declare insert-before)
;; entries-map :: Map (UUID -> DLLEntry)
;; DLLEntry :: {uuid, prev-uuid, next-uuid, value}

;; TODO: maybe just make it work for anything that has a :uuid property? Dynamic typing FTW.

(deftype Node [^obj value ^string prev-uuid ^string next-uuid]
  ;; Implemented pretty-printing for easier debugging
  IPrintWithWriter
  (-pr-writer [n writer opts]
   (-write writer "#Node{value: ") (-write writer (.-value n))
   (-write writer ", prev-uuid: ") (-write writer (.-prev-uuid n))
   (-write writer ", next-uuid: ") (-write writer (.-next-uuid n))
   (-write writer "}")))

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

(defn dll
  "Constructor for a new doubly-linked-list."
  ([]
   (DoublyLinkedList. {} nil nil))
  ([& xs]
   (reduce (fn [dll x] (conj dll x)) (dll) xs)))

(defn assoc-node [^Node node & kvs]
  (reduce (fn [^Node new-node [k v]]
            (case k
              :value (Node. v (.-prev-uuid new-node) (.-next-uuid new-node))
              :prev-uuid (Node. (.-value new-node) v (.-next-uuid new-node))
              :next-uuid (Node. (.-value new-node) (.-prev-uuid new-node) v)))
          node (partition 2 kvs)))

;; TODO: add condition that dll cannot be empty.
(defn insert-after
  "Inserts `val` into the double-linked list `dll`
   immediately after the node with uuid = `prev-uuid`."
  [^DoublyLinkedList dll prev-uuid val]
  (let [next-uuid (.-next-uuid (get (.-entries-map dll) prev-uuid))
        new-uuid (:uuid val)
        new-last (if (= prev-uuid (.-last-uuid dll))
                   new-uuid
                   (.-last-uuid dll))
        new-entries (cond-> (.-entries-map dll)
                      :always           (update prev-uuid assoc-node :next-uuid new-uuid)
                      :always           (assoc new-uuid (Node. val prev-uuid next-uuid))
                      (some? next-uuid) (update next-uuid assoc-node :prev-uuid next-uuid))]
    (DoublyLinkedList. new-entries (.-first-uuid dll) new-last)))

(def l (dll {:uuid "1" :content "foo"} {:uuid "2" :content "bar"}))
(get (.-entries-map l) "1")
(get (.-entries-map l) "2")


;; (first (dll {:uuid 1, :content "foo"}))
;; (assoc (dll) 1 {:uuid 1, :content "foo"})

;; INext