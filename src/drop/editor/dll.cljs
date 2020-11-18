(ns drop.editor.dll
  ;; TODO: more detailed description
  "A fully-persistent, UUID-based doubly-linked list. Used for the list of paragraphs."
  (:refer-clojure :exclude [first last next remove])
  (:require-macros [drop.editor.dll :refer [node-uuid]]))

(declare first)
(declare last)
(declare next)
(declare prev)
(declare remove)
(declare dll)
(declare insert-after)
(declare insert-before)
(declare make-seq)

;; entries-map :: Map (UUID -> DLLEntry)
;; DLLEntry :: {uuid, prev-uuid, next-uuid, value}

;; TODO: It might be worth adding a dll/map function that takes and returns a DLL by default, similar to (mapv).

(deftype Node [^obj value ^string prev-uuid ^string next-uuid]
  IEquiv
  (-equiv [^Node node other]
    (if (instance? Node other)
      (and
       (= (.-value node) (.-value ^Node other))
       (= (.-prev-uuid node) (.-prev-uuid ^Node other))
       (= (.-next-uuid node) (.-next-uuid ^Node other)))
      false))

  ;; Implement pretty-printing for easier debugging
  IPrintWithWriter
  (-pr-writer [n writer opts]
   (-write writer "#Node{value: ") (-write writer (.-value n))
   (-write writer ", prev-uuid: ") (-write writer (.-prev-uuid n))
   (-write writer ", next-uuid: ") (-write writer (.-next-uuid n))
   (-write writer "}")))

;; TODO: this shit blows up the whole project when you try to pretty-print it and throws
;; a downright mysterious error to do with KeySeq. My best guess is that implementing one
;; of the protocols below (IMap?) is what causes the issue. Implement pretty-printing/fix it.
(deftype DoublyLinkedList [entries-map first-uuid last-uuid]
  IEquiv
  (-equiv [^DoublyLinkedList dll other]
    (if (instance? DoublyLinkedList other)
      (and
       (= (.-entries-map dll) (.-entries-map ^DoublyLinkedList other))
       (= (.-first-uuid dll) (.-first-uuid ^DoublyLinkedList other))
       (= (.-last-uuid dll) (.-last-uuid ^DoublyLinkedList other)))
      false))

  ISeq
  (-first [^DoublyLinkedList dll] (get (.-entries-map dll) (.-first-uuid dll)))

  ISeqable
  (-seq [^DoublyLinkedList dll] (make-seq dll))

  ICounted
  (-count [dll] (count (.-entries-map dll)))

  ICollection
  (-conj [^DoublyLinkedList dll val]
    (if (empty? (.-entries-map dll))
      (let [uuid (:uuid val), node (Node. val nil nil)]
        (DoublyLinkedList. {(:uuid val) node} uuid uuid))
      (insert-after dll (.-last-uuid dll) val)))

  IMap
  (-dissoc [^DoublyLinkedList dll uuid] (remove dll uuid))

  ILookup
  (-lookup [^DoublyLinkedList dll uuid] (-lookup dll uuid nil))
  (-lookup [^DoublyLinkedList dll uuid not-found]
    (if-let [entry ^Node (get (.-entries-map dll) uuid)]
      (.-value entry)
      not-found))

  #_IPrintWithWriter
  #_(-pr-writer [dll writer opts]
    (-write writer "#DoublyLinkedList{entries-map: ") (-write writer (.-entries-map dll))
    (-write writer ", first-uuid: ") (-write writer (.-first-uuid dll))
    (-write writer ", last-uuid: ") (-write writer (.-last-uuid dll))
    (-write writer "}")))

(defn- assoc-node
  "Helper function for creating a new [[Node]] based on the value of an old one.
   Takes keywords for the keys and works the same way you would expect assoc to
   on a normal map, for example:
   ```
   ; creates a new Node with identical to some-node
   ; but with the value of prev-uuid set to 12
   (assoc-node some-node :prev-uuid \"12\")
   ```"
  [^Node node & kvs]
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

(defn- make-seq
  "Makes a DLL into a seq."
  ([^DoublyLinkedList dll]
   (if (empty? (.-entries-map dll))
     nil
     (make-seq dll ^str (.-first-uuid dll))))
  ([^DoublyLinkedList dll uuid]
   (lazy-seq
    (let [entry ^Node (get (.-entries-map dll) uuid)]
      (when entry
        (cons (.-value entry) (make-seq dll (.-next-uuid entry))))))))

;; TODO: add condition to both these that dll cannot be empty.
(defn insert-before
  "Inserts `val` into the double-linked list `dll` immediately before the node with uuid = `prev-uuid`."
  [^DoublyLinkedList dll next-uuid val]
  {:pre [(seq dll)]}
  (let [prev-uuid (.-prev-uuid (get (.-entries-map dll) next-uuid))
        new-entries (insert-between (.-entries-map dll) val prev-uuid next-uuid)
        new-first-uuid (if (= (.-first-uuid dll) next-uuid)
                         (:uuid val)
                         (.-first-uuid dll))]
    (DoublyLinkedList. new-entries new-first-uuid (.-last-uuid dll))))

(defn insert-after
  "Inserts `val` into the double-linked list `dll` immediately after the node with uuid = `prev-uuid`."
  [^DoublyLinkedList dll prev-uuid val]
  {:pre [(seq dll)]}
  (let [next-uuid (.-next-uuid (get (.-entries-map dll) prev-uuid))
        new-entries (insert-between (.-entries-map dll) val prev-uuid next-uuid)
        new-last-uuid (if (= (.-last-uuid dll) prev-uuid)
                        (:uuid val)
                        (.-last-uuid dll))]
    (DoublyLinkedList. new-entries (.-first-uuid dll) new-last-uuid)))

(defn remove
  "Removes the node with `uuid` from the list. Calling (dissoc) on the DLL works identically"
  [^DoublyLinkedList dll uuid]
  (if-let [node ^Node (get (.-entries-map dll) uuid)]
    (let [entries (.-entries-map dll)
          first ^Node (get entries (.-first-uuid dll))
          last ^Node (get entries (.-last-uuid dll))
          new-entries (cond-> entries
                        true (dissoc uuid)
                        (some? (.-prev-uuid node)) (update (.-prev-uuid node) assoc-node :next-uuid (.-next-uuid node))
                        (some? (.-next-uuid node)) (update (.-next-uuid node) assoc-node :prev-uuid (.-prev-uuid node)))
          new-first (if (= uuid (.-first-uuid dll))
                      (node-uuid (get new-entries (.-next-uuid first)))
                      (node-uuid first))
          new-last (if (= uuid (:uuid (.-value last)))
                     (node-uuid (get new-entries (.-prev-uuid last)))
                     (node-uuid last))]
      (DoublyLinkedList. new-entries new-first new-last))
    dll))

(defn next
  "Get successive item in the doubly-linked list given either a UUID of a
   node or the value at that node. For example:
   ```
   ;; Both these work equivalently:
   (let [val1 {:uuid \"1\" :val :foo}, val2 {:uuid \"2\" :val :bar}]
     (next (dll val1 val2) \"1\")    ; => {:uuid \"2\" :val :bar}
     (next (dll val2 val2) val1))  ; => {:uuid \"2\" :val :bar}
   ```
   Returns `nil` if there is no next element."
  [^DoublyLinkedList dll uuid-or-elem]
  (if-let [uuid (:uuid uuid-or-elem)]
    (next dll uuid)
    (when-let [next-uuid (.-next-uuid (get (.-entries-map dll) uuid-or-elem
                                           #_(throw (str "ERROR: No item in list with UUID of " uuid-or-elem))))]
      (.-value (get (.-entries-map dll) next-uuid)))))

(defn prev
  "Get previous item in the doubly-linked list given either a UUID of a
   node or the value at that node. For example:
   ```
   ;; Both these work equivalently:
   (let [val1 {:uuid \"1\" :val :foo}, val2 {:uuid \"2\" :val :bar}]
     (prev (dll val1 val2) \"2\")    ; => {:uuid \"1\" :val :foo}
     (prev (dll val2 val2) val2))  ; => {:uuid \"1\" :val :foo}
   ```
   Returns `nil` if there is no previous element."
  [^DoublyLinkedList dll uuid-or-elem]
  (if-let [uuid (:uuid uuid-or-elem)]
    (prev dll uuid)
    (when-let [prev-uuid (.-prev-uuid (get (.-entries-map dll) uuid-or-elem
                                           #_(throw (str "ERROR: No item in list with UUID of " uuid-or-elem))))]
      (.-value (get (.-entries-map dll) prev-uuid)))))

(defn first
  "Returns first element in DLL."
  [^DoublyLinkedList dll]
  (if-let [first-node ^Node (get (.-entries-map dll) (.-first-uuid dll))]
    (.-value first-node)))

(defn last
  "Returns last element in DLL."
  [^DoublyLinkedList dll]
  (if-let [first-node ^Node (get (.-entries-map dll) (.-last-uuid dll))]
    (.-value first-node)))

(defn dll
  "Constructor for a doubly-linked-list."
  ([]
   (DoublyLinkedList. {} nil nil))
  ([& xs]
   (reduce (fn [dll x] (conj dll x)) (dll) xs)))

(comment
  (def val1 {:uuid "1" :content "foo"})
  (def l (dll val1 {:uuid "2" :content "bar"} {:uuid "3" :content "bizz"} {:uuid "5" :content "bang"}))
  (def l1 (insert-before l "5" {:uuid "4" :content "bar"}))
  (def l2 (insert-before l "1" {:uuid "-1" :content "pre"}))
  (def mine (filter #(not= "-1" (:uuid %)) l2))

  (first l)
  (last l)
  (first (dll))
  (last (dll))

  (= (into (dll) l2) l2)
  (empty? (dll))

  (next l2 "1")
  (next l2 {:uuid "1"})
  (next l2 {:uuid "5"})
  (next l2 "doesntexist")

  (prev l2 "2")
  (prev l2 {:uuid "2"})
  (prev l2 {:uuid "-1"})
  (prev l2 "doesntexist")

  (get l2 "2")
  (get l2 "2" :floof)
  (get l2 "12" :floof)

  (.-entries-map (dissoc l2 "1")))
