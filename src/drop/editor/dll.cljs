(ns drop.editor.dll
  "A fully-persistent, UUID-based doubly-linked list. Used for the list of paragraphs.
   The central problem here is that for our paragraph list, we need a few things:

   1. An efficient way to get the the next or previous paragraph from the list, given a paragraph.
      This is used when keying up/down between paragraphs, among other things.
   2. An efficient way to *random-access* a paragraph. When it's DOM node is clicked, for example.
   3. Efficient access to both the first and last paragraph, for jump to start/end.
   4. Efficient removal of paragraph or paragraphs at an arbitrary point in the list.
   5. The list must be *ordered.*
   6. Ideally, we do not want the indexes for accessing a paragraph to get invalidated across
      versions, provided that paragraph is still present. We need this so that event listeners
      set on <p> elements in the view can know which paragraphs to modify in the model, without
      having to reset event listeners for every single DOM element each time we change the list.

   Any one of these is trivial, but getting all of them at once is hard. #1, #2, and #3 are easy
   with a vector or an array. #4 is possible with finger trees or RRB vectors. #5 is a property
   of any list type. #6 is impossible using numeric indexing, as removing a paragraph would invalidate
   the indexes of any paragraph(s) after it. It could be achieved with a map of UUIDs -> Paragraphs,
   but this would get rid of properties #1, #3, and #5.

   Maintaining a doubly-linked list of paragraphs, along with a hashmap of UUIDs -> Nodes could
   *almost* get us what we want, but a traditional doubly-linked list cannot be made fully-persistent
   in the way a singly-linked list can, at least without making every operation O(n). However, if we
   swap the `prev` and `next` pointers on each node and instead use UUIDs referencing the previous and
   next nodes, and maintain a map of UUIDs -> Nodes, it can be made into a functional version of a DLL.

   You don't really need to be aware of most of these details in order to write code that uses the DLL
   data structure in this namespace. All you really need to know are the public functions it exposes:
   `first`, `last`, `next`, `prev`, `remove`, `insert-after`, `insert-before`, and the constructor,
   `dll` (plus some friends). Aside from that DLLs behave basically like other Clojure collections: you
   can call `conj`, `get`, `seq`, `map`, `filter`, `count` or `reduce` on them, and convert them to and
   from other types using `into`. Destructuring and all your favorite Clojure goodies work as expected.

   They are also decoupled from the rest of the code -- there's no reason you couldn't put something
   other than paragraphs inside a DLL, though it's doubtful you'd need those specific set of properties
   in any other application (but hey, maybe).

   The only catch is that every item inserted must have a :uuid property. So `(dll {:uuid \"123\" :val 1})`
   will work, but `(dll {:val 1})` will throw an error."
  (:refer-clojure :exclude [first last next remove])
  (:require-macros [drop.editor.dll :refer [node-uuid]]))

;; TODO: It might be worth adding a dll/map function that takes and returns a DLL by default, similar to (mapv).
(declare first)
(declare last)
(declare next)
(declare prev)
(declare remove)
(declare insert-after)
(declare insert-before)
(declare replace-range)
(declare dll)

(declare make-seq)
(declare assoc-node)

;; TODO: I totally did not realize this, but inside a deftype declaration you can just reference fields
;; by their names, similar to Java, without any (.-whatever this) business. This stuff can probably be
;; cleaned up in light of that...

(deftype Node [^obj value ^string prev-uuid ^string next-uuid]
  IEquiv
  (-equiv [^Node node other]
    (if (instance? Node other)
      (and
       (= value (.-value ^Node other))
       (= prev-uuid (.-prev-uuid ^Node other))
       (= next-uuid (.-next-uuid ^Node other)))
      false))

  ;; Implement pretty-printing for easier debugging
  IPrintWithWriter
  (-pr-writer [n writer opts]
   (-write writer "#Node{value: ") (-write writer value)
   (-write writer ", prev-uuid: ") (-write writer prev-uuid)
   (-write writer ", next-uuid: ") (-write writer next-uuid)
   (-write writer "}")))

;; TODO: implement IFn for lookup (like maps and vectors)

;; TODO: this shit blows up the whole project when you try to pretty-print it and throws
;; a downright mysterious error to do with KeySeq. My best guess is that implementing one
;; of the protocols below (IMap?) is what causes the issue. Implement pretty-printing/fix it.
(deftype DoublyLinkedList [entries-map ; map of (UUID -> DLLEntry)
                           first-uuid
                           last-uuid]
  ISequential

  IEquiv
  (-equiv [^DoublyLinkedList dll other]
    (if (instance? DoublyLinkedList other)
      (and
       (= entries-map (.-entries-map ^DoublyLinkedList other))
       (= first-uuid (.-first-uuid ^DoublyLinkedList other))
       (= last-uuid (.-last-uuid ^DoublyLinkedList other)))
      false))

  ISeq
  (-first [^DoublyLinkedList dll] (first dll))
  (-rest [^DoublyLinkedList dll]
    (if (seq entries-map)
      (make-seq dll (.-next-uuid (get entries-map first-uuid)))
      ()))

  ISeqable
  (-seq [^DoublyLinkedList dll] (make-seq dll))

  ICounted
  (-count [dll] (count entries-map))

  ICollection
  (-conj [^DoublyLinkedList dll val]
    (if (empty? entries-map)
      (let [uuid (:uuid val), node (Node. val nil nil)]
        (DoublyLinkedList. {(:uuid val) node} uuid uuid))
      (insert-after dll last-uuid val)))

  IMap
  (-dissoc [^DoublyLinkedList dll uuid] (remove dll uuid))

  IAssociative
  (-assoc [^DoublyLinkedList dll k v]
    (if (= k (:uuid v))
      (if (contains? entries-map k)
        (DoublyLinkedList. (update entries-map k #(assoc-node % :value v)) first-uuid last-uuid)
        (throw (js/Error. "Attempting (assoc) a DLL key that does not exist.")))
      (throw (js/Error. "Attempting to change the UUID of an item in the DLL with (assoc)! This will break things!"))))
  (-contains-key? [^DoublyLinkedList dll k]
    (contains? entries-map k))

  ILookup
  (-lookup [^DoublyLinkedList dll uuid] (-lookup dll uuid nil))
  (-lookup [^DoublyLinkedList dll uuid not-found]
    (if-let [entry ^Node (get entries-map uuid)]
      (.-value entry)
      not-found))

  IFn
  (-invoke [^DoublyLinkedList dll uuid] (-lookup dll uuid))
  (-invoke [^DoublyLinkedList dll uuid not-found] (-lookup dll uuid not-found))

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
    (when-let [entry ^Node (get (.-entries-map dll) uuid)]
      (cons (.-value entry) (make-seq dll (.-next-uuid entry)))))))

;; TODO: add condition to both these that dll cannot be empty.
(defn insert-before
  "Inserts `val` into the double-linked list `dll` immediately before the node with uuid = `next-uuid`."
  [^DoublyLinkedList dll next-uuid val]
  {:pre [(seq dll) (contains? dll next-uuid)]}
  (let [prev-uuid (.-prev-uuid (get (.-entries-map dll) next-uuid))
        new-entries (insert-between (.-entries-map dll) val prev-uuid next-uuid)
        new-first-uuid (if (= (.-first-uuid dll) next-uuid)
                         (:uuid val)
                         (.-first-uuid dll))]
    (DoublyLinkedList. new-entries new-first-uuid (.-last-uuid dll))))

(defn insert-after
  "Inserts `val` into the double-linked list `dll` immediately after the node with uuid = `prev-uuid`."
  [^DoublyLinkedList dll prev-uuid val]
  {:pre [(seq dll) (contains? dll prev-uuid)]}
  (let [next-uuid (.-next-uuid (get (.-entries-map dll) prev-uuid))
        new-entries (insert-between (.-entries-map dll) val prev-uuid next-uuid)
        new-last-uuid (if (= (.-last-uuid dll) prev-uuid)
                        (:uuid val)
                        (.-last-uuid dll))]
    (DoublyLinkedList. new-entries (.-first-uuid dll) new-last-uuid)))

;; TODO: is this needed?
#_(defn insert-all-before
  "Inserts all items in list `xs` directly before the node with uuid == `next-uuid`"
  [dll next-uuid xs]
  {:pre [(seq dll)
         (contains? dll next-uuid)
         (sequential? xs)]}
  (if (empty? xs)
    dll
    (let [x (clojure.core/first xs)]
      (recur (insert-before dll next-uuid x) (:uuid x) (rest xs)))))

(defn insert-all-after
  "Inserts all items in list `xs` directly after the node with uuid == `prev-uuid`"
  [dll prev-uuid xs]
  {:pre [(seq dll)
         (contains? dll prev-uuid)
         (sequential? xs)]}
  (if (empty? xs)
    dll
    (let [x (clojure.core/first xs)]
      (recur (insert-after dll prev-uuid x) (:uuid x) (rest xs)))))

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
          new-first (cond
                      (empty? new-entries) nil
                      (= uuid (.-first-uuid dll)) (node-uuid (get new-entries (.-next-uuid first)))
                      :else (node-uuid first))
          new-last (cond
                     (empty? new-entries) nil
                     (= uuid (:uuid (.-value last))) (node-uuid (get new-entries (.-prev-uuid last)))
                     :else (node-uuid last))]
      (DoublyLinkedList. new-entries new-first new-last))
    dll))

(defn remove-all
  "Removes all the items between the nodes with uuid1 and uuid2 (both **inclusive**)."
  [dll uuid1 uuid2]
  {:pre [(contains? dll uuid1) (contains? dll uuid2)]}
  (let [first-removed (remove dll uuid1)]
    (if (= uuid1 uuid2)
      first-removed
      (recur first-removed (:uuid (next dll uuid1)) uuid2))))

(defn replace-range
  "Replaces all the nodes between uuid1 and uuid2 (both inclusive) with
   the supplied item or list of items. Example:
   ```
   (def lst (dll {:uuid 1, :val :a} {:uuid 2, :val :b} {:uuid 3, :val :c } {:uuid 4, :val :d}))
   (replace-range lst 2 3 {:uuid 12, :val :e})

   => (dll {:uuid 1, :val :a} {:uuid 12, :val :e} {:uuid 4, :val :d})
   (replace-range lst 2 3 [{:uuid 12, :val :e} {:uuid 13 :val :f}])

   => (dll {:uuid 1, :val :a} {:uuid 12, :val :e} {:uuid 13, :val :f} {:uuid 4, :val :d})
   ```"
  [dll uuid1 uuid2 to-insert]
  (let [removed (remove-all dll uuid1 uuid2)
        node-before (prev dll uuid1)
        node-after (next dll uuid2)
        insert (cond
                 node-before #(insert-after removed (:uuid node-before) %)
                 node-after #(insert-before removed (:uuid node-after) %)
                 :else #(conj removed %))]
    (if (sequential? to-insert)
      (let [first (clojure.core/first to-insert)]
        (-> (insert first)
            (insert-all-after (:uuid first) (rest to-insert))))
      (insert to-insert))))

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
    (when-let [prev-uuid (.-prev-uuid (get (.-entries-map dll) uuid-or-elem))]
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

;; TODO: make invariant that items must have UUID property
(defn dll
  "Constructor for a doubly-linked-list, optionally taking a list of
   items to insert. Note each item must have a :uuid property."
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

  (def a (dll {:uuid "#1" :content "CHANGED"} {:uuid "#2" :content "CHANGED"}))
  (replace-range l "1" "5" a)

  (.-entries-map (assoc l "2" {:uuid "2" :content "oyeah"}))
  (.-entries-map (update l "2" (fn [x] {:uuid (:uuid x) :content "baybee"})))

  (rest (dll))
  (rest l)

  (let [[a b] l]
    #{a b})

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
