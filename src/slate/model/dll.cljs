(ns slate.model.dll
  "A fully-persistent list type using fractional indexing. Used to hold the list of paragraphs.

   Okay, time for some background. The central problem here is that for our paragraph list, we
   need a few things:

   1. An efficient way to get the the next or previous paragraph from the list, given a paragraph.
      This is used when keying up/down between paragraphs, among other things.
   2. An efficient way to _random access_ a paragraph. When its DOM node is clicked, for example.
   3. Efficient access to both the first and last paragraph. For jump to start/end, among other things.
   4. Efficient removal of paragraph or paragraphs at an arbitrary point in the list.
   5. The list must be *ordered.*
   6. We do not want the indices for accessing a paragraph to get invalidated across versions, provided that
      paragraph is still present. We need this so that whenever a <p> element is clicked in the DOM, we can
      get the paragraph associated with it in constant time. And we need to be able to do this without updating
      every <p>'s associated data that ties it to that paragraph, every single time -- if we used regular numeric
      indexing and then inserted at the start of the document, then references to the indices of paragraph after
      the first would need to be incremented by one. Likewise removing a paragraph would mean indices after
      would have to be decremented. This is O(n) in the worst case and too costly for large documents.
   7. It should be possible to tell if one paragraph lies before or after another, in constant time.
   8. Ideally, it should be fully persistent, so it's idiomatic to the Clojure way of doing things.

   Any one of these is trivial, but getting all of them at once is hard. #1, #2, and #3 are easy
   with a vector or an array. #4 is possible with finger trees or RRB vectors. #5 is a property
   of any list type. #6 is impossible using regular numeric indexing, and while you can ditch normal
   numeric indexing by using a linked list and indexing on UUIDs rather than numbers, that gets rid
   of property #8 (as well as making most things constant time operations).

   Enter fractional indexing. There are good references to this topic elsewhere, but the TL;DR version
   is that there are an infinite quantity of numbers between any two rational numbers. And while we normally
   use whole numbers for indexing list types, you can do arbitrary insertions at any point in the list
   _and keep all previous indices valid_ if you switch from only whole numbers to rationals. Insert between
   1 and 2 and the index of the newly inserted item is 1.5. Insert between 1 and 1.5 and it's 1.25. And so on.

   The data structure implemented in this file employs this fractional indexing strategy. Inserting repeatedly
   into an empty list without deleting will result in all whole number indices, starting from 1. Insert
   between two items and the index of the inserted item will be the midpoint between those two indices.
   Decimal.js, an arbitrary precision number library, is employed so that this can be done any number of times.

   Under the hood, the data structure is a persistent, doubly-linked list. The list maintains:

   1. A map of indices -> Nodes (for O(1) access to any element)
   2. The first and last index

   Each Node contains:

   1. Its own index
   2. The index of the previous item in the list (`nil` if none)
   3. The index of the next item in the list (`nil` if none)
   4. The data for that Node

   Notice that the Nodes use references to the index of the next/prev element, rather than pointers to
   the Nodes themselves as in a traditional doubly-linked list; this allows the list to be fully persistent
   while still having most operations be O(1).

   You don't really need to be aware of most of these details in order to write code that uses the DLL
   data structure presented in this namespace. All you really need to know are the public functions it
   exposes: `first`, `last`, `next-index`, `next`, `prev-index`, `prev`, `remove`, `insert-after`,
   `insert-before`, and the constructor, `dll` (plus some friends). Aside from that, DLLs behave basically
   like other Clojure collections: you can call `conj`, `get`, `seq`, `map`, `filter`, `count` or `reduce`
   on them, and convert them to and from other sequential types using `into`. Destructuring and all your
   favorite Clojure goodies work as expected.

   They are also decoupled from the rest of the code -- there's no reason you couldn't put something other than
   paragraphs inside a DLL, though it's doubtful one would need these incredibly specific set of properties for
   any other use."
  (:refer-clojure :exclude [first last next remove range list])
  (:require [clojure.set :as set]
            ["decimal.js" :refer [Decimal]]
            [hashp.core]))

;; TODO: implement (keys)
;; TODO: would be nice to be able to use plain numbers in place of big-decimals and have them auto-convert

(set! *warn-on-infer* false)

;; TODO: It might be worth adding a dll/map function that takes and returns a DLL by default, similar to (mapv).
(declare first)
(declare first-index)

(declare last)
(declare last-index)

(declare next-node)
(declare next-index)
(declare next)

(declare prev-node)
(declare prev-index)
(declare prev)

(declare remove)
(declare insert-before)
(declare insert-after)
(declare prepend)
(declare append)
(declare replace-range)
(declare replace-between)
(declare all-indices)
(declare dll)

(declare make-seq)
(declare assoc-node)

;; IEquiv must be implemented for Decimal type in order
;; for Decimal to work as the key in a Clojure map.
(extend-type Decimal
  IEquiv
  (-equiv [d1 d2]
    ;; Decimal .eq method will throw error on null arg.
    (if (instance? Decimal d2)
      (.eq d1 d2)
      false))

  IComparable
  (-compare [d1 d2]
    (.comparedTo d1 d2))

  IHash
  (-hash [decimal]
    (hash (.toString decimal)))

  IPrintWithWriter
  (-pr-writer [decimal writer opts]
    (-write writer (str "#Decimal " (pr-str (.toString decimal))))))

(defn big-dec
  "Construct new Decimal object. Arg may be a number of a string."
  [n]
  (Decimal. n))

(defn big-dec?
  "Returns true if arg is an instance of Decimal."
  [o]
  (instance? Decimal o))

(defn create-changelist
  "Constructor for a new changelist object. Changelists are used for tracking what indices have
   changed between different versions of a DLL.

   A changelist is composed of 3 fields: :changed-indices, :inserted-indices, and :deleted-indices,
   which are sets containing the indices of the paragraphs that have been changed,
   newly inserted, or removed from the list, respectively, since the last EditorState, respectively.

   Takes keyword arguments :changed-indices :inserted-indices, and :deleted-indices (each
   default to an empty set). If no arguments supplied, returns an empty changelist."
  [& {:keys [changed-indices inserted-indices deleted-indices]}]
  {:pre [(or (nil? changed-indices) (set? changed-indices))
         (or (nil? inserted-indices) (set? inserted-indices))
         (or (nil? deleted-indices) (set? deleted-indices))]}
  {:changed-indices (or changed-indices #{})
   :inserted-indices (or inserted-indices #{})
   :deleted-indices (or deleted-indices #{})})

(defn merge-changelists
  "Takes two changelists and returns a third that combines them. Indices are rearranged
   as necessary according to the following rules:

   - If an index is **deleted**:
     - And then inserted: move to changed
   - If a index is **changed**:
     - And then deleted: move to deleted
   - If a index is **inserted**:
     - And then deleted: remove from both inserted and deleted
     - And then changed: move to inserted.

   It is assumed that c2 happened immediately after c1. You cannot supply random
   changelists on wholly unrelated lists, or lists from non-adjacent points in time.

   The purpose of this is so that we can roll many changelists into one, and re-render the document only once."
  [c1 c2]
  (let [deleted-then-inserted (set (filter #(contains? (:deleted-indices c1) %) (:inserted-indices c2)))
        changed-then-deleted  (set (filter #(contains? (:changed-indices c1) %) (:deleted-indices c2)))
        inserted-then-deleted (set (filter #(contains? (:inserted-indices c1) %) (:deleted-indices c2)))
        inserted-then-changed (set (filter #(contains? (:inserted-indices c1) %) (:changed-indices c2)))

        new-deleted (-> (set/union (:deleted-indices c1) (:deleted-indices c2))
                        (set/difference deleted-then-inserted inserted-then-deleted)
                        (set/union changed-then-deleted))
        new-changed (-> (set/union (:changed-indices c1) (:changed-indices c2))
                        (set/difference changed-then-deleted inserted-then-changed)
                        (set/union deleted-then-inserted))
        new-inserted (-> (set/union (:inserted-indices c1) (:inserted-indices c2))
                         (set/difference inserted-then-deleted deleted-then-inserted)
                         (set/union inserted-then-changed))]
    {:deleted-indices new-deleted
     :changed-indices new-changed
     :inserted-indices new-inserted}))

(defn reverse-changelist
  "Taking a changelist that contains update information to go from state A to state B,
   produces a new changelists with update information on on how to go from state B to A."
  [{:keys [inserted-indices changed-indices deleted-indices]}]
  {:inserted-indices deleted-indices
   :changed-indices changed-indices
   :deleted-indices inserted-indices})

(defn changelist
  [list]
  ^obj (.-changelist list))

(deftype Node [^obj value ^obj index ^obj prev-index ^obj next-index]
  IEquiv
  (-equiv [^Node node other]
    (if (instance? Node other)
      (and
       (= value (.-value ^Node other))
       (= index (.-index ^Node other))
       (= prev-index (.-prev-index ^Node other))
       (= next-index (.-next-index ^Node other)))
      false))

  ;; Implement pretty-printing for easier debugging
  IPrintWithWriter
  (-pr-writer [n writer opts]
   (-write writer "#Node {value: ") (-write writer value)
   (-write writer ", prev-index: ") (-write writer prev-index)
   (-write writer ", next-index: ") (-write writer next-index)
   (-write writer "}")))

(deftype DoublyLinkedList [meta
                           changelist
                           entries-map ; map of (index -> Node)
                           first-index
                           last-index]
  ISequential

  IEquiv
  (-equiv [^DoublyLinkedList dll other]
    (if (seqable? other)
      (= (seq dll) (seq other))
      false))

  ISeq
  (-first [^DoublyLinkedList dll] (first dll))
  (-rest [^DoublyLinkedList dll]
    (if (seq entries-map)
      (make-seq dll (.-next-index (get entries-map first-index)))
      ()))

  ISeqable
  (-seq [^DoublyLinkedList dll] (make-seq dll))

  ICounted
  (-count [_dll] (count entries-map))

  ICollection
  (-conj [^DoublyLinkedList dll value] (append dll value))

  IMap
  (-dissoc [^DoublyLinkedList dll index] (remove dll index))

  IAssociative
  (-assoc [^DoublyLinkedList _dll index v]
    (if (contains? entries-map index)
      (DoublyLinkedList. meta
                         (merge-changelists changelist {:changed-indices #{index}})
                         (update entries-map index #(assoc-node % :value v))
                         first-index
                         last-index)
      (throw (js/Error. "Attempting (assoc) a DLL key that does not exist."))))
  (-contains-key? [^DoublyLinkedList _dll k]
    (contains? entries-map k))

  ILookup
  (-lookup [^DoublyLinkedList dll index] (-lookup dll index nil))
  (-lookup [^DoublyLinkedList _dll index not-found]
    (if-let [entry ^Node (get entries-map index)]
      (.-value entry)
      not-found))

  IStack
  (-peek [^DoublyLinkedList dll] (last dll))
  (-pop [^DoublyLinkedList dll] (remove dll (.-last-index dll)))

  IFn
  (-invoke [^DoublyLinkedList dll index] (-lookup dll index))
  (-invoke [^DoublyLinkedList dll index not-found] (-lookup dll index not-found))

  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (DoublyLinkedList. new-meta changelist entries-map first-index last-index)))

  IMeta
  (-meta [_dll] meta)

  IPrintWithWriter
  (-pr-writer [dll writer opts]
    (-write writer "#DoublyLinkedList ")
    (-write writer (->> (all-indices dll)
                        (mapv #(vector % (get dll %)))))))

(defn- assoc-node
  "Helper function for creating a new [[Node]] based on the value of an old one.
   Takes keywords for the keys and works the same way you would expect assoc to
   on a normal map, for example:
   ```
   ; creates a new Node with identical to some-node
   ; but with the value of prev-index set to 12
   (assoc-node some-node :prev-index (big-dec \"12\"))
   ```"
  [^Node node & kvs]
  (reduce (fn [^Node new-node [k v]]
            (case k
              :value (Node. v (.-index new-node) (.-prev-index new-node) (.-next-index new-node))
              :index (Node. (.-value new-node) v (.-prev-index new-node) (.-next-index new-node))
              :prev-index (Node. (.-value new-node) (.-index new-node) v (.-next-index new-node))
              :next-index (Node. (.-value new-node) (.-index new-node) (.-prev-index new-node) v)))
          node (partition 2 kvs)))

(defn- make-seq
  "Makes a DLL into a seq."
  ([^DoublyLinkedList dll]
   (if (empty? (.-entries-map dll))
     nil
     (make-seq dll ^str (.-first-index dll))))
  ([^DoublyLinkedList dll index]
   (lazy-seq
    (when-let [node ^Node (get (.-entries-map dll) index)]
      (cons (.-value node) (make-seq dll (.-next-index node)))))))

(defn- insert-between
  "Inserts `val` into the list immediately between `prev-index` and `next-index`.
   If you call this with values of `prev-index` and `next-index` that are not adjacent, you
   will die a horrible death (aka it will throw)."
  [list prev-idx next-idx value]
  {:pre [(instance? Decimal prev-idx)
         (instance? Decimal next-idx)]}
  (let [new-idx (.. prev-idx (add next-idx) (div 2))]
    (DoublyLinkedList. (.-meta list)
                       (merge-changelists (.-changelist list) (create-changelist :inserted-indices #{new-idx}))
                       (-> (.-entries-map list)
                           (assoc new-idx (Node. value new-idx prev-idx next-idx))
                           (update prev-idx assoc-node :next-index new-idx)
                           (update next-idx assoc-node :prev-index new-idx))
                       (first-index list)
                       (last-index list))))

(defn insert-before
  "Inserts `value` into the list immediately before index `next-idx`."
  [^DoublyLinkedList list next-idx value]
  {:pre [(seq list) (contains? list next-idx)]}
  (if-let [prev-idx (.-prev-index (get (.-entries-map list) next-idx))]
    ;; Inserting between two indices
    (insert-between list prev-idx next-idx value)
    ;; Inserting at the start of the list
    (prepend list value)))

(defn insert-after
  "Inserts `value` into the list immediately after the node with index `prev-index`."
  [^DoublyLinkedList list prev-idx value]
  {:pre [(seq list) (contains? list prev-idx)]}
  (if-let [next-idx (.-next-index (get (.-entries-map list) prev-idx))]
    ;; Inserting between two indices
    (insert-between list prev-idx next-idx value)
    ;; Inserting at the end of the list
    (append list value)))

(defn prepend
  "Adds an element to the start of the list.

   Indices will always be > 0. If a DLL is created, the first item inserted
   will be given index 1 by default. If a new item is inserted before that,
   it will be given index 0.5. If another before that, its index will be 0.25,
   then 0.125, 0.0625, etc."
  [list value]
  {:pre [(instance? DoublyLinkedList list)]}
  (if (empty? list)
    (append list value)
    (let [first-idx (first-index list)
          new-idx (.. first-idx (div 2))]
      (DoublyLinkedList. (.-meta list)
                         (merge-changelists (.-changelist list) (create-changelist :inserted-indices #{new-idx}))
                         (-> (.-entries-map list)
                             (assoc new-idx (Node. value new-idx nil first-idx))
                             (update first-idx assoc-node :prev-index new-idx))
                         new-idx
                         (last-index list)))))

(defn append
  "Adds an element to the end of the list.

   Indices will always be > 0. If a DLL is empty, the first item inserted
   will be given index 1 by default. Any element appended will have an index
   of last-index + 1 by default."
  ([list value idx]
   {:pre [(or (and (empty? list) (.gt idx 0))
              (.gt idx (last-index list)))]}
   (if (empty? (.-entries-map list))
     (DoublyLinkedList. (.-meta list)
                        (merge-changelists (.-changelist list) (create-changelist :inserted-indices #{idx}))
                        {idx (Node. value idx nil nil)}
                        idx
                        idx)
     (DoublyLinkedList. (.-meta list)
                        (merge-changelists (.-changelist list) (create-changelist :inserted-indices #{idx}))
                        (-> (.-entries-map list)
                            (update (last-index list) assoc-node :next-index idx)
                            (assoc idx (Node. value idx (last-index list) nil)))
                        (first-index list)
                        idx)))
  ([list value]
   (append list value (if (empty? list) (big-dec 1) (.add (last-index list) 1)))))

(defn remove
  "Removes the node with `index` from the list. Calling (dissoc) on the list works identically."
  [^DoublyLinkedList list index]
  (if-let [node ^Node (get (.-entries-map list) index)]
    (let [entries (.-entries-map list)
          first-node ^Node (get entries (.-first-index list))
          last-node ^Node (get entries (.-last-index list))
          new-entries (cond-> entries
                        true
                        (dissoc index)

                        (some? (.-prev-index node))
                        (update (.-prev-index node) assoc-node :next-index (.-next-index node))

                        (some? (.-next-index node))
                        (update (.-next-index node) assoc-node :prev-index (.-prev-index node)))
          new-first (cond
                      (empty? new-entries) nil
                      (= index (.-first-index list)) (.-next-index first-node)
                      :else (.-first-index list))
          new-last (cond
                     (empty? new-entries) nil
                     (= index (.-last-index list)) (.-prev-index last-node)
                     :else (.-last-index list))]
      (DoublyLinkedList. (.-meta list)
                         (merge-changelists (.-changelist list) (create-changelist :deleted-indices #{index}))
                         new-entries
                         new-first
                         new-last))
    list))

(defn remove-range
  "Removes all the items between the nodes at `index1` and `index2` (__inclusive__ on both ends)."
  [list index1 index2]
  {:pre [(contains? list index1)
         (contains? list index2)
         (.lte index1 index2)]}
  (let [first-removed (remove list index1)]
    (if (= index1 index2)
      first-removed
      (recur first-removed (next-index list index1) index2))))

(defn remove-between
  "Removes all the items between the nodes at `index1` and `index2` (__non-inclusive__ on both end).
   If `index1` and `index2` are the same or adjacent, the list will be unchanged."
  [list index1 index2]
  {:pre [(instance? DoublyLinkedList list)
         (contains? list index1)
         (contains? list index2)
         (.lte index1 index2)]}
  (if (or (= index1 index2)
          (= (next-index list index1) index2))
    list
    (remove-range list (next-index list index1) (prev-index list index2))))

(defn replace-range
  "Replaces all the nodes between `index1` and `index2` (both inclusive) with
   the supplied item or list of items. Example:
   ```
   (def lst (dll {:val :a} {:val :b} {:val :c} {:val :d}))
   (replace-range lst 2 3 {:val :e})
   ; => (dll {:val :a} {:val :e} :val :d})

   (replace-range lst 2 3 [{:val :e} {:val :f}])
   ; => (dll {:val :a} {:val :e} {:val :f} {:val :d})
   ```"
  [list index1 index2 to-insert]
  {:pre [(.lte index1 index2)]}
  (if-not (sequential? to-insert)
    (replace-range list index1 index2 [to-insert])
    (let [new-list (remove-between list index1 index2)
          new-list (assoc new-list index1 (clojure.core/first to-insert))
          new-list (cond
                     (= index1 index2)
                     new-list

                     (> (count to-insert) 1)
                     (assoc new-list index2 (clojure.core/last to-insert))

                     :else
                     (dissoc new-list index2))
          insert-items (drop 1 to-insert)
          insert-items (if (not= index1 index2)
                         (drop-last 1 insert-items)
                         insert-items)]
      (loop [lst new-list
             insert-items insert-items
             index-to-insert-after index1]
        (if (empty? insert-items)
          lst
          (let [new-list (insert-after lst index-to-insert-after (clojure.core/first insert-items))]
            (recur new-list
                   (rest insert-items)
                   (next-index new-list index-to-insert-after))))))))

(defn replace-between
  "Same as `replace-range`, but __not__ inclusive of either end."
  [list index1 index2 to-insert]
  (let [first-index (next-index list index1)]
    (if (= first-index index2)
      list
      (replace-range list (next-index list index1) (prev-index list index2) to-insert))))

(defn indices-between
  "Returns a sequence of all the indices between `index1` and `index2`"
  [list index1 index2]
  {:pre [(contains? list index1)
         (contains? list index2)
         (.lte index1 index2)]}
  (if (= index1 index2)
    []
    (loop [index (next-index list index1), indices []]
      (if (= index index2)
        indices
        (recur (next-index list index)
               (conj indices index))))))

(defn indices-range
  "Returns a sequence of all the indices between `index1` and `index2` (including both `index1` and `index2`)"
  [list index1 index2]
  {:pre [(contains? list index1)
         (contains? list index2)
         (.lte index1 index2)]}
  (let [index-after-index2 (next-index list index2)]
    (loop [index index1, indices []]
      (if (= index index-after-index2)
        indices
        (recur (next-index list index)
               (conj indices index))))))

(defn all-indices
  "Returns a sequence of all indices in the list."
  [list]
  (if (empty? list)
    []
    (indices-range list (first-index list) (last-index list))))

(defn between
  "Returns a sub-list of all the nodes between (but not including) `index1` and `index2`."
  [list index1 index2]
  {:pre [(instance? DoublyLinkedList list)]
   :post [(instance? DoublyLinkedList %)]}
  (apply dll (map #(get list %) (indices-between list index1 index2))))

(defn range
  "Returns a sub-list of all the nodes between (and including) `index1` and `index2`."
  [list index1 index2]
  {:pre [(instance? DoublyLinkedList list)]
   :post [(instance? DoublyLinkedList %)]}
  (apply dll (map #(get list %) (indices-range list index1 index2))))

(defn- get-node
  "Get node at given index. Returns `nil` if there is no such node."
  [^DoublyLinkedList list index]
  {:pre [(instance? DoublyLinkedList list)]}
  (get (.-entries-map list) index))

(defn- next-node
  "Get successive node in the list given either an index.
   Returns `nil` if there is no next element."
  [^DoublyLinkedList list index]
  {:pre [(instance? DoublyLinkedList list)]}
  (if-let [node (get-node list index)]
    (when-let [next-index ^Node (.-next-index node)]
      (get (.-entries-map list) next-index))
    (throw (js/Error. (str "There is no element with index " index)))))

(defn next-index
  "Get successive index in the doubly-linked list given an index.
   Returns `nil` if there is no next element."
  [^DoublyLinkedList list index]
  {:pre [(instance? DoublyLinkedList list)
         (contains? list index)]}
  (when-let [n (get-node list index)]
    (.-next-index n)))

(defn safe-next-index
  "Get successive index in the doubly-linked list given an index.
   Returns the last element if there is no next element."
  [^DoublyLinkedList list index]
  {:pre [(instance? DoublyLinkedList list)
         (contains? list index)]
   :post [(some? %)]}
  (or (.-next-index (get-node list index)) (.-last-index list)))

(defn next
  "Get successive item in the doubly-linked list given an index.
   Returns `nil` if there is no next element."
  [^DoublyLinkedList list index]
  {:pre [(instance? DoublyLinkedList list)]}
  (when-let [nn (next-node list index)]
    (.-value nn)))

(defn- prev-node
  "Get previous node in the doubly-linked list given an index.
   Returns `nil` if there is no previous element."
  [^DoublyLinkedList list index]
  {:pre [(instance? DoublyLinkedList list)]}
  (if-let [node (get-node list index)]
    (when-let [prev-index ^Node (.-prev-index node)]
      (get (.-entries-map list) prev-index))
    (throw (js/Error. (str "There is no element with index " index)))))

(defn prev-index
  "Get previous index in the doubly-linked list given an index.
   Returns `nil` if there is no previous element."
  [^DoublyLinkedList list index]
  {:pre [(instance? DoublyLinkedList list)]}
  (when-let [n (get-node list index)]
    (.-prev-index n)))

(defn safe-prev-index
  "Get previous index in the doubly-linked list given an index.
   Returns the first index if there is no previous element."
  [^DoublyLinkedList list index]
  {:pre [(instance? DoublyLinkedList list)
         (contains? list index)]
   :post [(some? %)]}
  (or (.-prev-index (get-node list index)) (.-first-index list)))

(defn prev
  "Get previous item in the doubly-linked list given an index.
   Returns `nil` if there is no previous element."
  [^DoublyLinkedList list index]
  {:pre [(instance? DoublyLinkedList list)]}
  (when-let [pn (prev-node list index)]
    (.-value pn)))

(defn first
  "Returns first element in DLL."
  [^DoublyLinkedList list]
  {:pre [(instance? DoublyLinkedList list)]}
  (when-let [first-node ^Node (get (.-entries-map list) (.-first-index list))]
    (.-value first-node)))

(defn first-index
  "Returns index of first element in DLL."
  [^DoublyLinkedList list]
  {:pre [(instance? DoublyLinkedList list)]}
  (.-first-index list))

(defn last
  "Returns last element in DLL."
  [^DoublyLinkedList list]
  {:pre [(instance? DoublyLinkedList list)]}
  (when-let [last-node ^Node (get (.-entries-map list) (.-last-index list))]
    (.-value last-node)))

(defn last-index
  "Returns last element in DLL."
  [^DoublyLinkedList list]
  {:pre [(instance? DoublyLinkedList list)]}
  (.-last-index list))

(defn dll
  "Constructor for a doubly-linked-list, optionally taking a list of items to insert."
  ([]
   (DoublyLinkedList. nil (create-changelist) {} nil nil))
  ([& xs]
   (reduce (fn [list x] (conj list x)) (dll) xs)))

(defn from-indexed-items
  "Creates a DLL from a list of [index, item_at_index] tuples, like the one the DLL is printed
  as (see -pr-writer implementation above). Used when (de)serializing to / from EDN, so that
   indexes are preserved."
  [indexed-items]
  (reduce (fn [list [idx item]]
            (append list item idx))
          (dll) indexed-items))

(defn clear-changelist
  "Returns an identical DLL with a blank changelist."
  [list]
  (DoublyLinkedList. (.-meta list)
                     (create-changelist)
                     (.-entries-map list)
                     (.-first-index list)
                     (.-last-index list)))
