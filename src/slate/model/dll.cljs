(ns slate.model.dll
  "A fully-persistent, UUID-based doubly-linked list. Used for the list of paragraphs.
   The central problem here is that for our paragraph list, we need a few things:

   1. An efficient way to get the the next or previous paragraph from the list, given a paragraph.
      This is used when keying up/down between paragraphs, among other things.
   2. An efficient way to *random-access* a paragraph. When it's DOM node is clicked, for example.
   3. Efficient access to both the first and last paragraph, for jump to start/end.
   4. Efficient removal of paragraph or paragraphs at an arbitrary point in the list.
   5. The list must be *ordered.*
   6. Ideally, we do not want the indexes for accessing a paragraph to get invalidated across
      versions, provided that paragraph is still present. We need this so that whenever a <p> element
      is clicked in the DOM, we can get the paragraph model associated with it in constant time (and we need
      to be able to do this without updating every <p>'s `id` every single time).

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
   data structure presented in this namespace. All you really need to know are the public functions it
   exposes: `first`, `last`, `next`, `prev`, `remove`, `insert-after`, `insert-before`, and the constructor,
   `dll` (plus some friends). Aside from that, DLLs behave basically like other Clojure collections: you
   can call `conj`, `get`, `seq`, `map`, `filter`, `count` or `reduce` on them, and convert them to and
   from other sequential types using `into`. Destructuring and all your favorite Clojure goodies work as
   expected.

   They are also decoupled from the rest of the code -- there's no reason you couldn't put something
   other than paragraphs inside a DLL, though it's doubtful you'd need those incredibly specific set of
   properties for any other use (but hey, weirder things have happened).

   ~~The only catch is that every item inserted MUST have a :index property. So `(dll {:index \"123\" :val 1})`
   will work, but `(dll {:val 1})` will throw an error.~~"
  (:refer-clojure :exclude [first last next remove range list])
  (:require ["decimal.js" :refer [Decimal]]
            [hashp.core]))

;; Indices will always be > 0. If a DLL is created, the first item inserted will be given index 1 by default.
;; If a new item is inserted before that, it will be given index 0.5. If another before that, its index will
;; be 0.25, then 0.125, 0.0625, etc.

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
    (-write writer (str "Decimal{" decimal "}"))))

(defn big-dec "Construct new Decimal object." [n] (Decimal. n))

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
   (-write writer "#Node{value: ") (-write writer value)
   (-write writer ", prev-index: ") (-write writer prev-index)
   (-write writer ", next-index: ") (-write writer next-index)
   (-write writer "}")))

;; TODO: this blows up the whole project when you try to pretty-print it and throws
;; a downright mysterious error to do with KeySeq. My best guess is that implementing one
;; of the protocols below (IMap?) is what causes the issue. Implement pretty-printing/fix it.
(deftype DoublyLinkedList [entries-map ; map of (index -> DLLEntry)
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
  (-conj [^DoublyLinkedList dll value]
    (if (empty? entries-map)
      (let [index (big-dec 1)
            node (Node. value index nil nil)]
        (DoublyLinkedList. {index node} index index))
      (insert-after dll (.-last-index dll) value)))

  IMap
  (-dissoc [^DoublyLinkedList dll index] (remove dll index))

  IAssociative
  (-assoc [^DoublyLinkedList _dll index v]
    (if (contains? entries-map index)
      (DoublyLinkedList. (update entries-map index #(assoc-node % :value v)) first-index last-index)
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

  IPrintWithWriter
  (-pr-writer [dll writer opts]
    (-write writer "#DoublyLinkedList")
    (-write writer (vec dll)))
  #_(-pr-writer [dll writer opts]
                (-write writer "#DoublyLinkedList{entries-map: ") (-write writer (.-entries-map dll))
                (-write writer ", first-index: ") (-write writer (.-first-index dll))
                (-write writer ", last-index: ") (-write writer (.-last-index dll))
                (-write writer "}")))

(defn- assoc-node
  "Helper function for creating a new [[Node]] based on the value of an old one.
   Takes keywords for the keys and works the same way you would expect assoc to
   on a normal map, for example:
   ```
   ; creates a new Node with identical to some-node
   ; but with the value of prev-index set to 12
   (assoc-node some-node :prev-index \"12\")
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
  "Inserts `val` into the map of `entries` immediately between `prev-index` and `next-index`.
   If you call this with values of `prev-index` and `next-index` that are not adjacent, you
   will die a horrible death.

   Also note that this function operates on the entries-map, NOT the DLL itself."
  [entries value prev-idx next-idx]
  {:pre [(instance? Decimal prev-idx)
         (instance? Decimal next-idx)]}
  (let [new-idx (cond
                  (and prev-idx next-idx)
                  (.. prev-idx (add next-idx) (div 2))

                  prev-idx
                  (.add prev-idx 1)

                  next-idx
                  (if (.greaterThan next-idx 1)
                    (.sub next-idx 1)
                    (.. (Decimal. 0) (add next-idx) (div 2))))]
    (cond-> entries
      (some? prev-idx) (update prev-idx assoc-node :next-index new-idx)
      (some? next-idx) (update next-idx assoc-node :prev-index new-idx)
      :always (assoc new-idx (Node. value new-idx prev-idx next-idx)))))

(defn insert-before
  "Inserts `val` into the double-linked list `dll` immediately before index `next-index`."
  [^DoublyLinkedList dll next-index value]
  {:pre [(seq dll) (contains? dll next-index)]}
  (let [prev-idx (.-prev-index (get (.-entries-map dll) next-index))
        new-entries (if prev-idx
                      (insert-between (.-entries-map dll) value prev-idx next-index)
                      (let [first-idx (.-first-index dll)
                            new-idx (.. first-idx (div 2))]
                        (-> (.-entries-map dll)
                            (assoc new-idx (Node. value new-idx nil first-idx))
                            (update first-idx assoc-node :prev-index new-idx))))
        new-first-index (if (= (.-first-index dll) next-index)
                          (.-prev-index (get new-entries (.-first-index dll)))
                          (.-first-index dll))]
    (DoublyLinkedList. new-entries new-first-index (.-last-index dll))))

(defn insert-after
  "Inserts `val` into the double-linked list `dll` immediately after the node with index = `prev-index`."
  [^DoublyLinkedList dll prev-idx value]
  {:pre [(seq dll) (contains? dll prev-idx)]}
  (let [next-index (.-next-index (get (.-entries-map dll) prev-idx))
        new-entries (if next-index
                      (insert-between (.-entries-map dll) value prev-idx next-index)
                      (let [last-idx (.-last-index dll)
                            new-idx (.. last-idx (add 1))]
                        (-> (.-entries-map dll)
                            (assoc new-idx (Node. value new-idx last-idx nil))
                            (update last-idx assoc-node :next-index new-idx))))
        new-last-index (if (= (.-last-index dll) prev-idx)
                         (.-next-index (get new-entries prev-idx))
                         (.-last-index dll))]
    (DoublyLinkedList. new-entries (.-first-index dll) new-last-index)))

(defn prepend
  "Inserts `val` into `dll` at the beginning of the list."
  [list val]
  {:pre [(instance? DoublyLinkedList list)]}
  (if (empty? list)
    (conj list val)
    (insert-before list (first-index list) val)))

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
      (DoublyLinkedList. new-entries new-first new-last))
    list))

(defn remove-range
  "Removes all the items between the nodes with index1 and index2 (both __inclusive__)."
  [list index1 index2]
  {:pre [(contains? list index1)
         (contains? list index2)
         (.lte index1 index2)]}
  (let [first-removed (remove list index1)]
    (if (= index1 index2)
      first-removed
      (recur first-removed (next-index list index1) index2))))

(defn remove-between
  "Removes all the items between the nodes with index1 and index2 (non-inclusive of both).
   If index1 and index2 are the same or adjacent, the list will be unchanged."
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
  "Replaces all the nodes between index1 and index2 (both inclusive) with
   the supplied item or list of items. Example:
   ```
   (def lst (dll {:val :a} {:val :b} {:val :c} {:val :d}))
   (replace-range lst 2 3 {:val :e})
   ; => (dll {:val :a} {:val :e} :val :d})

   (replace-range lst 2 3 [{:val :e} {:val :f}])
   ; => (dll {:val :a} {:val :e} {:val :f} {:val :d})
   ```"
  [list index1 index2 to-insert]
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

(comment
  (= [1] (replace-range (dll :a :b) (big-dec 1) (big-dec 2) 1))
  (= [:a 1 :d] (replace-range (dll :a :b :c :d) (big-dec 2) (big-dec 3) 1))

  (= [:a 1 2 :c :d :e]
     (replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 2) [1 2]))
  (= [:a 1 2 3 :c :d :e]
     (replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 2) [1 2 3]))

  (all-indices (replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 4) [1 2 3 4 5 6 7 8 9 10]))

  (= [:a 1 2 3 :e]
     (replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 4) [1 2 3]))
  (= [(big-dec 1) (big-dec 2) (big-dec 3) (big-dec 4) (big-dec 5)]
     (all-indices (replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 4) [1 2 3])))

  (= [:a 1 2 :e]
     (replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 4) [1 2]))
  (= [(big-dec 1) (big-dec 2) (big-dec 4) (big-dec 5)]
     (all-indices (replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 4) [1 2])))

  (= [(big-dec 1) (big-dec 2) (big-dec 5)]
     (all-indices (replace-range (dll :a :b :c :d :e) (big-dec 2) (big-dec 5) [1 2])))

  )

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
         (contains? list index2)]}
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
         (contains? list index2)]}
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
  (if-let [node (get (.-entries-map list) index)]
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
  (if-let [node (get (.-entries-map list) index)]
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
   (DoublyLinkedList. {} nil nil))
  ([& xs]
   (reduce (fn [list x] (conj list x)) (dll) xs)))
