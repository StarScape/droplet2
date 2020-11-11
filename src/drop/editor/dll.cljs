(ns drop.editor.dll
  "A fully-persistent, UUID-based doubly-linked list. Used for the list of paragraphs."
  (:refer-clojure :exclude [last next])
  )

;;(declare first)
;;(declare last)
;;(declare next)
;;(declare prev)
;;(declare insert-after)
;;(declare insert-before)
;; entries-map :: Map (UUID -> DLLEntry)
;; DLLEntry :: {uuid, prev-uuid, next-uuid, value}

;; TODO: maybe just make it work for anything that has a :uuid property? Dynamic typing FTW.

(deftype Node [value prev-uuid next-uuid]
  ;; Implemented pretty-printing for easier debugging
  IPrintWithWriter
  (-pr-writer [n writer opts]
   (-write writer "#Node{value: ") (-write writer (.-value n))
   (-write writer ", prev-uuid: ") (-write writer (.-prev-uuid n))
   (-write writer ", next-uuid: ") (-write writer (.-next-uuid n))
   (-write writer "}")))

(deftype DoublyLinkedList [entries-map first-node last-node]
  ISeq
  (-first [^DoublyLinkedList dll] (.. dll -first-node -value))

  ICounted
  (-count [dll] (count (.-entries-map dll)))

  ICollection
  ;; TODO: rewrite as calling into a insert-after function (and implement that)
  (-conj [^DoublyLinkedList dll val]
    (let [uuid (:uuid val)
          currently-empty? (empty? (.-entries-map dll))
          node (if currently-empty?
                 (Node. val nil nil)
                 (Node. val (:uuid (.. dll -last-node -value)) nil))
          first (if currently-empty? node (.-first-node dll))
          old-last-node (.-last-node dll)
          new-entries-map (cond-> (.-entries-map dll)
                            (not currently-empty?)
                            (assoc (:uuid (.-value old-last-node))
                                   (Node. ^obj (.-value old-last-node) ^str (.-prev-uuid old-last-node) uuid))

                            :always
                            (assoc uuid node))]
      (DoublyLinkedList. new-entries-map first node)))

  IMap
  (-dissoc [^DoublyLinkedList dll uuid]
    (let [new-entries (dissoc (.-entries-map dll) uuid)
          first ^Node (.-first-node dll)
          last ^Node (.-last-node dll)
          new-first (cond-> first
                      (= uuid (:uuid (.-value first))) (get new-entries (.-next-uuid first)))
          new-last (cond-> last
                     (= uuid (:uuid (.-value last))) (get new-entries (.-prev-uuid last)))]
      (DoublyLinkedList. new-entries new-first new-last))))

(defn dll
  "Constructor for a new doubly-linked-list."
  ([]
   (DoublyLinkedList. {} nil nil))
  ([& xs]
   (reduce (fn [dll x] (conj dll x)) (dll) xs)))

(def l (dll {:uuid 1 :content "foo"} {:uuid 2 :content "bar"}))
(.log js/console (get (.-entries-map l) 1))
(get (.-entries-map l) 1)
(get (.-entries-map l) 2)


;; (first (dll {:uuid 1, :content "foo"}))
;; (assoc (dll) 1 {:uuid 1, :content "foo"})

;; INext