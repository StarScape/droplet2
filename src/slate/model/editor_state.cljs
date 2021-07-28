(ns slate.model.editor-state
  (:require [clojure.set :as set]
            [slate.dll :as dll :refer [dll]]
            [slate.map-utils :refer [remove-nil-vals-from-map]]
            [slate.model.common :refer [TextContainer
                                        Selectable
                                        insert
                                        delete
                                        insert-start
                                        insert-end
                                        len
                                        selected-content
                                        toggle-format
                                        apply-format
                                        remove-format
                                        shared-formats
                                        char-before
                                        char-at]]
            [slate.model.run :as r :refer [Run]]
            [slate.model.paragraph :as p :refer [Paragraph]]
            [slate.model.selection :as sel :refer [Selection]])
  #_(:require-macros [slate.model.doc :refer [transact->]]))

(declare merge-transactions)

(defrecord Document [children]
  TextContainer
  (len [doc] (reduce #(+ %1 (len %2)) 0 (:children doc)))
  (blank? [doc] (zero? (len doc))))

(defn document
  "Creates a new document."
  ([children]
   (cond
     (= (type children) dll/DoublyLinkedList)
     (->Document children)

     (sequential? children)
     (->Document (into (dll) children))

     :else (throw "Error: non-sequence supplied as `children` to `document` constructor.")))
  ([]
   (->Document (dll))))

;; Document helper functions
(defn- split-paragraph
  "Splits the selected paragraph at the (single) selection and returns the two halves in a vector.
   The first paragraph will keep the UUID of the paragraph it was split from, the second will be assigned
   a random UUID, unless one is provided as the third parameter."
  ([doc sel new-uuid]
   {:pre [(sel/single? sel)]}
  ;; TODO: the two new halves are assigned 2 new UUIDs, should that happen?
  ;; TODO: no, keep the UUID on the first and then assigna random to the second
   (let [target-paragraph (-> (:children doc)
                              (get (-> sel :start :paragraph)))
         [left-runs, right-runs] (-> target-paragraph
                                     (get :runs)
                                     (p/split-runs (sel/caret sel)))]
     [(p/paragraph (:uuid target-paragraph) left-runs)
      (p/paragraph new-uuid right-runs)]))
  ([doc sel]
   (split-paragraph doc sel (random-uuid))))

(defn- merge-paragraph-with-previous
  "Returns a new doc with the paragraph at `para-idx` merged into the one before it.
   The resulting paragraph will have the UUID of the previous paragraph, *not* `para-uuid`."
  [doc para-uuid]
  (let [children (:children doc)
        para (get children para-uuid)
        prev (dll/prev children para-uuid)
        merged (insert-end prev (:runs para))
        new-children (-> children (dissoc para-uuid) (assoc (:uuid prev) merged))]
    (assoc doc :children new-children)))

(defn- replace-paragraph-with
  "Returns a new doc with the paragraph having UUID `uuid` replaced with
   `content`, which can be either a paragraph or a list of paragraphs."
  [doc uuid content]
  (update doc :children #(dll/replace-range % uuid uuid content)))

(defn- insert-into-single-paragraph
  "Helper function. For document inserts where we only have to worry about a single paragraph,
   meaning we can basically just delegate to the paragraph insert function and replace the paragraph."
  [doc sel content]
  {:pre [(satisfies? TextContainer content)]}
  (let [target-uuid (-> sel :start :paragraph)
        target-para (get (:children doc) target-uuid)
        new-para (insert target-para sel content)]
    {:doc (assoc-in doc [:children target-uuid] new-para)
     :selection (-> sel
                    (sel/collapse-start)
                    (sel/shift-single (len content)))
     :changed-uuids #{target-uuid}}))

(defn- insert-paragraphs-into-doc
  "Helper function. Inserts multiple paragraphs into the document.
   The selection MUST be a single-selection. This is just a helper and
   it's assumed any deleting of a range selection has already been done."
  [doc sel paragraphs]
  {:pre [(sel/single? sel)]}
  (let [target-para-uuid (-> sel :start :paragraph)
        target-para (get (:children doc) target-para-uuid)
        sel-caret (sel/caret sel)

        first-paragraph
        (-> target-para
            (p/delete-after sel-caret)
            (insert-end (:runs (first paragraphs))))

        last-paragraph-in-list (peek paragraphs)
        last-paragraph
        (-> target-para
            (p/delete-before sel-caret)
            (insert-start (:runs last-paragraph-in-list))
            (assoc :uuid (:uuid last-paragraph-in-list)))

        ;; TODO: optimize for case where `paragraphs` is DLL?
        in-between-paragraphs
        (->> paragraphs (drop 1) (drop-last 1))

        ;; New paragraphs taking the place of target-para
        all-modified-paragraphs
        (flatten [first-paragraph in-between-paragraphs last-paragraph])

        new-children
        (dll/replace-range (:children doc) target-para-uuid target-para-uuid all-modified-paragraphs)]
    {:doc (assoc doc :children new-children)
     :selection (sel/selection [(:uuid last-paragraph-in-list), (len last-paragraph-in-list)])
     :changed-uuids #{(:uuid first-paragraph)}
     :inserted-uuids (-> (map :uuid in-between-paragraphs)
                         (conj (:uuid last-paragraph-in-list))
                         (set))}))

#_(defn- doc-insert-list
    [doc sel runs-or-paras]
    (if (sel/single? sel)
      (condp = (type (first runs-or-paras))
        Run (insert-into-single-paragraph doc sel (p/paragraph runs-or-paras))
        Paragraph (insert-paragraphs-into-doc doc sel runs-or-paras))
      (let [delete-transaction (delete doc sel)
            insert-transaction (insert (:doc delete-transaction) (:selection delete-transaction) runs-or-paras)]
        (merge-transactions delete-transaction insert-transaction))))

;; Document main operations ;;

(defmethod insert [Document Selection [Run]]
  [doc sel runs]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel (p/paragraph runs))
    (let [delete-transaction (delete doc sel)
          insert-transaction (insert (:doc delete-transaction) (:selection delete-transaction) runs)]
      (merge-transactions delete-transaction insert-transaction))))

(defmethod insert [Document Selection [Paragraph]]
  [doc sel paragraphs]
  (if (sel/single? sel)
    (insert-paragraphs-into-doc doc sel paragraphs)
    (let [delete-transaction (delete doc sel)
          insert-transaction (insert (:doc delete-transaction) (:selection delete-transaction) paragraphs)]
      (merge-transactions delete-transaction insert-transaction))))

(defmethod insert [Document Selection Paragraph]
  [doc sel para]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel para)
    (let [delete-transaction (delete doc sel)
          insert-transaction (insert-into-single-paragraph (:doc delete-transaction) (:selection delete-transaction) para)]
      (merge-transactions delete-transaction insert-transaction))))

(defmethod insert [Document Selection Run]
  [doc sel r]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel r)
    (let [delete-transaction (delete doc sel)
          insert-transaction (insert-into-single-paragraph (:doc delete-transaction) (:selection delete-transaction) r)]
      (merge-transactions delete-transaction insert-transaction))))

(defmethod insert [Document Selection js/String]
  [doc sel text]
  (insert-into-single-paragraph doc sel (r/run text)))

(defn insert-paragraph-before
  "Inserts an empty paragraph into the document immediately before the paragraph with UUID `uuid`.
   Optionally takes a UUID to assign to the new paragraph. Returns a new document, NOT a transaction."
  ([doc uuid new-para-uuid]
   (replace-paragraph-with doc uuid [(assoc (p/paragraph) :uuid new-para-uuid) ((:children doc) uuid)]))
  ([doc uuid]
   (insert-paragraph-before doc uuid (random-uuid))))

(defn insert-paragraph-after
  "Inserts an empty paragraph into the document immediately after the paragraph with UUID `uuid`.
   Optionally takes a UUID to assign to the new paragraph. Returns a new document, NOT a transaction."
  ([doc uuid new-para-uuid]
   (replace-paragraph-with doc uuid [((:children doc) uuid) (assoc (p/paragraph) :uuid new-para-uuid)]))
  ([doc uuid]
   (insert-paragraph-after doc uuid (random-uuid))))

;; TODO: insert-after and insert-before functions which take a doc, a paragraph uuid/index, and a paragraph,
;; and inserts that paragraph either before or after the paragraph with the uuid/index provided.

(defn- doc-single-delete
  "Helper function for deleting with a single selection."
  [doc sel]
  {:pre [(sel/single? sel)]}
  (let [para-uuid (sel/start-para sel)]
    (if (zero? (sel/caret sel))
      (if (= para-uuid (-> doc :children dll/first :uuid))
        ; First char of first paragraph, do nothing
        {:doc doc, :selection sel}
        ; First char of a different paragraph, merge with previous
        (let [prev-para (dll/prev (:children doc) para-uuid)]
          {:doc (merge-paragraph-with-previous doc para-uuid)
           :selection (sel/selection [(:uuid prev-para), (len prev-para)])
           :changed-uuids #{(:uuid prev-para)}
           :deleted-uuids #{para-uuid}}))
      ; Not the first char of the selected paragraph, normal backspace
      {:doc (update-in doc [:children para-uuid] delete sel)
       :selection (sel/shift-single sel -1)
       :changed-uuids #{para-uuid}})))

(defn- doc-range-delete
  "Helper function for deleting with a range selection."
  [doc sel]
  {:pre [(sel/range? sel)]}
  (let [children (:children doc)
        startp-uuid (-> sel :start :paragraph)
        endp-uuid (-> sel :end :paragraph)
        ;; Replace one paragraph if start and end are in the same paragraph, or all of them if not.
        new-para (if (= startp-uuid endp-uuid)
                   (delete (children startp-uuid) sel)
                   (p/merge-paragraphs
                    (p/delete-after (children startp-uuid) (-> sel :start :offset))
                    (p/delete-before (children endp-uuid) (-> sel :end :offset))))
        new-children (dll/replace-range children startp-uuid endp-uuid new-para)]
    (remove-nil-vals-from-map
     {:doc (assoc doc :children new-children)
      :selection (sel/collapse-start sel)
      :changed-uuids #{startp-uuid}
      :deleted-uuids (when-not (= startp-uuid endp-uuid)
                       (-> (dll/uuids-range children startp-uuid endp-uuid)
                           (rest)
                           (set)))})))

(defmethod delete [Document sel/Selection]
  [doc sel]
  (if (sel/single? sel)
    (doc-single-delete doc sel)
    (doc-range-delete doc sel)))

(defn enter
  "Equivalent to what happens when the user hits the enter button.
   Creates a new paragraph in the appropriate position in the doc.
   Optionally takes a UUID to assign to the new paragraph, otherwise
   a random one will be used."
  ([doc sel new-uuid]
   (let [caret (sel/caret sel)
         uuid (-> sel :start :paragraph)
         para ((:children doc) uuid)]
     (if (sel/range? sel)
       (let [{:keys [doc selection] :as t1} (delete doc sel)
             t2 (enter doc selection new-uuid)]
         (merge-transactions t1 t2))
       (cond
         (= caret (len para))
         {:doc (insert-paragraph-after doc uuid new-uuid)
          :selection (sel/selection [new-uuid 0])
          :inserted-uuids #{new-uuid}}

         (= caret 0)
         {:doc (insert-paragraph-before doc uuid new-uuid)
          :selection sel
          :inserted-uuids #{new-uuid}}

         :else
         (let [[para1 para2] (split-paragraph doc sel new-uuid)]
           {:doc (replace-paragraph-with doc uuid [para1 para2])
            :selection (sel/selection [(:uuid para2) 0])
            :changed-uuids #{uuid}
            :inserted-uuids #{(:uuid para2)}})))))
  ([doc sel]
   (enter doc sel (random-uuid))))

;; TODO: move to block below?
(defn doc-selected-content
  [doc sel]
  (let [start-para-uuid (-> sel :start :paragraph)
        start-para ((:children doc) start-para-uuid)
        end-para-uuid (-> sel :end :paragraph)
        end-para ((:children doc) end-para-uuid)]
    (if (sel/single-paragraph? sel)
      (selected-content ((:children doc) start-para-uuid) sel)
      (-> (dll/between (:children doc) start-para-uuid end-para-uuid)
          (dll/prepend (p/delete-before start-para (-> sel :start :offset)))
          (conj (p/delete-after end-para (-> sel :end :offset)))))))

(defn doc-shared-formats [doc sel]
  (if (sel/single-paragraph? sel)
    ;; TODO: it is probably worth having a (get-paragraph) function that takes a Document and UUID
    (shared-formats ((:children doc) (-> sel :start :paragraph)) sel)
    (->> (selected-content doc sel)
         (map shared-formats)
         (apply set/intersection))))

(defn doc-toggle-format [doc sel format]
  (if (sel/single-paragraph? sel)
    {:doc (update-in doc [:children (sel/start-para sel)] #(toggle-format % sel format))
     :selection sel
     :changed-uuids #{(sel/start-para sel)}}
    (let [children (:children doc)
          common-formats (shared-formats doc sel)
          format-fn (if (contains? common-formats format) remove-format apply-format)

          start-para-uuid (-> sel :start :paragraph)
          start-para (children start-para-uuid)
          start-para-sel (sel/selection [(:uuid start-para) (-> sel :start :offset)] [(:uuid start-para) (len start-para)])
          new-start-para (format-fn start-para start-para-sel format)

          end-para-uuid (-> sel :end :paragraph)
          end-para (children end-para-uuid)
          end-para-sel (sel/selection [(:uuid end-para) 0] [(:uuid end-para) (-> sel :end :offset)])
          new-end-para (format-fn end-para end-para-sel format)

          in-between-paras (dll/between (:children doc) start-para-uuid end-para-uuid)
          new-in-between-paras (map #(format-fn % format) in-between-paras)

          new-children (-> children
                           (assoc start-para-uuid new-start-para)
                           (assoc end-para-uuid new-end-para)
                           (dll/replace-between start-para-uuid end-para-uuid new-in-between-paras))]
      {:doc (assoc doc :children new-children)
       :selection sel
       :changed-uuids (set (dll/uuids-range new-children start-para-uuid end-para-uuid))})))

(defn first-para?
  "Returns true if the supplied Paragraph is the first paragraph in the Document."
  [doc para]
  (= para (dll/first (:children doc))))

(defn last-para?
  "Returns true if the supplied Paragraph is the last paragraph in the Document."
  [doc para]
  (= para (dll/last (:children doc))))

;; TODO: should the functions be inlined here?
(extend-type Document
  Selectable
  (char-at [doc sel] (char-at ((:children doc) (sel/start-para sel)) sel))
  (char-before [doc sel] (char-before ((:children doc) (sel/start-para sel)) sel))
  (selected-content [doc sel] (doc-selected-content doc sel)) ; TODO: how to handle UUIDs with this?
  (shared-formats [doc sel] (doc-shared-formats doc sel))
  (toggle-format [doc sel format] (doc-toggle-format doc sel format))
  ;; Formattable can be implemented as well if needed
  )

(defn merge-transactions
  "Takes two transactions and returns a third that combines them. UUIDs are rearranged
   as necessary according to the following rules:

   - If a pid is **deleted**:
     - And then inserted: move to changed
   - If a pid is **changed**:
     - And then deleted: move to deleted
   - If a pid is **inserted**:
     - And then deleted: remove from both inserted and deleted
     - And then changed: move to inserted.

   The :doc and :selection properties of the returned transaction will be those of t2.

   It is assumed that t2 is a transaction that happened immediately after t1. You cannot simply
   randomly transactions on wholly unrelated editor states, or states at different points in time.

   The purpose of this is so that we can combine many transactions, but still only rerender the document once."
  [t1 t2]
  (let [deleted-then-inserted (set (filter #(contains? (:deleted-uuids t1) %) (:inserted-uuids t2)))
        changed-then-deleted  (set (filter #(contains? (:changed-uuids t1) %) (:deleted-uuids t2)))
        inserted-then-deleted (set (filter #(contains? (:inserted-uuids t1) %) (:deleted-uuids t2)))
        inserted-then-changed (set (filter #(contains? (:inserted-uuids t1) %) (:changed-uuids t2)))

        new-deleted (-> (set/union (:deleted-uuids t1) (:deleted-uuids t2))
                        (set/difference deleted-then-inserted inserted-then-deleted)
                        (set/union changed-then-deleted))
        new-changed (-> (set/union (:changed-uuids t1) (:changed-uuids t2))
                        (set/difference changed-then-deleted inserted-then-changed)
                        (set/union deleted-then-inserted))
        new-inserted (-> (set/union (:inserted-uuids t1) (:inserted-uuids t2))
                         (set/difference inserted-then-deleted deleted-then-inserted)
                         (set/union inserted-then-changed))]
    (-> (merge t1 t2 {:deleted-uuids new-deleted
                      :changed-uuids new-changed
                      :inserted-uuids new-inserted})
        (remove-nil-vals-from-map))))

;; TODO: spec for transaction?

(comment
  (def p1 (p/paragraph [(r/run "foo" #{:italic})
                        (r/run "bar" #{:bold :italic})
                        (r/run "bizz" #{:italic})
                        (r/run "buzz" #{:bold :italic})]))

  (def p2 (p/paragraph [(r/run "aaa" #{:bold :italic})
                        (r/run "bbb" #{})
                        (r/run "ccc" #{})
                        (r/run "ddd" #{})]))

  (def doc (document [(p/paragraph "p1" [(r/run "Hello, ")])]))
  (-> (insert-into-single-paragraph doc (sel/selection ["p1" 0] ["p1" 5]) (r/run "Goodbye"))
      :children
      (vec))
  (def sel (sel/selection ["p1" 7])))