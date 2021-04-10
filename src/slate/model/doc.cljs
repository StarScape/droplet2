(ns slate.model.doc
  (:require [clojure.set :as set]
            [slate.dll :as dll :refer [dll]]
            [slate.model.common :refer [TextContainer
                                        Formattable
                                        Selectable
                                        insert
                                        delete
                                        insert-start
                                        insert-end
                                        len
                                        blank?
                                        selected-content
                                        toggle-format
                                        apply-format
                                        remove-format
                                        shared-formats
                                        char-before
                                        char-at]]
            [slate.model.run :as r :refer [Run]]
            [slate.model.paragraph :as p :refer [Paragraph]]
            [slate.selection :as sel :refer [Selection]]))

(defrecord Document [children]
  TextContainer
  (len [doc] (reduce #(+ %1 (len %2)) 0 (:children doc)))
  (blank? [doc] (zero? (len doc))))

(defn document
  "Creates a new document."
  ([children]
   (cond
     ;; TODO: test
     (= (type children) dll/DoublyLinkedList)
     (->Document children)

     (sequential? children)
     (->Document (into (dll) children))

     :else (throw "Error: non-sequence supplied as `children` to `document` constructor.")))
  ([]
   (->Document (dll))))

;; Document helper functions
(defn- split-paragraph
  "Splits the selected paragraph at the (single) selection and returns the two halves."
  [doc sel]
  ;; TODO: the two new halves are assigned 2 new UUIDs, should that happen?
  (map p/paragraph (-> (:children doc)
                       (get (-> sel :start :paragraph))
                       (get :runs)
                       (p/split-runs (sel/caret sel)))))

(defn- merge-paragraph-with-previous
  "Returns a new doc with the paragraph at `para-idx` merged into the one before it."
  [doc para-uuid]
  (let [children (:children doc)
        para (get children para-uuid)
        prev (dll/prev children para-uuid)
        merged (insert-end prev (:runs para))
        new-children (-> children (dissoc para-uuid) (assoc (:uuid prev) merged))]
    (assoc doc :children new-children)))

(defn- replace-paragraph-with
  "Returns a new doc with the paragraph having `uuid` replaced with
   `content`, which can be either a paragraph or a list of paragraphs."
  [doc uuid content]
  (update doc :children #(dll/replace-range % uuid uuid content)))

(defn- insert-into-single-paragraph
  "Helper function. For document inserts where we only have to worry about a single paragraph,
   meaning we can basically just delegate to the paragraph insert function and replace the paragraph."
  [doc sel run]
  (let [target-uuid (-> sel :start :paragraph)
        target-para (get (:children doc) target-uuid)
        new-para (insert target-para sel run)]
    (assoc-in doc [:children target-uuid] new-para)))

;; TODO: switch this to a faster concat using the rrb library
(defn- insert-paragraphs-into-doc
  "Helper function. Inserts multiple paragraphs into the document.
   The selection MUST be a single-selection. This is just a helper and
   it's assumed any deleting of a range selection has already been done."
  [doc sel paragraphs]
  (let [target-para-uuid (-> sel :start :paragraph)
        target-para (get (:children doc) target-para-uuid)
        sel-caret (sel/caret sel)

        first-paragraph
        (-> target-para
            (p/delete-after sel-caret)
            (insert-end (:runs (first paragraphs))))

        last-paragraph
        (-> target-para
            (p/delete-before sel-caret)
            (insert-start (:runs (peek paragraphs)))
            ;; needs a new UUID
            (assoc :uuid (random-uuid)))

        ;; TODO: optimize for case where `paragraphs` is DLL?
        in-between-paragraphs
        (->> paragraphs (drop 1) (drop-last 1))

        ;; New paragraphs taking the place of target-para
        all-modified-paragraphs
        (flatten [first-paragraph in-between-paragraphs last-paragraph])

        new-children
        (dll/replace-range (:children doc) target-para-uuid target-para-uuid all-modified-paragraphs)]
    (assoc doc :children new-children)))

;; Document main operations

;; TODO: cleanup m8
(defn- doc-insert-list
  [doc sel runs-or-paras]
  (if (sel/single? sel)
    (condp = (type (first runs-or-paras))
      Run (insert-into-single-paragraph doc sel runs-or-paras)
      Paragraph (insert-paragraphs-into-doc doc sel runs-or-paras))
    (insert (first (delete doc sel)) (sel/collapse-start sel) runs-or-paras)))

;; TODO: make method instance with DLL as third param
;; TODO: could we make the delete-before-moving on logic here a little more elegant with a `cond->` form?
(defmethod insert [Document Selection PersistentVector]
  [doc sel runs-or-paras]
  (doc-insert-list doc sel runs-or-paras))

(defmethod insert [Document Selection dll/DoublyLinkedList]
  [doc sel runs-or-paras]
  (doc-insert-list doc sel runs-or-paras))

(defmethod insert [Document Selection Paragraph]
  [doc sel para]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel (:runs para))
    (insert-into-single-paragraph (first (delete doc sel)) (sel/collapse-start sel) (:runs para))))

(defmethod insert [Document Selection Run]
  [doc sel r]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel r)
    (insert-into-single-paragraph (first (delete doc sel)) (sel/collapse-start sel) r)))

(defmethod insert [Document Selection js/String]
  [doc sel text]
  (insert-into-single-paragraph doc sel (r/run text)))

(defn insert-paragraph-before
  "Inserts an empty paragraph into the document immediately before the paragraph with UUID `uuid`.
   Optionally takes a UUID to assign to the new paragraph."
  ([doc uuid new-para-uuid]
   (replace-paragraph-with doc uuid [(assoc (p/paragraph) :uuid new-para-uuid) ((:children doc) uuid)]))
  ([doc uuid]
   (insert-paragraph-before doc uuid (random-uuid))))

(defn insert-paragraph-after
  "Inserts an empty paragraph into the document immediately after the paragraph with UUID `uuid`.
   Optionally takes a UUID to assign to the new paragraph."
  ([doc uuid new-para-uuid]
   (replace-paragraph-with doc uuid [((:children doc) uuid) (assoc (p/paragraph) :uuid new-para-uuid)]))
  ([doc uuid]
   (insert-paragraph-after doc uuid (random-uuid))))

(defn- doc-single-delete [doc sel]
  (if (zero? (sel/caret sel))
    (if (= (sel/start-para sel) (-> doc :children dll/first :uuid))
      [doc, sel]
      (let [uuid (sel/caret-para sel)
            prev-para (dll/prev (:children doc) uuid)]
        [(merge-paragraph-with-previous doc uuid)
         (sel/selection [(:uuid prev-para), (len prev-para)])]))
    [(update-in doc [:children (sel/start-para sel)] delete sel)
     (sel/shift-single sel -1)]))

(defn- doc-range-delete [doc sel]
  (let [startp-uuid (-> sel :start :paragraph)
        endp-uuid (-> sel :end :paragraph)
        children (:children doc)
        ;; Replace one paragraph if start and end in the same paragraph, or all of them if not.
        new-para (if (= startp-uuid endp-uuid)
                   (delete (children startp-uuid) sel)
                   (p/merge-paragraphs
                    (p/delete-after (children startp-uuid) (-> sel :start :offset))
                    (p/delete-before (children endp-uuid) (-> sel :end :offset))))
        ;; new-sel (if (= startp-uuid endp-uuid))
        new-children (dll/replace-range children startp-uuid endp-uuid new-para)]
    [(assoc doc :children new-children)
     (sel/collapse-start sel)]))

;; TODO: switch this over to returning a document change object
(defmethod delete [Document sel/Selection]
  [doc sel]
  (if (sel/single? sel)
    (doc-single-delete doc sel)
    (doc-range-delete doc sel)))

(defn enter
  "Equivalent to what happens when the user hits the enter button.
   Creates a new paragraph in the appropriate position in the doc."
  [doc sel]
  (let [caret (sel/caret sel)
        uuid (-> sel :start :paragraph)
        para ((:children doc) uuid)]
    (if (sel/range? sel)
      (let [[new-doc, new-sel] (delete doc sel)]
        (enter new-doc new-sel))
      (cond
        (= caret 0)
        [(insert-paragraph-before doc uuid), sel]

        (= caret (len para))
        (let [new-uuid (random-uuid)]
          [(insert-paragraph-after doc uuid new-uuid), (sel/selection [new-uuid 0])])

        :else
        (let [[para1 para2] (split-paragraph doc sel)]
          [(replace-paragraph-with doc uuid [para1 para2])
           (sel/selection [(:uuid para2) 0])])))))

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
    (update-in doc [:children (sel/start-para sel)] #(toggle-format % sel format))
    (let [children (:children doc)
          common-formats (shared-formats doc sel)
          format-fn (if (common-formats format) remove-format apply-format)
          start-para-uuid (-> sel :start :paragraph)
          end-para-uuid (-> sel :end :paragraph)
          start-para (children start-para-uuid)
          end-para (children end-para-uuid)
          new-start-para (format-fn
                          start-para
                          (sel/selection [(:uuid start-para) (-> sel :start :offset)] [(:uuid start-para) (len start-para)])
                          format)
          new-end-para (format-fn
                        end-para
                        (sel/selection [(:uuid end-para) 0] [(:uuid end-para) (-> sel :end :offset)])
                        format)
          ;; in-between-paras (subvec (selected-content doc sel) (inc start-para-idx) end-para-idx)
          in-between-paras (dll/between (:children doc) start-para-uuid end-para-uuid)
          in-between-paras-new (map #(format-fn % format) in-between-paras)
          new-children (-> children
                           (assoc start-para-uuid new-start-para)
                           (assoc end-para-uuid new-end-para)
                           (dll/replace-between start-para-uuid end-para-uuid in-between-paras-new))]
      (empty? in-between-paras)
      (assoc doc :children new-children))))

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
  ;; TODO: how to handle UUIDs with this?
  (selected-content [doc sel] (doc-selected-content doc sel))
  (shared-formats [doc sel] (doc-shared-formats doc sel))
  (toggle-format [doc sel format] (doc-toggle-format doc sel format)))
