(ns slate.model.doc
  (:require [clojure.set :as set]
            [slate.dll :as dll :refer [dll]]
            [slate.model.common :as sl :refer [TextContainer
                                               Selectable
                                               Fragment
                                               Formattable
                                               insert
                                               delete
                                               insert-start
                                               insert-end
                                               text
                                               len
                                               selected-content
                                               toggle-format
                                               apply-format
                                               remove-format
                                               formatting
                                               char-before
                                               char-at]]
            [slate.model.run :as r :refer [Run]]
            [slate.model.paragraph :as p :refer [Paragraph]]
            [slate.model.selection :as sel :refer [Selection]]))

(def ^:const types-preserved-on-enter #{:ul, :ol})

(declare merge-transactions)

(defrecord Document [children]
  TextContainer
  (text [doc] (reduce str (map text (:children doc))))
  (len [doc] (reduce #(+ %1 (len %2)) 0 (:children doc)))
  (blank? [doc] (zero? (len doc))))

(defrecord DocumentFragment [paragraphs]
  Fragment
  (items [document-fragment] (:paragraphs document-fragment))
  (fragment-type [_] :document)

  TextContainer
  (text [f] (reduce str (map text (sl/items f))))
  (len [f] (reduce #(+ %1 (len %2)) 0 (sl/items f)))
  (blank? [f] (zero? (len f))))

(defn document
  "Creates a new document."
  ([children]
   ;; TODO: check that children all have unique IDs, but not in production.
   (cond
     (= (type children) dll/DoublyLinkedList)
     (->Document children)

     (sequential? children)
     (->Document (into (dll) children))

     :else
     (throw "Error: non-sequence type supplied as `children` to `document` constructor.")))
  ([]
   (->Document (dll))))

(defn fragment
  "Creates a new DocumentFragment."
  [paragraph-or-paragraphs]
  (if (sequential? paragraph-or-paragraphs)
    (->DocumentFragment paragraph-or-paragraphs)
    (->DocumentFragment [paragraph-or-paragraphs])))

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
         paragraph-type (:type target-paragraph)
         [left-runs, right-runs] (-> target-paragraph
                                     (get :runs)
                                     (p/split-runs (sel/caret sel)))]
     [(p/paragraph (:uuid target-paragraph) paragraph-type left-runs)
      (p/paragraph new-uuid paragraph-type right-runs)]))
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
  (let [target-uuid (-> sel :start :paragraph)
        target-para (get (:children doc) target-uuid)
        new-para (insert target-para sel content)]
    (assoc-in doc [:children target-uuid] new-para)))

(defn- insert-paragraphs-into-doc
  "Helper function. Inserts multiple paragraphs into the document.
   The selection MUST be a single-selection. This is just a helper and
   it's assumed any deleting of a range selection has already been done."
  [doc sel paragraphs]
  {:pre [(sel/single? sel)
         (sequential? paragraphs)
         (> (count paragraphs) 1)]}
  (let [target-para-uuid (-> sel :start :paragraph)
        target-para (get (:children doc) target-para-uuid)
        sel-caret (sel/caret sel)
        first-paragraph-in-list (first paragraphs)
        first-paragraph (-> target-para
                            (p/delete-after sel-caret)
                            (insert-end first-paragraph-in-list)
                            ;; TODO: should this be conditional based on the selection offset or something?
                            ;; Is this current behavior intuitive?
                            #_(assoc :type (:type first-paragraph-in-list)))
        last-paragraph-in-list (if (satisfies? IStack paragraphs)
                                 (peek paragraphs)
                                 (last paragraphs))
        last-paragraph (-> target-para
                           (p/delete-before sel-caret)
                           (insert-start last-paragraph-in-list)
                           (assoc :uuid (:uuid last-paragraph-in-list)))
        ;; TODO: optimize for case where `paragraphs` is DLL?
        in-between-paragraphs (->> paragraphs (drop 1) (drop-last 1))
        ;; New paragraphs taking the place of target-para
        all-modified-paragraphs (flatten [first-paragraph in-between-paragraphs last-paragraph])
        new-children (dll/replace-range (:children doc)
                                        target-para-uuid
                                        target-para-uuid
                                        all-modified-paragraphs)]
    (assoc doc :children new-children)))

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
    (insert-into-single-paragraph doc sel runs)
    (-> (delete doc sel)
        (insert (sel/collapse-start sel) runs))))

(defmethod insert [Document Selection [Paragraph]]
  [doc sel paragraphs]
  (if (sel/single? sel)
    (insert-paragraphs-into-doc doc sel paragraphs)
    (-> (delete doc sel)
        (insert (sel/collapse-start sel) paragraphs))))

(defmethod insert [Document Selection Paragraph]
  [doc sel para]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel para)
    (-> (delete doc sel)
        (insert (sel/collapse-start sel) para))))

(defmethod insert [Document Selection Run]
  [doc sel r]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel r)
    (-> (delete doc sel)
        (insert (sel/collapse-start sel) r))))

(defmethod insert [Document Selection js/String]
  [doc sel text]
  (insert-into-single-paragraph doc sel (r/run text (:formats sel))))

(defn insert-paragraph-before
  "Inserts an empty paragraph into the document immediately before the paragraph with UUID `uuid`.
   Optionally takes a UUID to assign to the new paragraph. Returns a new document."
  ([doc uuid new-para-uuid type]
   (replace-paragraph-with doc uuid [(assoc (p/paragraph) :uuid new-para-uuid :type type),
                                     (get (:children doc) uuid)]))
  ([doc uuid]
   (insert-paragraph-before doc uuid (random-uuid) :body)))

(defn insert-paragraph-after
  "Inserts an empty paragraph into the document immediately after the paragraph with UUID `uuid`.
   Optionally takes a UUID to assign to the new paragraph. Returns a new document."
  ([doc uuid new-para-uuid type]
   (replace-paragraph-with doc uuid [((:children doc) uuid),
                                     (assoc (p/paragraph) :uuid new-para-uuid :type type)]))
  ([doc uuid]
   (insert-paragraph-after doc uuid (random-uuid) :body)))

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
        doc
        ; First char of a different paragraph, merge with previous
        (merge-paragraph-with-previous doc para-uuid))
      ; Not the first char of the selected paragraph, normal backspace
      (update-in doc [:children para-uuid] delete sel))))

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
    (assoc doc :children new-children)))

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
   {:pre [(sel/single? sel)]}
   (let [caret (sel/caret sel)
         uuid (-> sel :start :paragraph)
         para ((:children doc) uuid)
         new-para-type (if (contains? types-preserved-on-enter (:type para))
                         (:type para)
                         :body)]
     (cond
       (= caret 0)
       (insert-paragraph-before doc uuid new-uuid new-para-type)

       (= caret (len para))
       (insert-paragraph-after doc uuid new-uuid new-para-type)

       :else
       (let [[para1 para2] (split-paragraph doc sel new-uuid)]
         (replace-paragraph-with doc uuid [para1 para2])))))
  ([doc sel]
   (enter doc sel (random-uuid))))

(defn doc-selected-content
  [doc sel]
  (let [start-para-uuid (-> sel :start :paragraph)
        start-para ((:children doc) start-para-uuid)
        end-para-uuid (-> sel :end :paragraph)
        end-para ((:children doc) end-para-uuid)]
    (if (sel/single-paragraph? sel)
      (if (p/whole-paragraph-selected? start-para sel)
        (fragment start-para)
        (selected-content start-para sel))
      (let [start-offset (-> sel :start :offset)
            end-offset (-> sel :end :offset)
            fragment-paragraphs (as-> (dll/between (:children doc) start-para-uuid end-para-uuid) $
                                  ;; Only include start paragraph if selection doesn't start at the very end of that paragraph
                                  (if (< start-offset (sl/len start-para))
                                    (dll/prepend $ (p/delete-before start-para start-offset))
                                    $)
                                  ;; Only include end paragraph if selection doesn't end at the very start of that paragraph
                                  (if (pos? end-offset)
                                    (conj $ (p/delete-after end-para end-offset))
                                    $))]
        (fragment fragment-paragraphs)))))

(defn- set-intersection
  "Like set/intersection, but will return an empty set if supplied no arguments"
  ([] #{})
  ([& sets] (apply set/intersection sets)))

(defn doc-formatting [doc sel]
  (let [caret-para (get (:children doc) (sel/caret-para sel))]
    (if (sel/single-paragraph? sel)
      (formatting caret-para sel)
      (->> (selected-content doc sel)
           (sl/items)
           (map (comp set formatting))
           ;; ↓↓↓ Calling apply on set/intersection with empty seq yields
           ;; nil, we want to make sure that this always yields a set.
           (apply set-intersection)))))

(defn doc-toggle-format [doc sel format]
  (if (sel/single-paragraph? sel)
    (update-in doc [:children (sel/start-para sel)] #(toggle-format % sel format))
    (let [children (:children doc)
          common-formats (formatting doc sel)
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
      (assoc doc :children new-children))))

(defn first-para?
  "Returns true if the supplied Paragraph is the first paragraph in the Document."
  [doc para]
  (= para (dll/first (:children doc))))

(defn last-para?
  "Returns true if the supplied Paragraph is the last paragraph in the Document."
  [doc para]
  (= para (dll/last (:children doc))))

(extend-type Document
  Selectable
  (char-at [doc sel] (char-at ((:children doc) (sel/caret-para sel)) sel))
  (char-before [doc sel] (char-before ((:children doc) (sel/caret-para sel)) sel))
  (selected-content [doc sel] (doc-selected-content doc sel)) ; TODO: how to handle UUIDs with this?
  (formatting [doc sel] (doc-formatting doc sel))

  Formattable
  (toggle-format [doc sel format] (doc-toggle-format doc sel format)))

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
