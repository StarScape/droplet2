(ns slate.model.doc
  (:require [clojure.set :as set]
            [slate.dll :as dll :refer [dll]]
            [slate.model.common :as sl :refer [TextContainer
                                               Selectable
                                               Fragment
                                               text
                                               len
                                               selected-content
                                               formatting
                                               char-before
                                               char-at]]
            [slate.model.run :as r :refer [Run]]
            [slate.model.paragraph :as p :refer [Paragraph]]
            [slate.model.selection :as sel]))

(def ^:const types-preserved-on-enter #{:ul, :ol})

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
   (cond
     (= (type children) dll/DoublyLinkedList)
     (->Document children)

     (sequential? children)
     (->Document (into (dll) children))

     :else
     (throw (js/Error. "Error: non-sequence type supplied as `children` to `document` constructor."))))
  ([]
   (->Document (dll))))

(defn fragment
  "Creates a new DocumentFragment."
  [paragraph-or-paragraphs]
  (if (sequential? paragraph-or-paragraphs)
    (->DocumentFragment paragraph-or-paragraphs)
    (->DocumentFragment [paragraph-or-paragraphs])))

;; Document helper functions
(defn split-paragraph
  "Splits the selected paragraph at the (single) selection and returns the two halves in a vector."
  [doc sel]
  {:pre [(sel/single? sel)]}
  (let [target-paragraph (-> (:children doc)
                             (get (-> sel :start :paragraph)))
        paragraph-type (:type target-paragraph)
        [left-runs, right-runs] (-> target-paragraph
                                    (get :runs)
                                    (p/split-runs (sel/caret sel)))]
    [(p/paragraph paragraph-type left-runs)
     (p/paragraph paragraph-type right-runs)]))

(defn- merge-paragraph-with-previous
  "Returns a new doc with the paragraph at `para-idx` merged into the one before it."
  [doc para-idx]
  (let [children (:children doc)
        para (get children para-idx)
        prev-idx (dll/prev-index children para-idx)
        prev (dll/prev children para-idx)
        merged (p/insert-end prev (p/fragment (:runs para)))
        new-children (-> children
                         (dissoc para-idx)
                         (assoc prev-idx merged))]
    (assoc doc :children new-children)))

(defn replace-paragraph-with
  "Returns a new doc with the paragraph at index `idx replaced with
   `content`, which can be either a paragraph or a list of paragraphs."
  [doc idx content]
  (update doc :children #(dll/replace-range % idx idx content)))

(defn- insert-into-single-paragraph
  "Helper function. For document inserts where we only have to worry about a single paragraph,
   meaning we can basically just delegate to the paragraph insert function and replace the paragraph."
  [doc sel content]
  (let [target-idx (-> sel :start :paragraph)
        target-para (get (:children doc) target-idx)
        new-para (p/insert target-para sel content)]
    (assoc-in doc [:children target-idx] new-para)))

(defn- insert-paragraphs-into-doc
  "Helper function. Inserts multiple paragraphs into the document.
   The selection MUST be a single-selection. This is just a helper and
   it's assumed any deleting of a range selection has already been done."
  [doc sel paragraphs]
  {:pre [(sel/single? sel)
         (sequential? paragraphs)
         #_(> (count paragraphs) 1)]}
  (let [target-para-idx (-> sel :start :paragraph)
        target-para (get (:children doc) target-para-idx)
        sel-caret (sel/caret sel)
        first-paragraph-in-list (first paragraphs)
        first-paragraph (-> target-para
                            (p/delete-after sel-caret)
                            (p/insert-end first-paragraph-in-list)
                            ;; TODO: should this be conditional based on the selection offset or something?
                            ;; Is this current behavior intuitive?
                            #_(assoc :type (:type first-paragraph-in-list)))
        last-paragraph-in-list (if (satisfies? IStack paragraphs)
                                 (peek paragraphs)
                                 (last paragraphs))
        last-paragraph (-> target-para
                           (p/delete-before sel-caret)
                           (p/insert-start last-paragraph-in-list))
        ;; TODO: optimize for case where `paragraphs` is DLL?
        in-between-paragraphs (->> paragraphs (drop 1) (drop-last 1))
        ;; New paragraphs taking the place of target-para
        all-modified-paragraphs (flatten [first-paragraph in-between-paragraphs last-paragraph])
        new-children (dll/replace-range (:children doc)
                                        target-para-idx
                                        target-para-idx
                                        all-modified-paragraphs)]
    (assoc doc :children new-children)))

;; Document main operations ;;
(defn delete
  "Deletes the selection from the Document. If single selection, acts as backspace."
  [doc sel]
  (if (sel/single? sel)
    (let [para-idx (sel/start-para sel)]
      (if (zero? (sel/caret sel))
        (if (= para-idx (dll/first-index (:children doc)))
          ; First char of first paragraph, do nothing
          doc
          ; First char of a different paragraph, merge with previous
          (merge-paragraph-with-previous doc para-idx))
        ; Not the first char of the selected paragraph, normal backspace
        (update-in doc [:children para-idx] p/delete sel)))
    (let [children (:children doc)
          start-para-idx (-> sel :start :paragraph)
          end-para-idx (-> sel :end :paragraph)
          ;; Replace one paragraph if start and end are in the same paragraph, or all of them if not.
          new-para (if (= start-para-idx end-para-idx)
                     (p/delete (children start-para-idx) sel)
                     (p/merge-paragraphs (p/delete-after (children start-para-idx) (-> sel :start :offset))
                                         (p/delete-before (children end-para-idx) (-> sel :end :offset))))
          new-children (dll/replace-range children start-para-idx end-para-idx new-para)]
      (assoc doc :children new-children))))

(defmulti insert "Inserts into the Document."
  {:arglists '([document selection content-to-insert])}
  (fn [& args] (type (last args))))

(defmethod insert
  p/ParagraphFragment
  [doc sel fragment]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel fragment)
    (-> (delete doc sel)
        (insert (sel/collapse-start sel) fragment))))

(defmethod insert
  DocumentFragment
  [doc sel fragment]
  (if (sel/single? sel)
    (let [paragraphs (sl/items fragment)]
      (if (= 1 (count paragraphs))
        (insert-into-single-paragraph doc sel (first paragraphs))
        (insert-paragraphs-into-doc doc sel paragraphs)))
    (-> (delete doc sel)
        (insert (sel/collapse-start sel) fragment))))

(defmethod insert
  Paragraph
  [doc sel para]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel para)
    (-> (delete doc sel)
        (insert (sel/collapse-start sel) para))))

(defmethod insert
  Run
  [doc sel r]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel r)
    (-> (delete doc sel)
        (insert (sel/collapse-start sel) r))))

(defmethod insert
  js/String
  [doc sel text]
  (if (sel/single? sel)
    (insert-into-single-paragraph doc sel (r/run text (:formats sel)))
    (-> (delete doc sel)
        (insert (sel/collapse-start sel) text))))

(defn insert-paragraph-before
  "Inserts an empty paragraph into the document immediately before the paragraph at index `idx`."
  ([doc idx new-paragraph-type]
   (update doc :children dll/insert-before idx (assoc (p/paragraph) :type new-paragraph-type)))
  ([doc idx]
   (insert-paragraph-before doc idx :body)))

(defn insert-paragraph-after
  "Inserts an empty paragraph into the document immediately after the paragraph at index `idx`.
   Optionally takes a UUID to assign to the new paragraph. Returns a new document."
  ([doc idx new-paragraph-type]
   (update doc :children dll/insert-after idx (assoc (p/paragraph) :type new-paragraph-type)))
  ([doc idx]
   (insert-paragraph-after doc idx :body)))

#_(defn enter
  "Equivalent to what happens when the user hits the enter button.
   Creates a new paragraph in the appropriate position in the doc."
  ([doc sel]
   {:pre [(sel/single? sel)]}
   (let [caret (sel/caret sel)
         para-idx (-> sel :start :paragraph)
         para ((:children doc) para-idx)
         new-para-type (if (contains? types-preserved-on-enter (:type para))
                         (:type para)
                         :body)]
     (cond
       (= caret 0)
       (insert-paragraph-before doc para-idx new-para-type)

       (= caret (len para))
       (insert-paragraph-after doc para-idx new-para-type)

       :else
       (let [[para1 para2] (split-paragraph doc sel)]
         (replace-paragraph-with doc para-idx [para1 para2]))))))

(defn doc-selected-content
  [doc sel]
  (let [start-para-idx (-> sel :start :paragraph)
        start-para ((:children doc) start-para-idx)
        end-para-idx (-> sel :end :paragraph)
        end-para ((:children doc) end-para-idx)]
    (if (sel/single-paragraph? sel)
      (if (p/whole-paragraph-selected? start-para sel)
        (fragment start-para)
        (selected-content start-para sel))
      (let [start-offset (-> sel :start :offset)
            end-offset (-> sel :end :offset)
            fragment-paragraphs (-> (dll/between (:children doc) start-para-idx end-para-idx)
                                    (dll/prepend (p/delete-before start-para start-offset))
                                    (conj (p/delete-after end-para end-offset)))]
        (fragment fragment-paragraphs)))))

(defn- set-intersection
  "Like set/intersection, but will return an empty set if supplied no arguments"
  ([] #{})
  ([& sets] (apply set/intersection sets)))

(defn doc-formatting
  [doc sel]
  (let [caret-para (get (:children doc) (sel/caret-para sel))]
    (if (sel/single-paragraph? sel)
      (formatting caret-para sel)
      (->> (doc-selected-content doc sel)
           (sl/items)
           ;; Filter out empty paragraph from the selected paragraphs.
           ;; Why? Because an empty paragraph is either (a) inconsequential to formatting, e.g.
           ;; in the case of an empty paragraph between two content paragraphs or (b) the source
           ;; paragraph is not actual empty, but the start of the selection is situated on the very
           ;; end of a paragraph, or the end of the selection is situated at the very start of a paragraph,
           ;; both of which mean no actual _text content_ from the paragraph is selected.
           ;; ---
           ;; In the future, it may be necessary to provide a "strict" and "non-strict" version of this function
           ;; (and/or the (selected-content) function), where the strict version does not include empty paragraphs
           ;; from (selected-content), but the non-strict version does.
           (filter #(not (sl/blank? %)))
           ;; Get formatting for each remaining paragraph, calling set to insure uniqueness and type
           (map (comp set formatting))
            ;; ↓↓↓ Calling apply on set/intersection with empty seq yields
            ;; nil, we want to make sure that this always yields a set.
           (apply set-intersection)))))

(defn toggle-format [doc sel format]
  (if (sel/single-paragraph? sel)
    (update-in doc [:children (sel/start-para sel)] #(p/toggle-format % sel format))
    (let [children (:children doc)
          common-formats (formatting doc sel)
          format-fn (if (contains? common-formats format) p/remove-format p/apply-format)

          start-para-idx (-> sel :start :paragraph)
          start-para (children start-para-idx)
          start-para-sel (sel/selection [start-para-idx (-> sel :start :offset)] [start-para-idx (len start-para)])
          new-start-para (format-fn start-para start-para-sel format)

          end-para-idx (-> sel :end :paragraph)
          end-para (children end-para-idx)
          end-para-sel (sel/selection [end-para-idx 0] [end-para-idx (-> sel :end :offset)])
          new-end-para (format-fn end-para end-para-sel format)

          in-between-paras (dll/between (:children doc) start-para-idx end-para-idx)
          new-in-between-paras (map #(format-fn % format) in-between-paras)

          new-children (-> children
                           (assoc start-para-idx new-start-para)
                           (assoc end-para-idx new-end-para)
                           (dll/replace-between start-para-idx end-para-idx new-in-between-paras))]
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
  (formatting [doc sel] (doc-formatting doc sel)))

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
