(ns slate.model.editor-state
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [slate.dll :as dll :refer [dll]]
            [slate.map-utils :refer [remove-nil-vals-from-map]]
            [slate.model.common :refer [TextContainer
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
            [slate.model.doc :as d :refer [Document]]
            [slate.model.selection :as sel :refer [Selection]]))

(declare merge-changelists)

(defrecord EditorState [doc
                        selection
                        changelist]
  TextContainer
  (len [{:keys [doc]}] (len doc))
  (blank? [{:keys [doc]}] (blank? doc)))

(s/def ::editor-state #(instance? EditorState %))

(defn changelist
  "Constructor for a new changelist object"
  ([_base-state-hash]
   {:resolved? false
    ;; :base-state-hash base-state-hash
    :changed-uuids #{}
    :inserted-uuids #{}
    :deleted-uuids #{}})
  ([] (changelist nil)))

(defn editor-state
  "Creates a new EditorState object with the given doc and selection
   If no args supplied, creates an EditorState with an empty, single-document
   paragraph and the selection at the start of that paragraph."
  ([doc selection]
   (map->EditorState
    {:doc doc
     :selection selection
     :changelist (changelist)}))
  ([]
   (let [p (p/paragraph)]
     (editor-state (d/document [p]) (sel/selection [(:uuid p) 0])))))

(defn assoc-state
  "Works like normal assoc, but updates editor-state's changelist automatically."
  [editor-state & {:keys [doc selection changelist]}]
  (assoc editor-state
         :doc (or doc (:doc editor-state))
         :selection (or selection (:selection editor-state))
         :changelist (merge-changelists (:changelist editor-state) changelist)))

(defn- insert-text-container
  [{:keys [doc selection] :as editor-state}, content]
  {:pre [(satisfies? TextContainer content)]}
  (if (sel/range? selection)
    (-> (delete editor-state)
        (insert content))
    (assoc-state editor-state
                 :doc (insert doc selection content)
                 :selection (sel/shift-single selection (len content))
                 :changelist {:changed-uuids #{(sel/start-para selection)}})))
(defmethod insert [EditorState [Run]]
  [editor-state runs]
  (insert editor-state (p/paragraph runs)))

(defmethod insert [EditorState [Paragraph]]
  [{:keys [doc selection] :as editor-state}, paragraphs]
  (if (sel/range? selection)
    (-> (delete editor-state)
        (insert paragraphs))
    (let [new-selection (let [last-paragraph (peek paragraphs)]
                          (sel/selection [(:uuid last-paragraph) (len last-paragraph)]))
          new-doc (insert doc selection paragraphs)
          new-changelist {:changed-uuids #{(sel/start-para selection)}
                          :inserted-uuids (-> (map :uuid (drop 1 paragraphs))
                                              (set))}]
      (assoc-state editor-state
                   :doc new-doc
                   :selection new-selection
                   :changelist new-changelist))))

(defmethod insert [EditorState Paragraph]
  [editor-state paragraph]
  (insert-text-container editor-state paragraph))

(defmethod insert [EditorState Run]
  [editor-state run]
  (insert-text-container editor-state run))

(defmethod insert [EditorState js/String]
  [editor-state text]
  (insert-text-container editor-state text))

;; TODO: insert-after and insert-before functions which take a doc, a paragraph uuid/index, and a paragraph,
;; and inserts that paragraph either before or after the paragraph with the uuid/index provided.

#_(defn- doc-single-delete
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

#_(defn- doc-range-delete
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

(defmethod delete [EditorState]
  [{:keys [doc selection] :as editor-state}]
  (let [new-doc (delete doc selection)
        startp-uuid (-> selection :start :paragraph)]
    (if (sel/single? selection)
      ;; Single selection
      (if (zero? (sel/caret selection))
        (if (= startp-uuid (-> doc :children dll/first :uuid))
          ; First char of first paragraph, do nothing
          (assoc-state editor-state :doc new-doc)
          ; First char of a different paragraph, merge with previous
          (let [prev-para (dll/prev (:children doc) startp-uuid)]
            (assoc-state editor-state
                         :doc new-doc
                         :selection (sel/selection [(:uuid prev-para), (len prev-para)])
                         :changelist {:changed-uuids #{(:uuid prev-para)}
                                      :deleted-uuids #{startp-uuid}})))
        ; Not the first char of the selected paragraph, normal backspace
        (assoc-state editor-state
                     :doc new-doc
                     :selection (sel/shift-single selection -1)
                     :changelist {:changed-uuids #{startp-uuid}}))
      ;; Range selection
      (let [startp-uuid (-> selection :start :paragraph)
            endp-uuid (-> selection :end :paragraph)
            new-changelist {:changed-uuids #{startp-uuid}
                            :deleted-uuids (when-not (= startp-uuid endp-uuid)
                                             (-> (dll/uuids-range (:children doc) startp-uuid endp-uuid)
                                                 (rest)
                                                 (set)))}]
        (assoc-state editor-state
                     :doc new-doc
                     :selection (sel/collapse-start selection)
                     :changelist new-changelist)))))

(defn enter
  "Equivalent to what happens when the user hits the enter button.
   Creates a new paragraph in the appropriate position in the doc.
   Optionally takes a UUID to assign to the new paragraph, otherwise
   a random one will be used."
  ([{:keys [doc selection] :as editor-state} new-uuid]
   (if (sel/range? selection)
     (-> (delete editor-state)
         (enter new-uuid))
     (let [uuid (-> selection :start :paragraph)
           caret (sel/caret selection)
           new-doc (d/enter doc selection new-uuid)]
       (assoc-state editor-state
                    :doc new-doc
                    :selection (sel/selection [new-uuid 0])
                    :changelist {:inserted-uuids #{new-uuid}
                                 :changed-uuids (if (or (= caret 0)
                                                        (= caret (len (get-in doc [:children uuid]))))
                                                  #{}
                                                  #{uuid})}))))
  ([editor-state]
   (enter editor-state (random-uuid))))

;; TODO: any point in implementing this for EditorState (keep in mind: YAGNI)
#_(extend-type EditorState
  Selectable
  (char-at [{:keys [doc selection]}]
    (char-at doc selection))
  (char-before [{:keys [doc selection]}]
    (char-before doc selection))
  (selected-content [{:keys [doc selection]}]
    (selected-content doc selection))
  (shared-formats [{:keys [doc selection]}]
    (shared-formats doc selection))
  (toggle-format [{:keys [doc selection] :as editor-state}]
    (let [doc-children (:children doc)
          start-para-uuid (-> selection :start :paragraph)
          end-para-uuid (-> selection :end :paragraph)
          changed-uuids (dll/uuids-range doc-children start-para-uuid end-para-uuid)]
      (assoc-state :doc (toggle-format doc selection)
                   :changelist {:changed-uuids (set changed-uuids)})))
  ;; Formattable can be implemented as well if needed
  )

(defn merge-changelists
  "Takes two transactions and returns a third that combines them. UUIDs are rearranged
   as necessary according to the following rules:

   - If a pid is **deleted**:
     - And then inserted: move to changed
   - If a pid is **changed**:
     - And then deleted: move to deleted
   - If a pid is **inserted**:
     - And then deleted: remove from both inserted and deleted
     - And then changed: move to inserted.

   The :doc and :selection properties of the returned transaction will be those of c2.

   It is assumed that c2 is a transaction that happened immediately after c1. You cannot simply
   randomly transactions on wholly unrelated editor states, or states at different points in time.

   The purpose of this is so that we can combine many transactions, but still only rerender the document once."
  [c1 c2]
  (if (:resolved? c1)
    c2
    (let [deleted-then-inserted (set (filter #(contains? (:deleted-uuids c1) %) (:inserted-uuids c2)))
          changed-then-deleted  (set (filter #(contains? (:changed-uuids c1) %) (:deleted-uuids c2)))
          inserted-then-deleted (set (filter #(contains? (:inserted-uuids c1) %) (:deleted-uuids c2)))
          inserted-then-changed (set (filter #(contains? (:inserted-uuids c1) %) (:changed-uuids c2)))

          new-deleted (-> (set/union (:deleted-uuids c1) (:deleted-uuids c2))
                          (set/difference deleted-then-inserted inserted-then-deleted)
                          (set/union changed-then-deleted))
          new-changed (-> (set/union (:changed-uuids c1) (:changed-uuids c2))
                          (set/difference changed-then-deleted inserted-then-changed)
                          (set/union deleted-then-inserted))
          new-inserted (-> (set/union (:inserted-uuids c1) (:inserted-uuids c2))
                           (set/difference inserted-then-deleted deleted-then-inserted)
                           (set/union inserted-then-changed))]
      {:resolved? false
     ;; :base-state-hash (:base-state-hash c1)
       :deleted-uuids new-deleted
       :changed-uuids new-changed
       :inserted-uuids new-inserted})))

(comment
  (def p1 (p/paragraph [(r/run "foo" #{:italic})
                        (r/run "bar" #{:bold :italic})
                        (r/run "bizz" #{:italic})
                        (r/run "buzz" #{:bold :italic})]))

  (def p2 (p/paragraph [(r/run "aaa" #{:bold :italic})
                        (r/run "bbb" #{})
                        (r/run "ccc" #{})
                        (r/run "ddd" #{})]))

  #_(def doc (document [(p/paragraph "p1" [(r/run "Hello, ")])]))
  #_(-> (insert-text-container doc (sel/selection ["p1" 0] ["p1" 5]) (r/run "Goodbye"))
      :children
      (vec))
  (def sel (sel/selection ["p1" 7])))