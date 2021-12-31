(ns slate.model.editor-state
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [slate.dll :as dll :refer [dll]]
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

(defprotocol Monad
  "Standard monad interface. See any of the myriad monad tutorials online for deeper explanations of its mechanics.
  EditorUpdates are modeled as monads. Every operation on EditorState can return a new EditorUpdate. They can then be
  chained using bind."
  (bind [ma, a->b] [ma, a->b, args]
    "Bind operation. Takes a monad of type a, a function of a to b, and a produces a monad of type b.
     Second arity also takes a list of arguments to be passed in to the function after its first arg.")
  (return [val] "Returns a new monad wrapping a."))

(defn >>=
  "Sugared version of Monad's bind, taking extra arguments as varargs instead of a list."
  [ma f & args]
  (bind ma f args))

(defrecord EditorState [doc, selection]
  TextContainer
  (len [{:keys [doc]}] (len doc))
  (blank? [{:keys [doc]}] (blank? doc)))

(defrecord EditorUpdate [editor-state, changelist]
  ;; EditorUpdate implements monad for the sake of easily tracking changelists (which
  ;; is needed in order to efficiently update the UI), and maintaining this as separate
  ;; from the core logic of EditorState while still allowing for easy chaining.
  ;; Most functions in this namespace return an EditorUpdate.
  Monad
  (bind [update, state->update, args]
    (let [update2 (apply state->update (:editor-state update) args)
          merged-changelists (merge-changelists (:changelist update) (:changelist update2))
          combined-update (assoc update2 :changelist merged-changelists)]
      combined-update))
  (bind [update, state->update]
    (bind update state->update [])))

(defn changelist
  "Constructor for a new changelist object. Takes keyword arguments :change-uuids, :inserted-uuids, and
  :deleted-uuids, set to an empty set by default. If no arguments supplied, returns an empty changelist."
  ([& {:keys [changed-uuids inserted-uuids deleted-uuids]}]
   {#_#_:resolved? false ;; TODO: probably not needed now that we have monadic updates
    ;; :base-state-hash base-state-hash
    :changed-uuids (or changed-uuids #{})
    :inserted-uuids (or inserted-uuids #{})
    :deleted-uuids (or deleted-uuids #{})}))

(defn identity-update
  "Returns an EditorUpdate with no changes, and therefore no effects."
  [editor-state]
  (->EditorUpdate editor-state (changelist)))

(s/def ::editor-state #(instance? EditorState %))

(defn editor-state
  "Creates a new EditorState object with the given doc and selection
   If no args supplied, creates an EditorState with an empty, single-document
   paragraph and the selection at the start of that paragraph."
  ([doc selection]
   (map->EditorState
    {:doc doc
     :selection selection}))
  ([]
   (let [p (p/paragraph)]
     (editor-state (d/document [p]) (sel/selection [(:uuid p) 0])))))

(defn- insert-text-container
  [{:keys [doc selection] :as editor-state}, content]
  {:pre [(satisfies? TextContainer content)]}
  (if (sel/range? selection)
    (-> (delete editor-state)
        (>>= insert content))
    (->EditorUpdate (assoc editor-state
                           :doc (insert doc selection content)
                           :selection (sel/shift-single selection (len content)))
                    (changelist :changed-uuids #{(sel/start-para selection)}))))

(defmethod insert [EditorState [Run]]
  [editor-state runs]
  (insert editor-state (p/paragraph runs)))

(defmethod insert [EditorState [Paragraph]]
  [{:keys [doc selection] :as editor-state}, paragraphs]
  (if (sel/range? selection)
    (-> editor-state (delete) (insert paragraphs))
    (->EditorUpdate (assoc editor-state
                           :doc (insert doc selection paragraphs)
                           :selection (let [last-paragraph (peek paragraphs)]
                                        (sel/selection [(:uuid last-paragraph) (len last-paragraph)])))
                    (changelist :changed-uuids #{(sel/start-para selection)}
                                :inserted-uuids (-> (map :uuid (drop 1 paragraphs))
                                                    (set))))))

(defmethod insert [EditorState Paragraph]
  [editor-state paragraph]
  (insert-text-container editor-state paragraph))

(defmethod insert [EditorState Run]
  [editor-state run]
  (insert-text-container editor-state run))

(defmethod insert [EditorState js/String]
  [editor-state text]
  (insert-text-container editor-state text))

(defmethod delete [EditorState]
  [{:keys [doc selection] :as editor-state}]
  (let [new-doc (delete doc selection)
        startp-uuid (-> selection :start :paragraph)]
    (if (sel/single? selection)
      ;; Single selection
      (if (zero? (sel/caret selection))
        (if (= startp-uuid (-> doc :children dll/first :uuid))
          ; First char of first paragraph, do nothing
          (identity-update editor-state)
          ; First char of a different paragraph, merge with previous
          (let [prev-para (dll/prev (:children doc) startp-uuid)]
            (->EditorUpdate (assoc editor-state
                                   :doc new-doc
                                   :selection (sel/selection [(:uuid prev-para), (len prev-para)]))
                            (changelist :changed-uuids #{(:uuid prev-para)}
                                        :deleted-uuids #{startp-uuid}))))
        ; Not the first char of the selected paragraph, normal backspace
        (->EditorUpdate (assoc editor-state
                               :doc new-doc
                               :selection (sel/shift-single selection -1))
                        (changelist :changed-uuids #{startp-uuid})))
      ;; Range selection
      (let [startp-uuid (-> selection :start :paragraph)
            endp-uuid (-> selection :end :paragraph)
            new-changelist (changelist :changed-uuids #{startp-uuid}
                                       :deleted-uuids (when-not (= startp-uuid endp-uuid)
                                                        (-> (dll/uuids-range (:children doc) startp-uuid endp-uuid)
                                                            (rest)
                                                            (set))))]
        (->EditorUpdate (assoc editor-state
                               :doc new-doc
                               :selection (sel/collapse-start selection))
                        new-changelist)))))

(defn enter
  "Equivalent to what happens when the user hits the enter button.
   Creates a new paragraph in the appropriate position in the doc.
   Optionally takes a UUID to assign to the new paragraph, otherwise
   a random one will be used."
  ([{:keys [doc selection] :as editor-state} new-uuid]
   (if (sel/range? selection)
     (-> (delete editor-state)
         (>>= enter new-uuid))
     (let [uuid (-> selection :start :paragraph)
           caret (sel/caret selection)
           new-doc (d/enter doc selection new-uuid)]
       (->EditorUpdate (assoc editor-state
                              :doc new-doc
                              :selection (sel/selection [new-uuid 0]))
                       (changelist :inserted-uuids #{new-uuid}
                                   :changed-uuids (if (or (= caret 0)
                                                          (= caret (len (get-in doc [:children uuid]))))
                                                    #{}
                                                    #{uuid}))))))
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

   The purpose of this is so that we can combine many transactions, but still only re-render the document once."
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
      {
       ;; :resolved? false
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