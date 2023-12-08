(ns slate.model.editor-state
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [slate.model.dll :as dll]
            [slate.model.common :refer [TextContainer len blank?] :as m]
            [slate.model.run :as r :refer [Run]]
            [slate.model.paragraph :as p :refer [Paragraph ParagraphFragment]]
            [slate.model.doc :as doc :refer [DocumentFragment]]
            [slate.model.selection :as sel]
            [slate.model.navigation :as nav :refer [Navigable]]))

(declare EditorState)

(s/def ::doc ::doc/Document)
(s/def ::selection ::sel/Selection)
(s/def ::editor-state
  (s/and #(instance? EditorState %)
         (s/keys :req-un [::doc
                          ::selection])))

(defrecord EditorState [doc, selection]
  ;; TODO: needed for EditorState?
  TextContainer
  (len [{:keys [doc]}] (len doc))
  (blank? [{:keys [doc]}] (blank? doc)))

(defn editor-state
  "Creates a new EditorState object with the given doc and selection.
   If no args supplied, creates an EditorState with an empty, single-document
   paragraph and the selection at the start of that paragraph."
  ([doc selection]
   (map->EditorState
    {:doc doc
     :selection selection}))
  ([doc]
   (editor-state doc (sel/selection [(-> doc :children dll/first-index) 0])))
  ([]
   (editor-state (doc/document) (sel/selection [(dll/big-dec 1) 0]))))

(defn delete
  "Deletes the current selection. Returns a new EditorState"
  [{:keys [doc selection] :as editor-state}]
  (let [new-doc (doc/delete doc selection)
        start-para-idx (-> selection :start :paragraph)]
    (if (sel/single? selection)
      ;; Single selection
      (if (zero? (sel/caret selection))
        (if (= start-para-idx (-> doc :children dll/first-index))
          ; First char of first paragraph, do nothing
          editor-state
          ; First char of a different paragraph, merge with previous
          (let [prev-para-idx (dll/prev-index (:children doc) start-para-idx)
                prev-para ((:children doc) prev-para-idx)
                new-selection (->> (sel/selection [prev-para-idx, (len prev-para)])
                                   (nav/autoset-formats prev-para))]
            (assoc editor-state
                   :doc new-doc
                   :selection new-selection)))
        ; Not the first char of the selected paragraph, normal backspace
        (assoc editor-state
               :doc new-doc
               :selection (nav/prev-char doc selection)))
      ;; Range selection
      (assoc editor-state
             :doc new-doc
             :selection (nav/autoset-formats new-doc (sel/collapse-start selection))))))

(defmulti insert
  "Inserts into the EditorState's document at the current selection. Returns an EditorState."
  {:arglists '([editor-state selection content-to-insert])}
  (fn [& args] (type (last args))))

(defn- insert-text-container
  [{:keys [doc selection] :as editor-state}, content]
  {:pre [(satisfies? TextContainer content)]}
  (if (sel/range? selection)
    (-> (delete editor-state) (insert content))
    (let [new-doc (doc/insert doc selection content)
          new-sel (->> (sel/shift-single selection (len content))
                       (nav/autoset-formats new-doc))]
      (assoc editor-state
             :doc new-doc
             :selection new-sel))))

(defmethod insert
  Run
  [editor-state run]
  (insert-text-container editor-state run))

(defmethod insert
  Paragraph
  [editor-state paragraph]
  (insert-text-container editor-state paragraph))

(defmethod insert
  ParagraphFragment
  [{:keys [doc selection] :as editor-state} paragraph-fragment]
  (if (sel/range? selection)
    (-> (delete editor-state) (insert paragraph-fragment))
    (let [new-doc (doc/insert doc selection paragraph-fragment)
          new-sel (->> (sel/shift-single selection (len paragraph-fragment))
                       (nav/autoset-formats new-doc))]
      (assoc editor-state
             :doc new-doc
             :selection new-sel))))

(defmethod insert
  DocumentFragment
  [{:keys [doc selection] :as editor-state}, fragment]
  (let [paragraphs (m/items fragment)]
    (if (= (count paragraphs) 1)
      ;; For a number of reasons, it's easier to handle inserting a single paragraph as a distinct case.
      (insert editor-state (first paragraphs))
      ;; Insert multiple paragraphs
      (if (sel/range? selection)
        (-> (delete editor-state) (insert fragment))
        (let [new-doc (doc/insert doc selection (doc/fragment paragraphs))
              sel-para-idx (sel/start-para selection)
              para-after-sel-idx (dll/next-index (:children doc) sel-para-idx)
              last-inserted-para-idx (if (nil? para-after-sel-idx)
                                       (dll/last-index (:children new-doc))
                                       (dll/prev-index (:children new-doc) para-after-sel-idx))
              last-paragraph (last paragraphs)
              new-selection (->> (sel/selection [last-inserted-para-idx (len last-paragraph)])
                                 (nav/autoset-formats last-paragraph))]
          (assoc editor-state
                 :doc new-doc
                 :selection new-selection))))))

(defmethod insert
  js/String
  [{:keys [selection] :as editor-state} text]
  (let [paragraphs (str/split-lines text)]
    (if (< 1 (count paragraphs))
      (insert editor-state (doc/fragment (map #(p/paragraph [(r/run %)]) paragraphs)))
      ;; Insert string == insert run with current active formats
      (insert editor-state (r/run text (:formats selection))))))

(defn enter
  "Equivalent to what happens when the user hits the enter button.
   Creates a new paragraph in the appropriate position in the doc."
  ([{:keys [doc selection] :as editor-state}]
   (if (sel/range? selection)
     (-> (delete editor-state) (enter))
     (let [caret (sel/caret selection)
           paragraph-idx (-> selection :start :paragraph)
           paragraph ((:children doc) paragraph-idx)
           new-para-type (if (contains? doc/types-preserved-on-enter (:type paragraph))
                           (:type paragraph)
                           :body)
           ;; If a paragraph is empty, the caret is currently at both the start
           ;; AND end of that paragraph. However, hitting enter within an empty
           ;; paragraph gets treated the same as at the END, i.e. insert a paragraph
           ;; below and move the cursor down, rather than insert a paragraph above
           ;; and keep the cursor where it is.
           ;;
           ;; Having this var here helps not get confused  aboutthat and keeps
           ;; things easy the expressions further down.
           pos-in-paragraph (cond
                              (= caret (len paragraph)) :end
                              (= caret 0) :start
                              :else :middle)
           new-doc (cond
                     (= :end pos-in-paragraph)
                     (doc/insert-paragraph-after doc paragraph-idx new-para-type)

                     (= :start pos-in-paragraph)
                     (doc/insert-paragraph-before doc paragraph-idx new-para-type)

                     (= :middle pos-in-paragraph)
                     (let [[para1 para2] (doc/split-paragraph doc selection)]
                       (doc/replace-paragraph-with doc paragraph-idx [para1 para2])))
           new-index (if (= :start pos-in-paragraph)
                       (dll/prev-index (:children new-doc) paragraph-idx)
                       (dll/next-index (:children new-doc) paragraph-idx))
           new-paragraph (get (:children new-doc) new-index)
           new-selection (if (= :start pos-in-paragraph)
                           (sel/selection [(sel/caret-para selection) 0])
                           (sel/selection [new-index 0] [new-index 0]))
           ;; If inserting a new (empty) paragraph, then the selection should inherit the :formats
           ;; of the current selection. If splitting an existing paragraph into two using enter, the
           ;; now-2nd paragraph's should just set its :formats based on whatever formats are active
           ;; at its start.
           formats (if (blank? new-paragraph)
                     (:formats selection)
                     (m/formatting new-paragraph new-selection))
           new-selection (assoc new-selection :formats formats)]
       (assoc editor-state
              :doc new-doc
              :selection new-selection)))))

(defn current-paragraph
  "Returns current paragraph if selection is a single selection."
  [{:keys [selection doc]}]
  {:pre [(sel/single? selection)]}
  (get (:children doc) (sel/caret-para selection)))

(defn replace-paragraph
  "Returns a new EditorState replacing the paragraph at index `index` with `new-paragraph`.
   If the selection is inside the paragraph replaced, and its offset is invalidated (i.e.
   the new paragraph is shorter than the one previously there, and the selection's start or
   end offset is greater than the new paragraph's length), it will be reset to the end of the
   paragraph."
  [{:keys [doc selection] :as _editor-state} index new-paragraph]
  (let [new-doc (assoc-in doc [:children index] new-paragraph)
        adjust-side (fn [{:keys [paragraph offset] :as side}]
                      (if (and (= paragraph index)
                               (> offset (m/len new-paragraph)))
                        {:paragraph paragraph, :offset (m/len new-paragraph)}
                        side))
        new-selection (-> selection
                          (update :start adjust-side)
                          (update :end adjust-side))]
    (editor-state new-doc new-selection)))

(defn update-paragraph
  "Passes the paragraph with at index `paragraph-idx` to function f and replaces the paragraph with the value returned."
  [{:keys [doc] :as editor-state} paragraph-idx f & args]
  (let [paragraph (get (:children doc) paragraph-idx)
        new-paragraph (apply f paragraph args)]
    (replace-paragraph editor-state paragraph-idx new-paragraph)))

;; TODO: Auto-set :formats on selection
(defn set-selection
  "Returns a new EditorState with the selection set to `new-selection`."
  [editor-state new-selection]
  (assoc editor-state :selection new-selection))

(defn select-all
  [{:keys [selection doc] :as editor-state}]
  (let [start-side (:start (nav/start doc))
        end-side (:end (nav/end doc))
        new-selection (assoc selection
                             :start start-side
                             :end end-side)]
    (set-selection editor-state new-selection)))

(defn select-whole-word
  [{:keys [doc selection] :as editor-state}]
  {:pre [(sel/single? selection)]}
  (let [para (current-paragraph editor-state)
        para-idx (sel/caret-para selection)
        raw-selection (cond
                        (nav/inside-word? para selection)
                        (sel/from-singles (nav/prev-word doc selection) (nav/next-word doc selection))

                        (nav/at-word-start? para selection)
                        (sel/from-singles selection (nav/next-word doc selection))

                        (nav/at-word-end? para selection)
                        (sel/from-singles (nav/prev-word doc selection) selection)

                        :else
                        (let [char (m/char-at para selection)
                              [backward-fn, forward-fn] (cond
                                                          (nav/word? char) [nav/back-until-non-word nav/until-non-word]
                                                          (nav/separator? char) [nav/back-until-non-separator nav/until-non-separator]
                                                          (nav/whitespace? char) [nav/back-until-non-whitespace nav/until-non-whitespace]
                                                          :else [(fn [_ i] i), (fn [_ i] i)])
                              para-text (m/text para)]
                          (sel/selection [para-idx (backward-fn para-text (sel/caret selection))]
                                         [para-idx (forward-fn para-text (sel/caret selection))])))
        new-selection (nav/autoset-formats para raw-selection)]
    (set-selection editor-state new-selection)))

(defn select-whole-paragraph
  [{:keys [selection] :as editor-state}]
  {:pre [(sel/single? selection)]}
  (let [para (current-paragraph editor-state)
        para-idx (sel/caret-para selection)
        raw-selection (sel/selection [para-idx 0] [para-idx (m/len para)])
        new-selection (nav/autoset-formats para raw-selection)]
    (set-selection editor-state new-selection)))

;; TODO: Write unit test
(defn toggle-paragraph-type
  [{:keys [selection doc] :as editor-state} type]
  (let [start-paragraph-idx (sel/start-para selection)
        end-paragraph-idx (sel/end-para selection)
        selected-paragraphs (dll/range (:children doc) start-paragraph-idx end-paragraph-idx)
        set-paragraph-type-true? (not-every? #(= (:type %) type) selected-paragraphs)
        type-to-set (if set-paragraph-type-true? type :body)
        selected-paragraphs-updated (map #(assoc % :type type-to-set) selected-paragraphs)
        new-doc (update doc :children dll/replace-range start-paragraph-idx end-paragraph-idx selected-paragraphs-updated)]
    (assoc editor-state :doc new-doc)))

(defn toggle-format
  [{:keys [doc selection] :as editor-state} format]
  (if (sel/single? selection)
    (update editor-state :selection sel/toggle-format format)
    (let [common-formats (m/formatting doc selection)
          format-modify-fn (if (contains? common-formats format) disj conj)
          new-selection (update selection :formats format-modify-fn format)]
      (assoc editor-state
             :doc (doc/toggle-format doc selection format)
             :selection new-selection))))

(defn auto-surround
  ([{:keys [doc selection] :as editor-state} opening closing]
   (if (sel/single? selection)
     (assoc editor-state
            :doc (doc/insert doc selection (str opening closing))
            :selection (sel/shift-single selection (len opening)))
     (let [opening-insert-point (sel/collapse-start selection)
           closing-insert-point (sel/collapse-end selection)
           same-para? (= (sel/caret-para opening-insert-point) (sel/caret-para closing-insert-point))
           closing-insert-point (if same-para?
                                  (sel/shift-single closing-insert-point (len opening))
                                  closing-insert-point)
           new-doc (-> doc
                       (doc/insert opening-insert-point opening)
                       (doc/insert closing-insert-point closing))
           new-selection (update-in selection [:start :offset] + (len opening))
           new-selection (if same-para?
                           (sel/shift-end new-selection (len opening))
                           new-selection)]
       (assoc editor-state :doc new-doc :selection new-selection))))
  ([editor-state surround] (auto-surround editor-state surround surround)))

(defn after-anchor?
  "Returns true if the right-expanded caret new-caret comes after old-caret
   in the document. This assumes you can only jump ahead one paragraph at a time."
  [new-caret old-selection]
  {:pre [(sel/single? new-caret), (:backwards? old-selection)]}
  (let [new-caret-side (:start new-caret)
        old-sel-anchor (:end old-selection)]
    (cond
      (= (:paragraph old-sel-anchor) (:paragraph new-caret-side))
      (>= (:offset new-caret-side) (:offset old-sel-anchor))

      (= (:paragraph new-caret-side) (:paragraph old-sel-anchor))
      true

      :else
      false)))

(defn before-anchor?
  "Returns true if the left-expanded caret new-caret comes before old-caret's
   anchor in the document. This assume you can only jump back one paragraph
   at a time. "
  [new-caret old-selection]
  {:pre [(sel/single? new-caret), (not (:backwards? old-selection))]}
  (let [new-caret-side (:start new-caret)
        old-sel-anchor (:start old-selection)]
    (cond
      (= (:paragraph old-sel-anchor) (:paragraph new-caret-side))
      (< (:offset new-caret-side) (:offset old-sel-anchor))

      (= (:paragraph new-caret-side) (:paragraph old-sel-anchor))
      true

      :else
      false)))

(defn expand-caret-right
  [{:keys [selection] :as editor-state} new-caret]
  {:pre [(sel/single? new-caret)]}
  (let [new-caret-side (:start new-caret) ; single sel so :start or :end the same
        new-selection (if (:backwards? selection)
                        ;; backwards selection
                        (if (after-anchor? new-caret selection)
                          (-> selection
                              (assoc :start (:end selection))
                              (assoc :end new-caret-side)
                              (assoc :backwards? false))
                          (assoc selection :start new-caret-side))
                        ;; forwards selection
                        (assoc selection :end new-caret-side))
        new-editor-state (assoc editor-state :selection new-selection)]
    new-editor-state))

(defn expand-caret-left
  [{:keys [selection] :as editor-state} new-caret]
  {:pre [(sel/single? new-caret)]}
  (let [new-caret-side (:start new-caret) ; single sel so :start or :end the same
        new-selection (if (not (:backwards? selection))
                        ;; forwards selection
                        (if (before-anchor? new-caret selection)
                          (-> selection
                              (assoc :end (:start selection))
                              (assoc :start new-caret-side)
                              (assoc :backwards? true))
                          (assoc selection :end new-caret-side))
                        ;; backwards selection
                        (assoc selection :start new-caret-side))
        new-editor-state (assoc editor-state :selection new-selection)]
    new-editor-state))

(defn- nav-fallthrough
  "Little helper method for generating a new EditorState with a selection made
   by calling nav-method with the `editor-state`'s `doc` and `selection`."
  [{:keys [doc selection] :as editor-state}, nav-method]
  (let [new-selection (nav-method doc selection)]
    (assoc editor-state :selection new-selection)))

(extend-type EditorState
  Navigable
  (start [{:keys [doc] :as editor-state}]
    (let [new-selection (nav/start doc)]
      (assoc editor-state :selection new-selection)))

  (end [{:keys [doc] :as editor-state}]
    (let [new-selection (nav/end doc)]
      (assoc editor-state :selection new-selection)))

  (next-char [editor-state]
    (nav-fallthrough editor-state nav/next-char))

  (prev-char [editor-state]
    (nav-fallthrough editor-state nav/prev-char))

  (next-word [editor-state]
    (nav-fallthrough editor-state nav/next-word))

  (prev-word [editor-state]
    (nav-fallthrough editor-state nav/prev-word))

  (next-clause [editor-state]
    (nav-fallthrough editor-state nav/next-clause))

  (prev-clause [editor-state]
    (nav-fallthrough editor-state nav/prev-clause))

  (next-sentence [editor-state]
    (nav-fallthrough editor-state nav/next-sentence))

  (prev-sentence [editor-state]
    (nav-fallthrough editor-state nav/prev-sentence))

  (prev-paragraph [{:keys [doc selection] :as editor-state}]
    (let [caret-para-idx (sel/caret-para selection)
          prev-para-idx (dll/prev-index (:children doc) (sel/caret-para selection))
          prev-paragraph (get (:children doc) prev-para-idx)]
      (if (or (pos? (sel/caret selection))
              (nil? prev-paragraph))
        (assoc editor-state :selection (sel/selection [caret-para-idx 0]))
        (assoc editor-state :selection (sel/selection [prev-para-idx (m/len prev-paragraph)])))))

  (next-paragraph [{:keys [doc selection] :as editor-state}]
    (let [caret-para-idx (sel/caret-para selection)
          paragraph (get (:children doc) caret-para-idx)
          next-paragraph-idx (dll/next-index (:children doc) (sel/caret-para selection))]
      (if (and (= (m/len paragraph) (sel/caret selection))
               next-paragraph-idx)
        (assoc editor-state :selection (sel/selection [next-paragraph-idx 0]))
        (assoc editor-state :selection (sel/selection [caret-para-idx (m/len paragraph)])))))

  nav/Selectable
  (shift+right [editor-state] (nav-fallthrough editor-state nav/shift+right))
  (shift+left [editor-state] (nav-fallthrough editor-state nav/shift+left))
  (ctrl+shift+right [editor-state] (nav-fallthrough editor-state nav/ctrl+shift+right))
  (ctrl+shift+left [editor-state] (nav-fallthrough editor-state nav/ctrl+shift+left))

  m/Selectable
  (char-at [{:keys [doc selection]}]
    (m/char-at doc selection))

  (char-before [{:keys [doc selection]}]
    (m/char-before doc selection))

  (selected-content [{:keys [doc selection]}]
    (m/selected-content doc selection))

  (formatting [{:keys [doc selection]}]
    (m/formatting doc selection)))

(defn all-selected-indices
  "Returns the indices of all paragraphs that are wholly
   or partially inside the current selection, as a set."
  [editor-state]
  {:post [(set? %)]}
  (set (dll/indices-range (-> editor-state :doc :children)
                          (-> editor-state :selection :start :paragraph)
                          (-> editor-state :selection :end :paragraph))))

(defn get-changelist
  [editor-state]
  (dll/changelist (-> editor-state :doc :children)))

(defn clear-changelist
  "Returns an identical EditorState with the changelist of its doc's `children` DLL cleared."
  [editor-state]
  (update-in editor-state [:doc :children] dll/clear-changelist))

