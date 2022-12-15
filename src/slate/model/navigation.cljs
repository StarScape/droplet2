(ns slate.model.navigation
  "Functions for modifying navigating around the document by modifying selections.
   Note that everything in here is pure -- functions take Documents/Paragraphs and a
   selection, and return a new selection.

   Also, nothing here should be dependent on the viewmodel or the DOM. Things that have to
   be aware of either of those should go in the `view` namespace."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [slate.model.common :as m]
            [slate.model.paragraph :as p :refer [Paragraph]]
            [slate.model.doc :as doc :refer [Document]]
            [slate.dll :as dll]
            [slate.model.selection :as sel :refer [selection caret smart-collapse single? range?]]))

;; Some helpers and useful primitives ;;

(def sentence-separators
  #{"." "!" "?"})

(def clause-separators
  (set/union sentence-separators
             #{"," ";" ":" "\u2014"}))

(def word-separators
  (set/union
   clause-separators
   #{"/" "\\" "(" ")" "\""
     "'" "-" "<" ">" "~"
     "@" "#" "$" "%" "^"
     "&" "*" "|" "+" "="
     "[" "]" "{" "}" "`"}))

(defn char-or-blank? [x] (or (= x "") (char? x)))

(defn separator?
  "Is argument a separator char?"
  [char]
  {:pre [(char-or-blank? char)]}
  (contains? word-separators char))

(defn whitespace?
  "Is argument a whitespace char?"
  [char]
  {:pre [(char-or-blank? char)]}
  (if (and (str/blank? char) (not= char "") (not= char nil))
    true
    false))

(defn content?
  "Is argument a content char?"
  [char]
  (and (not (whitespace? char))
       (not= "" char)
       (not= nil char)))

(defn word?
  "Is argument a word char?"
  [char]
  {:pre [(char-or-blank? char)]}
  (and (not (whitespace? char))
       (not (separator? char))
       (not= nil char)
       (not= "" char)))

(defn inside-word?
  "Are the characters on either side of the char at index `idx` in the string `text` word-chars?"
  [text idx]
  (and (word? (.charAt text (dec idx)))
       (word? (.charAt text (inc idx)))))

(defn until
  "Advance in string `text` beginning at index `start` until a character
   is found for which predicate `pred` returns true, and return that index."
  [text start pred]
  (loop [i start]
    (if (or (>= i (count text))
            (pred (nth text i)))
      i
      (recur (inc i)))))

(defn back-until
  "Go back in string `text` beginning at index `start` until the character **before** the matches predicate `pred`.
   The standard behavior of prev-word is more dependent on what lies immediately *before* the caret than underneath
   it, which is why this function works a little different from `until`.

   Note the index returned is not the one the predicate matches, but the one after."
  [text start pred]
  (loop [i start]
    (if (or (<= i 0)
            (pred (nth text (dec i))))
      i
      (recur (dec i)))))

(defn until-non-whitespace [text start] (until text start #(not (whitespace? %))))
(defn back-until-non-whitespace [text start] (back-until text start #(not (whitespace? %))))

(defn until-non-separator [text start] (until text start #(not (separator? %))))
(defn back-until-non-separator [text start] (back-until text start #(not (separator? %))))

(defn until-non-word [text start] (until text start #(not (word? %))))
(defn back-until-non-word [text start] (back-until text start #(not (word? %))))

;; Main functionality ;;

(defn next-word-offset
  "Helper function for `next-word`, but taking a plain string and offset instead of a paragraph and selection.
   Returns the new offset, NOT a selection."
  [text start-offset]
  {:pre [(and (nat-int? start-offset)
              (<= start-offset (.-length text)))]
   :post [(and (nat-int? %)
               (<= % (.-length text)))]}
  (if (>= start-offset (count text))
    (count text)
    (let [first-char (nth text start-offset)]
      (cond
        (whitespace? first-char)
        (let [idx (until-non-whitespace text start-offset)
              char (nth text idx "")]
          (if (separator? char)
            (until-non-separator text idx)
            (until-non-word text idx)))

        (separator? first-char)
        (if (inside-word? text start-offset)
          (until-non-word text (inc start-offset))
          (until-non-separator text start-offset))

        ;; Word character
        :else
        (until-non-word text start-offset)))))

(defn prev-word-offset
  "Helper function for `prev-word`, but taking a plain string and offset instead of a paragraph and selection.
   Returns the new offset, NOT a selection."
  [text start-offset]
  {:pre [(and (nat-int? start-offset)
              (<= start-offset (.-length text)))]
   :post [(and (nat-int? %)
               (<= % (.-length text)))]}
  (if (<= start-offset 0)
    0
    (let [before-start (nth text (dec start-offset))]
      (cond
        (whitespace? before-start)
        (let [idx (back-until-non-whitespace text start-offset)
              char-before-idx (nth text (dec idx) "")]
          (if (separator? char-before-idx)
            (back-until-non-separator text idx)
            (back-until-non-word text idx)))

        (separator? before-start)
        ;; Using inside-word? as our metric might not be the best way, time will tell
        (if (inside-word? text (dec start-offset))
          (back-until-non-word text (dec start-offset))
          (back-until-non-separator text start-offset))

        ;; Word character
        :else
        (back-until-non-word text start-offset)))))

(defn next-clause-offset
  "Helper function for `next-clause` but taking a plain string and offset instead of a paragraph and selection.
   Returns the new offset, NOT a selection."
  [text start-offset]
  ;; (let [start-offset (if )])
  (as-> start-offset offset
    (until text offset clause-separators)
    (until text offset (complement clause-separators))))

(defn prev-clause-offset
  "Helper function for `prev-clause` but taking a plain string and offset instead of a paragraph and selection.
   Returns the new offset, NOT a selection."
  [text start-offset]
  (let [prev-clause-end (as-> start-offset offset
                          (back-until text offset (complement clause-separators))
                          (back-until text offset clause-separators))
        current-clause-start (until-non-whitespace text prev-clause-end)]
    ;; If there is another clause before the current one that was just navigated to the start of,
    ;; there will be leading whitespace. Place the cursor in front of that leading whitespace if applicable.
    (if (or (zero? prev-clause-end)
            (= current-clause-start start-offset))
      prev-clause-end
      current-clause-start)))

(defn next-sentence-offset
  "Helper function for `next-sentence` but taking a plain string and offset instead of a paragraph and selection.
   Returns the new offset, NOT a selection."
  [text start-offset]
  (as-> start-offset offset
    (until text offset sentence-separators)
    (until text offset (complement sentence-separators))))

(defn prev-sentence-offset
  "Helper function for `prev-clause` but taking a plain string and offset instead of a paragraph and selection.
   Returns the new offset, NOT a selection."
  [text start-offset]
  (let [prev-sentence-end (as-> start-offset offset
                          (back-until text offset (complement sentence-separators))
                          (back-until text offset sentence-separators))
        current-sentence-start (until-non-whitespace text prev-sentence-end)]
    ;; If there is another sentence before the current one that was just navigated to the start of,
    ;; there will be leading whitespace. Place the cursor in front of that leading whitespace if applicable.
    (if (or (zero? prev-sentence-end)
            (= current-sentence-start start-offset))
      prev-sentence-end
      current-sentence-start)))

(defprotocol Navigable
  "Methods for navigating around. Implemented for Paragraphs, Documents, and EditorStates.
  All methods return a new Selection, except for on EditorStates, which returns EditorUpdates."
  (start
    [this]
    "Go to start of Paragraph or Document.")

  (end
    [this]
    "Go to end of Paragraph or Document.")

  (next-char
    [this sel]
    [editor-state]
    "Move forward by 1 character, or returns the same selection if not possible.
    Equivalent to pressing the right arrow on the keyboard.")

  (prev-char
    [this sel]
    [editor-state]
    "Move backward by 1 character, or return the same selection if not possible.
    Equivalent to pressing the left arrow on the keyboard.")

  (next-word
    [this sel]
    [editor-state]
    "Returns selection after jumping to the end of the next word from selection `sel`.
    Equivalent to the standard behavior of ctrl+right (Windows/Linux) or option+right (Mac).")

  (prev-word
    [this sel]
    [editor-state]
    "Returns selection after jumping to the start of the previous word from selection `sel`.
    Equivalent to the standard behavior of ctrl+right (Windows/Linux) or option+right (Mac).")

  (next-clause
    [this sel]
    [editor-state]
    "Returns selection (or EditorUpdate) after jumping ahead to the end of the next clause (defined as
    the end of the current sentence or the next intra-sentence punctuation mark such as commas, colons,
    and semicolons).")

  (prev-clause
    [this sel]
    [editor-state]
    "Returns selection (or EditorUpdate) after jumping back to the start of the current clause (defined as
    the start of the current sentence or the next intra-sentence punctuation mark such as commas, colons,
    and semicolons).")

  (next-sentence
    [this sel]
    [editor-state]
    "Returns selection (or EditorUpdate) after jumping ahead to the end of the next clause (defined as
    the end of the current sentence or the next intra-sentence punctuation mark such as commas, colons,
    and semicolons).")

  (prev-sentence
    [this sel]
    [editor-state]
    "Returns selection (or EditorUpdate) after jumping back to the start of the current clause (defined as
    the start of the current sentence or the next intra-sentence punctuation mark such as commas, colons,
    and semicolons).")

  ;; These should really not be in this protocol when they are only implemented for EditorState, but this will do for now.
  (next-paragraph [editor-state])
  (prev-paragraph [editor-state]))

(defprotocol Selectable
  "Methods for expanding and contracting selections."
  (shift+right
    [this sel]
    [editor-state]
    "If selection is not backwards, returns a new selection with the right side expanded by 1.
    If the selection *is* backwards, will *contract* the *left* side of the selection. In other
    words, equivalent to the behavior of pressing shift+right.")

  (shift+left
    [this sel]
    [editor-state]
    "If selection is not backwards, returns a new selection with the right side contracted by 1.
    If the selection *is* backwards, will *expand* the *left* side of the selection. In other
    words, equivalent to the behavior of pressing shift+left.")

  (ctrl+shift+right
    [this sel]
    [editor-state]
    "Expands or contracts the caret side of the selection by a word, depending if the selection is
    forwards or backwards, respectively. Equivalent to the behavior of pressing ctrl+shift+right.")

  (ctrl+shift+left
    [this sel]
    [editor-state]
    "Expands or contracts the caret side of the selection by a word, depending if the selection is
    backwards or forwards respectively. Equivalent to the behavior of pressing ctrl+shift+left."))

(defn autoset-formats
  [para-or-doc selection]
  (assoc selection :formats #p (m/formatting #p para-or-doc selection)))

(defn- next-grapheme
  [paragraph offset]
  {:pre [(single? selection)]}
  (let [graphemes (m/graphemes paragraph)
        next-grapheme-segment (first (filter #(> (:offset %) offset) graphemes))]
    (or (:offset next-grapheme-segment) (m/len paragraph))))

(defn- prev-grapheme
  [paragraph offset]
  {:pre [(single? selection)]}
  (let [graphemes (m/graphemes paragraph)
        ;; TODO: this can almost certainly be made for efficient
        prev-grapheme-segment (first (reverse (filter #(< (:offset %) offset) graphemes)))]
    (or (:offset prev-grapheme-segment) 0)))

(extend-type Paragraph
  Navigable
  (start [para]
    (autoset-formats para (selection [(:uuid para) 0])))

  (end [para]
    (autoset-formats para (selection [(:uuid para) (m/len para)])))

  (next-char [para sel]
    (cond
      (range? sel)
      (autoset-formats para (sel/collapse-end sel))

      (and (single? sel) (< (caret sel) (m/len para)))
      (let [next-grapheme-offset (next-grapheme para (sel/caret sel))]
        (autoset-formats para (sel/set-single sel next-grapheme-offset)))

      :else
      sel))

  (prev-char [para sel]
    (cond
      (range? sel)
      (autoset-formats para (sel/collapse-start sel))

      (and (single? sel) (pos? (caret sel)))
      (let [prev-grapheme-offset (prev-grapheme para (sel/caret sel))]
        (autoset-formats para (sel/set-single sel prev-grapheme-offset)))

      :else
      sel))

  (next-word [para sel]
    (let [text (m/text para)
          collapsed (smart-collapse sel)
          offset (next-word-offset text (caret collapsed))]
      (autoset-formats para (sel/set-single collapsed offset))))

  (prev-word [para sel]
    (let [text (m/text para)
          collapsed (smart-collapse sel)
          offset (prev-word-offset text (caret collapsed))]
      (autoset-formats para (sel/set-single collapsed offset))))

  (next-clause [para sel]
    (let [text (m/text para)
          collapsed (smart-collapse sel)
          offset (next-clause-offset text (caret collapsed))]
      (autoset-formats para (sel/set-single collapsed offset))))

  (prev-clause [para sel]
    (let [text (m/text para)
          collapsed (smart-collapse sel)
          offset (prev-clause-offset text (caret collapsed))]
      (autoset-formats para (sel/set-single collapsed offset))))

  (next-sentence [para sel]
    (let [text (m/text para)
          collapsed (smart-collapse sel)
          offset (next-sentence-offset text (caret collapsed))]
      (autoset-formats para (sel/set-single collapsed offset))))

  (prev-sentence [para sel]
    (let [text (m/text para)
          collapsed (smart-collapse sel)
          offset (prev-sentence-offset text (caret collapsed))]
      (autoset-formats para (sel/set-single collapsed offset)))))

(defn next-method [doc sel paragraph-fn]
  (let [collapsed (smart-collapse sel)
        para-uuid (sel/start-para collapsed)
        para ((:children doc) para-uuid)]
    (if (and (= (sel/caret collapsed) (m/len para))
             (not= para (dll/last (:children doc))))
      (start (dll/next (:children doc) para))
      (paragraph-fn ((:children doc) (sel/start-para collapsed)) collapsed))))

(defn prev-method [doc sel paragraph-fn]
  (let [collapsed (smart-collapse sel)
        para-uuid (sel/start-para collapsed)
        para ((:children doc) para-uuid)]
    (if (and (= 0 (sel/caret collapsed))
             (not= para (dll/first (:children doc))))
      (end (dll/prev (:children doc) para))
      (paragraph-fn ((:children doc) (sel/start-para collapsed)) collapsed))))

(extend-type Document
  Navigable
  (start [doc] (start (dll/first (:children doc))))

  (end [doc] (end (dll/last (:children doc))))

  (next-char [doc sel]
    (if (range? sel)
      (autoset-formats doc (sel/collapse-end sel))
      (let [para ((:children doc) (-> sel :start :paragraph))]
        ;; At end of paragraph?
        (if (= (caret sel) (m/len para))
          ;; At end of _last_ paragraph?
          (if (doc/last-para? doc para)
            sel
            ;; At end of different paragraph, goto start of next one
            (start (dll/next (:children doc) para)))
          ;; Not at end of paragraph, defer to Paragraph's next-char impl
          (next-char para sel)))))

  (prev-char [doc sel]
    (if (range? sel)
      (autoset-formats doc (sel/collapse-start sel))
      (let [para ((:children doc) (-> sel :start :paragraph))]
        ;; At start of paragraph?
        (if (zero? (caret sel))
          ;; At start of _first_ paragraph?
          (if (doc/first-para? doc para)
            sel
            ;; At start of different paragraph, goto end of previous one
            (end (dll/prev (:children doc) para)))
          ;; Not at start of paragraph, defer to Paragraph's prev-char impl
          (prev-char para sel)))))

  (next-word [doc sel]
    (next-method doc sel next-word))

  (prev-word [doc sel]
    (prev-method doc sel prev-word))

  (next-clause [doc sel]
    (next-method doc sel next-clause))

  (prev-clause [doc sel]
    (prev-method doc sel prev-clause))

  (next-sentence [doc sel]
    (next-method doc sel next-sentence))

  (prev-sentence [doc sel]
    (prev-method doc sel prev-sentence))

  ;; TODO: It's possible these can be cleaned up, but *write tests* before
  ;; trying to make them more elegant. That will make it a lot easier to prove
  ;; easily that I'm not breaking them every time I switch an if around or something.
  Selectable
  (shift+right [doc sel]
    (autoset-formats
     doc
     (let [caret-offset (sel/caret sel)
           para ((:children doc) (sel/caret-para sel))
           para-length (m/len para)
           next-para (dll/next (:children doc) para)]
       (if (and (:backwards? sel) (sel/range? sel))
         (cond
           (= caret-offset para-length)
           (-> sel
               (assoc :start {:offset 0, :paragraph (:uuid next-para)})
               ;; We just moved the caret from p1 to p2, removing p1 from the selection
               ;; and making p2 the new start. Remove p2 from :between if it's present.
               (sel/remove-ends-from-between))

           :else
           (sel/shift-caret sel (- (next-grapheme para caret-offset) caret-offset)))
         (cond
           (and (doc/last-para? doc para)
                (= caret-offset para-length))
           sel

           (= caret-offset para-length)
           (-> sel
               (assoc :end {:offset 0, :paragraph (:uuid next-para)})
              ;; We just shift+right'd from p1 to p2, so add p1 to :between IF it's not also :start
               (sel/add-to-between (:uuid para)))

           :else
           (sel/shift-caret sel (- (next-grapheme para caret-offset) caret-offset)))))))

  (shift+left [doc sel]
    (autoset-formats
     doc
     (let [caret-offset (sel/caret sel)
           para ((:children doc) (sel/caret-para sel))
           prev-para (dll/prev (:children doc) para)
           prev-grapheme-offset (prev-grapheme para caret-offset)]
       (if (sel/single? sel)
         (cond
           (and (doc/first-para? doc para)
                (zero? caret-offset))
           sel

           (zero? caret-offset)
           (-> sel
               (assoc :start {:paragraph (:uuid prev-para)
                              :offset (m/len prev-para)}
                      :backwards? true))
              ;; No need to update :between because this is operating on a single selection

           :else
           (-> sel
               (assoc-in [:start :offset] prev-grapheme-offset)
               (assoc :backwards? true)))
         (cond
           (and (doc/first-para? doc para)
                (zero? caret-offset))
           sel

           (zero? caret-offset)
           (if (:backwards? sel)
             (-> sel
                 (assoc :start {:offset (m/len prev-para)
                                :paragraph (:uuid prev-para)})
                 ;; Add what was previously the :start paragraph to the :between,
                 ;; as long as it is not also the :end paragraph (i.e. if the selection
                 ;; only spanned a single paragraph).
                 (sel/add-to-between (:uuid para)))
             (-> sel
                 (assoc :end {:offset (m/len prev-para)
                              :paragraph (:uuid prev-para)})
                ;; Previous paragraph was either in :between or the :start para. It's
                ;; now the new :end paragraph, so make sure it's removed from :between.
                 (sel/remove-ends-from-between)))

           :else
           (sel/shift-caret sel (- prev-grapheme-offset caret-offset)))))))

  (ctrl+shift+right [doc sel]
    (autoset-formats
     doc
     (let [next-word-sel (next-word doc sel)
           new-caret {:paragraph (sel/caret-para next-word-sel)
                      :offset (sel/caret next-word-sel)}]
       (if (and (:backwards? sel) (sel/range? sel))
         ;; Backwards range selection
         (-> (if (and (>= (:offset new-caret) (-> sel :end :offset))
                      (= (:paragraph new-caret) (-> sel :end :paragraph)))
               (assoc sel :start (:end sel), :end new-caret, :backwards? false)
               (assoc sel :start new-caret))
             (sel/remove-ends-from-between))
         ;; Forwards range or single selection
         (-> sel
             (assoc :end new-caret, :backwards? false)
             (sel/add-to-between (sel/end-para sel)))))))

  (ctrl+shift+left [doc sel]
    (autoset-formats
     doc
     (let [prev-word-sel (prev-word doc sel)
           new-caret {:paragraph (sel/caret-para prev-word-sel)
                      :offset (sel/caret prev-word-sel)}]
       (if (or (sel/single? sel) (and (:backwards? sel) (sel/range? sel)))
         ;; Single selection or backwards range-selection
         (-> sel
             (assoc :start new-caret, :backwards? true)
             (sel/add-to-between (sel/start-para sel)))
         ;; Forwards range selection
         (-> (if (and (< (:offset new-caret) (-> sel :start :offset))
                      (= (:paragraph new-caret) (-> sel :start :paragraph)))
               (assoc sel :start new-caret, :end (:start sel), :backwards? true)
               (assoc sel :end new-caret, :backwards? false))
             (sel/remove-ends-from-between)))))))

(comment
  (def my-par (p/paragraph [(r/run "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")]))
  (prev-word-offset (apply str (map :text (:runs my-par))) 0)
  (next-word-offset (apply str (map :text (:runs my-par))) 80)

  (def my-doc (doc/document [my-par, (p/paragraph [(r/run "foo bar?")])]))
  (caret (next-word my-doc (selection [1 0])))
  (caret (prev-word my-doc (selection [0 7]))))
