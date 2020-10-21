(ns drop.editor.viewmodel
  "The view model is an intermediate representation between the core editor data types -> HTML.
   An intermediate stage is necessary because we handle the splitting of paragraphs into lines ourselves,
   rather than having the browser do it for us -- this in turn is needed because we draw our own text caret
   and selections, and in order to have fine-grained control of layout-sensitive operations such as navigating
   the caret to the end of the line (⌘ + → on macOS, End on Windows/Linux), or when and how to collapse selections.

   The DOM APIs for handling selection are not terribly user-friendly, or conducive to the
   decoupling of your underlying data respresentation from how they are rendered. For example, the `Selection`
   API relies on an anchor node and an offset into it, but if anchorNode is an element and not a text node, the
   offset will be the *number of elements* into the anchor node, not the *text* offset, which is obviously what
   we're interested in here, since we're dealing with manipulations on text.

   For this and other reasons, manually calculating lines into a 'ViewModel' -- and then keeping track of how the text
   offsets of these correspond to those inside of the model -- winds up being a better solution.

   Oh, and don't get too hung up on the term 'viewmodel' -- I choose that simply because it's an intermediate
   representation between the model and the view, not because it's directly inspired by any of the MVC/psuedo-MVC
   frameworks that also happen to use that term."
  (:require [clojure.string :as str]
            ["./CharRuler" :refer (CharRuler fakeRuler)]
            [drop.editor.core :as c]))

(defrecord ViewModel [paragraphs])
(defrecord Paragraph [paragraph container-width lines])
(defrecord Line [spans width start-offset])
(defrecord Span [text start-offset width])

;; TODO: is it worth replacing the reliance on CharRuler with a simple function `(measure)` that wraps it?

;; (defn from-doc
;;   "Convert [[Document]] to ViewModel."
;;   [doc container-width]
;;   (->ViewModel (mapv #(split-into-lines % container-width) (:children doc))))

(defn line [] (->Line [] 0 0))
(defn span [] (->Span "" 0 0))

(defn get-words
  "Splits string `s` into a vector of words."
  [s]
  (str/split s #"(\s+)"))

(defn add-span
  "Adds the span to the given line and updates the line's width."
  [line span]
  (-> line
      (update :spans conj span)
      (update :width + (:width span))))

(defn max-words
  "Takes the maximum number of words from `src-str` without exceeding `space-left`,
   as measured by function `measure-fn`. Returns two strings: all the text added, and
   all the text that would not fit (an empty string if everything fit)."
  [src space-left measure-fn]
  (loop [out "", words (get-words src), i 0]
    (let [next-word (first words)
          new-text (str out next-word)
          new-width (measure-fn (.trim new-text))]
      (if (and (< i 20) (<= (int new-width) space-left))
        (recur new-text (rest words) (inc i))
        [out (apply str words)]))))

(comment
  (max-words "the second line now. " 300 #(.measureString fakeRuler % #{})))

(defn add-max-to-span
  "Adds as many words to `span` as it can without exceeding the width of `space-left`,
   then returns the span and a run with the text that would not fit (if any)."
  [span run space-left ruler]
  (let [measure #(.measureString ruler % (:formats run))
        [span-text, remaining-text] (max-words (:text run) space-left measure)]
    [(assoc span :text span-text :width (measure span-text))
     (c/run remaining-text (:formats run))]))

(comment
  (add-max-to-span (span) (c/run "foobar bizz buzz hello hello goodbye") 300 fakeRuler)
  (add-max-to-span (span) (c/run "foobar bizz buzz hello hello a goodbye") 10 fakeRuler) ; be sure and test this
  (add-max-to-span (span) (c/run "the second line now. ") 300 fakeRuler))

(defn add-max-to-lines
  "Adds the maximum amount of chars from `run` onto the last line in `line`, adding
   an extra line to the end of lines if necessary. Returns updated list of lines, and
   a run with text that did not fit on line (can be empty), as a pair."
  [lines run width ruler]
  (let [space-left (- width (:width (peek lines)))
        [new-span, remaining] (add-max-to-span (span) run space-left ruler)]
    ;; #p (update lines (dec (count lines)) add-span new-span)
    [(cond-> lines
       (not-empty (:text new-span)) (update (dec (count lines)) add-span new-span)
       (not-empty (:text remaining)) (conj (line)))
     remaining]))

(comment
  (add-max-to-lines [(line)] (c/run "foobar bizz buzz hello hello goodbye") 300 fakeRuler)
  (add-max-to-lines [(line)] (c/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now. ") 300 fakeRuler)
  (add-max-to-lines [(line)] (c/run "the second line now. ") 300 fakeRuler)) ;; bug

(defn lineify
  ([runs width ruler]
   (lineify [(line)] runs width ruler 0))
  ([lines runs width ruler c]
   (if (every? #(empty? (:text %)) runs)
     lines
     (let [[lines' remaining] (add-max-to-lines lines (first runs) width ruler)]
       (recur lines' (cons #p remaining (rest runs)) width ruler (inc c))))))

(comment
  (lineify [(c/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now.")] 300 fakeRuler))

(def p
  (c/paragraph [(c/run "foofoofoofoo" #{:italic})
                   (c/run "barbarbarbar" #{})]))
(def ruler (CharRuler.))


