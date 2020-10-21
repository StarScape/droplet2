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
            ["./CharRuler" :refer (CharRuler)]
            [drop.editor.core :as core]
            [drop.editor.vec-utils :refer [replace-last]]))

(defrecord ViewModel [paragraphs])
(defrecord Paragraph [paragraph container-width lines])
(defrecord Line [spans width start-offset])
(defrecord Span [text start-offset width])

;; (defn from-doc
;;   "Convert [[Document]] to ViewModel."
;;   [doc container-width]
;;   (->ViewModel (mapv #(split-into-lines % container-width) (:children doc))))

(defn line []
  (->Line [] 0 0))

(defn span []
  (->Span "" 0 0))

(defn add-span [line span]
  (-> line
      (update :spans conj span)
      (update :width + (:width span))))

(defn words [s]
  (str/split s #"(\s+)"))

(defn add-max-to-span
  [span run space-left ruler]
  (let [words (words run)]
    (reduce
     (fn [sp word]
       (let [new-text (str (:text span) word)
             ; We DON'T want to count whitespace at the end of a line, hence the trim.
             new-width (.measure ruler (.trim new-text) (:formats span))]
         (if (<= (.floor js/Math new-width) space-left)
           (-> sp (update :text str word) (assoc :width (.measure ruler new-text (:formats span))))
           sp)))
     span words)))

(defn add-max-to-lines
  "Adds the maximum amount of chars from `run` onto the last line in `line`, adding
   an extra line to the end of lines if necessary. Returns updated list of lines, and
   a run with chars that did not fit on line (can be empty), as a pair."
  [lines run width ruler]
  (let [space-left (- width (:width (peek lines)))
        [new-span, remaining] (add-max-to-span (span) run space-left ruler)]
    [(if (= remaining run)
       (conj lines (line)) ; nothing added, add newline
      ;  (replace-last lines (add-span last-line new-span))
       (update lines (dec (count lines)) add-span new-span))
     remaining]))

(defn lineify
  ([runs width ruler]
   (lineify [(line)] runs width ruler))
  ([lines runs width ruler]
   (if (empty? runs)
     lines
     (let [[lines' remaining]
           (add-max-to-lines lines (first runs) width ruler)]
       (recur lines' (cons remaining runs) width ruler)))))

(def p
  (core/paragraph [(core/run "foofoofoofoo" #{:italic})
                   (core/run "barbarbarbar" #{})]))
(def ruler (CharRuler.))

(lineify p 100 ruler)
