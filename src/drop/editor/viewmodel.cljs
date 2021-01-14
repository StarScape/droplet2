(ns drop.editor.viewmodel
  "The view model is an intermediate representation between the core editor data types and HTML.
   An intermediate stage is necessary because we handle the splitting of paragraphs into lines ourselves,
   rather than having the browser do it for us -- this in turn is needed because we draw our own text caret
   and selections, and in order to have fine-grained control of layout-sensitive operations such as navigating
   the caret to the end of the line (⌘ + → on macOS, End on Windows/Linux), or when and how to collapse selections.
   The browser simply doesn't provide very good tools for making a customizable text-editing surface, so I've
   built my own out of divs and spans.

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
            [drop.editor.measurement :refer [fake-measure-fn]]
            [drop.editor.core :as c]))

;; It is worth noting that this is some of my least favorite code in the whole project.
;; So if any unsuspecting soul happens to look at this someday, don't judge me too hard --
;; I don't even like having this layer of indirection *here*, and I would gladly get rid
;; of it, only there's not another solution that doesn't wind up being even more nasty.
;; The good news is the nastiness is **largely** contained here.

;; (defrecord DocumentViewModel [paragraphs container-width])
(defrecord ParagraphViewModel [lines paragraph container-width])
(defrecord Line [spans start-offset end-offset width])
(defrecord Span [text formats start-offset width])

(defn empty-span [] (->Span "" #{} 0 0))
(defn empty-line [] (->Line [] 0 0 0))

(defn get-words
  "Splits string `s` into a vector of words."
  [s]
  (str/split s #"(\s+)"))

(defn add-span
  "Adds the span to the given line and updates the line's width."
  [line span]
  (let [prev-span (peek (:spans line))
        offset (if prev-span
                 (+ (count (:text prev-span)) (:start-offset prev-span))
                 (:start-offset line))
        span' (assoc span :start-offset offset)]
    (-> line
        (update :spans conj span')
        (update :width + (:width span'))
        (update :end-offset + (count (:text span'))))))

(defn line-after
  "Returns a new empty line with offsets set to immediately after `prev-line`."
  [prev-line]
  (->Line [] (:end-offset prev-line) (:end-offset prev-line) 0))

(defn max-words
  "Takes the maximum number of words from string `src` without exceeding `space-left`,
   as measured by function `measure-fn`. Returns two strings: all the text added,
   and all the text that would not fit (which is an empty string if everything fit)."
  [src formats space-left measure-fn]
  (loop [out "", words (get-words src)]
    (let [next-word (first words)
          new-text (str out next-word)
          new-width (measure-fn (.trim new-text) formats)]
      (if (and (not-empty words)
               (<= (int new-width) space-left))
        (recur new-text (rest words))
        [out (apply str words)]))))

(comment
  (max-words "the second line now. " #{} 300 fake-measure-fn)
  (max-words "foobar bizz buzz hello hello a goodbye" #{} 300 fake-measure-fn))

;; TODO: rename to `max-span-from-run` or something similar.
(defn add-max-to-span
  "Adds as many words to `span` as it can without exceeding the width of `space-left`,
   then returns the span and a run with the text that would not fit (if any)."
  [run space-left measure-fn]
  (let [span (->Span "" (:formats run) 0 0)
        [span-text, remaining-text] (max-words (:text run) (:formats run) space-left measure-fn)]
    [(assoc span :text span-text :width (measure-fn span-text (:formats run)))
     (c/run remaining-text (:formats run))]))

(comment
  (add-max-to-span (c/run "foobar bizz buzz hello hello goodbye") 300 fake-measure-fn)
  (add-max-to-span (c/run "foobar bizz buzz hello hello a goodbye") 300 fake-measure-fn) ; be sure and test this
  (add-max-to-span (c/run "foobar bizz buzz hello hello a goodbye") 10 fake-measure-fn) ; be sure and test this
  (add-max-to-span (c/run "the second line now. ") 300 fake-measure-fn))

(defn add-max-to-lines
  "Adds the maximum amount of chars from `run` onto the last line in `line`, adding
   an extra line to the end of lines if necessary. Returns updated list of lines, and
   a run with text that did not fit on line (can be empty), as a pair."
  [lines run width measure-fn]
  (let [last-line (peek lines)
        space-left (- width (:width last-line))
        [new-span, remaining] (add-max-to-span run space-left measure-fn)]
    [(as-> lines lines'
       (if (not-empty (:text new-span))
         (update lines' (dec (count lines')) add-span new-span)
         lines')
       (if (not-empty (:text remaining))
         (conj lines' (line-after (peek lines')))
         lines'))
     remaining]))

(comment
  (add-max-to-lines [(empty-line)] (c/run "foobar bizz buzz hello hello goodbye") 300 fake-measure-fn)
  (add-max-to-lines [(empty-line)] (c/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now. ") 300 fake-measure-fn)
  (add-max-to-lines [(empty-line)] (c/run "the second line now. ") 300 fake-measure-fn))

(defn lineify
  "Convert vector of runs to a vector of lines, with no line exceeding `width`.
   Consuming APIs shouldn't use this directly, see instead `from-para`."
  ([runs width measure-fn]
   (lineify [(empty-line)] runs width measure-fn))
  ([lines runs width measure-fn]
   (if (empty? runs)
     lines
     (let [[lines' leftover] (add-max-to-lines lines (first runs) width measure-fn)
           remaining-runs (cond->> (rest runs)
                            (not-empty (:text leftover)) (cons leftover))]
       (recur lines' remaining-runs width measure-fn)))))

(comment
  (lineify [(c/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now.")] 300 fake-measure-fn)
  (lineify [(c/run "abc" #{:italic}) (c/run "foobar")] 300 fake-measure-fn))

(defn from-para
  "Converts a [[Paragraph]] to a ParagraphViewModel."
  [para width measure-fn]
  (->ParagraphViewModel (lineify (:runs para) width measure-fn) para width))

;; TODO: testme
(defn from-doc
  "Takes a [[Document]], converts each of its [[Paragraph]]s to [[ParagraphViewModel]]s,
   and returns a map of UUIDs -> ParagraphViewModels."
  [doc width measure-fn]
  (loop [paragraphs (:children doc), uuids->vms {}]
    (if (empty? paragraphs)
      uuids->vms
      (let [p (first paragraphs)
            vm (from-para p width measure-fn)]
        (recur (rest paragraphs) (assoc uuids->vms (:uuid p) vm))))))
