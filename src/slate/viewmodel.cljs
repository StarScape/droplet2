(ns slate.viewmodel
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
            [slate.dll :as dll :refer [dll]]
            [slate.core :as c]
            [slate.measurement :refer [fake-measure-fn ruler]]))

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
(defn empty-line [] (->Line [(empty-span)] 0 0 0))

(defn get-words
  "Splits string `s` into a vector of words."
  [s]
  (str/split s #"( )+"))

(defn add-span
  "Adds the span to the given line and updates the line's width."
  [line span]
  (let [prev-span (peek (:spans line))
        offset (if prev-span
                 (+ (count (:text prev-span)) (:start-offset prev-span))
                 (:start-offset line))
        span' (assoc span :start-offset offset)
        ;; If there is an empty span hanging on the line, trim it off
        line-cleaned (cond-> line
                       (and prev-span (empty? (:text prev-span)))
                       (update :spans pop))]
    (-> line-cleaned
        (update :spans conj span')
        (update :width + (:width span'))
        (update :end-offset + (count (:text span'))))))

(defn add-line
  "Adds a new empty line to the vector of lines, returning the new list."
  [lines]
  (let [prev-line (or (peek lines) (empty-line))]
    (conj lines (->Line [] (:end-offset prev-line) (:end-offset prev-line) 0))))

(defn max-chars-from-word
  "Takes the maximum number of chars from string `word` without exceeding
   `width`, and returns a tuple of [chars that fit, chars that did not].
   This is useful when splitting up a word that is too big to fit on one line."
  [word formats width measure-fn]
  (loop [chars-left word, chars-fit ""]
    (let [new-chars-fit (str chars-fit (first chars-left))
          new-width (measure-fn new-chars-fit formats)]
      (if (or (> new-width width)
              (empty? chars-left))
        [chars-fit, (apply str chars-left)]
        (recur (next chars-left) new-chars-fit)))))

(defn max-words
  "Takes the maximum number of words from string `src` without exceeding `width-left`,
   as measured by function `measure-fn`. Returns two strings: all the text added,
   and all the text that would not fit (which is an empty string if everything fit)."
  [src formats width-left line-width measure-fn]
  (loop [words-fit "", words (get-words src)]
    (let [next-word (first words)
          new-text (str words-fit next-word)
          new-width (measure-fn (.trimEnd new-text) formats)]
      (cond
        (and (seq words) (<= (int new-width) width-left))
        (recur new-text (rest words))

        ;; If there is a word that is greater than the total allowed
        ;; line width, fit what we can in this span and move on.
        (and (seq words) (> (measure-fn next-word formats) line-width))
        (let [left-on-line (- width-left (measure-fn words-fit formats))
              [word-fit, not-fit] (max-chars-from-word next-word formats left-on-line measure-fn)]
          [word-fit, (apply str not-fit (next words))])

        :else
        [words-fit, (apply str words)]))))

(comment
  (max-words "the second line now. " #{} 300 300 fake-measure-fn)
  (max-words "foobar bizz buzz hello hello a goodbye" #{} 300 300 fake-measure-fn))

(defn max-span-from-run
  "Constructs a span of as many of the words from `run` as it can without exceeding
   the width of `width-left`, then returns the span and a run with the text that would
   not fit (if any)."
  [run width-left line-width measure-fn]
  (let [span
        (->Span "" (:formats run) 0 0)

        [span-text, remaining-text]
        (max-words (:text run) (:formats run) width-left line-width measure-fn)]
    [(assoc span :text span-text
                 :width (measure-fn span-text (:formats run)))
     (c/run remaining-text (:formats run))]))

(comment
  (max-span-from-run (c/run "foobar bizz buzz hello hello goodbye") 300 300 fake-measure-fn)
  (max-span-from-run (c/run "foobar bizz buzz hello hello a goodbye") 300 300 fake-measure-fn) ; be sure and test this
  (max-span-from-run (c/run "foobar bizz buzz hello hello a goodbye") 10 300 fake-measure-fn) ; be sure and test this
  (max-span-from-run (c/run "the second line now. ") 300 300 fake-measure-fn))

(defn add-max-to-last-line
  "Adds the maximum amount of chars from `run` onto the last line in `line`, adding
   an extra line to the end of lines if necessary. Returns updated list of lines, and
   a run with text that did not fit on line (can be empty), as a pair."
  [lines run width measure-fn]
  (let [width-left (- width (:width (peek lines)))
        [new-span, remaining] (max-span-from-run run width-left width measure-fn)
        new-lines (cond-> lines
                    ;; If any words were able to fit, put them on the line
                    (seq (:text new-span))
                    (update (dec (count lines)) add-span new-span)

                    ;; If there is any remaining text that would not fit, add a new line
                    (seq (:text remaining))
                    (add-line))]
    [new-lines, remaining]))

(comment
  (add-max-to-last-line [(empty-line)] (c/run "foobar bizz buzz hello hello goodbye") 300 fake-measure-fn)
  (add-max-to-last-line [(empty-line)] (c/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now. ") 300 fake-measure-fn)
  (add-max-to-last-line [(empty-line)] (c/run "the second line now. ") 300 fake-measure-fn))

(defn lineify
  "Convert vector of runs to a vector of lines, with no line exceeding `width`.
   Consuming code shouldn't use this directly, see instead `from-para`."
  ([runs width measure-fn]
   (lineify [(empty-line)] runs width measure-fn))
  ([lines [run & runs] width measure-fn]
   (if-not run
     lines
     (let [[new-lines, leftover] (add-max-to-last-line lines run width measure-fn)
           remaining-runs (cond->> runs
                            ;; if there is leftover text that didn't fit on the
                            ;; line, add it back to the list of runs for the next line
                            (seq (:text leftover)) (cons leftover))]
       (recur new-lines remaining-runs width measure-fn)))))

(comment
  (lineify [(c/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now.")] 300 fake-measure-fn)
  (lineify [(c/run "abc" #{:italic}) (c/run "foobar")] 300 fake-measure-fn)
  ;; TODO: add these as test cases
  (lineify [(c/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now. aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")]
           300 fake-measure-fn)
  (lineify [(c/run "foobar bizz buzz hello hello goodbye. And this should be on the second line now. aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")]
           300 fake-measure-fn)
  (lineify [(c/run "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")] 300 fake-measure-fn)
  (lineify [(c/run "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")] 300 (ruler "15px" "serif")))

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

#_(defn from-doc
  "Takes a [[Document]], converts each of its [[Paragraph]]s to [[ParagraphViewModel]]s,
   and returns a a [[DLL]] of ParagraphViewModels."
  [doc width measure-fn]
  (dll)
  #_(from-para p width measure-fn))