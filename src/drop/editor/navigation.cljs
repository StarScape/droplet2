(ns drop.editor.navigation
  (:require [clojure.string :as str]
            [drop.editor.core :as core]
            [drop.editor.dll :as dll]
            [drop.editor.selection :as sel :refer [selection caret smart-collapse single? range?]]))

;; Some helpers and useful primitives ;;

(def separators
  "Non-whitespace word separators"
  #{"." "/" "\\" "(" ")" "\"" "'"
    "-" ":" "," ";" "<" ">" "~" "!"
    "@" "#" "$" "%" "^" "&" "*" "|"
    "+" "=" "[" "]" "{" "}" "`" "?"})

(defn separator? "Is argument a separator char?" [char] (contains? separators char))

(defn whitespace?
  "Is argument a whitespace char?"
  [char]
  (if (and (str/blank? char) (not= char "") (not= char nil))
    true
    false))

(defn word?
  "Is argument a word char?"
  [char]
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
   is found for which predicate `pred` returns true, and returns that index."
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
  {:pre [(<= start-offset (.-length text))]
   :post [(<= % (.-length text))]}
  (if (>= start-offset (count text))
    (count text)
    (let [first-char (nth text start-offset)]
      (cond
        (whitespace? first-char)
        (let [idx (until-non-whitespace text start-offset)
              char (nth text idx)]
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

(defn- prev-word-offset
  "Helper function for `prev-word`, but taking a plain string and offset instead of a paragraph and selection.
   Returns the new offset, NOT a selection."
  [text start-offset]
  {:pre [(<= start-offset (.-length text))]
   :post [(<= % (.-length text))]}
  (if (<= start-offset 0)
    0
    (let [before-start (nth text (dec start-offset))]
      (cond
        (whitespace? before-start)
        (let [idx (back-until-non-whitespace text start-offset)
              char-before-idx (nth text (dec idx))]
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

;; TODO: should there be selection functions? Can be copied from JS implementation.
;; UPDATE: I think this is a good idea and don't see why not.
(defprotocol Navigable
  "Methods for navigating around. Implemented for Paragraphs and Documents. All methods return a new Selection."
  (start
   [this]
   "Go to start of Paragraph or Document.")

  (end
   [this]
   "Go to end of Paragraph or Document.")

  (next-char
   [this sel]
   "Move forward by 1 character, or returns the same selection if not possible.
    Equivalent to pressing the right arrow on the keyboard.")

  (prev-char
   [this sel]
   "Move backward by 1 character, or return the same selection if not possible.
    Equivalent to pressing the left arrow on the keyboard.")

  (next-word
    [this sel]
    "Returns selection after jumping to the end of the next word from selection `sel`.
    Equivalent to the standard behavior of ctrl+right (Windows/Linux) or option+right (Mac).")

  (prev-word
    [this sel]
    "Returns selection after jumping to the start of the previous word from selection `sel`.
    Equivalent to the standard behavior of ctrl+right (Windows/Linux) or option+right (Mac)."))

(extend-type core/Paragraph
  Navigable
  (start [para]
    (selection [(:uuid para) 0]))

  (end [para]
    (selection [(:uuid para) (core/text-len para)]))

  (next-char [para sel]
    (cond
      (range? sel) (sel/collapse-end sel)
      (and (single? sel) (< (caret sel) (core/text-len para))) (sel/shift-single sel 1)
      :else sel))

  (prev-char [para sel]
    (cond
      (range? sel) (sel/collapse-start sel)
      (and (single? sel) (pos? (caret sel))) (sel/shift-single sel -1)
      :else sel))

  (next-word [para sel]
    (let [text (apply str (map :text (:runs para)))
          collapsed (smart-collapse sel)
          offset (next-word-offset text (caret collapsed))]
      (sel/set-single collapsed offset)))

  (prev-word [para sel]
    (let [text (apply str (map :text (:runs para)))
          collapsed (smart-collapse sel)
          offset (prev-word-offset text (caret collapsed))]
      (sel/set-single collapsed offset))))

(extend-type core/Document
  Navigable
  (start [doc]
    (selection [(:uuid (dll/first (:children doc))) 0]))

  (end [doc]
    (let [last-para (dll/last (:children doc))]
      (selection [(:uuid last-para) (core/text-len last-para)])))

  (next-char [doc sel]
    (if (range? sel)
      (sel/collapse-end sel)
      (let [para ((:children doc) (-> sel :start :paragraph))]
        (if (= (caret sel) (core/text-len para))
          (if (core/last-para? doc para)
            sel
            (start (dll/next (:children doc) para)))
          (next-char para sel)))))

  (prev-char [doc sel]
    (if (range? sel)
      (sel/collapse-start sel)
      (let [para ((:children doc) (-> sel :start :paragraph))]
        (if (zero? (caret sel))
          (if (core/first-para? doc para)
            sel
            (end (dll/prev (:children doc) para)))
          (prev-char para sel)))))

  (next-word [doc sel]
    (let [collapsed (smart-collapse sel)
          para-uuid (sel/start-para collapsed)
          para ((:children doc) para-uuid)]
      (if (and (= (sel/caret collapsed) (core/text-len para))
               (not= para (dll/last (:children doc))))
        ;; TODO: can change to a call to (start) once that is implemented
        (selection [(:uuid (dll/next (:children doc) para)), 0])
        (next-word ((:children doc) (sel/start-para collapsed)) collapsed))))

  (prev-word [doc sel]
    (let [collapsed (smart-collapse sel)
          para-uuid (sel/start-para collapsed)
          para ((:children doc) para-uuid)]
      (if (and (= 0 (sel/caret collapsed))
               (not= para (dll/first (:children doc))))
        ;; TODO: can change to a call to (end) once that is implemented
        (let [prev-para (dll/prev (:children doc) para)]
          (selection [(:uuid prev-para), (core/text-len prev-para)]))
        (prev-word ((:children doc) (sel/start-para collapsed)) collapsed)))))


(comment
  (def my-par (core/paragraph [(core/run "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")]))
  (prev-word-offset (apply str (map :text (:runs my-par))) 0)
  (next-word-offset (apply str (map :text (:runs my-par))) 80)

  (def my-doc (core/document [my-par, (core/paragraph [(core/run "foo bar?")])]))
  (caret (next-word my-doc (selection [1 0])))
  (caret (prev-word my-doc (selection [0 7])))
  )
