(ns drop.editor.navigation
  (:require [clojure.string :as str]
            [drop.editor.core :as core]
            [drop.editor.selection :as sel :refer [selection caret smart-collapse]]))

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

(defprotocol Navigable
  "Methods for navigating around. Implemented for Paragraphs and Documents. All methods return a new Selection."
  ;; (start [this sel] "Go to start of paragraph or document.")
  ;; (end [this sel] "Go to end of paragraph or document.")
  ;; (next-char [this sel] "Move forward by 1 character.")
  ;; (prev-char [this sel] "Move backward by 1 character.")

  (next-word
    [this sel]
    "Returns selection after jumping to the end of the next word from selection `sel`.
    Equivalent to the standard behavior of ctrl+right (Windows/Linux) or option+right (Mac).")

  (prev-word
    [this sel]
    "Returns selection after jumping to the start of the previous word from selection `sel`.
    Equivalent to the standard behavior of ctrl+right (Windows/Linux) or option+right (Mac)."))

;; TODO: next-word and prev-word should collapse first thing

(extend-type core/Paragraph
  Navigable
  (next-word [para sel]
    (let [text (apply str (map :text (:runs para)))
          offset (next-word-offset text (caret (smart-collapse sel)))]
      (sel/set-single sel offset)))

  (prev-word [para sel]
    (let [text (apply str (map :text (:runs para)))
          offset (prev-word-offset text (caret (smart-collapse sel)))]
      (sel/set-single sel offset))))

(extend-type core/Document
  Navigable
  (next-word [doc sel]
    (let [collapsed (smart-collapse sel)
          para-idx (sel/start-para collapsed)
          para ((:children doc) para-idx)]
      (if (and (= (sel/caret collapsed) (core/text-len para))
               (not= para-idx (-> doc :children count dec)))
        (selection [(inc para-idx) 0])
        (next-word ((:children doc) (sel/start-para sel)) sel)))))


(comment
  (def my-par (core/paragraph [(core/run "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")]))
  (prev-word-offset (apply str (map :text (:runs my-par))) 0)
  (next-word-offset (apply str (map :text (:runs my-par))) 80)

  (def my-doc (core/document [my-par, (core/paragraph [(core/run "foo bar?")])]))
  (caret (next-word my-doc (selection [1 0])))
  )
