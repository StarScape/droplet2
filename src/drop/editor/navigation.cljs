(ns drop.editor.navigation
  (:require [clojure.string :as str]
            [drop.editor.core :as core :refer [caret]]))

;; Some helpers and useful primitives ;;

(def separators
  "Non-whitespace word separators"
  #{"." "/" "\\" "(" ")" "\"" "'"
    "-" ":" "," ";" "<" ">" "~" "!"
    "@" "#" "$" "%" "^" "&" "*" "|"
    "+" "=" "[" "]" "{" "}" "`" "?"})

(defn separator? "Is argument a separator char?" [char] (separators char))

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
       (not (separator? char))))

(defn until
  "Advance in string `text` beginning at index `start` until a character
   is found for which predicate `pred` returns true, and returns that index."
  [text start pred]
  (loop [i start]
    (if (or (>= i (count text))
            (pred (nth text i)))
      i
      (recur (inc i)))))

(defn until-non-separator [text start] (until text start #(not (separator? %))))

(defn until-non-word [text start] (until text start #(not (word? %))))

;; Main functionality ;;

(defprotocol Navigable
  "Methods for navigating around. Implemented for Paragraphs and Documents. All methods return a new selection."
  ;; (forward [this sel] "Move forward by 1 character.")
  ;; (backward [this sel] "Move backward by 1 character.")
  ;; (start [this sel] "Go to start of paragraph or document.")
  ;; (end [this sel] "Go to end of paragraph or document.")

  (next-word
    [this sel]
    "Returns selection after jumping to the end of the next word from selection `sel`.
    Equivalent to the standard behavior of ctrl+right (Windows/Linux) or alt+right (Mac).")

  (prev-word
    [this sel]
    "Returns selection after jumping to the start of the previous word from selection `sel`.
    Equivalent to the standard behavior of ctrl+right (Windows/Linux) or alt+right (Mac)."))

(defn- next-word-offset
  "Helper function for `next-word`, but taking a plain string and offset instead of a paragraph and selection."
  [text start-offset]
  (let [first-char (nth text start-offset)]
    (cond
      (whitespace? first-char)
      (let [idx (until text start-offset #(not (whitespace? %)))
            char (nth text idx)]
        (if (separator? char)
          (until-non-separator text idx)
          (until-non-word text idx)))

      (separator? first-char)
      (let [next-char (.charAt text (inc start-offset))]
        (if (word? next-char)
          (until-non-word text (inc start-offset))
          (until-non-separator text start-offset)))

      ;; Word character
      :else
      (until-non-word text start-offset))))

;; TODO
(defn- prev-word-offset [text start-offset]
  0)

(extend-type core/Paragraph
  Navigable
  (next-word [para sel]
    (let [text (apply str (map :text (:runs para)))
          offset (next-word-offset text (caret sel))]
      (core/selection [para offset])))

  (prev-word [para sel]
    (let [text (apply str (map :text (:runs para)))
          offset (prev-word-offset text (caret sel))]
      (core/selection [para offset]))))

(comment
  (def my-par (core/paragraph [(core/run "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")]))
  (next-word my-par 77))
