(ns drop.editor.navigation
  (:require [clojure.string :as str]
            [drop.editor.core :as core :refer [caret]]))

;; TODO: a good approach might be to make a protocol `Navigable` and implement it for Paragraphs and Documents

;; Non-whitespace word separators
(def separators #{"." "/" "\\"
                  "(" ")" "\"" "'"
                  "-" ":" "," ";"
                  "<" ">" "~" "!"
                  "@" "#" "$" "%"
                  "^" "&" "*" "|"
                  "+" "=" "[" "]"
                  "{" "}" "`" "?"})

(defn whitespace?
  "Is argument a whitespace char?"
  [char]
  (if (and (str/blank? char) (not= char "") (not= char nil))
    true
    false))

(defn separator? "Is argument a separator char?" [char] (separators char))

(defn word?
  "Is argument a word char?"
  [char]
  (and (not (whitespace? char))
       (not (separator? char))))

(defn until
  "Advance in string `text` beginning at index `start` until a character is found matching predicate `pred`."
  [text start pred]
  (loop [i start]
    (if (or (>= i (count text))
            (pred (nth text i)))
      i
      (recur (inc i)))))

(defn until-non-separator [text start]
  (until text start #(not (separator? %))))

(defn until-non-word [text start]
  (until text start #(not (word? %))))

; Might be better named `jump-right`, since it doesn't *really* correspond to going to the next word in all circumstances
(defn next-word
  "Returns off of the end of the next word in paragraph after offset `start`.
   Equivalent to the standard behavior of ctrl+right (Windows/Linux) or alt+right (Mac)."
  [para start]
  (let [text (apply str (map :text (:runs para)))
        first-char (nth text start)]
    (cond
      (whitespace? first-char)
      (let [idx (until text start #(not (whitespace? %)))
            char (nth text idx)]
        (if (separator? char)
          (until-non-separator text idx)
          (until-non-word text idx)))

      (separator? first-char)
      (let [next-char (.charAt text (inc start))]
        (if (word? next-char)
          (until-non-word text (inc start))
          (until-non-separator text start)))

      ;; Word character
      :else
      (until-non-word text start))))

; (count "Hello. world.")
(def my-par (core/paragraph [(core/run "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")]))

(next-word my-par 77)
