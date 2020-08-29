(ns drop.editor.navigation
  (:require [drop.editor.core :as core :refer [caret]]))

;; Non-whitespace word separators
(def separators #{"." "/" "\\"
                  "(" ")" "\"" "'"
                  "-" ":" "," ";"
                  "<" ">" "~" "!"
                  "@" "#" "$" "%"
                  "^" "&" "*" "|"
                  "+" "=" "[" "]"
                  "{" "}" "`" "?"})

;; TODO: use regex or something else
(defn whitespace? [char] (= char " "))

(defn separator? [char] (separators char))

(defn word? [char]
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
      (until-non-separator text start)

      :else
      (until-non-word text start))))

; (count "Hello. world.")
(def my-par (core/paragraph [(core/run "Hello world. Hello    world, my name is Jack...and this is my counterpart, R2-D2")]))

(next-word my-par 77)
