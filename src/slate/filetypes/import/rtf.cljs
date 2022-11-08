(ns slate.filetypes.import.rtf
  (:require [clojure.string :as str]))

;;  {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
;;  This is some {\b bold} text.\par
;;  }

(def basic "{\\rtf1\\ansi{\\fonttbl\\f0\\fswiss Helvetica;}\\f0\\pard\n\rThis is some {\\b bold} text.\\par\n\r}")

(defn re-matches? [re str]
  (.test re str))

(defn append-or-create-text
  [group char]
  (if (string? (peek group))
    (update group (dec (count group)) #(str % char))
    (conj group char)))

(defn parse-ascii-escape [rtf-str i]
  (let [hex-digits (.substring rtf-str i (+ 2 i))
        charcode (js/parseInt hex-digits 16)]
    [(+ 2 i), (js/String.fromCharCode charcode)]))

(defn parse-command-parametrized
  [rtf-str start-i])

(defn parse-command [rtf-str start-i]
  (loop [i start-i, command-name nil, num-arg-start-i nil]
    (let [char (nth rtf-str i)]
      (cond
        (re-matches? #"[a-z]" char)
        (recur (inc i) nil nil)

        (re-matches? #"-|[0-9]" char)
        (recur (inc i) (.substring start-i i) (or num-arg-start-i i))

        :else
        (if command-name
          {:command command-name
           :num (.substring num-arg-start-i i)}
          {:command command-name
           :num (.substring num-arg-start-i i)})))))

(re-matches? #"\-|[0-9]" "-")

(defn parse-backslash-entity
  [rtf-str i]
  (let [char-after-backslash (nth rtf-str i)]
    (cond
      (re-matches? #"[a-z]" char-after-backslash) (parse-command rtf-str i)
      (= char-after-backslash "'") (parse-ascii-escape rtf-str (inc i))
      :else [(inc i), {:escape char-after-backslash}])))

(defn parse-group
  [rtf-str start-i]
  (loop [i start-i
         group []]
    (let [char (nth rtf-str i)]
      (cond
        (= char "{") (let [[new-i, parsed-group] (parse-group rtf-str (inc i))]
                       (recur new-i (conj group parsed-group)))
        (= char "}") [(inc i), group]
        (= char "\\") (let [[new-i, parsed-entity] (parse-backslash-entity rtf-str (inc i))]
                        (recur new-i (conj group parsed-entity)))
        :else (recur (inc i) (append-or-create-text group char))))))

(comment (parse-ascii-escape "\\'eaAnd" 2))
