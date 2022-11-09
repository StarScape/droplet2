(ns slate.filetypes.import.rtf
  (:require [clojure.string :as str]))

(def basic "{\\rtf1\\ansi{\\fonttbl\\f0\\fswiss Helvetica;}\\f0\\pard\n\rThis is some {\\b bold} text.\\par\n\r}")

(defn re-matches?
  [re str]
  {:pre [(or (string? str) (nil? str))]}
  (if str (.test re str) false))

(defn append-or-create-text
  [group char]
  (let [char (when-not (re-matches? #"\n|\r" char) char)]
    (if (string? (peek group))
      (update group (dec (count group)) #(str % char))
      (if (some? char) (conj group char) group))))

(defn parse-ascii-escape [rtf-str i]
  (let [hex-digits (.substring rtf-str i (+ 2 i))
        charcode (js/parseInt hex-digits 16)]
    [(+ 2 i), (js/String.fromCharCode charcode)]))

(defn parse-command [rtf-str start-i]
  (loop [i start-i, command-name nil, num-arg-start-i nil]
    (let [char (get rtf-str i)]
      (cond
        (re-matches? #"[a-z]" char)
        (recur (inc i) nil nil)

        (re-matches? #"-|[0-9]" char)
        (recur (inc i) (.substring rtf-str start-i i) (or num-arg-start-i i))

        :else
        (let [cmd (if command-name
                    {:command command-name
                     :num (js/parseInt (.substring rtf-str num-arg-start-i i))}
                    (keyword (.substring rtf-str start-i i)))
              i (if (re-matches? #"\s" char) (inc i) i)]
          [i, cmd])))))

(comment
  (+ 1 1)
  (parse-command "\\para1234" 1)
  )

(defn parse-backslash-entity
  [rtf-str i]
  (let [char-after-backslash (nth rtf-str i)]
    (cond
      (re-matches? #"[a-z]" char-after-backslash) (parse-command rtf-str i)
      (= char-after-backslash "'") (parse-ascii-escape rtf-str (inc i))
      :else [(inc i), {:escape char-after-backslash}])))

(defn parse-group
  ([rtf-str start-i]
   (loop [i start-i
          group []]
     (let [char (get rtf-str i)]
       (cond
         (= i (count rtf-str)) (first group)
         (= char "{") (let [[new-i, parsed-group] (parse-group rtf-str (inc i))]
                        (recur new-i (conj group parsed-group)))
         (= char "}") [(inc i), group]
         (= char "\\") (let [[new-i, parsed-entity] (parse-backslash-entity rtf-str (inc i))]
                         (recur new-i (conj group parsed-entity)))
         :else (recur (inc i) (append-or-create-text group char))))))
  ([rtf-str] (parse-group rtf-str 0)))

;;  {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
;;  This is some {\b bold} text.\par
;;  }

(comment
  (parse-ascii-escape "\\'eaAnd" 2)
  (count basic)
  (parse-group basic 0)

  )
