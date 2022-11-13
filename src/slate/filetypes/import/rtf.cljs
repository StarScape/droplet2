(ns slate.filetypes.import.rtf
  "Code for parsing and importing RTF documents.
   Internally, this import process is separated into two logical parts:
   First the raw RTF is parsed into a data structure easily readable by
   Clojure (a sort of IR), and another in which this data structure is
   converted into a Slate Document."
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [clojure.string :as str]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]))

;; Text to IR Parsing ;;

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
        (re-matches? #"[a-zA-Z]" char)
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

(defn parse-backslash-entity
  [rtf-str i]
  (let [char-after-backslash (nth rtf-str i)]
    (cond
      (re-matches? #"[a-z]" char-after-backslash) (parse-command rtf-str i)
      (= char-after-backslash "'") (parse-ascii-escape rtf-str (inc i))
      :else [(inc i), {:escape char-after-backslash}])))

(defn parse-group
  ([rtf-str start-i]
   (letfn [(conj-to-group [group entity]
            ((if (string? entity) append-or-create-text conj) group entity))]
     (loop [i start-i
            group []]
       (let [char (nth rtf-str i)]
         (cond
           (= char "{") (let [[new-i, parsed-group] (parse-group rtf-str (inc i))]
                          (recur new-i (conj-to-group group parsed-group)))
           (= char "}") [(inc i), group]
           (= char "\\") (let [[new-i, parsed-entity] (parse-backslash-entity rtf-str (inc i))]
                           (recur new-i (conj-to-group group parsed-entity)))
         ;;(= i (count rtf-str)) (throw (js/Error. "Parsing of RTF document failed."))
           :else (recur (inc i) (append-or-create-text group char)))))))
  ([rtf-str] (parse-group rtf-str 0)))

(defn parse-rtf-doc-str
  [rtf-str]
  (assert (.startsWith rtf-str "{\\rtf1") "Argument to parse-rtf-doc-str is not a valid RTF document.")
  (let [[_, main-group] (parse-group rtf-str 1)]
    main-group))

;; IR to Slate native data types conversion ;;

(defn- command?
  [rtf-ir-entity]
  (or (keyword? rtf-ir-entity)
      (contains? rtf-ir-entity :command)))

(defn- command-name
  [rtf-ir-entity]
  (or (:command rtf-ir-entity) rtf-ir-entity))

;; All of the handle-* fns take [something, parser-state] and return a new parser-state

(defn- handle-text
  [text {:keys [run] :as _parser-state}]
  (if run
    (update run :text str text)
    (run text)))

(defn- handle-command
  "Handles the RTF command appropriately by updating
   the parser-state and returning a new parser state."
  [cmd parser-state]
  (let [run-format (fn [cmd run-format]
                     (let [update-fn (if (zero? (:num cmd)) conj disj)]
                       (update-in parser-state [:run :formats] (update-fn run-format))))]
    (case (command-name cmd)
      :i (run-format cmd :italic)
      :b (run-format cmd :bold)
      :para (let [{:keys [paragraph document]} parser-state]
              ;; Add current working paragraph to document and create new one
              (merge parser-state {:document (update document :children conj paragraph)
                                   ;; Paragraph with no runs, run will be added when first proceeding group with text ends
                                   :paragraph (p/paragraph [])}))
      ;; Create new paragraph if there is no current one, otherwise reset to default formatting
      :parad (update parser-state :paragraph #(if-not %
                                                (p/paragraph [])
                                                (assoc % :type :body))))))

(defn- parse-ir-group
  [group parser-state]
  (reduce (fn [parser-state entity]
            (cond
              (command? entity)
              (handle-command entity parser-state)

              (string? entity)
              (handle-text entity parser-state)))
          parser-state group))

(defn parse-ir
  [rtf-ir parse-state]
  (let [initial-state (or parse-state
                          {:document (document)
                           ;; No paragraph to start, paragraph will be instantiated on finding the first \parad or \para
                           :paragraph nil
                           ;; No run either, run will be instantiated on finding first text
                           :run (run)})]
   (parse-ir-group rtf-ir initial-state)))


;;  {\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard
;;  This is some {\b bold} text.\par
;;  }

(defn extract-text-from-ir [ir]
  (->> ir
       (filter #(not (and (vector? %) (= {:escape "*"} (first %)))))
       (map #(if (vector? %) (extract-text-from-ir %) %))
       (filter string?)
       (apply str)))

(def basic "{\\rtf1\\ansi{\\fonttbl\\f0\\fswiss Helvetica;}\\f0\\pard\n\rThis is some {\\b bold} text.\\'ea\\par\n\r}")

(comment
  (parse-ascii-escape "\\'eaAnd" 2)
  (count basic)
  (parse-rtf-doc-str basic)
  (extract-text-from-ir (parse-rtf-doc-str basic))
  (parse-ir (parse-rtf-doc-str basic))
  (parse-rtf-doc-str (slurp-file "test_files/rtf/conversion_test.rtf"))
  (extract-text-from-ir (parse-rtf-doc-str (slurp-file "test_files/rtf/conversion_test.rtf")))

  )
