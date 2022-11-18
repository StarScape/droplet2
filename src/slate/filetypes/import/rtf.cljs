(ns slate.filetypes.import.rtf
  "Code for parsing and importing RTF documents.
   Internally, this import process is separated into two logical parts:
   First the raw RTF is parsed into a data structure easily readable by
   Clojure (a sort of IR), and another in which this data structure is
   converted into a Slate Document."
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [clojure.string :as str]
            [slate.model.common :as m]
            [slate.model.doc :as doc]
            [slate.model.paragraph :as p]
            [slate.model.run :as r]))

(defn assoc?
  "Like regular `assoc`, but only sets `k` to `v` if `k` is not present, or `nil`."
  [coll k v]
  (if (contains? coll k) coll (assoc coll k v)))

;; ================== ;;
;; Text to IR Parsing ;;
;; ================== ;;

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
        (recur (inc i) (or command-name (.substring rtf-str start-i i)) (or num-arg-start-i i))

        :else
        (let [cmd (if command-name
                    {:command (keyword command-name)
                     :num (js/parseInt (.substring rtf-str num-arg-start-i i))}
                    (keyword (.substring rtf-str start-i i)))
              i (if (re-matches? #"\s" char) (inc i) i)]
          [i, cmd])))))

(comment
  (parse-command "\\fs52" 1)
  (parse-command "\\f52" 1)
  (parse-command "\\f5" 1)
  (parse-command "\\fs5" 1)
  (parse-command "\\fs" 1)
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

;; ======================================== ;;
;; IR to Slate native data types conversion ;;
;; ======================================== ;;

(defn- escape?
  [rtf-ir-entity]
  (contains? rtf-ir-entity :escape))

(defn- command?
  [rtf-ir-entity]
  (or (keyword? rtf-ir-entity)
      (contains? rtf-ir-entity :command)))

(defn- command-name
  [rtf-ir-entity]
  (or (:command rtf-ir-entity) rtf-ir-entity))

;; This is just a utility function I was using at the very beginning stages
;; of building out this NS, it can probably be thrown away at some point.
(defn- extract-text-from-ir [ir]
  (->> ir
       (filter #(not (and (vector? %) (= {:escape "*"} (first %)))))
       (map #(if (vector? %) (extract-text-from-ir %) %))
       (filter string?)
       (apply str)))

(defn- add-run?
  "Add run if (a) one does not exist and (b) paragraph exists"
  [{:keys [paragraph run] :as parser-state}]
  (if (and paragraph (not run))
    (assoc parser-state :run (r/run))
    parser-state))

(defn- add-run-to-paragraph
  "Adds run to paragraph, if possible, otherwise returns parser-state as-is."
  [{:keys [paragraph run] :as parser-state}]
  (if (and paragraph run)
    (-> parser-state
        (update :paragraph m/insert-end run)
        (assoc :run nil))
    parser-state))

(defn- add-paragraph-to-doc
  [{:keys [paragraph] :as parser-state}]
  (-> parser-state
      ;; Add current working paragraph to Document
      (update-in [:document :children] conj paragraph)
      ;; ...and create new one. No runs, run will be added when first subsequent group with text ends
      (assoc :paragraph (p/paragraph []))))

(defn- add-paragraph-to-doc?
  [{:keys [paragraph] :as parser-state}]
  (if (m/blank? paragraph)
    parser-state
    (add-paragraph-to-doc parser-state)))

;; All of the handle-* fns take [something, parser-state] and return a new parser-state

(defn- handle-text
  [text {:keys [run paragraph] :as parser-state}]
  (assoc parser-state :run (cond
                             (and run paragraph) (m/insert-end run text)
                             paragraph (r/run text)
                             :else nil)))

(defn- handle-escape
  [escape parser-state]
  ;; TODO
  parser-state)

(defn- handle-par
  [parser-state]
  (-> parser-state
      (add-run-to-paragraph)
      (add-paragraph-to-doc)))

(defn- handle-pard
  [parser-state]
  ;; Create new paragraph if there is no current one, otherwise reset to default formatting
  (update parser-state :paragraph #(if-not %
                                     (p/paragraph [])
                                     (assoc % :type :body))))

(comment
  (code-point->unicode-char 24314)
  )

(comment
  (code-point->unicode-char)
  )

(defn- handle-fs
  [{:keys [num] :as _cmd} parser-state]
  parser-state)

(defn- handle-u
  [{:keys [num] :as _cmd} parser-state]
  (handle-text (js/String.fromCharCode num) parser-state))

(defn- handle-command
  "Handles the RTF command appropriately by updating
   the parser-state and returning a new parser state."
  [cmd parser-state]
  (letfn [(format-current-run [cmd run-format]
            (let [parser-state (add-run? parser-state)
                  update-fn (if (zero? (:num cmd)) disj conj)]
              (update-in parser-state [:run :formats] update-fn run-format)))]
    (case (command-name cmd)
      :i (format-current-run cmd :italic)
      :b (format-current-run cmd :bold)
      :strike (format-current-run cmd :strike)
      :par (handle-par parser-state)
      :pard (handle-pard parser-state)
      :u (handle-u cmd parser-state)
      :fs (handle-fs cmd parser-state)
      parser-state)))

(defn- *-escape-group?
  "Returns `true` if the RTF group begins with a \\* escape. These escapes (in the
   format {\\*\\cmdname ...}) signal to the RTF processor 'ignore until the end of
   this group if the command following \\* is not understood by this program', and
   is typically used for marking custom or nonstandard behavior. For the basic needs
   of Slate's RTF importer, any group with a \\* command can be safely ignored."
  [group]
  (and (= {:escape "*"} (first group))
       (command? (second group))))

(defn- handle-group
  [group parser-state]
  (if (*-escape-group? group)
    parser-state
    (as-> parser-state $
      (add-run-to-paragraph $)
      (reduce (fn [parser-state entity]
                (cond
                  ;; RTF Command
                  (command? entity)
                  (handle-command entity parser-state)

                  ;; RTF Group
                  (vector? entity)
                  (handle-group entity parser-state)

                  ;; RTF Escape
                  (escape? entity)
                  (handle-escape entity parser-state)

                  ;; Plaintext
                  (string? entity)
                  (handle-text entity parser-state)))
              $ group)
      (add-run-to-paragraph $))))

(defn parse-ir
  [rtf-ir]
  (let [initial-state {:document (doc/document)
                       ;; No paragraph to start, paragraph will be instantiated on finding the first \parad or \para
                       :paragraph nil
                       ;; No run either, run will be instantiated on finding first text
                       :run nil}]
   (->> initial-state
        (handle-group rtf-ir)
        (add-paragraph-to-doc?)
        (:document))))

(defn rtf->doc
  "Main entry function for converting an RTF document (as a string) to a Document."
  [rtf-str]
  (parse-ir (parse-rtf-doc-str rtf-str)))

;; TODO: implement unicode command
;; TODO: implement h1/h2
;; TODO: implement lists
;; TODO: implement tabs/leading indent

(def basic "{\\rtf1\\ansi{\\fonttbl\\f0\\fswiss Helvetica;}\\f0\\pard\n\rThis is some {\\b bold} text.\\'ea\\par\n\r}")

(comment
  (parse-ascii-escape "\\'eaAnd" 2)
  (count basic)
  (parse-rtf-doc-str basic)
  (extract-text-from-ir (parse-rtf-doc-str basic))
  (parse-ir (parse-rtf-doc-str basic))
  (parse-rtf-doc-str (slurp-file "test_files/rtf/conversion_test.rtf"))
  (extract-text-from-ir (parse-rtf-doc-str (slurp-file "test_files/rtf/conversion_test.rtf")))

  (parse-ir (parse-rtf-doc-str basic))

  (parse-rtf-doc-str (slurp-file "test_files/rtf/conversion_test.rtf"))
  (rtf->doc (slurp-file "test_files/rtf/conversion_test.rtf"))
  )