(ns slate.filetypes.rtf-import
  "Code for parsing and importing RTF documents.
   Internally, this import process is separated into two logical parts:
   First the raw RTF is parsed into a data structure easily readable by
   Clojure (a sort of IR), and another in which this data structure is
   converted into a Slate Document."
  (:require-macros [slate.utils :refer [slurp-file]])
  (:require [clojure.string :as str]
            [slate.model.dll :as dll]
            [slate.model.common :as m]
            [slate.model.doc :as doc]
            [slate.model.paragraph :as p]
            [slate.model.run :as r]
            [slate.model.selection :as sel]))

;; ================== ;;
;; Text to IR Parsing ;;
;; ================== ;;

(defn- re-matches?
  [re str]
  {:pre [(or (string? str) (nil? str))]}
  (if str (.test re str) false))

(defn- append-or-create-text
  [group char]
  (let [char (when-not (re-matches? #"\n|\r" char) char)]
    (if (string? (peek group))
      (update group (dec (count group)) #(str % char))
      (if (some? char) (conj group char) group))))

(defn- parse-ascii-escape [rtf-str i]
  (let [hex-digits (.substring rtf-str i (+ 2 i))
        charcode (js/parseInt hex-digits 16)]
    [(+ 2 i), (js/String.fromCharCode charcode)]))

(defn- parse-command-argument [rtf-str start-i]
  (let [initial-i (if (= "-" (get rtf-str start-i))
                    (inc start-i)
                    start-i)]
    (loop [i initial-i]
      (let [char (get rtf-str i)]
        (if (re-matches? #"[0-9]" char)
          (recur (inc i))
          [i, (js/parseInt (.substring rtf-str start-i i))])))))

(comment
  (parse-command-argument "\\fs24" 3)
  (parse-command-argument "\\fs-24" 3)
  (parse-command-argument "\\fs24-" 3)
  (parse-command-argument "\\fs-2-4" 3)
  )

(defn- parse-command [rtf-str start-i]
  (let [skip-space? (fn [i]
                      (if (= " " (get rtf-str i)) (inc i) i))]
    (loop [i start-i]
      (let [char (get rtf-str i)]
        (cond
          (re-matches? #"[a-zA-Z]" char)
          (recur (inc i))

          (re-matches? #"-|[0-9]" char)
          (let [command-name (keyword (.substring rtf-str start-i i))
                [new-i, parsed-num] (parse-command-argument rtf-str i)]
            [(skip-space? new-i), {:command command-name
                                   :num parsed-num}])

          :else
          [(skip-space? i), (keyword (.substring rtf-str start-i i))])))))

(comment
  (parse-command "\\fs52" 1)
  (parse-command "\\fs52This" 1)
  (parse-command "\\fs52 This" 1)
  (parse-command "\\fs-52 This" 1)
  (parse-command "\\fs This" 1)
  (parse-command "\\f52" 1)
  (parse-command "\\f5" 1)
  (parse-command "\\f-5" 1)
  (parse-command "\\fs5" 1)
  (parse-command "\\fs" 1)
  )

(defn- parse-backslash-entity
  [rtf-str i]
  (let [char-after-backslash (nth rtf-str i)]
    (cond
      (re-matches? #"[a-z]" char-after-backslash) (parse-command rtf-str i)
      (= char-after-backslash "'") (parse-ascii-escape rtf-str (inc i))
      :else [(inc i), {:escape char-after-backslash}])))

(defn- parse-group
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

(defn- parse-rtf-doc-str
  "Main entry function for parsing an RTF string to IR."
  [rtf-str]
  (assert (.startsWith rtf-str "{\\rtf1") "Argument to parse-rtf-doc-str is not a valid RTF document.")
  (let [[_, main-group] (parse-group rtf-str 1)]
    main-group))

(comment
  (def test
    "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Times;}}
{\\*\\listtable{\\list\\listtemplateid1\\listhybrid{\\listlevel\\levelnfc0\\levelnfcn0\\leveljc0\\leveljcn0\\levelfollow0\\levelstartat1\\levelspace360\\levelindent0{\\*\\levelmarker \\{decimal\\}.}{\\leveltext\\leveltemplateid1\\'02\\'00.;}{\\levelnumbers\\'01;}\\fi-360\\li720\\lin720 }{\\listname ;}\\listid1}
{\\list\\listtemplateid2\\listhybrid{\\listlevel\\levelnfc23\\levelnfcn23\\leveljc0\\leveljcn0\\levelfollow0\\levelstartat1\\levelspace360\\levelindent0{\\*\\levelmarker \\{disc\\}}{\\leveltext\\leveltemplateid101\\'01\\uc0\\u8226 ;}{\\levelnumbers;}\\fi-360\\li720\\lin720 }{\\listname ;}\\listid2}}
{\\*\\listoverridetable{\\listoverride\\listid1\\listoverridecount0\\ls1}{\\listoverride\\listid2\\listoverridecount0\\ls2}}
{\\pard\\fs48This is an H1\\par}
{\\pard\\fs36This is an H2\\par}
{\\pard\\fs24\\par}
{\\pard\\fs24Normal paragraph with a sentence, some {\\i italics}, {\\b bold}, and {\\strike strikethrough}. Plus some emoji and special symbols: \\uc0\\u55358\\uc0\\u56718, \\uc0\\u55356\\uc0\\u57331\\uc0\\u65039\\uc0\\u8205\\uc0\\u55356\\uc0\\u57096, \\uc0\\u55358\\uc0\\u56614\\uc0\\u55356\\uc0\\u57341, \\uc0\\u241, \\{, \\}, \\\\.\\par}
{\\pard\\fs24\\par}
\\pard\\tx220\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\li720\\fi-720\\fs24\\ls1\\ilvl0{\\listtext	1.	}OL 1\\
{\\listtext	2.	}OL 2\\
{\\listtext	3.	}OL 3\\
{\\pard\\fs24\\par}
\\pard\\tx220\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\li720\\fi-720\\fs24\\ls2\\ilvl0{\\listtext	\\uc0\\u8226 	}UL 1\\
{\\listtext	\\uc0\\u8226 	}UL 2\\
{\\listtext	\\uc0\\u8226 	}UL 3\\
{\\pard\\fs24\\par}
{\\pard\\fs24	And a longer indented paragraph after, with Unicode: \\uc0\\u24314\\uc0\\u21069. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.\\par}
{\\pard\\fs24\\par}
}")

  (parse-rtf-doc-str test)
  )

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
        (update :paragraph p/insert-end run)
        (assoc :run nil))
    parser-state))

(defn- convert-paragraph-type?
  "If paragraph starts with '1.', '2.', etc, convert to an OL.
   If it starts with ●, •, etc, convert to an UL."
  [paragraph]
  (let [text (m/text paragraph)
        ol-regex (js/RegExp. "^\\s*[0-9]*\\.\\s?" "g")
        ul-regex (js/RegExp. "^\\s*[●·•⁃◦ ]\\s?" "g")
        delete-first-n-chars (fn [paragraph n]
                               (p/delete paragraph (sel/selection [nil 0] [nil n])))]
    (cond
      (.test ol-regex text)
      (assoc (delete-first-n-chars paragraph (.-lastIndex ol-regex)) :type :ol)

      (.test ul-regex text)
      (assoc (delete-first-n-chars paragraph (.-lastIndex ul-regex)) :type :ul)

      :else
      paragraph)))

;; (defn- convert-leading-tab?
;;   [paragraph]
;;   (if (.startsWith (m/text paragraph) "\t")
;;     (-> paragraph
;;         (p/delete (sel/selection [nil 1]))
;;         (p/insert-start "\t"))
;;     paragraph))

(defn- adjust-paragraph?
  [paragraph]
  (-> paragraph
      (convert-paragraph-type?)
      ;; #_(convert-leading-tab?)
      ))

(defn- add-paragraph-to-doc
  "Adds current paragraph to doc and creates a new, empty paragraph"
  [{:keys [paragraph] :as parser-state}]
  (-> parser-state
      ;; Add current working paragraph to Document
      (update-in [:document :children] conj (adjust-paragraph? paragraph))
      ;; ...and create new one. No runs, run will be added when first subsequent group with text ends
      (assoc :paragraph (p/paragraph (:type paragraph) []))))

(defn- add-paragraph-to-doc?
  [{:keys [paragraph] :as parser-state}]
  (if (m/blank? paragraph)
    parser-state
    (add-paragraph-to-doc parser-state)))

(defn- set-paragraph-type
  [{:keys [paragraph] :as parser-state} type]
  (if paragraph
    (assoc-in parser-state [:paragraph :type] type)
    parser-state))

;; All of the handle-* fns take [something?, parser-state] and return a new parser-state

(defn- handle-text
  [text {:keys [run paragraph] :as parser-state}]
  (assoc parser-state :run (cond
                             (and run paragraph) (r/insert-end run text)
                             paragraph (r/run text)
                             :else nil)))

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

(defn- handle-escape
  [{:keys [escape]} parser-state]
  (case escape
    "\n" (handle-par parser-state)
    "\\" (handle-text "\\" parser-state)
    "{" (handle-text "{" parser-state)
    "}" (handle-text "}" parser-state)
    parser-state))

(defn- handle-fs
  [{:keys [num] :as _cmd} {:keys [paragraph] :as parser-state}]
  (let [font-size (/ num 2)] ; RTF doubles font sizes so you can use half points without having to parse floats
    (cond
      (>= font-size 25) (set-paragraph-type parser-state :h1)
      (>= font-size 15) (set-paragraph-type parser-state :h2)
      :else (set-paragraph-type parser-state (or (:type paragraph) :body)))))

(defn- handle-fi
  [{:keys [num] :as _cmd} parser-state]
  (if (and (>= num 100) (:paragraph parser-state))
    ;; Insert tab at start of paragraph if fi (first indent) is above a given threshold
    (update parser-state :paragraph p/insert-start "\t")
    parser-state))

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
              (if (:run parser-state)
                (update-in parser-state [:run :formats] update-fn run-format)
                parser-state)))]
    (case (command-name cmd)
      :i (format-current-run cmd :italic)
      :b (format-current-run cmd :bold)
      :strike (format-current-run cmd :strikethrough)
      :par (handle-par parser-state)
      :pard (handle-pard parser-state)
      :u (handle-u cmd parser-state)
      :fs (handle-fs cmd parser-state)
      :fi (handle-fi cmd parser-state)
      :tab (handle-text "\t" parser-state)
      :emdash (handle-text "—" parser-state)
      :lquote (handle-text "'" parser-state)
      :rquote (handle-text "'" parser-state)
      :rdblquote (handle-text "\"" parser-state)
      :ldblquote (handle-text "\"" parser-state)
      parser-state)))

(defn- *-escape-group?
  "Returns `true` if the RTF group begins with a \\* escape. These escapes (in the
   format `{\\*\\cmdname ...}`) signal to the RTF processor 'ignore until the end of
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
  (let [initial-state {:document (doc/->Document (dll/dll))
                       ;; No paragraph to start, paragraph will be instantiated on finding the first \parad or \para
                       :paragraph nil
                       ;; No run either, run will be instantiated on finding first text
                       :run nil}]
    ;; The astute Clojurist (Clojurian? Clojurer? Clo-bro?) will note that the RTF conversion
    ;; algorithm is, despite the functional patina given to it by essentially being one big
    ;; reduction, very stateful. Tehcnically everything is a pure function, but all the child
    ;; functions called by `parse-ir` basically just build up the `parser-state` value. This is on
    ;; purpose--RTF itself is a *very* stateful format, to the point that it might be better to
    ;; consider it a simple imperative language for mutating a document than a declarative format
    ;; for defining one. Swimming upstream here is not worth the effort, and all things considered,
    ;; I think this implementation works quite well.
    (->> initial-state
         (handle-group rtf-ir)
         (add-paragraph-to-doc?)
         (:document))))

(defn rtf->doc
  "Main entry function for converting an RTF document (as a string) to a Document."
  [rtf-str]
  (parse-ir (parse-rtf-doc-str rtf-str)))

(comment
  (def basic "{\\rtf1\\ansi{\\fonttbl\\f0\\fswiss Helvetica;}\\f0\\pard\n\rThis is some {\\b bold} text.\\'ea\\par\n\r}")

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