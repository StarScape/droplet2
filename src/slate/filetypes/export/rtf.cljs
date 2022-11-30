(ns slate.filetypes.export.rtf
  (:require [clojure.string :as str]
            [slate.model.common :as m]
            [slate.model.run :as run :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]))

(def h1-font-size 48) ;; in half pts
(def h2-font-size 36) ;; in half pts

(def list-headers
  "{\\*\\listtable{\\list\\listtemplateid1\\listhybrid{\\listlevel\\levelnfc0\\levelnfcn0\\leveljc0\\leveljcn0\\levelfollow0\\levelstartat1\\levelspace360\\levelindent0{\\*\\levelmarker \\{decimal\\}.}{\\leveltext\\leveltemplateid1\\'02\\'00.;}{\\levelnumbers\\'01;}\\fi-360\\li720\\lin720 }{\\listname ;}\\listid1}
{\\list\\listtemplateid2\\listhybrid{\\listlevel\\levelnfc23\\levelnfcn23\\leveljc0\\leveljcn0\\levelfollow0\\levelstartat1\\levelspace360\\levelindent0{\\*\\levelmarker \\{disc\\}}{\\leveltext\\leveltemplateid101\\'01\\uc0\\u8226 ;}{\\levelnumbers;}\\fi-360\\li720\\lin720 }{\\listname ;}\\listid2}}
{\\*\\listoverridetable{\\listoverride\\listid1\\listoverridecount0\\ls1}{\\listoverride\\listid2\\listoverridecount0\\ls2}}")

(def header (str "\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Times;}}\n" list-headers))

(defn- str-map [f coll] (apply str (map f coll)))

(defn- formats->commands
  [run-formats]
  (let [mapping {:italic "\\i"
                 :bold "\\b"
                 :strikethrough "\\strike"}]
    (str-map mapping run-formats)))

(defn- convert-non-ascii-char
  "If char is not ASCII, will convert it to an RTF unicode command."
  [char]
  (let [char-code (.charCodeAt char 0)]
    (if (<= char-code 127)
      char
      (str "\\uc0\\u" char-code))))

(defn- text->rtf
  "RTF only accepts ASCII, and even then there are some restrictions, so anything outside
   of alphanumeric character range is escaped."
  [text]
  (str-map convert-non-ascii-char text))

(defn- run->rtf
  [run]
  (let [formatting-commands (formats->commands (:formats run))
        text-body (text->rtf (:text run))]
    (if (str/blank? formatting-commands)
      text-body
      (str "{" formatting-commands " " text-body "}"))))

(defn- runs->rtf [runs]
  (str-map run->rtf runs))

(defn- ul->rtf
  [paragraph]
  (str "{\\listtext	\\uc0\\u8226 	}" (runs->rtf (:runs paragraph)) "\\\n"))

(defn- ol->rtf
  [paragraph list-num]
  (str "{\\listtext	" list-num ".	}" (runs->rtf (:runs paragraph)) "\\\n"))

(defn- paragraph->rtf
  [paragraph]
  (case (:type paragraph)
    :h1 (str "{\\pard\\fs" h1-font-size (runs->rtf (:runs paragraph)) "\\par}")
    :h2 (str "{\\pard\\fs" h2-font-size (runs->rtf (:runs paragraph)) "\\par}")
    (str "{\\pard\\fs24" (runs->rtf (:runs paragraph)) "\\par}")))

(def ^:private list-preludes
  (let [common "\\pard\\tx220\\tx720\\tx1440\\tx2160\\tx2880\\tx3600\\tx4320\\tx5040\\tx5760\\tx6480\\tx7200\\tx7920\\tx8640\\li720\\fi-720\\fs24"]
    {:ol (str common "\\ls1\\ilvl0")
     :ul (str common "\\ls2\\ilvl0")}))

(defn- list-paragraphs->rtf
  [paragraphs]
  (let [list-type (:type (first paragraphs))]
    (loop [paras paragraphs
           list-num 1
           rtf-str (get list-preludes list-type)]
      (let [para (first paras)]
        (if-not para
          rtf-str
          (recur (next paras)
                 (inc list-num)
                 (str rtf-str (case list-type
                                :ul (ul->rtf para)
                                :ol (ol->rtf para list-num)))))))))

(defn doc->rtf [doc]
  (str "{" header "\n"
       (loop [paragraphs (:children doc)
              rtf-str ""]
         (let [paragraph (first paragraphs)
               paragraph-type (:type paragraph)]
           (if-not paragraph
             rtf-str
             (if (or (= :ol paragraph-type)
                     (= :ul paragraph-type))
               (let [[list-paragraphs, remaining-paragraphs] (split-with #(= (:type %) paragraph-type) paragraphs)
                     list-paragraphs-rtf (list-paragraphs->rtf list-paragraphs)]
                 (recur remaining-paragraphs
                        (str rtf-str list-paragraphs-rtf)))
               (recur (next paragraphs)
                      (str rtf-str (paragraph->rtf paragraph) "\n"))))))
       "}"))

(def test-doc
  (document [(paragraph (random-uuid) :h1 [(run "This is an H1")])
             (paragraph (random-uuid) :h2 [(run "This is an H2")])
             (paragraph [(run "")])
             (paragraph [(run "Normal paragraph with a sentence, some ")
                         (run "italics" #{:italic})
                         (run ", ")
                         (run "bold" #{:bold})
                         (run ", and ")
                         (run "strikethrough" #{:strikethrough})
                         (run ".")])
             (paragraph [(run "")])
             (paragraph (random-uuid) :ol [(run "OL 1")])
             (paragraph (random-uuid) :ol [(run "OL 2")])
             (paragraph (random-uuid) :ol [(run "OL 3")])
             (paragraph [(run "")])
             (paragraph (random-uuid) :ul [(run "UL 1")])
             (paragraph (random-uuid) :ul [(run "UL 2")])
             (paragraph (random-uuid) :ul [(run "UL 3")])
             (paragraph [(run "")])
             (paragraph [(run "\u2003And a longer indented paragraph after, with Unicode: 建前. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])
             (paragraph [(run "")])]))

;; TODO: handle tabs
;; TODO: UI in droplet

(comment
  (print (doc->rtf test-doc))
  )
