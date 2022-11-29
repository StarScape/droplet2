(ns slate.filetypes.export.rtf
  (:require [clojure.string :as str]
            [slate.model.common :as m]
            [slate.model.run :as run :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]))

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


(defn- text->rtf
  "RTF only accepts ASCII, and even then there are some restrictions, so anything outside
   of alphanumeric character range is escaped."
  [text]
  text)

(defn- run->rtf
  [run]
  (let [formatting-commands (formats->commands (:formats run))
        text-body (text->rtf (:text run))]
    (if (str/blank? formatting-commands)
      text-body
      (str "{" formatting-commands " " text-body "}"))))

(defn- ol->rtf
  [runs-rtf list-num]
  (str "{\\pard\\ls1\\ilvl0\\li720\\fi-720 {\\listtext	" list-num ".	} " runs-rtf "\\par}"))

(defn- paragraph->rtf
  [paragraph list-num?]
  (let [runs-rtf (str-map run->rtf (:runs paragraph))]
    (case (:type paragraph)
      :ol (ol->rtf runs-rtf list-num?)
      ;;:ul (ul->rtf runs-rtf)
      (str "{\\pard " runs-rtf "\\par}"))))

(defn- list-paragraphs->rtf [paragraphs])

(defn doc->rtf [doc]
  (str "{" header "\n"
       (loop [paragraphs (:children doc)
              rtf-str ""
              list-num 0]
         (let [paragraph (first paragraphs)
               list-num (if (= :ol (:type paragraph)) (inc list-num) 0)]
           (if-not paragraph
             rtf-str
             (if (or (= :ol (:type paragraph))
                     (= :ul (:type paragraph)))
               (let [list-paragraphs
                     [paragraphs, rtf] (list-paragraphs->rtf (drop paragraphs))]
                 (recur (next paragraphs)
                        (str rtf-str (paragraph->rtf paragraph list-num) "\n")
                        list-num))))))
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
             (paragraph [(run "\u2003And a longer indented paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])
             (paragraph [(run "")])]))

;; TODO: rtf H1
;; TODO: rtf H2
;; TODO: handle tabs

(comment
  (print (doc->rtf test-doc))
  )
