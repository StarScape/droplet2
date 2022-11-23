(ns slate.filetypes.export.rtf
  (:require [clojure.string :as str]
            [slate.model.common :as m]
            [slate.model.run :as run :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.doc :as doc :refer [document]]
            [clojure.string :as str]))

(def ^:private header "\\rtf1\\ansi\\deff0 {\fonttbl {\f0 Times;}")

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

(defn- paragraph->rtf
  [paragraph]
  (str "{\\pard " (str-map run->rtf (:runs paragraph)) "\\par}"))

(defn doc->rtf [doc]
  (str "{" header (str-map paragraph->rtf (:children doc)) "}"))

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

(comment
  (doc->rtf test-doc)
  )
