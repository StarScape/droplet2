(ns slate.filetypes.core
  (:refer-clojure :exclude [import])
  (:require [slate.filetypes.import.html :refer [html->doc]]
            [slate.filetypes.export.html :refer [doc->html]]))

(defmulti import
  "Imports the Document from the specified format. Returns a Document."
  (fn [filetype _file-contents] filetype))

(defmethod import "html" [_ file-contents] (html->doc file-contents))

(defmulti export
  "Exports the Document to the specified format. Returns a string."
  (fn [filetype _doc] filetype))

(defmethod export "html" [_ doc] (doc->html doc))
