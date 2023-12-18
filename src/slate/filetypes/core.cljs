(ns slate.filetypes.core
  (:refer-clojure :exclude [import])
  (:require [slate.filetypes.html-import :refer [html->doc]]
            [slate.filetypes.rtf-import :refer [rtf->doc]]
            [slate.filetypes.html-export :refer [doc->html]]
            [slate.filetypes.rtf-export :refer [doc->rtf]]))

(defmulti import
  "Imports the Document from the specified format. Returns a Document."
  (fn [filetype _file-contents] filetype))

(defmethod import "html" [_ file-contents] (html->doc file-contents))

(defmethod import "rtf" [_ file-contents] (rtf->doc file-contents))

(defmulti export
  "Exports the Document to the specified format. Returns a string."
  (fn [filetype _doc] filetype))

(defmethod export "html" [_ doc] (doc->html doc))

(defmethod export "rtf" [_ doc] (doc->rtf doc))
