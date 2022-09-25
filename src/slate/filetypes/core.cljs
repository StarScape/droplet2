(ns slate.filetypes.core
  (:refer-clojure :exclude [import])
  (:require [slate.filetypes.import.html :refer [html->doc]]))

(defmulti import (fn [filetype _file-contents] filetype))

(defmethod import "html" [_ file-contents] (html->doc file-contents))

#_#_(defmulti export (fn [filetype _file-contents] filetype))
(defmethod export "html" [_ file-contents] (html->doc file-contents))
