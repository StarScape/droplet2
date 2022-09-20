(ns slate.macros)

(defmacro slurp-file
  "Loads a file's whole contents and returns a string, at compile time (filepath is relative to project root)."
  [filepath]
  (slurp (java.io.File. filepath)))