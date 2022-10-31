(ns slate.macros)

(defmacro comptime
  "Executes the code inside at compile time (in Clojure, not CLJS), and returns the result.
   Can be used for things like, e.g. programatically creating a large object and then injecting
   it into the code, without wasting cycles generating it at runtime."
  [& body]
  (eval `(do ~@body)))

(defmacro slurp-file
  "Loads a file's whole contents and returns a string, at compile time (filepath is relative to project root)."
  [filepath]
  (slurp (java.io.File. filepath)))
