;; TODO: this file can be deleted at some point maybe?
;; If so, be sure and keep any still-relevant TODOs


;; TODO: allow interceptors of the type "# " which fires when the user types a key sequence of pound then space.
;; These need to coexist with the existing control-key oriented interceptors. I think there should be three types:
;;
;; 1. Keyword (e.g. :click, :ctrl+left) - the ones that currently exist
;; 2. String (e.g. "# " or "1. ") - fires once the sequence of characters in the string is typed
;; 3. Vectors (e.g. [:ctrl+a "1"], [:ctrl+a, :ctrl+b]) - used to mix types 1 and 2, or to create chords
;;    out of keyboard shortcuts. This type is probably not as strictly necessary as the first two, but
;;    it should probably be added for the sake of completeness/extensibility.
;;
;; Definitely implement types 1 and 2 first. I'll have to give some careful thought about the data structures I need
;; to use to achieve this.
(ns slate.core
  "Main entrypoint for using and initializing the Slate editor."
  (:require [slate.editor-ui-state :as ui-state]))


(def init ui-state/init)
