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

;; TODO change name to core, change name of old core to "model"
(ns slate.main
  (:require [slate.events :as events]
            [slate.editor :as editor]
            [slate.measurement :refer [ruler-for-elem]]
            [slate.navigation :as nav]
            [slate.viewmodel :as vm]))

;; TODO: Can just create the hidden-elem programmatically in this function.
(defn init
  "Initializes the editor surface, and returns an atom containing the editor state. This
   atom will continue to be update throughout the lifetime of the editor. Takes a series
   of keyword arguments:

   Required:
   :editor-elem - The DOM element for the editor
   :hidden-input - The hidden <input> needed by the editor to capture keystrokes

   Optional:
   :doc - The initial document to load into the editor (default to an empty document)
   :selection - Initial selection (defaults to the start of the document)"
  [& {:keys [doc selection editor-elem hidden-input]}]
  (let [measure-fn (ruler-for-elem editor-elem)
        editor-state (atom {:doc doc
                            :selection (or selection (nav/start doc))
                            ;; TODO: just change to a DLL of viewmodels?
                            :viewmodels (vm/from-doc doc 200 measure-fn)
                            :dom-elem editor-elem
                            :hidden-input hidden-input
                            :measure-fn measure-fn
                            :input-history []
                            :interceptors (events/interceptor-map)})]
    (events/init-default-events editor-state)
    (editor/sync-dom @editor-state)

    editor-state))
