(ns slate.model.common
  "Protocols and multi-fns used across different model data types.")

(defprotocol TextContainer
  (len [this] "Returns the number of chars in container (run/paragraph).")
  (blank? [this] "Returns true if the text container is empty."))

(extend-type string
  TextContainer
  (len [s] (count s))
  (blank? [s] (zero? (count s))))

(defprotocol Formattable
  "Primitive operations for formatting text-containers (runs, paragraphs, documents)."
  (apply-format
    [this format]
    [this sel format]
    "Returns a new container with the format applied.
    Arity taking a selection is not implemented for runs.")
  (remove-format
    [this format]
    [this sel format]
    "Returns a new container with the format removed, if it is present.
    Arity taking a selection is not implemented for runs."))

(defprotocol Selectable
  "Operations for any text container on which paragraph offset selections are valid.
   Note that this precludes runs, as they are contained inside paragraphs
   and therefore a 'paragraph offset into a run' would not make sense.
   Basically, this is a set of common operations on paragraphs and documents."

  (char-at
    [container sel]
    "Returns the character under the block cursor at the given single selection `sel`")
  (char-before
    [container sel]
    "Returns the character immediately before the block cursor at the given single selection `sel`")
  (selected-content
    [container sel]
    "Returns the content within the range-selection inside the container, either as a vector
    of runs or a list (probably a dll) of paragraphs, depending which is appropriate.")
  (shared-formats
    [container]
    [container sel]
    "Returns the set of all the formats shared by each run that is wholly or partially
    inside the selection. Will return an empty set if there are no formats shared.
    If not passed a selection, will return shared formats for the whole container.")
  (toggle-format
    [doc sel format]
    "Either applies the selected format to the selection (if the selected text
    does not already have that format) or removes it (if the selected text **does**
    have that format)."))

(defn type-dispatch [& args] (mapv type args))

(defn content-type [c]
  (if (sequential? c)
    [(type (first c))]
    (type c)))

(defn insert-dispatch [container location content]
  [(type container), (type location), (content-type content)])

(defn no-location-dispatch [container content]
  [(type container), (content-type content)])

;; TODO: once all implementations of insert are done, there should be a HELLUVA
;; docstring explaining its use...the various forms it can take, etc. The goal is
;; for it to be "don't think about it, it just works, using this general form."
(defmulti insert "Inserts into a Run/Paragraph/Document."
  {:arglists '([container location content-to-insert])}
  #'insert-dispatch)

(defmulti insert-start "Shortcut for insert at the start of a text container."
  {:arglists '([container content-to-insert])}
  #'no-location-dispatch)

(defmulti insert-end "Shortcut for insert at the end of a text container."
  {:arglists '([container content-to-insert])}
  #'no-location-dispatch)

(defmulti delete "Deletes from a Run/Paragraph/Document."
  {:arglists '([container location])}
  #'type-dispatch)
