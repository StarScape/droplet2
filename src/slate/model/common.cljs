(ns slate.model.common
  "Protocols and multi-fns used across different model data types.")

(defprotocol TextContainer
  (text [s] "Returns the text within the container, as a string.")
  (len [this] "Returns the number of chars (UTF-16 codepoints) in the container.")
  (blank? [this] "Returns true if the text container is empty.")
  (graphemes [this] "Returns a seq of the graphemes in the TextContainer."))

(extend-type string
  TextContainer
  (text [s] s)
  (len [s] (count s))
  (blank? [s] (zero? (count s)))
  (graphemes [s]
    (let [segmenter (js/Intl.Segmenter. "en-US" #js {:granularity "grapheme"})
          iterator (js* "~{}[Symbol.iterator]()" (.segment segmenter s))
          transform-segment (fn [segment] {:offset (.-index segment)
                                           :grapheme ^js/Object (.-segment segment)})]
      (map transform-segment (es6-iterator-seq iterator)))))

;; TODO: this name overlaps with nav/Selectable
(defprotocol Selectable
  "Operations for any text container on which paragraph offset selections are valid.
   Note that this precludes runs, as they are contained inside paragraphs
   and therefore a 'paragraph offset into a run' would not make sense.
   Basically, this is a set of common operations on paragraphs and documents."
  (char-at
   [container]
   [container sel]
   "Returns the character under the block cursor at the given single selection `sel`")
  (char-before
   [container]
   [container sel]
   "Returns the character immediately before the block cursor at the given single selection `sel`")
  (selected-content
   [container]
   [container sel]
   "Returns the content within the range selection inside the container, either as a vector
    of runs or a list (probably a dll) of paragraphs, depending which is appropriate.")
  (formatting
   [container]
   [container sel]
   "Returns the set of all the formats shared by each run that is wholly or partially
    inside the selection. Will return an empty set if there are no formats shared.
    If not passed a selection, will return shared formats for the whole container."))

(defprotocol Fragment
  "Fragment of a Document or Paragraph"
  (items [fragment] "Returns a seqable list of the items in the fragment")
  (fragment-type [fragment] "Returns either :document (fragment is a series of paragraphs) or :paragraph (fragment is a series of runs)"))

(defn content-type [c]
  (if (sequential? c)
    [(type (first c))]
    (type c)))

(defn insert-dispatch [& args]
  (conj (vec (map type (butlast args)))
        (content-type (last args))))

;; TODO: once all implementations of insert are done, there should be a HELLUVA
;; docstring explaining its use...the various forms it can take, etc. The goal is
;; for it to be "don't think about it, it just works, using this general form."
(defmulti insert "Inserts into a Run/Paragraph/Document."
  {:arglists '([container location content-to-insert])}
  #'insert-dispatch)
