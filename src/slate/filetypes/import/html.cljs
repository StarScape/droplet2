(ns slate.filetypes.import.html
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [clojure.set :as set]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]))

(def ^:private *iframe (atom nil))

(defn- get-or-create-iframe!
  []
  (if @*iframe
    @*iframe
    (let [iframe (js/document.createElement "iframe")]
      (set! (.-className iframe) "slate-iframe")
      (js/document.body.appendChild iframe)
      (reset! *iframe iframe))))

(defn- add-to-iframe!
  "Adds HTML document to iframe and returns the Document object."
  [html-document-str]
  (let [iframe (get-or-create-iframe!)]
    (set! (.-src iframe) "about:blank")
    (doto (.-contentDocument iframe)
      (.open)
      (.write html-document-str)
      (.close))))

(defn- em->px
  [elem em]
  (* em (js/parseFloat (.-fontSize (js/getComputedStyle (.-parentElement elem))))))

(defn- child-nodes
  [html-node]
  (js/Array.from (.-childNodes html-node)))

(defn- is-text-node?
  [iframe-node]
  (let [iframe @*iframe]
    (instance? (.. iframe -contentWindow -Text) iframe-node)))

(defn- is-elem?
  [iframe-node]
  (let [iframe @*iframe]
    (instance? (.. iframe -contentWindow -Element) iframe-node)))

(defn- clean-whitespace
  "Removes non-standard whitespace characters and makes them regular spaces"
  [str]
  (.replace str (js/RegExp "\\s") " "))

(defn- html-element->styles
  [html-element]
  (let [computed-style (js/getComputedStyle html-element)
        styles (transient #{})]
    (when (>= (-> computed-style (.-fontWeight) js/parseInt) 700)
      (conj! styles :bold))
    (when (= "italic" (.-fontStyle computed-style))
      (conj! styles :italic))
    (when (= "line-through" (.-textDecorationLine computed-style))
      (conj! styles :strikethrough))
    (persistent! styles)))

(defn font-size-px [elem]
  (let [computed-style (js/getComputedStyle elem)]
    (js/parseFloat (.-fontSize computed-style))))

(defn is-ul? [node]
  (and (is-elem? node) (= "UL" (.-tagName node))))

(defn is-ol? [node]
  (and (is-elem? node) (= "OL" (.-tagName node))))

(defn is-li? [node]
  (and (is-elem? node)
       (or (= "LI" (.-tagName node))
           (= "list-item" (.-display (js/getComputedStyle node))))))

(defn is-p? [node]
  (and (is-elem? node) (= "P" (.-tagName node))))

(defn is-br? [node]
  (and (is-elem? node) (= "BR" (.-tagName node))))

(defn is-h1? [node]
  (and (is-elem? node)
       (or (= "H1" (.-tagName node))
           (>= (font-size-px node) (em->px node 2.0)))))

(defn is-h2? [node]
  (and (is-elem? node)
       (or (= "H2" (.-tagName node))
           (>= (font-size-px node) (em->px node 1.5)))))

(defn is-block-elem? [node]
  (and (is-elem? node)
       (let [computed-style (js/getComputedStyle node)]
         (= "block" (.-display computed-style)))))

(defn is-inline-elem? [node]
  (and (is-elem? node)
       (let [computed-style (js/getComputedStyle node)]
         (= "inline" (.-display computed-style)))))

(defn block-elem->para-type
  [elem]
  (cond
    (is-li? elem) nil
    (is-p? elem) :body
    (is-h1? elem) :h1
    (is-h2? elem) :h2))

(declare ^:dynamic *paragraph-type*)

(defn convert-text-node
  [text-node]
  (let [text (clean-whitespace (.-wholeText text-node))
        parent-elem (.-parentElement text-node)
        formats (html-element->styles parent-elem)]
    (cond
      (is-h1? parent-elem)
      (set! *paragraph-type* :h1)

      (is-h2? parent-elem)
      (set! *paragraph-type* :h2))

    (run text formats)))

(defn convert-node
  ([node]
   (let [map-children #(flatten (map %1 (js/Array.from (.-childNodes %2))))]
       (cond
         (is-ul? node)
         (binding [*paragraph-type* :ul]
           (map-children convert-node node))

         (is-ol? node)
         (binding [*paragraph-type* :ol]
           (map-children convert-node node))

         (is-br? node)
         (p/empty-paragraph)

         (is-block-elem? node)
         (binding [*paragraph-type* (if *paragraph-type*
                                      *paragraph-type*
                                      (block-elem->para-type node))]
           (let [children (map-children convert-node node)]
             (paragraph (random-uuid) *paragraph-type* children)))

         (is-inline-elem? node)
         (map-children convert-node node)

         (is-li? node)
         (map-children convert-node node)

         (is-text-node? node)
         (convert-text-node node)

         :else (do
                 (js/console.log node)
                 (js-debugger)
                 (throw "Unhandled condition in convert-node."))))))

(defn html->droplet
  "Converts an HTML string to a Droplet native format (either a Document, DocumentFragment, or ParagraphFragment)."
  [html-str]
  (let [dom (add-to-iframe! html-str)
        body-contents (js/Array.from (.. dom -body -children))
        results (flatten (map convert-node body-contents))]
    (cond
      (= (-> results first type) r/Run)
      (p/fragment results)

      (= (-> results first type) p/Paragraph)
      (doc/fragment results)

      :else (throw (js/Error. "Unrecognized type of `results` when converting HTML.")))))

;; TODO: rethink html import approach by recursing document tree and keeping track of style state
;;       pull in some test string from g docs and see how they import

(comment
  (.-styleSheets (str->document test-file))
  (js/Array.from (.-children (.-body (str->document test-file))))
  (js/Array.from (.. (str->document test-file) -body -children))
  (js/console.log (.-style (nth one 1)))
  (add-to-iframe! test-file)

  (let [children (js/Array.from (.. (add-to-iframe! (slurp-file "test_files/html/conversion_test.html")) -body -children))
        ol (nth children 5)
        ol-li (aget (.-children ol) 0)]
    (js/console.log ol-li))

  (html->doc (slurp-file "test_files/html/conversion_test.html"))
  (html->doc (slurp-file "test_files/html/the_quiet_universe.html"))
  )
