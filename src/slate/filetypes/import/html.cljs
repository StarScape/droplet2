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
  (and (is-elem? node) (= "LI" (.-tagName node))))

(defn is-p? [node]
  (and (is-elem? node) (= "P" (.-tagName node))))

(defn is-br? [node]
  (and (is-elem? node) (= "BR" (.-tagName node))))

(defn is-h1? [node]
  (and (is-elem? node)
       (or (= "H1" (.-tagName node))
           (>= font-size-px (em->px node 2.0)))))

(defn is-h2? [node]
  (and (is-elem? node)
       (or (= "H2" (.-tagName node))
           (>= font-size-px (em->px node 1.5)))))

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

(defn convert-text-node
  [text-node]
  (js/console.log "text-node")
  (let [text #p (clean-whitespace #p (.-wholeText text-node))
        formats #p (html-element->styles #p (.-parentElement text-node))]
    (js/console.log "body")
    #p (run text formats)))

(defn convert-node
  ([node applied-para-type]
   (let [map-children #(map %1 (js/Array.from (.-childNodes %2)))]
     (cond
       #p (is-ul? node)
       (map-children #(convert-node % :ul) node)

       #p (is-ol? node)
       (map-children #(convert-node % :ol) node)

       #p (is-br? node)
       (p/empty-paragraph)

       #p (is-block-elem? node)
       (let [ptype (or (block-elem->para-type node) applied-para-type)]
         (paragraph (random-uuid) ptype (map-children #(convert-node % ptype) node)))

       #p (is-inline-elem? node)
       (flatten (map-children convert-node node))

       #p (is-text-node? node)
       #p (convert-text-node node)

       :else (js/console.log "ELSE!!!"))))
  ([node] (convert-node node :body)))

(defn html->droplet
  "Converts an HTML string to a Droplet native format (either a Document, DocumentFragment, or ParagraphFragment)."
  [html-str]
  (let [dom (add-to-iframe! html-str)
        body-contents (js/Array.from (.. dom -body -children))]
    (flatten (map convert-node body-contents))))

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
