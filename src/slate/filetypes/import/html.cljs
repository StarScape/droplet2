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

#_#_(defn- root-font-size
  "Returns root font size for document, in pixels."
  [document]
  (js/parseFloat (.-fontSize (js/getComputedStyle (.-documentElement document)))))

(defn- rem->px
  [document rem]
  (* rem (root-font-size document)))

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

(defn- is-element?
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

(defn- html-node->run-or-runs
  ([node styles]
   {:pre [(set? styles)]}
   (cond
     (is-text-node? node)
     (run (clean-whitespace (.-wholeText node)) styles)

     (is-element? node)
     (let [styles (set/union styles (html-element->styles node))]
       (->> (child-nodes node)
            (map #(html-node->run-or-runs % styles))
            (flatten)))

     :else (throw "Unrecognized Node type!")))
  ([node] (html-node->run-or-runs node #{})))

(defn- html-elem->para-or-paras	
  [html-elem]
  (let [computed-style (js/getComputedStyle html-elem)]
    (if (or (= "OL" (.-tagName html-elem))
            (= "UL" (.-tagName html-elem)))
      (map html-elem->para-or-paras (js/Array.from (.-children html-elem)))
      (let [indented? (pos? (js/parseFloat (.-textIndent computed-style)))
            font-size-px (js/parseFloat (.-fontSize computed-style))
            ptype (cond
                    (or (= "H1" (.-tagName html-elem))
                        (>= font-size-px (em->px html-elem 2.0))) :h1
                    (or (= "H2" (.-tagName html-elem))
                        (>= font-size-px (em->px html-elem 1.5))) :h2
                    (and (= "LI" (.-tagName html-elem))
                         (= "OL" (.-tagName (.-parentElement html-elem)))) :ol
                    (and (= "LI" (.-tagName html-elem))
                         (= "UL" (.-tagName (.-parentElement html-elem)))) :ul
                    :else :body)
            children (->> (child-nodes html-elem)
                          (map html-node->run-or-runs)
                          (flatten))
            children (if indented?
                       (-> children
                           (vec)
                           (update-in [0 :text] #(str "\u2003" %)))
                       children)]
        (paragraph (random-uuid) ptype children)))))

(defn block-elem->para-type
  [elem])

(defn is-ul? [node])

(defn is-ol? [node])

(defn is-block-elem? [node])

(defn is-inline-elem? [node])

(defn convert-text-node
  [text-node]
  (run (clean-whitespace (.-wholeText text-node)) (html-element->styles (.-parentElement text-node))))

(defn convert-node
  ([node applied-para-type]
   (let [map-children #(map %1 (js/Array.from (.-childNodes %2)))]
     (cond
       (is-ul? node)
       (map-children #(convert-node % :ul) node)

       (is-ol? node)
       (map-children #(convert-node % :ol) node)

       (is-block-elem? node)
       (let [ptype (or (block-elem->para-type node) applied-para-type)]
         (paragraph (random-uuid) ptype (map-children #(convert-node % ptype) node)))

       (is-inline-elem? node)
       (flatten (map-children convert-node node))

       (is-text-node? node)
       (convert-text-node node))))
  ([node] (convert-node node :body)))

(defn html->droplet
  "Converts an HTML string to a Droplet native format (either a Document, DocumentFragment, or ParagraphFragment)."
  [html-str]
  (let [dom (add-to-iframe! html-str)
        body-contents (js/Array.from (.. dom -body -children))
        paragraphs (flatten (map html-elem->para-or-paras body-contents))]
    (document paragraphs)))

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
