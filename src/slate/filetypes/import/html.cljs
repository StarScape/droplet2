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

(defn- clean-whitespace
  "Removes non-standard whitespace characters and makes them regular spaces"
  [str]
  (.replaceAll str (js/RegExp. "\\s" "g") " "))

(defn- root-font-size
  "Returns root font size for document, in pixels."
  [document]
  (js/parseFloat (.-fontSize (js/getComputedStyle (.-documentElement document)))))

(defn- rem->px
  [node rem]
  (* rem (root-font-size (.-ownerDocument node))))

(defn- em->px
  [elem em]
  (* em (js/parseFloat (.-fontSize (js/getComputedStyle (.-parentElement elem))))))

(defn- font-size-px [elem]
  (let [computed-style (js/getComputedStyle elem)]
    (js/parseFloat (.-fontSize computed-style))))

(defn- text-node?
  [iframe-node]
  (if (nil? iframe-node)
    false
    (= js/Node.TEXT_NODE (.-nodeType iframe-node))))

(defn- elem-node?
  [iframe-node]
  (if (nil? iframe-node)
    false
    (= js/Node.ELEMENT_NODE (.-nodeType iframe-node))))

(defn- comment-node?
  [iframe-node]
  (if (nil? iframe-node)
    false
    (= js/Node.COMMENT_NODE (.-nodeType iframe-node))))

(defn- elem?
  [iframe-node]
  (let [iframe @*iframe]
    (instance? (.. iframe -contentWindow -HTMLElement) iframe-node)))

(defn- p? [node]
  (and (elem? node) (= "P" (.-tagName node))))

(defn- br? [node]
  (and (elem? node) (= "BR" (.-tagName node))))

(defn- remove-comment-nodes!
  "Removes all comments nodes from HTML document."
  [root-node]
  (let [child-nodes (js/Array.from (.-childNodes root-node))]
    (doseq [child child-nodes]
      (if (comment-node? child)
        (.removeChild root-node child)
        (remove-comment-nodes! child)))))

(defn- parent-of-first-text-node
  "Returns the parent element of the first text node contained inside the element."
  [node]
  {:pre [(some? node)]}
  (cond
    (zero? (.-childElementCount node)) node
    (text-node? node) (.-parentElement node)
    :else (recur (aget (.-childNodes node) 0))))

(defn- h1-font-size? [elem]
  (>= (font-size-px elem) (rem->px elem 2.0)))

(defn- h1? [node]
  (and (elem? node)
       (or (= "H1" (.-tagName node))
           (h1-font-size? node)
           (h1-font-size? (parent-of-first-text-node node)))))

(defn- h2-font-size? [elem]
  (>= (font-size-px elem) (rem->px elem 1.3)))

(defn- h2? [node]
  (and (elem? node)
       (or (= "H2" (.-tagName node))
           (h2-font-size? node)
           (h2-font-size? (parent-of-first-text-node node)))))

(defn- ul? [node]
  (and (elem? node)
       (or (= "UL" (.-tagName node))
           ;; Element is considered :ul if it starts with "· " (· is a middle-dot).
           ;; This is a bit of a sloppy heuristic, but necessary since some word
           ;; processors (looking at you, Word), spit out non-standard HTML lists.
           (.test #"^·\s" (.-innerText node)))))

(defn- ol? [node]
  (and (elem? node)
       (or (= "OL" (.-tagName node))
           ;; Element is considered :ol if it starts with "1. ", "2. ", "3. " etc.
           ;; This is a bit of a sloppy heuristic, but necessary since some word
           ;; processors (looking at you, Word), spit out non-standard HTML lists.
           (.test #"^[0-9]+\.\s" (.-innerText node)))))

(defn- li? [node]
  (and (elem? node)
       (or (= "LI" (.-tagName node))
           (= "list-item" (.-display (js/getComputedStyle node))))))

(defn- block-elem? [node]
  (and (elem? node)
       (let [computed-style (js/getComputedStyle node)]
         (or (= "block" (.-display computed-style))
             (= "list-item" (.-display computed-style))))))

(defn- inline-elem? [node]
  (and (elem? node)
       (let [computed-style (js/getComputedStyle node)]
         (or (= "inline" (.-display computed-style))
             (= "inline-block" (.-display computed-style))))))

(defn- no-block-level-children? [node]
  (and (elem? node)
       (let [children (js/Array.from (.-children node))]
         (and (pos? (.-length children))
              (every? (complement block-elem?) children)))))

(defn- indented? [elem]
  (pos? (js/parseFloat (.-textIndent (js/getComputedStyle elem)))))

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

;; Some styles (like text-decoration and text-decoration-line) are propogated to children,
;; but NOT inherited. This makes them a gigantic pain in th ass to detect with getComputedStyle,
;; so our only options are (a) recurse all the way up on finding a text node to check if any of
;; its ancestors have a propogated style property, which is wildly inefficient or (b) keep track
;; of any elements with a propogated style like text-decoration in a dynamic var. This is the
;; latter option.
(def ^:dynamic *propogated-styles* #{})
(defn- update-propogated-styles [node]
  (if-not (elem? node)
    *propogated-styles*
    (let [computed-style (js/getComputedStyle node)
          styles (transient #{})]
      (when (= "line-through" (.-textDecorationLine computed-style))
        (conj! styles :strikethrough))
      (set/union *propogated-styles* (persistent! styles)))))

(def ^:dynamic *paragraph-type* :body)
(defn- update-paragraph-type [node]
  (if (block-elem? node)
    (cond
      ;; If *paragraph-type* has been set to a list type somewhere up the call stack, keep it.
      (or (= *paragraph-type* :ul)
          (= *paragraph-type* :ol))
      *paragraph-type*

      (ul? node) :ul
      (ol? node) :ol
      (h1? node) :h1
      (h2? node) :h2
      :else :body)
    *paragraph-type*))

(defn convert-text-node
  [text-node]
  (let [text (clean-whitespace (.-wholeText text-node))
        parent-elem (.-parentElement text-node)
        formats (set/union *propogated-styles* (html-element->styles parent-elem))]
    (run text formats)))

(defn convert-node
  ([node]
   (let [map-children #(flatten (map %1 (js/Array.from (.-children %2))))
         map-child-nodes #(flatten (map %1 (js/Array.from (.-childNodes %2))))]
     (binding [*propogated-styles* (update-propogated-styles node)
               *paragraph-type* (update-paragraph-type node)]
       (cond
         (br? node)
         (p/empty-paragraph)

         (and (or (block-elem? node) (li? node))
              (no-block-level-children? node))
         (let [paragraph (paragraph (random-uuid) *paragraph-type* (map-child-nodes convert-node node))
               paragraph (if (indented? node)
                           (p/indent paragraph)
                           paragraph)
               paragraph (if (or (= :ol (:type paragraph))
                                 (= :ul (:type paragraph)))
                           (p/trim-start paragraph)
                           paragraph)]
           paragraph)

         (ul? node)
         (map-children convert-node node)

         (ol? node)
         (map-children convert-node node)

         (or (li? node) (block-elem? node))
         (map-children convert-node node)

         (inline-elem? node)
         (map-child-nodes convert-node node)

         (text-node? node)
         (convert-text-node node)

         :else (do
                 #_(js-debugger)
                 (throw "Unhandled condition in (convert-node)!")))))))

(defn html->fragment
  "Converts an HTML string to a Droplet native format (either a DocumentFragment or ParagraphFragment)."
  [html-str]
  (let [dom (add-to-iframe! html-str)
        _ (remove-comment-nodes! (.-body dom))
        body-contents (js/Array.from (.. dom -body -children))
        results (binding [*paragraph-type* :body]
                  (flatten (map convert-node body-contents)))]
    (cond
      (= (-> results first type) r/Run)
      (p/fragment results)

      (= (-> results first type) p/Paragraph)
      (doc/fragment results)

      :else (throw (js/Error. "Unrecognized type of `results` when converting HTML.")))))

(defn html->doc
  [html-doc-str]
  ;; Just convert the DocumentFragment to a Document.
  (let [paragraphs (:paragraphs (html->fragment html-doc-str))]
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
