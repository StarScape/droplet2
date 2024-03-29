(ns slate.filetypes.html-import
  (:require-macros [slate.utils :refer [slurp-file]])
  (:require [clojure.set :as set]
            [drop.utils :as drop-utils]
            [slate.model.doc :as doc :refer [document Document]]
            [slate.model.paragraph :as p :refer [paragraph Paragraph]]
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

(def non-standard-whitespace
  "Regex matching all whitespace not used by Slate.
   Note: currently this is a naive implementation because Slate
   is only concerned with English. Eventually it will need to support
   double-width spaces and such as well."
  (js/RegExp. "[\\r\\n\\t\\f\\v\u00a0\u1680\u2000\u2001\u2002\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]" "g"))

(defn- clean-whitespace
  "Removes non-standard whitespace characters and makes them regular spaces,
   as well as converts any runs of 3 or more consecutive spaces to a tab char."
  [str]
  (.. str
      ;; (replaceAll (js/RegExp. "\\t" "g") "\u2003")
      (replaceAll non-standard-whitespace " ")
      (replaceAll (js/RegExp. "\\s{3,}" "g") "\t")))

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
         (every? (complement block-elem?) children))))

(defn- indented? [elem]
  (pos? (js/parseFloat (.-textIndent (js/getComputedStyle elem)))))

(defn- display-none? [node]
  (and (elem? node)
       (= "none" (.-display (js/getComputedStyle node)))))

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
  [node]
  (let [map-children #(filter some? (flatten (map %1 (js/Array.from (.-children %2)))))
        map-child-nodes #(filter some? (flatten (map %1 (js/Array.from (.-childNodes %2)))))]
    (binding [*propogated-styles* (update-propogated-styles node)
              *paragraph-type* (update-paragraph-type node)]
      (cond
        (br? node)
        (p/paragraph)

        (and (or (block-elem? node) (li? node))
             (no-block-level-children? node))
        (let [paragraph (paragraph *paragraph-type* (map-child-nodes convert-node node))
              paragraph (if (indented? node)
                          (p/indent paragraph)
                          paragraph)
              paragraph (if (or (= :ol (:type paragraph))
                                (= :ul (:type paragraph)))
                          (p/trim-start paragraph)
                          paragraph)]
          (p/trim-end paragraph))

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

        (display-none? node)
        nil

        :else (if drop-utils/DEV
                (do
                  (js/console.error "Node: " node)
                  (throw (js/Error. "Unhandled condition in (convert-node):")))
                (do
                  (js/console.warn "Unhandled condition in (convert-node): ")
                  (js/console.warn "Node: " node)
                  nil))))))

(defn convert-all-nodes [nodes]
  (filter some? (flatten (map convert-node nodes))))

(defn html->slate
  "Converts an HTML string to a Slate data structure (either a Document or Paragraph)."
  [html-str]
  (let [dom (add-to-iframe! html-str)
        _ (remove-comment-nodes! (.-body dom))
        body-contents (js/Array.from (.. dom -body -children))
        results (binding [*paragraph-type* :body]
                  (convert-all-nodes body-contents))]
    (cond
      (= (-> results first type) r/Run)
      (p/paragraph results)

      (= (-> results first type) p/Paragraph)
      (document results)

      :else (throw (js/Error. "Unrecognized type of `results` when converting HTML.")))))

(defn html->doc
  [html-doc-str]
  (let [converted (html->slate html-doc-str)]
    (condp = (type converted)
      Paragraph (document [converted])
      Document converted)))

(comment
  (.-styleSheets (str->document test-file))
  (js/Array.from (.-children (.-body (str->document test-file))))
  (js/Array.from (.. (str->document test-file) -body -children))
  (add-to-iframe! test-file)

  #_(let [children (js/Array.from (.. (add-to-iframe! (slurp-file "test_files/html/conversion_test.html")) -body -children))
        ol (nth children 5)
        ol-li (aget (.-children ol) 0)])

  (html->doc (slurp-file "test_files/html/conversion_test.html"))
  (html->doc (slurp-file "test_files/html/the_quiet_universe.html"))
  )
