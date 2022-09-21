(ns slate.filetypes.html
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [clojure.set :as set]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]))

(def test-file (slurp-file "test_files/html/the_quiet_universe.html"))

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

(defn- child-nodes
  [html-node]
  (js/Array.from (.-childNodes html-node)))

(defn- is-text?
  [iframe-node]
  (let [iframe @*iframe]
    (instance? (.. iframe -contentWindow -Text) iframe-node)))

(defn- is-element?
  [iframe-node]
  (let [iframe @*iframe]
    (instance? (.. iframe -contentWindow -Element) iframe-node)))

(defn- html-element->styles
  [html-element]
  (let [computed-style (js/getComputedStyle html-element)
        styles (transient #{})]
    (when (>= (-> computed-style (.-fontWeight) js/parseInt) 700)
      (conj! styles :bold))
    (when (= "italic" (.-fontStyle computed-style))
      (conj! styles :italic))
    (when (= "line-through" (.-textDecoration computed-style))
      (conj! styles :italic))
    (persistent! styles)))

(defn- html-node->run-or-runs
  ([node styles]
   {:pre [(set? styles)]}
   (cond
     (is-text? node)
     (run (.-wholeText node) styles)

     (is-element? node)
     (let [styles (set/union styles (html-element->styles node))]
       (->> (child-nodes node)
            (map #(html-node->run-or-runs % styles))
            (flatten)))

     :else (throw "Unrecognized Node type!")))
  ([node] (html-node->run-or-runs node #{})))

(defn- html-elem->para
  [html-elem]
  (paragraph (->> (child-nodes html-elem)
                  (map html-node->run-or-runs)
                  (flatten))))

(defn html->doc
  "Converts an HTML string to a Droplet document."
  [html-str]
  (let [dom (add-to-iframe! html-str)
        body-contents (js/Array.from (.. dom -body -children))
        paragraphs (map html-elem->para body-contents)]
    (document paragraphs)))

(comment
  (.-styleSheets (str->document test-file))
  (js/Array.from (.-children (.-body (str->document test-file))))
  (js/Array.from (.. (str->document test-file) -body -children))
  (js/console.log (.-style (nth one 1)))
  (add-to-iframe! test-file)

  (html->doc test-file)
  )
