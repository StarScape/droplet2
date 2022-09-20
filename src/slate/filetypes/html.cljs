(ns slate.filetypes.html
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [clojure.set :as set]
            [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]))

(def test-file (slurp-file "test_files/html/the_quiet_universe.html"))

(defn- str->document
  [html-str]
  (.parseFromString (js/DOMParser.) html-str "text/html"))

(defn- child-nodes
  [html-node]
  (js/Array.from (.-childNodes html-node)))

(defn- html-element->styles
  [html-element]
  (let [computed-style (js/getComputedStyle html-element)
        styles (transient #{})]
    (js/console.log html-element)
    (js/console.log computed-style)
    (when (>= (-> computed-style (.-fontWeight) js/parseInt) 700)
      (conj! styles :bold))
    (when (= "italic" #p (.-fontStyle computed-style))
      (conj! styles :italic))
    (when (= "line-through" (.-textDecoration computed-style))
      (conj! styles :italic))
    (persistent! styles)))

(defn- html-node->run-or-runs
  ([node styles]
   {:pre [(set? styles)]}
   (cond
     (instance? js/Text node)
     (run (.-wholeText node) styles)

     (instance? js/Element node)
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
  (let [dom (str->document html-str)
        body-contents (js/Array.from (.. dom -body -children))
        paragraphs (map html-elem->para body-contents)]
    paragraphs))

(comment
  (.-body (str->document test-file))
  (js/Array.from (.-children (.-body (str->document test-file))))
  (js/Array.from (.. (str->document test-file) -body -children))
  (js/console.log (.-style (nth one 1)))
  )
(html->doc test-file)

