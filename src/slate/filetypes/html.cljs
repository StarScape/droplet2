(ns slate.filetypes.html
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [slate.model.doc :as doc :refer [document]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.model.run :as r :refer [run]]))

(def test-file (slurp-file "test_files/html/the_quiet_universe.html"))

(defn- str->document
  [html-str]
  (.parseFromString (js/DOMParser.) html-str "text/html"))

(defn html->doc
  "Converts an HTML string to a Droplet document."
  [html-str]
  (let [dom (str->document html-str)
        html-elem->para #(paragraph [(run (.-innerText #p %))])
        body-contents (js/Array.from (.. dom -body -children))
        paragraphs (map html-elem->para body-contents)]
    (js/console.log body-contents)
    paragraphs))

#_(js/Array.from (.-children (.-body (str->document test-file))))
(html->doc test-file)
