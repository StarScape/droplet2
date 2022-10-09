(ns slate.filetypes.export.html
  (:require [slate.model.common :refer [items fragment-type blank?]]
            [slate.model.paragraph :as p :refer [ParagraphFragment]]
            [slate.model.doc :refer [DocumentFragment]]
            [reagent.dom.server :refer [render-to-static-markup]]))

(defn- list-paragraph? [paragraph]
  (or (= (:type paragraph) :ul) (= (:type paragraph) :ol)))

(defn- flatten-when [pred coll]
  (reduce (fn [result next]
            (if (pred next)
              (apply conj result next)
              (conj result next)))
          [] coll))

(defn run-css [{:keys [formats] :as _run}]
  (let [styles (transient {})]
    (when (contains? formats :bold)
      (assoc! styles :font-weight "bold"))
    (when (contains? formats :italic)
      (assoc! styles :font-style "italic"))
    (when (contains? formats :strikethrough)
      (assoc! styles :text-decoration "line-through"))
    (persistent! styles)))

(defn paragraph-css [para]
  (if (p/indented? para)
    {:text-indent "30px"}
    {}))

(comment
  (run-css (run "foobar" #{:italic :bold}))
  (run-css (run "foobar" #{:strikethrough}))
  )

(defn render-runs [runs]
  (for [r runs]
    [:span {:style (run-css r)
            :key (random-uuid)}
     (:text r)]))

(def default-p-styles {:margin-top "0px" :margin-bottom "0px"})

(defn render-paragraph [p]
  (let [tag (case (:type p)
              :body :p
              :h1 :h1
              :h2 :h2
              :ol :li
              :ul :li)
        runs (if (p/indented? p)
               (update-in (:runs p) [0 :text] #(.substr % 1))
               (:runs p))]
    (if (blank? p)
      [:br {:key (random-uuid)}]
      [tag {:style (merge default-p-styles (paragraph-css p))
            :key (random-uuid)}
       (render-runs runs)])))

(defn render-paragraphs
  [paragraphs]
  (for [document-chunk (->> paragraphs
                   (partition-by #(:type %))
                   (flatten-when #(not (list-paragraph? (first %)))))]
    (if (sequential? document-chunk)
      (case (-> document-chunk first :type)
        :ol [:ol {:key (random-uuid)}
             (for [p document-chunk]
               (render-paragraph p))]
        :ul [:ul {:key (random-uuid)}
             (for [p document-chunk]
               (render-paragraph p))])
      (render-paragraph document-chunk))))

(defn fragment->html
  [fragment]
  (let [rendered-hiccup (case (fragment-type fragment)
                          :document (render-paragraphs (items fragment))
                          :paragraph (render-runs (items fragment)))]
    (render-to-static-markup rendered-hiccup)))

(defn doc->html
  "Converts a droplet document to an HTML string."
  [droplet-doc]
  (render-to-static-markup
   [:html
    [:head]
    [:body
     (render-paragraphs (:children droplet-doc))]]))

(comment
  (def test-doc
    (document [(paragraph (random-uuid) :h1 [(run "This is an H1")])
               (paragraph (random-uuid) :h2 [(run "This is an H2")])
               (paragraph [(run "")])
               (paragraph [(run "Normal paragraph with a sentence, some ")
                           (run "italics" #{:italic})
                           (run ", ")
                           (run "bold" #{:bold})
                           (run ", and ")
                           (run "strikethrough" #{:strikethrough})
                           (run ".")])
               (paragraph [(run "")])
               (paragraph (random-uuid) :ol [(run "OL 1")])
               (paragraph (random-uuid) :ol [(run "OL 2")])
               (paragraph (random-uuid) :ol [(run "OL 3")])
               (paragraph [(run "")])
               (paragraph (random-uuid) :ul [(run "UL 1")])
               (paragraph (random-uuid) :ul [(run "UL 2")])
               (paragraph (random-uuid) :ul [(run "UL 3")])
               (paragraph [(run "")])
               (paragraph [(run "\u2003And a longer indented paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])]))
  (doc->html test-doc)
  (->> (:children test-doc)
       (partition-by #(:type %))
       (flatten-when #(not (list-paragraph? (first %)))))

  (p/indented? (last (:children test-doc)))
  )
