(ns slate.filetypes.html-export
  (:require [slate.model.common :refer [blank?]]
            [slate.model.paragraph :as p :refer [Paragraph]]
            [slate.model.doc :refer [Document]]
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
               (update-in (:runs p) [0 :text] #(.substring % 1))
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

(defn slate->html
  [slate-type]
  (let [rendered-hiccup (condp = (type slate-type)
                          Document (render-paragraphs (:children slate-type))
                          Paragraph (render-runs (:runs slate-type)))]
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
    (document [(paragraph :h1 [(run "This is an H1")])
               (paragraph :h2 [(run "This is an H2")])
               (paragraph [(run "")])
               (paragraph [(run "Normal paragraph with a sentence, some ")
                           (run "italics" #{:italic})
                           (run ", ")
                           (run "bold" #{:bold})
                           (run ", and ")
                           (run "strikethrough" #{:strikethrough})
                           (run ".")])
               (paragraph [(run "")])
               (paragraph :ol [(run "OL 1")])
               (paragraph :ol [(run "OL 2")])
               (paragraph :ol [(run "OL 3")])
               (paragraph [(run "")])
               (paragraph :ul [(run "UL 1")])
               (paragraph :ul [(run "UL 2")])
               (paragraph :ul [(run "UL 3")])
               (paragraph [(run "")])
               (paragraph [(run "\tAnd a longer indented paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after. And a longer paragraph after.")])]))
  (doc->html test-doc)
  (->> (:children test-doc)
       (partition-by #(:type %))
       (flatten-when #(not (list-paragraph? (first %)))))

  (p/indented? (last (:children test-doc)))
  )
