(ns slate.test-utils)

(defn doc=
  "Tests if two docs are equal, disregarding paragraph UUIDs."
  [doc1 doc2]
  (letfn [(strip-uuids [doc]
            (update doc :children
                    (fn [children] (map #(dissoc % :uuid) children))))]
    (= (strip-uuids doc1) (strip-uuids doc2))))

(defn doc-frag=
  [frag1 frag2]
  (= (map #(dissoc % :uuid) (:paragraphs frag1))
     (map #(dissoc % :uuid) (:paragraphs frag2))))
