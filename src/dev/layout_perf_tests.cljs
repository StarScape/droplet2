(ns dev.layout-perf-tests
  (:require [clojure.string :as str]
            [slate.model.run :as r :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.viewmodel :as vm]))

(def words #js ["lorem" "ipsum" "dolor" "sit" "amet" "consectetur" "adipiscing" "elit" "donec" "interdum"
                "lobortis" "fringilla" "fusce" "id" "tempor" "diam" "lorem" "ipsum" "dolor" "sit" "amet"
                "consectetur" "adipiscing" "elit" "curabitur" "tincidunt" "lacus" "eu" "arcu" "eleifend"
                "aliquam" "nulla" "facilisi" "nunc" "molestie" "est" "nec" "volutpat" "euismod" "ipsum"
                "purus" "aliquet" "odio" "malesuada" "placerat" "magna" "felis" "in" "mauris" "proin"
                "cursus" "varius" "augue" "a" "finibus" "tortor" "cursus" "non" "cras" "tempor" "est" 
                "non" "mollis" "ultrices" "mauris" "dignissim" "dignissim" "enim" "vitae" "hendrerit" "nunc"])

(defn capitalize [word]
  (str (.toUpperCase (aget word 0)) (.substring word 1)))

(defn random-word []
  (rand-nth words))

(defn random-word-capitalized []
  (capitalize (random-word)))

(defn gen-sentence
  "Returns a random lorem ipsum sentence between min-length and max-length (inclusive) words long."
  [min-length, max-length]
  (let [length (+ min-length (rand-int (inc (- max-length min-length))))
        words (concat [(random-word-capitalized)] (repeatedly (dec length)))]
    (str (random-word-capitalized) " " (str/join " " (repeatedly (dec length) random-word)) ".")))

(defn gen-sentences [n min-sentence-length max-sentence-length]
  (let [sentences (repeatedly n #(gen-sentence min-sentence-length max-sentence-length))]
    (str/join " " sentences)))

;; (def test-paragraph (paragraph [(run paragraph-text)]))

(defn perf-test [n min-sentence-length max-sentence-length]
  (let [para (paragraph [(run (gen-sentences n min-sentence-length max-sentence-length))])
        measure-fn (doto (:measure-fn @js/globalSlateInstance)
                     (when-not
                      (throw (js/Error. "measure-fn not found, cannot run perf test."))))
        start-time (js/performance.now)]
    (vm/from-para para 500 measure-fn)
    (let [end-time (js/performance.now)]
      (str "Time elapsed: " (- end-time start-time) "ms"))))

(comment
  (perf-test 1000 3 15)
  (vm/from-para (paragraph [(run (gen-sentences 10 3 15))])
                500
                (:measure-fn @js/globalSlateInstance))

  (gen-sentences 1000 3 15)
  (gen-sentence 3 15)
  (random-word-capitalized)
  (random-word)
  (capitalize "abc")
  )

