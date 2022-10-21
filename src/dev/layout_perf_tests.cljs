(ns dev.layout-perf-tests
  (:require-macros [slate.macros :refer [slurp-file]])
  (:require [clojure.string :as str]
            [slate.model.run :as r :refer [run]]
            [slate.model.paragraph :as p :refer [paragraph]]
            [slate.viewmodel :as vm]
            [slate.measurement :as measurement]
            [dev.performance-utils :as perf-utils :refer-macros [inside-time-measurement!]]))

(def long-str (slurp-file "test_files/performance_tests/long_str.txt"))
(def short-str (slurp-file "test_files/performance_tests/short_str.txt"))

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

(defn aget-vs-first-test [n-samples]
  (perf-utils/start-time-measurement! "aget-time")
  (perf-utils/start-time-measurement! "first-time")
  (repeatedly n-samples (fn []
                          (inside-time-measurement! "aget-time" (aget (random-word) 0))
                          (inside-time-measurement! "first-time" (first (random-word)))))
  (let [aget-time (perf-utils/stop-time-measurement! "aget-time")
        first-time (perf-utils/stop-time-measurement! "first-time")
        diff (abs (- aget-time first-time))
        faster (cond
                 (> aget-time first-time) "(first)"
                 (> first-time aget-time) "(aget)"
                 :else "wtf")]
  (str faster " is faster by " diff "ms")))

(comment
  ;; Negligible difference between aget and first on strings
  (aget-vs-length-test 1000000)
  )

(defn get-words-perf-test
  [test-str sample-times]
  (let [inner #(let [start-time (js/performance.now)]
                 (vm/get-words test-str)
                 (let [end-time (js/performance.now)]
                   (- end-time start-time)))
        runs (map (fn [_] (inner)) (range sample-times))
        average (/ (reduce + runs) (count runs))]
    (str "Average time was: " average "ms")))

(defn perf-test
  ([n min-sentence-length max-sentence-length]
   (perf-test (paragraph [(run (gen-sentences n min-sentence-length max-sentence-length))])))
  ([para]
   (let [measure-fn (doto (:measure-fn @js/window.globalSlateInstance)
                      (when-not
                       (throw (js/Error. "measure-fn not found, cannot run perf test."))))
         start-time (js/performance.now)]
     (set! js/window.measureFnCalls 0)
     (set! js/window.measureCharCalls 0)
     (set! js/window.logText true)
     (perf-utils/start-time-measurement! "get-words")
     (perf-utils/start-time-measurement! "measure-fn")
     ;; (js/console.profile "paragraph-layout-performance-test")

     (vm/from-para para 500 measure-fn)

     ;; (js/console.profileEnd "paragraph-layout-performance-test")
     (set! js/window.logText false)
     (let [end-time (js/performance.now)
           get-words-time (perf-utils/stop-time-measurement! "get-words")
           measure-fn-time (perf-utils/stop-time-measurement! "measure-fn")]
       (str "Time: " (- end-time start-time) "ms"
            ", measure-fn calls: " js/window.measureFnCalls
            ", measure-char calls: " js/window.measureCharCalls
            ", measure-fn time: " measure-fn-time "ms"
            ", get-words time: " get-words-time "ms")))))
(comment
  ;; 2000 will give a paragraph of roughly the same length as my big test document.
  (get-words-perf-test long-str 1000)
  (.-length short-str)
  (perf-test (paragraph [(run short-str)]))
  (perf-test (paragraph [(run long-str)]))
  (perf-test 2000 3 15)
  (perf-test (paragraph))
  (vm/from-para (paragraph [(run (gen-sentences 10 3 15))])
                500
                (:measure-fn @js/globalSlateInstance))

  (gen-sentences 1000 3 15)
  (gen-sentence 3 15)
  (random-word-capitalized)
  (random-word)
  (capitalize "abc")
  )

