(ns slate.interceptors-browser-test
  "Tests for interceptor functionality. We have to instantiate some JS Event objects
   in here to test things properly, hence why "
  (:require [cljs.test :include-macros true :refer [deftest is]]
            [slate.default-interceptors :refer [insert]]
            [slate.interceptors :as ints :include-macros true]))

(defn letter-input-event [letter]
  (js/InputEvent. "fake_input_event" #js {:data letter}))

(deftest input-history-test
  (is (= (ints/add-to-input-history [] insert (letter-input-event "a"))
         ["a"]))
  (is (= (ints/add-to-input-history ["a" "b"] insert (letter-input-event "c"))
         ["a" "b" "c"]))
  (let [full-history (vec (repeat ints/max-input-history "k"))]
    (is (= (ints/add-to-input-history full-history insert (letter-input-event "a"))
           (-> (drop 1 full-history) (vec) (conj "a"))))))

#_(deftest event->key-set-test
  (let [e (js/KeyboardEvent. "my_kb_event" #js {:ctrlKey true, :shiftKey true, :key "ArrowLeft"})]
    (is (= (event->key-set e) #{:ctrl :shift :left}))))
