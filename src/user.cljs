(ns user
  (:require [clojure.spec.alpha :as s]))

(defn stack []
  (println (str "Last error thrown:\n\n\"\n" (.-stack *e) "\n\""))
  (js/console.error *e))

(defn explain-spec-error [error]
  (s/explain (:cljs.spec.alpha/spec (.-data error)) (:cljs.spec.alpha/value (.-data error))))