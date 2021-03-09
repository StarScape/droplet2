(ns slate.measurement
  "Functions for measuring the widths of text as they will appear in the actual DOM."
  (:require [clojure.string :as str]))

(defn- formats->classes [formats]
  (map #(str (name %) "-format") formats))

(defn- create-elem [font-size font-family]
  (let [elem (.createElement js/document "div")
        style (.-style elem)]
    (set! (.-visibility style) "hidden")
    (set! (.-position style) "absolute")
    (set! (.-whiteSpace style) "pre")
    (set! (.-margin style) 0)
    (set! (.-padding style) 0)
    (set! (.-fontSize style) font-size)
    (set! (.-fontFamily style) font-family)
    (set! (.-fontStyle style) "normal")
    (set! (.-fontWeight style) "normal")
    elem))

(defn- apply-css [elem formats]
  (let [css-classes (formats->classes formats)]
    (set! (.-className elem) (str/join " " css-classes))))

(defn measure
  "Helper function for `ruler`. Not private, so it **can** be used directly, but generally should not be."
  ([elem cache text]
   (measure elem cache text #{}))
  ([elem cache text formats]
   (let [formats-hash (hash (seq formats))]
     (->> text
          (map (fn [char]
                 (let [cache-key (str char "-" formats-hash)
                       cache-val (aget cache cache-key)]
                   (if cache-val
                     cache-val
                     (do
                       (apply-css elem formats)
                       (set! (.-innerHTML elem) char)
                       (aset cache cache-key (.. elem (getBoundingClientRect) -width)))))))
          (reduce +)))))

(defn ruler
  "Given valid CSS values for a font size and family (e.g. `12px` and `Arial`),
   returns a function that takes a string and (optionally) a list of formats,
   and return the width (in pixels) that that string will **actually take up**
   in the real DOM. This is important for rendering as we split text into lines
   ourselves. Example:

   ```
   (def measure-fn (ruler \"12px\" \"Times\"))
   (measure-fn \"abc\") ; returns width 'abc' will take up when rendered in Times at 12px
   (measure-fn \"abc\" #{:italic}) ; returns width 'abc' will take up when rendered in Times at 12px in italic
   ```

   Measurements are made by capturing the width of a hidden DOM element, but
   a cache of these values is maintained in the background, so subsequent calls
   will be cheaper."
  [font-size font-family]
  (let [cache #js {}
        elem (create-elem font-size font-family)]
    (.. js/document -body (appendChild elem))
    (fn [& args] (apply measure (concat [elem cache] args)))))

(defn ruler-for-elem
  "Returns a measurement function for the given DOM element."
  [elem]
  (let [style (js/getComputedStyle elem)]
    (ruler
     (.getPropertyValue style "font-size")
     (.getPropertyValue style "font-family"))))

(defn fake-measure-fn
  "For testing, returns every char width as 10px.
   Meant to simulate a function created with `ruler`."
  ([str] (* 10 (.-length str)))
  ([str _formats] (* 10 (.-length str))))
