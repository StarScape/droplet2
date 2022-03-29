(ns slate.measurement
  "Functions for measuring the widths of text as they will appear in the actual DOM."
  (:require [clojure.string :as str]
            [slate.view :refer [formats->css-classes paragraph-type->css-class]]))

(defn- create-elem! [font-size font-family]
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

(defn- apply-css! [elem formats paragraph-type]
  (let [css-classes (-> (formats->css-classes formats)
                        (conj (paragraph-type->css-class paragraph-type)))]
    (set! (.-className elem) (str/join " " css-classes))))

(defn measure
  "Helper function for `ruler`. Not private and __can__ be used directly, but generally should not be."
  ([elem cache text]
   (measure elem cache text #{} :body))
  ([elem cache text formats]
   (throw "Invalid arity of measurement function!")
   #_(measure elem cache text formats nil))
  ([elem cache text formats paragraph-type]
   (let [formats-hash (hash (seq formats))
         type-hash (hash paragraph-type)
         measure-char (fn [char]
                        (let [cache-key (str char "-" formats-hash "-" type-hash)
                              cache-val (aget cache cache-key)]
                          (if cache-val
                            cache-val
                            (do
                              (apply-css! elem formats paragraph-type)
                              (set! (.-innerHTML elem) char)
                              (aset cache cache-key (.. elem (getBoundingClientRect) -width))))))]
     (->> text (map measure-char) (reduce +)))))

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
        elem (create-elem! font-size font-family)]
    (.. js/document -body (appendChild elem))
    (fn [& args] (apply measure elem cache args))))

(defn ruler-for-elem
  "Returns a measurement function for the given DOM element.
   The measurement-fn takes two parameters:

   - `text`: string to measure the width of
   - `formats`: set of formats for the element
   - `paragraph-type`: __(optional)__ the type of the paragraph, such as :h1 or :h2 (default `nil`)

   And returns the width the text will take up, in pixels."
  [elem]
  (let [style (js/getComputedStyle elem)]
    (ruler
     (.getPropertyValue style "font-size")
     (.getPropertyValue style "font-family"))))

(defn fake-measure-fn
  "For testing, returns every char width as 10px.
   Meant to simulate a function created with `ruler`."
  ([str] (* 10 (.-length str)))
  ([str _formats] (* 10 (.-length str)))
  ([str _formats _paragraph-type] (* 10 (.-length str))))
