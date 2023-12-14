(ns slate.measurement
  "Functions for measuring the widths of text as they will appear in the actual DOM."
  (:require [clojure.string :as str]
            [slate.model.common :refer [graphemes]]
            [slate.utils :refer [formats->css-classes paragraph-type->css-class]]
            [dev.performance-utils :refer-macros [inside-time-measurement!]]))

(defn- create-elem!
  "Creates measurement element and adds it to the DOM."
  [shadow-root font-size font-family tab-size]
  (let [outer-elem (.createElement js/document "div")
        elem (.createElement js/document "div")
        outer-style (.-style outer-elem)
        style (.-style elem)]
    ;; A new measurement function will be generated whenever the parameters (font size, font
    ;; style) change, so clean up any previous element associated with the same document UUID.
    (when-let [previous-elem (.querySelector shadow-root (str "." "measurement-elem"))]
      (.remove previous-elem))
    (set! (.-className outer-elem) "measurement-elem")

    ;; Set font-size on outer element since h1/h2 use relative font sizes (em)
    (set! (.-fontSize outer-style) font-size)
    (set! (.-visibility outer-style) "hidden")
    (set! (.-position outer-style) "absolute")
    (set! (.-top outer-style) "0px")
    (set! (.-whiteSpace style) "pre")
    (set! (.-margin style) 0)
    (set! (.-padding style) 0)
    (set! (.-fontFamily style) font-family)
    (set! (.-fontStyle style) "normal")
    (set! (.-fontWeight style) "normal")
    (set! (.-fontWeight style) "normal")
    (set! (.-tabSize style) tab-size)
    (.appendChild outer-elem elem)
    (.appendChild shadow-root outer-elem)
    elem))

(defn- apply-css! [elem formats paragraph-type]
  (let [css-classes (-> (formats->css-classes formats)
                        (conj (paragraph-type->css-class paragraph-type)))]
    (set! (.-className elem) (str/join " " css-classes))))

(defn measure
  "Helper function for `ruler`. Not private and __can__ be used directly, but generally should not be."
  ([elem cache text]
   (set! js/window.measureFnCalls (inc js/window.measureFnCalls))
   (measure elem cache text #{} :body))
  ([_elem _cache _text _formats]
   (throw (js/Error. "Invalid arity of measurement function!"))
   #_(measure elem cache text formats nil))
  ([elem cache text formats paragraph-type]
   #_(when (nil? text)
     (throw (js/Error. "wtf dawg")))
   (set! js/window.measureFnCalls (inc js/window.measureFnCalls))
   (let [formats-hash (hash formats)
         type-hash (hash paragraph-type)
         measure-grapheme (fn [grapheme]
                            (set! js/window.measureCharCalls (inc js/window.measureCharCalls))
                            (let [cache-key (str grapheme "-" formats-hash "-" type-hash)
                                  cache-val (aget cache cache-key)]
                              (if cache-val
                                cache-val
                                (do
                                  (apply-css! elem formats paragraph-type)
                                  (set! (.-innerHTML elem) grapheme)
                                  (aset cache cache-key (.. elem (getBoundingClientRect) -width))))))
         text-graphemes (if (= 1 (.-length text))
                          text
                          (->> text (graphemes) (map :grapheme)))]
     (->> text-graphemes (map measure-grapheme) (reduce +)))))

(defn ruler
  "Given valid CSS values for a font size and family (e.g. `\"12px\"` and `\"Arial\"`),
   as well as the tab size (e.g. `\"25px\"`), returns a function that takes a string
   and (optionally) a list of formats, and return the width (in pixels) that that
   string will **actually take up** in the real DOM. This is important for rendering
   as we split text into lines ourselves. Example:

   ```
   (def measure-fn (ruler \"12px\" \"Times\"))
   (measure-fn \"abc\") ; returns width 'abc' will take up when rendered in Times at 12px
   (measure-fn \"abc\" #{:italic}) ; returns width 'abc' will take up when rendered in Times at 12px in italic
   ```

   Measurements are made by capturing the width of a hidden DOM element, but
   a cache of these values is maintained in the background, so subsequent calls
   will be cheaper."
  [shadow-root font-size font-family tab-size]
  (let [cache #js {}
        elem (create-elem! shadow-root font-size font-family tab-size)]
    (fn [& args]
      (apply measure elem cache args))))

(defn ruler-for-elem
  "Returns a measurement function for the given DOM element.

   The measurement-fn takes three parameters:
   - `text`: string to measure the width of
   - `formats`: set of formats for the element
   - `paragraph-type`: __(optional)__ the type of the paragraph, such as `:h1` or `:h2` (default `:body`)

   And returns the width the text will take up, in pixels."
  [elem shadow-root]
  (let [style (js/getComputedStyle elem)
        ;; I literally don't know why but when you query the tab-size it always returns double what it should
        tab-size-adjusted (str "calc(" (.getPropertyValue style "tab-size") " / 2)")]
    (ruler shadow-root (.getPropertyValue style "font-size") (.getPropertyValue style "font-family") tab-size-adjusted)))

(defn fake-measure-fn
  "For testing, returns every char width as 10px.
   Meant to simulate a function created with `ruler`."
  ([str] (* 10 (.-length str)))
  ([str _formats] (* 10 (.-length str)))
  ([str _formats _paragraph-type] (* 10 (.-length str))))
