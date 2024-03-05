(ns slate.renderer.measurement
  "Functions for measuring the widths of text as they will appear in the actual DOM."
  (:require [slate.model.common :refer [graphemes]]))

(defn create-ctx! []
  (let [canvas (js/OffscreenCanvas. 0 0)
        ctx (.getContext canvas "2d")]
    (set! (.-fontKerning ctx) "none")
    ctx))

(defn- get-font-str
  [font-size font-family formats paragraph-type]
  (let [font-size (case paragraph-type
                    :h1 "30px"
                    :h2 "22px"
                    font-size)]
    (str (when (contains? formats :italic) "italic ")
         (when (contains? formats :bold) "700 ")
         font-size " " font-family)))

(defn- apply-font-style!
  [ctx font-size font-family formats paragraph-type]
  (set! (.-font ctx) (get-font-str font-size font-family formats paragraph-type)))

(defn measure
  "Helper function for `ruler`. Not private and __can__ be used directly, but generally should not be."
  ([ctx font-size font-family tab-size cache text]
   (measure ctx font-size font-family tab-size cache text #{} :body))
  ([ctx font-size font-family tab-size cache text formats paragraph-type]
   (let [formats-hash (hash formats)
         type-hash (hash paragraph-type)
         measure-grapheme (fn [grapheme]
                            (let [cache-key (str grapheme "-" formats-hash "-" type-hash)
                                  cache-val (aget cache cache-key)]
                              (if cache-val
                                cache-val
                                ;; Canvas `measureText` collapses tabs.
                                ;; See: https://stackoverflow.com/questions/37848455/measuretext-tab-character
                                (let [canvas-width (if (= "\t" grapheme)
                                                     tab-size
                                                     (do
                                                       (apply-font-style! ctx font-size font-family formats paragraph-type)
                                                       (.-width (.measureText ctx grapheme))))]
                                  (aset cache cache-key canvas-width)))))
         text-graphemes (if (= 1 (.-length text))
                          text
                          (->> text (graphemes) (map :grapheme)))]
     (->> text-graphemes (map measure-grapheme) (reduce +)))))

(defn get-measure-fn
  "Given valid CSS values for a font size and family (e.g. `\"12px\"` and `\"Arial\"`),
   as well as the tab size (e.g. `\"25px\"`), returns a function that takes a string
   and (optionally) a list of formats, and return the width (in pixels) that that
   string will **actually take up** in the real DOM. This is important for rendering
   as we split text into lines ourselves."
  [font-family font-size tab-size-px]
  (let [cache #js {}
        ctx (create-ctx!)]
    (set! (.-fontKerning ctx) "none")
    (fn [& args]
      (apply measure ctx font-size font-family tab-size-px cache args))))
