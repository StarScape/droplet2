(ns slate.renderer.utils)

(defn font-str
  ([font-size font-family formats paragraph-type]
   (let [font-size (case paragraph-type
                     :h1 "30px"
                     :h2 "22px"
                     (str (* js/window.devicePixelRatio font-size) "px"))]
     (str (when (contains? formats :italic) "italic ")
          (when (contains? formats :bold) "700 ")
          font-size " " font-family)))
  ([font-size font-family]
   (font-str font-size font-family #{} :body)))
