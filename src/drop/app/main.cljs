(ns drop.app.main
  (:require [clojure.string :as str]
            [drop.editor.core :as c]
            [drop.editor.selection :as sel]
            [drop.editor.viewmodel :as vm]
            ["/drop/editor/CharRuler" :refer (CharRuler)]))

(defn formats->classes [formats]
  (map #(str (name %) "-format") formats))

(defn split-span
  [span pid selection]
  (if-not (or (= pid (-> selection :start :paragraph))
              (= pid (-> selection :start :paragraph)))
    ["", "", (:text span)]
    (let [text (:text span)
          start (- (-> selection :start :offset) (:start-offset span))
          end (- (-> selection :end :offset) (:start-offset span))]
      [(.substring text 0 start), (.substring text start end), (.substring text end)])))

(defn caret-in-span? [span selection]
  (let [span-start (:start-offset span)
        span-end (+ (:start-offset span) (count (:text span)))
        caret (sel/caret selection)]
    (or (and (>= caret span-start)
             (< caret span-end)))))

(defn <span>
  "Returns a DOM string of a <span> element with `text` inside it.
   Will return an empty string if there is no text."
  [text classes]
  (if (empty? text)
    ""
    (str "<span class='span " (str/join " " classes) "'>"
         text
         "</span>")))

(def caret-span "<span class='text-caret'></span>")

;; TODO: these can be moved to viewmodel NS (or maybe a `view` namespace? instead)

;; TODO: this really needs some cleanup....Good first step is preserve having an :end-offset field for ViewmodelSpans
(defn vm-span->dom
  "Convert viewmodel span to DOM element."
  [span pid para-length selection]
  (let [format-classes (formats->classes (:formats span))
        [before-sel, inside-sel, after-sel] (split-span span pid selection)]
    (str (<span> before-sel format-classes)
         (when (and (:backwards? selection) (caret-in-span? span selection))
           caret-span)
         (<span> inside-sel (conj format-classes "range-selection"))
         (when (or (and (not (:backwards? selection))
                        (caret-in-span? span selection))
                   (= (sel/caret selection) para-length (+ (:start-offset span) (count (:text span)))))
           caret-span)
         (<span> after-sel format-classes))))

(defn vm-line->dom
  "Convert viewmodel line to DOM element."
  [line pid para-length selection]
  (str "<div class='line'>"
       (apply str (map #(vm-span->dom % pid para-length selection) (:spans line)))
       "</div>"))

(defn vm-para->dom
  "Convert viewmodel to DOM element."
  [viewmodel selection]
  (let [pid (-> viewmodel :paragraph :uuid)
        para-length (c/text-len (:paragraph viewmodel))]
    (str "<div class='paragraph'>"
         (apply str (map #(vm-line->dom % pid para-length selection) (:lines viewmodel)))
         "</div>")))

(defn vm-paras->dom
  "Convert the list of [[ParagraphViewModel]]s to DOM elements.
   Selection is provided in order to render the caret and highlighted text."
  [vm-paras selection]
  (str "<div class='document'>"
       (apply str (map #(vm-para->dom % selection) vm-paras))
       "</div>"))

(defn ruler-for-element
  "Returns a `CharRuler` object for the given DOM element."
  [elem]
  (let [style (js/getComputedStyle elem)]
    (CharRuler.
     (.getPropertyValue style "font-size")
     (.getPropertyValue style "font-family"))))

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))
(def ruler (ruler-for-element fake-editor))

(def test-para
  (c/paragraph [(c/run "Hello world, this is an example of a paragraph ")
                (c/run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                (c/run "Don't know what else to say. Hmmmm..." #{:bold})]))

(def doc-state (atom {:doc (c/document [test-para])
                      :selection (sel/selection [(:uuid test-para) 0] [(:uuid test-para) 179])
                      :pids->viewmodels {(:uuid test-para) (vm/from-para test-para 200 ruler)}}))

(defn sync-dom [elem doc-state ruler]
  (let [state @doc-state
        doc (:doc state)
        sel (:selection state)
        vm-paras (map #(vm/from-para % 200 ruler) (:children doc))]
    (set! (.-innerHTML elem) (vm-paras->dom vm-paras sel))))

(defn main []
  (sync-dom fake-editor doc-state ruler))

(defn ^:dev/after-load reload []
  (main))

;; TODO: handle left and right events
;; TODO: handle up and down events
;; TODO: handle input and deletion

