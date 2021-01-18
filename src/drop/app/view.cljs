(ns drop.app.view
  "Functions for converting from viewmodels to DOM elements (in the form of strings)."
  (:require [clojure.string :as str]
            [drop.editor.selection :as sel]
            [drop.editor.core :as c]))


(def caret-elem "<span class='text-caret'></span>")

(defn- caret-in-span? [span pid selection]
  (let [span-start (:start-offset span)
        span-end (+ (:start-offset span) (count (:text span)))
        caret (sel/caret selection)]
    (and (= pid (sel/caret-para selection))
         (>= caret span-start)
         (< caret span-end))))

(defn- <span>
  "Returns a DOM string of a <span> element with `text` inside it.
   Will return an empty string if there is no text.
   Used as a helper function by vm-span->dom"
  [text classes]
  (if (empty? text)
    ""
    (str "<span class='span " (str/join " " classes) "'>"
         text
         "</span>")))

(defn formats->classes [formats]
  (map #(str (name %) "-format") formats))

(defn split-span
  "Splits the span into three strings: everything before the start of the selection,
   everything inside the selection, and everything after the end of the selection.
   If any of those don't make sense (i.e. the whole span is inside the selection, or
   none of it is), empty strings will be returned."
  [span pid selection]
  ;; Maybe get rid of this if-not and add an explicit case in vm-span->dom for whether
  ;; or not the span interesects with any part of the selection? The "fall-through" here
  ;; is nice I guess, but it feels a lot like writing a special case implicitly but still
  ;; actually depending on it in practice.
  (if-not (or (= pid (-> selection :start :paragraph))
              (= pid (-> selection :end :paragraph)))
    ["", "", (:text span)]
    (let [text (:text span)
          start (- (-> selection :start :offset) (:start-offset span))
          end (- (-> selection :end :offset) (:start-offset span))]
      [(.substring text 0 start), (.substring text start end), (.substring text end)])))

(defn vm-span->dom
  "Convert viewmodel span to DOM element. Also responsible for rendering caret and range selection background."
  [span selection pid para-length in-selection?]
  (let [format-classes (formats->classes (:formats span))]
    (if in-selection?
      (let [[before-sel, inside-sel, after-sel] (split-span span pid selection)
            span-end-offset (+ (:start-offset span) (count (:text span)))]
        (str (<span> before-sel format-classes)

             (when (and (:backwards? selection) (caret-in-span? span pid selection))
               caret-elem)

             (<span> inside-sel (conj format-classes "range-selection"))

             (when (or (and (caret-in-span? span pid selection)
                            (not (:backwards? selection)))
                       ;; Handle case where caret is at the end of para (caret == len of paragraph)
                       (and (= (sel/caret selection) para-length span-end-offset)
                            (= (sel/caret-para selection) pid)))
               caret-elem)

             (<span> after-sel format-classes)))
      (<span> (:text span) format-classes))))

(defn vm-line->dom
  "Convert viewmodel line to DOM element."
  [line selection pid para-length in-selection?]
  (str "<div class='line'>"
       (apply str (map #(vm-span->dom % selection pid para-length in-selection?) (:spans line)))
       "</div>"))

(defn vm-para->dom
  "Convert viewmodel to DOM element."
  [viewmodel selection in-selection?]
  (let [pid (-> viewmodel :paragraph :uuid)
        para-length (c/text-len (:paragraph viewmodel))]
    (str "<div class='paragraph'>"
         (apply str (map #(vm-line->dom % selection pid para-length in-selection?) (:lines viewmodel)))
         "</div>")))

(defn vm-paras->dom
  "Convert the list of [[ParagraphViewModel]]s to DOM elements.
   Selection is provided in order to render the caret and highlighted text."
  [vm-paras selection]
  (let [selection-ongoing? (volatile! false)]
    (str "<div class='document'>"
         (apply str
           (map (fn [vm-para]
                  ;; Little state here, but it's not the worst.
                  ;; Set selection-ongoing? to true when the selection starts,
                  ;; and to false once the selection ends.
                  (when (= (-> vm-para :paragraph :uuid) (-> selection :start :paragraph))
                    (vreset! selection-ongoing? true))
                  (let [dom-string (vm-para->dom vm-para selection @selection-ongoing?)]
                    (when (= (-> vm-para :paragraph :uuid) (-> selection :end :paragraph))
                      (vreset! selection-ongoing? false))

                    dom-string))
                 vm-paras))
         "</div>")))

;; up/down nonsense
;; up/down have to be handled a little differently than other events because they
;; are depedent on where the document is split into lines.

;; TODO: delete this and use split-span above.
(defn split-span2
  "Splits the span into two at the paragraph offset, and return a vector of [before, after]."
  [span offset]
  (let [diff (- offset (:start-offset span))
        before (.substring (:text span) 0 diff)
        after (.substring (:text span) diff)]
    [(assoc span :text before), (assoc span :text after)]))

(defn spans-before-offset
  "Returns all spans in the line before the given paragraph offset."
  [line offset]
  (reduce (fn [spans-before, span]
            (let [span-end-offset (+ (count (:text span)) (:start-offset span))]
              (cond
                (<= span-end-offset offset)
                (conj spans-before span)

                (and (<= (:start-offset span) offset) (< offset span-end-offset))
                (conj spans-before (nth (split-span2 span offset) 0))

                :else
                (reduced spans-before))))
          [] (:spans line)))

(defn caret-line-idx
  "Returns index of the viewmodel line with the text caret inside of it."
  [viewmodels selection]
  {:pre [(sel/single? selection)]}
  (let [caret (sel/caret selection)
        vm (viewmodels (-> selection :start :paragraph))
        within-line? #(and (>= caret (:start-offset %)) (< caret (:end-offset %)))
        at-para-end? #(and (= caret (:end-offset %)) (= caret (c/text-len (:paragraph vm))))
        lines (:lines vm)]
    (loop [i 0]
      (when (> i (count lines)) (throw "Did not find line with caret inside it!"))

      (if (or (within-line? (lines i))
              (at-para-end? (lines i)))
        i
        (recur (inc i))))))

(defn line-with-caret
  "Returns the line in the viewmodel with the caret inside of it."
  [viewmodels selection]
  ((:lines (viewmodels (-> selection :start :paragraph))) (caret-line-idx viewmodels selection)))

(defn line-above-caret
  "Returns the line in the viewmodel immediately above the line with the caret inside of it.
   If there is no line above the current line, returns null."
  [viewmodels selection]
  (let [lines (:lines (viewmodels (-> selection :start :paragraph)))
        line-idx (dec (caret-line-idx viewmodels selection))]
    (get lines line-idx)))

(defn line-below-caret
  "Returns the line in the viewmodel immediately above the line with the caret inside of it.
   If there is no line above the current line, returns null."
  [viewmodels selection]
  (let [lines (:lines (viewmodels (-> selection :start :paragraph)))
        line-idx (inc (caret-line-idx viewmodels selection))]
    (get lines line-idx)))

(defn caret-px
  "Returns the horizontal offset of the text caret from the document's edge, in pixels."
  [selection line measure-fn]
  (let [spans-before-caret (spans-before-offset line (sel/caret selection))]
    (reduce (fn [width span]
              (+  width (measure-fn (:text span) (:formats span))))
            0 spans-before-caret)))

(defn chars-and-formats [span] (map #(hash-map :char %, :formats (:formats span)) (:text span)))

(defn nearest-line-offset-to-pixel
  [line target-px measure-fn]
  (let [chars-with-formats (->> (:spans line)
                                (map chars-and-formats (:spans line))
                                (flatten))]
    (loop [i 0
           offset (:start-offset line)
           offset-px 0
           prev-delta ##Inf]
      (if (= i (count chars-with-formats))
        (:end-offset line)
        (let [{:keys [char formats]} (nth chars-with-formats i)
              delta (js/Math.abs (- offset-px target-px))]
          (if (> delta prev-delta)
            (dec offset)
            (recur (inc i)
                   (inc offset)
                   (+ offset-px (measure-fn char formats))
                   delta)))))))

(defn down
  "Move the caret down into the next line. Returns a new selection."
  [{:keys [viewmodels] :as doc-state} measure-fn]
  (let [selection (sel/smart-collapse (:selection doc-state))
        line (line-with-caret viewmodels selection)
        next-line (line-below-caret viewmodels selection)]
    (if next-line
      (let [caret-offset-px (caret-px selection line measure-fn)
            next-line-offset (nearest-line-offset-to-pixel next-line caret-offset-px measure-fn)]
        (sel/set-single selection next-line-offset))
      selection)))

(defn up
  "Move the caret up into the next line. Returns a new selection."
  [{:keys [viewmodels] :as doc-state} measure-fn]
  (let [selection (sel/smart-collapse (:selection doc-state))
        line (line-with-caret viewmodels selection)
        prev-line (line-above-caret viewmodels selection)]
    (if prev-line
      (let [caret-offset-px (caret-px selection line measure-fn)
            next-line-offset (nearest-line-offset-to-pixel prev-line caret-offset-px measure-fn)]
        (sel/set-single selection next-line-offset))
      selection)))