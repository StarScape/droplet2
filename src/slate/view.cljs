(ns slate.view
  "Functions for converting from viewmodels to DOM elements (in the form of strings),
   as well as some utilities for dealing with the editor's DOM layer in general."
  (:require [clojure.string :as str]
            [slate.selection :as sel]
            [slate.core :as sl]
            [slate.dll :as dll]))

;; Utility functions
(defn match-elem-in-path
  "Returns the first element in the event's path that satisfies the given selector string
   (e.g. '.someClass'), or null if one is not found. Will throw a SYNTAX_ERR if the selector
   string is invalid."
  [e selector-string]
  (letfn [(selector-matches? [elem sstring]
            (when (.-matches elem) (.matches elem sstring)))]
    (first (filter #(selector-matches? % selector-string) (.-path e)))))

;; Dynamic var to indicate whether the selection is still ongoing.
;; This is something we need to keep track of between paragraphs
;; so it winds up a little more elegant to make careful use of a
;; dynamic var, rather than returning a selection-ongoing? value
;; up and down a bunch of different cycles of the callstack.
(declare ^:dynamic *selection-ongoing?*)

(def caret-elem "<span class='text-caret'></span>")

(defn- caret-in-span? [span pid selection]
  (let [span-start (:start-offset span)
        span-end (+ (:start-offset span) (count (:text span)))
        caret (sel/caret selection)]
    (and (= pid (sel/caret-para selection))
         (>= caret span-start)
         (< caret span-end))))

(defn formats->classes [formats]
  (map #(str (name %) "-format") formats))

(defn- <span>
  "Returns a DOM string of a <span> element with `text` inside it.
   Will return an empty string if text is nil. Used as a helper function
   by vm-span->dom."
  [text classes]
  (if (nil? text)
    ""
    (str "<span class='span " (str/join " " classes) "'>"
         ;; If text is an empty string, add a space.
         ;; This will only ever happen in the case of an empty paragraph.
         (or (not-empty text) " ")
         "</span>")))

(defn split-span
  "Splits the span into three strings: everything before the start of the selection,
   everything inside the selection, and everything after the end of the selection.
   Returns a map of each with keys :before-sel, :inside-sel, :after-sel.
   If any of those don't make sense (i.e. the whole span is inside the selection, or
   none of it is), those fields will be nil."
  [span pid selection selection-ongoing?]
  ;; Maybe get rid of this if-not and add an explicit case in vm-span->dom for whether
  ;; or not the span interesects with any part of the selection? The "fall-through" here
  ;; is nice I guess, but it feels a lot like writing a special case implicitly but still
  ;; actually depending on it in practice.
  (if-not (or (= pid (-> selection :start :paragraph))
              (= pid (-> selection :end :paragraph)))
    (if selection-ongoing?
      {:inside-sel (:text span)}
      {:after-sel (:text span)})
    (let [text (:text span)
          p1 (-> selection :start :paragraph)
          p2 (-> selection :end :paragraph)
          start (if (and (= pid p1) (not selection-ongoing?))
                  (- (-> selection :start :offset) (:start-offset span))
                  0)
          end (if (= pid p2)
                (- (-> selection :end :offset) (:start-offset span))
                (count text))]
      {:before-sel (not-empty (.substring text 0 start))
       :inside-sel (not-empty (.substring text start end))
       :after-sel (not-empty (.substring text end))})))

(defn vm-span->dom
  "Convert viewmodel span to DOM element. Also responsible for rendering caret and range selection background."
  [span selection pid para-length]
  (let [format-classes
        (formats->classes (:formats span))

        {:keys [before-sel, inside-sel, after-sel]}
        (split-span span pid selection *selection-ongoing?*)

        span-end-offset
        (+ (:start-offset span) (count (:text span)))

        start-in-span?
        (and (= pid (-> selection :start :paragraph))
             (>= (-> selection :start :offset) (:start-offset span))
             (< (-> selection :start :offset) span-end-offset))

        end-in-span?
        (or (and (= pid (-> selection :end :paragraph))
                 (< (-> selection :end :offset) span-end-offset))
            (and (= pid (-> selection :end :paragraph))
                 (= span-end-offset (-> selection :end :offset) para-length)))

        still-ongoing?
        (or (and *selection-ongoing?* (not end-in-span?))
            (and start-in-span? (not end-in-span?)))]
    (set! *selection-ongoing?* still-ongoing?)
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

         (<span> after-sel format-classes))))

(defn vm-line->dom
  "Convert viewmodel line to DOM element."
  [line selection pid para-length]
  (str "<div class='line'>"
       (apply str (map #(vm-span->dom % selection pid para-length) (:spans line)))
       "</div>"))

(defn vm-para->dom
  "Convert viewmodel to DOM element."
  [viewmodel selection]
  (let [lines (:lines viewmodel)
        pid (-> viewmodel :paragraph :uuid)
        para-length (sl/len (:paragraph viewmodel))]
    (str "<div class='paragraph' id='" (:uuid (:paragraph viewmodel)) "'>"
         (apply str (map #(vm-line->dom % selection pid para-length) lines))
         "</div>")))

(defn vm-paras->dom
  "Convert the list of [[ParagraphViewModel]]s to DOM elements.
   Selection is provided in order to render the caret and highlighted text."
  [vm-paras selection]
  (binding [*selection-ongoing?* false]
    (str "<div class='document'>"
         (apply str (map #(vm-para->dom % selection) vm-paras))
         "</div>")))

;; up/down nonsense
;; up/down have to be handled a little differently than other events because they
;; are depedent on where the document is split into lines.

;; TODO: delete this and use split-span above (somehow - unify the two abstracshuns if possible).
;; A thought on how to do that: instead returns a map with keys :before, :after, and optionally
;; :inside, and simply test for the existence of :inside in split-span - in this function it can
;; be assumed it doesn't exist, since we're splitting at a single offset.
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
        at-para-end? #(and (= caret (:end-offset %)) (= caret (sl/len (:paragraph vm))))
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
  ((:lines (viewmodels (sel/caret-para selection))) (caret-line-idx viewmodels selection)))

(defn line-above-caret
  "Returns the line in the viewmodel immediately above the line with the caret inside of it.
   If there is no line above the current line, returns null."
  [viewmodels selection]
  (let [lines (:lines (viewmodels (sel/caret-para selection)))
        line-idx (dec (caret-line-idx viewmodels selection))]
    (get lines line-idx)))

(defn line-below-caret
  "Returns the line in the viewmodel immediately above the line with the caret inside of it.
   If there is no line above the current line, returns null."
  [viewmodels selection]
  (let [lines (:lines (viewmodels (sel/caret-para selection)))
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

;; TODO: I think these functions could be moved to the `events` namespace (once it's created),
;; and we could just keep the helper functions it calls here, or better yet, move them into the
;; measurement namespace.
;;
;; TODO: these would both be much more elegant if we kept a DLL of the viewmodels instead
(defn down
  "Move the caret down into the next line. Returns a new selection."
  [{:keys [doc viewmodels] :as doc-state} measure-fn]
  (let [selection (sel/smart-collapse (:selection doc-state))
        para-uuid (sel/caret-para selection)
        viewmodel (get viewmodels para-uuid)
        line (line-with-caret viewmodels selection)

        ; last line in para?
        last-line? (= line (peek (:lines viewmodel)))
        next-line (if (not last-line?)
                    (line-below-caret viewmodels selection)
                    (-> (:children doc)
                        (dll/next para-uuid)
                        (:uuid)
                        (viewmodels)
                        (:lines)
                        (first)))
        new-uuid (if last-line?
                   (:uuid (dll/next (:children doc) para-uuid))
                   para-uuid)]
    (if next-line
      (let [caret-offset-px (caret-px selection line measure-fn)
            next-line-offset (nearest-line-offset-to-pixel next-line caret-offset-px measure-fn)]
        (sel/selection [new-uuid next-line-offset]))
      selection)))

(defn up
  "Move the caret up into the next line. Returns a new selection."
  [{:keys [doc viewmodels] :as doc-state} measure-fn]
  (let [selection (sel/smart-collapse (:selection doc-state))
        para-uuid (sel/caret-para selection)
        viewmodel (get viewmodels para-uuid)
        line (line-with-caret viewmodels selection)
        first-line? (= line (first (:lines viewmodel)))
        prev-line (if (not first-line?)
                    (line-above-caret viewmodels selection)
                    (-> (:children doc)
                        (dll/prev para-uuid)
                        (:uuid)
                        (viewmodels)
                        (:lines)
                        (peek)))
        new-uuid (if first-line?
                   (:uuid (dll/prev (:children doc) para-uuid))
                   para-uuid)]
    (if prev-line
      (let [caret-offset-px (caret-px selection line measure-fn)
            next-line-offset (nearest-line-offset-to-pixel prev-line caret-offset-px measure-fn)]
        (sel/selection [new-uuid next-line-offset]))
      selection)))

(defn shift+down
  "Move the caret down into the next line. Returns a new selection."
  [{:keys [selection] :as doc-state} measure-fn]
  ;; TODO: go to end of line if down-selection == selection (aka it's the last para)
  (let [down-selection (down doc-state measure-fn)
        para (sel/caret-para down-selection)
        offset (sel/caret down-selection)
        down-caret {:paragraph para, :offset offset}]
    (if (and (:backwards? selection) (sel/range? selection))
      (if (and (< offset (-> selection :end :offset)) (= para (-> selection :end :paragraph)))
        (assoc selection :start down-caret, :backwards? true)
        (assoc selection :start (:end selection), :end down-caret, :backwards? false))
      (assoc selection :end down-caret, :backwards? false))))

(defn shift+up
  "Move the caret up into the next line. Returns a new selection."
  [{:keys [selection] :as doc-state} measure-fn]
  ;; TODO: go to start of line if down-selection == selection (aka it's the first para)
  (let [up-selection (up doc-state measure-fn)
        para (sel/caret-para up-selection)
        offset (sel/caret up-selection)
        up-caret {:paragraph para, :offset offset}]
    (if (and (not (:backwards? selection)) (sel/range? selection))
      (if (and (< offset (-> selection :start :offset)) (= para (-> selection :start :paragraph)))
        (assoc selection :start up-caret, :end (:start selection), :backwards? true)
        (assoc selection :end up-caret, :backwards? false))
      (assoc selection :start up-caret, :backwards? true))))

(defn calc-line-height
  "Returns the actual *rendered* line height given a paragraph DOM element, in pixels."
  [paragraph-dom-elem]
  (let [first-dom-line (.querySelector paragraph-dom-elem ".line")]
    (when (nil? first-dom-line)
      (throw "Error in calc-line-height: the provided paragraph DOM element has no lines! This should never happen."))

    (-> first-dom-line
        (js/getComputedStyle)
        (.getPropertyValue "height")
        (js/parseFloat))))

(defn clicked-location
  "Returns a new single selection set to where a click occured within a paragraph.
   Parameters:

   - client-x, client-y: x and y of the click relative to browser viewport
   - paragraph-dom-elem: DOM element of the paragraph clicked
   - viewmodel: viewmodel associated with that paragraph
   - measure-fn: measure-fn for the document at the current font and font-size"
  [client-x client-y paragraph-dom-elem {:keys [lines paragraph]} measure-fn]
  (let [line-height (calc-line-height paragraph-dom-elem)
        dom-elem-rect (.getBoundingClientRect paragraph-dom-elem)
        px (- client-x (.-x dom-elem-rect))
        py (- client-y (.-y dom-elem-rect))
        line (cond
               (< py 0) (first lines)
               (>= py (.-height dom-elem-rect)) (peek lines)
               :else (nth lines (int (/ py line-height))))
        offset (cond
                 (< px 0) (:start-offset line)
                 (> px (.-width dom-elem-rect)) (:end-offset line)
                 :else (nearest-line-offset-to-pixel line px measure-fn))]
    (sel/selection [(:uuid paragraph) offset])))

(defn find-overlapping-paragraph
  "Finds paragraph that client-y is overlapping with in the y-axis and returns its UUID."
  [paragraphs client-y prev-event]
  ;; (println "Finding overlap!")
  (let [para->bounds (fn [{:keys [uuid]}]
                       (let [bounding-rect (.getBoundingClientRect (.getElementById js/document (str uuid)))]
                         {:uuid uuid
                          :top (.-top bounding-rect)
                          :bottom (.-bottom bounding-rect)}))
        first-para-bounds (-> paragraphs first para->bounds)
        last-para-bounds (-> paragraphs peek para->bounds)]
    (cond
      (< client-y (:top first-para-bounds))
      (-> paragraphs first :uuid)

      (> client-y (:bottom last-para-bounds))
      (-> paragraphs peek :uuid)

      ;; TODO: one way to further optimize this would be to cache the last drag event
      ;; and use that. I have a feeling that might be more trouble than its worth though,
      ;; unless we actually start to notice real perf problems with this.

      ;; Find paragraph that client-y is overlapping with in the y axis
      :else
      (let [overlaps? #(<= (:top %) client-y (:bottom %))
            prev-para (match-elem-in-path prev-event ".paragraph")
            ;; Start searching at either the paragraph the
            ;; previous event took place in, or the first one
            start-uuid (if prev-para (uuid (.-id prev-para)) (:uuid (first paragraphs)))
            advance (if (and prev-para (neg? (- client-y (.-y prev-event))))
                         dll/prev
                         dll/next)]
        ;; TODO: okay, definitely some errors here...
        (loop [p (get paragraphs start-uuid)]
          (when (nil? p) (throw "I don't think this should ever happen..."))

          (if (overlaps? (para->bounds p))
            (:uuid p)
            (recur (advance paragraphs p))))))))

;; TODO: change this to handle mousevents that are outside the paragraph.
(defn mouse-event->selection
  "Takes a MouseEvent object and the collection of viewmodels and, if its clientX and clientY
   are inside a paragraph, returns a single selection set to the paragraph and offset where the
   mouse pointer is at.

   If the mouse is *outside* any paragraph, it will return whatever paragraph is closest.
   For example, clicking off the right of a paragraph P (not actually inside of it) will return
   a selection inside of P with the offset set to the end of the line you clicked to the right of.
   Likewise, clicking above or below the first or line paragraphs will return selections in the first
   or last lines, respectively, with an offset based on the x coordinate of the mouse.

   Optionally, this function takes a `last-para` function, which is the last [[Paragraph]] that the mouse is
   *known* to have passed through. This is due to a limitation of the DOM (and how we handle events): If
   the MouseEvent does not have a '.paragraph' element in its event path, the only way to figure out the
   nearest paragraph is to get its bounding rect and compare. We don't want to have to search through the
   entire list of paragraphs and figure out which one is intersecting the mouse, so instead we can pass in
   a paragraph to start searching at."
  ([event {:keys [viewmodels doc]} measure-fn last-event]
   (let [paragraph-in-path (match-elem-in-path event ".paragraph")
         paragraph-uuid (if paragraph-in-path
                          (.-id paragraph-in-path)
                          (find-overlapping-paragraph (:children doc) (.-y event) last-event))
        ; The paragraph might have re-rendered since this MouseEvent was fired, and thus the
        ; paragraph element in the path may not actually be present in the DOM. It's ID/UUID
        ; will still be valid, however, so we can just grab the current element like this.
         paragraph-elem (.getElementById js/document paragraph-uuid)
         vm (get viewmodels (uuid (.-id paragraph-elem)))
         sel (clicked-location (.-x event) (.-y event) paragraph-elem vm measure-fn)]
     sel))
  ([event doc-state measure-fn]
   (mouse-event->selection event doc-state measure-fn nil)))

(defn- drag-direction
  [started-at currently-at mousedown-event mousemove-event]
  (if (= (sel/caret-para started-at) (sel/caret-para currently-at))
    (if (> (sel/caret currently-at) (sel/caret started-at))
      :forward
      :backward)
    (if (pos? (- (.-y mousemove-event) (.-y mousedown-event)))
      :forward
      :backward)))

(defn drag
  [mousedown-event mousemove-event doc-state measure-fn]
  (let [started-at (mouse-event->selection mousedown-event doc-state measure-fn)
        currently-at (mouse-event->selection mousemove-event doc-state measure-fn mousedown-event)
        dir (drag-direction started-at currently-at mousedown-event mousemove-event)]
    (if (= dir :forward)
      (sel/selection [(sel/caret-para started-at) (sel/caret started-at)]
                     [(sel/caret-para currently-at) (sel/caret currently-at)])
      (sel/selection [(sel/caret-para currently-at) (sel/caret currently-at)]
                     [(sel/caret-para started-at) (sel/caret started-at)]
                     true))))
