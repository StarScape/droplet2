(ns slate.view
  "Functions for converting from viewmodels to DOM elements (in the form of strings),
   as well as some utilities for dealing with the editor's DOM layer in general."
  (:require [clojure.string :as str]
            [slate.model.dll :as dll]
            [slate.model.selection :as sel]
            [slate.model.common :as sl]
            [slate.model.editor-state :as es]
            [slate.model.navigation :as nav]
            [slate.viewmodel :as vm]
            [slate.utils :refer [paragraph-type->css-class formats->css-classes]]))

;; Utility functions
(defn paragraph-index->dom-id
  [paragraph-index]
  (str "p-" paragraph-index))

(defn dom-id->paragraph-index
  [dom-id]
  (dll/big-dec (.substring dom-id 2)))

(defn escape-dom-id
  "Escapes `.` char in `dom-id` so it can be used in a query selector."
  [dom-id]
  (str/replace dom-id "." "\\."))

(defn get-paragraph-dom-elem ;; TODO: correct all references to make sure they are passed IDXs
  [editor-elem paragraph-index]
  {:pre [(some? paragraph-index)]}
  (let [dom-id (paragraph-index->dom-id paragraph-index)]
    (.querySelector editor-elem (str "#" (escape-dom-id dom-id)))))

(defn match-elem-in-path
  "Returns the first element in the event's path that satisfies the given selector string
   (e.g. '.someClass'), or null if one is not found. Will throw a SYNTAX_ERR if the selector
   string is invalid."
  [e selector-string]
  (when e
    (letfn [(selector-matches? [elem sstring]
              (when (.-matches elem) (.matches elem sstring)))]
      (first (filter #(selector-matches? % selector-string) (.-path e))))))

(defn elem-width
  "Returns the width of the UIState's dom element, in pixels."
  [ui-state]
  (.-width (.getBoundingClientRect (:dom-elem ui-state))))

(defn font-size
  "Returns the element's font size, in pixels, _as a number_, not a string."
  [dom-elem]
  (let [style (js/getComputedStyle dom-elem)
        font-size-str (.getPropertyValue style "font-size")]
    (-> font-size-str
        (subs 0 (- (.-length font-size-str) 2))
        (js/parseInt))))

(defn- split-elem-on-child!
  "Takes an element (e.g. a `<ol>`) and one of its children (e.g. an `<li>`) and clones
   the parent element into two, inserting everything up to and including the target child
   element into the left parent element.

   <ol>
     <li>A</li>
     <li>B</li>
     <li>C</li>
     <li>D</li>
   </ol>

   split ol at <li>B</li> =

   <ol>
     <li>A</li>
     <li>B</li>
   </ol>
   <ol>
     <li>C</li>
     <li>D</li>
   </ol>"
  [elem-to-split child-elem-to-split-at]
  (let [left-elem (.cloneNode elem-to-split)]
    (loop [node (.-firstElementChild elem-to-split)]
      (let [next-sibling (.-nextElementSibling node)]
        (.appendChild left-elem node)
        (if (= node child-elem-to-split-at)
          left-elem
          (recur next-sibling))))
    (.insertBefore (.-parentNode elem-to-split) left-elem elem-to-split)))

(defn- margins
  "Returns the elements margins as a map of {:top, :bottom, :left, :right}."
  [dom-elem]
  (let [style (js/getComputedStyle dom-elem)]
    {:top (js/parseFloat (.-marginTop style))
     :bottom (js/parseFloat (.-marginBottom style))
     :left (js/parseFloat (.-marginLeft style))
     :right (js/parseFloat (.-marginRight style))}))

(def caret-elem "<span class='slate-text-caret'></span>")

(defn- caret-in-span? [span pid selection]
  (let [span-start (:start-offset span)
        span-end (+ (:start-offset span) (count (:text span)))
        caret (sel/caret selection)]
    (and (= pid (sel/caret-para selection))
         (>= caret span-start)
         (< caret span-end))))

(defn- escape-html
  [text]
  (.. text
      (replaceAll "&", "&amp;")
      (replaceAll "<", "&lt;")
      (replaceAll ">", "&gt;")
      (replaceAll "\"", "&quot;")
      (replaceAll "'", "&#039;")))

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
         (escape-html (or (not-empty text) " "))
         "</span>")))

(defn- split-span
  "Splits the span into three strings: everything before the start of the selection,
   everything inside the selection, and everything after the end of the selection.
   Returns a map of each with keys :before-sel, :inside-sel, :after-sel.
   If any of those don't make sense (i.e. the whole span is inside the selection, or
   none of it is), those fields will be nil."
  [span paragraph-idx selection]
  ;; Maybe get rid of this if-not and add an explicit case in vm-span->dom for whether
  ;; or not the span interesects with any part of the selection? The "fall-through" here
  ;; is nice I guess, but it feels a lot like writing a special case implicitly but still
  ;; actually depending on it in practice.
  (let [start-para (-> selection :start :paragraph)
        start-offset (-> selection :start :offset)
        end-para (-> selection :end :paragraph)
        end-offset (-> selection :end :offset)]
    (if-not (or (= paragraph-idx start-para)
                (= paragraph-idx end-para))
      ;; Neither start or end of selection is in the paragraph containing this span
      (if (and (.gt paragraph-idx start-para)
               (.lt paragraph-idx end-para))
        ;; Paragraph containing span is between the start and
        ;; end paragraphs of the selection, i.e. wholly inside
        ;; of it, and therefore the whole span is too.
        {:inside-sel (:text span)}
        ;; Paragraph is not within selection at all. The use of :after-sel
        ;; to return the whole text of the span here is arbitrary. We could
        ;; just as well use :before-sel.
        {:after-sel (:text span)})
      ;; Start or end of selection is in the paragraph containing this span
      (let [text (:text span)
            inside-start (if (and (= paragraph-idx start-para)
                                  (>= start-offset (:start-offset span)))
                           (- start-offset (:start-offset span))
                           0)
            inside-end (if (= paragraph-idx end-para)
                         (- end-offset (:start-offset span))
                         (count text))]
        {:before-sel (not-empty (.substring text 0 inside-start))
         :inside-sel (not-empty (.substring text inside-start inside-end))
         :after-sel (not-empty (.substring text inside-end))}))))

(defn- vm-span->dom
  "Convert slate.viewmodel/Span to a DOM element. Also responsible for rendering caret and range selection background.
   Returns HTML string."
  [span selection paragraph-idx para-length]
  (let [format-classes
        (formats->css-classes (:formats span))

        {:keys [before-sel, inside-sel, after-sel]}
        (split-span span paragraph-idx selection)

        span-end-offset
        (+ (:start-offset span) (count (:text span)))]
    (str (<span> before-sel format-classes)

         (when (and (:backwards? selection) (caret-in-span? span paragraph-idx selection))
           caret-elem)

         (<span> inside-sel (conj format-classes "slate-range-selection"))

         (when (or (and (caret-in-span? span paragraph-idx selection)
                        (not (:backwards? selection)))
                    ;; Handle case where caret is at the end of para (caret == len of paragraph)
                   (and (= (sel/caret selection) para-length span-end-offset)
                        (= (sel/caret-para selection) paragraph-idx)))
           caret-elem)

         (<span> after-sel format-classes))))

(defn- vm-line->dom
  "Convert viewmodel line to DOM element. Returns HTML string."
  [line selection paragraph-idx para-length]
  (str "<div class='line'>"
       (apply str (map #(vm-span->dom % selection paragraph-idx para-length) (:spans line)))
       "</div>"))

(defn- vm-para->dom
  "Convert ParagraphViewModel to DOM element. Returns HTML string."
  [{:keys [lines paragraph-type paragraph-index] :as paragraph-vm} selection]
  (let [classes ["paragraph" (paragraph-type->css-class paragraph-type)]]
    (str "<div class='" (str/join " " classes) "' id='p-" paragraph-index "'>"
         (apply str (map #(vm-line->dom % selection paragraph-index (:length paragraph-vm)) lines))
         "</div>")))

(defn- nearest-top-level-ancestor
  [elem editor-elem]
  (loop [e elem]
    (if (= (.-parentElement e) editor-elem)
      e
      (recur (.-parentElement e)))))

(defn merge-list-elems-if-needed!
  "Takes two ADJACENT elements and, if they are both of the same list
   container type (e.g. are both a separate <ul> or <ol>), merges them together."
  [prev-elem next-elem]
  (when (and prev-elem next-elem
             (not= prev-elem next-elem)
             (= (.-tagName prev-elem) (.-tagName next-elem))
             (or (= "OL" (.-tagName prev-elem))
                 (= "UL" (.-tagName prev-elem))))
    (let [prev-elem-content (.-innerHTML prev-elem)
          next-elem-content (.-innerHTML next-elem)
          prev-elem-new-content (str prev-elem-content next-elem-content)]
      (.remove next-elem)
      (set! (.-innerHTML prev-elem) prev-elem-new-content))))

(defn- insert-list-para!
  "Implementation for inserting both a :ul or an :ol paragraph (the only difference is the tag name of the
   containing element, either <ul> or <ol>). Automatically wraps the element in `<ul>` / `<ol>` __if__ there is
   not already one present for the current run of list paragraphs, otherwise will insert into that `<ul>` / `<ol>`."
  [list-type editor-elem paragraph-idx viewmodel {:keys [doc selection] :as _es}]
  {:pre [(or (= list-type :ol)
             (= list-type :ul))]}
  (let [tag-name (name list-type)
        rendered-paragraph (vm-para->dom viewmodel selection)
        next-p-dom-elem (when-let [next-idx (dll/next-index (:children doc) paragraph-idx)]
                          (get-paragraph-dom-elem editor-elem next-idx))
        next-p-in-same-list? (when next-p-dom-elem
                               (= (.. next-p-dom-elem -parentNode -tagName toLowerCase) tag-name))
        prev-p-dom-elem (when-let [prev-idx (dll/prev-index (:children doc) paragraph-idx)]
                          (get-paragraph-dom-elem editor-elem prev-idx))
        prev-p-in-same-list? (when prev-p-dom-elem
                               (= (.. prev-p-dom-elem -parentNode -tagName toLowerCase) tag-name))
        p-outer-html (if (or next-p-in-same-list?
                             prev-p-in-same-list?)
                       rendered-paragraph
                       (str "<" tag-name ">" rendered-paragraph "</" tag-name ">"))]
    (cond
      ;; insert into (U|O)L after prev-p-dom-elem
      (and prev-p-dom-elem prev-p-in-same-list?)
      (.insertAdjacentHTML prev-p-dom-elem "afterend" p-outer-html)

      ;; insert into (U|O)L before next-p-dom-elem
      (and next-p-dom-elem next-p-in-same-list?)
      (.insertAdjacentHTML next-p-dom-elem "beforebegin" p-outer-html)

      ;; Neither prev nor next is a list paragraph but there is a prev/next paragraph in the DOM currently.
      ;; Insert directly before the _nearest ancestor_ of next-p-elem that is top-level within document elem.
      (some? prev-p-dom-elem)
      (.insertAdjacentHTML (nearest-top-level-ancestor prev-p-dom-elem editor-elem) "afterend" p-outer-html)

      (some? next-p-dom-elem)
      (.insertAdjacentHTML (nearest-top-level-ancestor next-p-dom-elem editor-elem) "beforebegin" p-outer-html)

      ;; No next elem, append to document end
      :else
      (.insertAdjacentHTML editor-elem "afterbegin" p-outer-html))))

(defmulti insert-para!
  "Renders and inserts the paragraph into the DOM."
  {:arglists '([editor-elem paragraph-idx viewmodel doc selection])}
  (fn [_ _ vm _ _] (:paragraph-type vm)))

(defmethod insert-para! :default
  [editor-elem paragraph-idx viewmodel {:keys [doc selection] :as _editor-state}]
  (let [rendered-paragraph (vm-para->dom viewmodel selection)
        paragraph-elem (js/document.createElement "p")
        elem-to-insert-after (when-let [next-para-idx (dll/prev-index (:children doc) paragraph-idx)]
                               (get-paragraph-dom-elem editor-elem next-para-idx))
        split-prev-elem-parent-if-needed! (fn [prev-elem]
                                            (let [parent-elem (.. prev-elem -parentElement)
                                                  parent-tag (.. parent-elem -tagName toLowerCase)]
                                              (if (and (or (= "ol" parent-tag) (= "ul" parent-tag))
                                                       (= parent-elem (some-> prev-elem .-nextElementSibling .-parentElement)))
                                                (split-elem-on-child! parent-elem prev-elem)
                                                prev-elem)))]
    (if elem-to-insert-after
      (.insertAdjacentElement (-> elem-to-insert-after
                                  (split-prev-elem-parent-if-needed!)
                                  (nearest-top-level-ancestor editor-elem))
                              "afterend"
                              paragraph-elem)
      (.insertAdjacentElement editor-elem "afterbegin" paragraph-elem))
    (set! (.-outerHTML paragraph-elem) rendered-paragraph)))

(defmethod insert-para! :ul
  [editor-elem paragraph-idx viewmodel editor-state]
  (insert-list-para! :ul editor-elem paragraph-idx viewmodel editor-state))

(defmethod insert-para! :ol
  [editor-elem paragraph-idx viewmodel editor-state]
  (insert-list-para! :ol editor-elem paragraph-idx viewmodel editor-state))

(defn insert-all!
  "Inserts __all__ paragraphs in the list into the DOM. __Note that the
   `paragraph-indices` list MUST be sorted!__

   If no paragraph-indices list is supplied, it will clear the editor-element
   and insert __every__ paragraph in the document. This should only be used in
   a fewspecial circumstances, like initial render, otherwise document should
   be updated piecewise using `(insert|remove|update)-para!`."
  ([editor-elem paragraph-indices viewmodel editor-state]
   (doseq [idx paragraph-indices]
     (insert-para! editor-elem idx (get viewmodel idx) editor-state)))
  ([editor-elem viewmodel editor-state]
   (set! (.-innerHTML editor-elem) "")
   (insert-all! editor-elem (dll/all-indices (-> editor-state :doc :children)) viewmodel editor-state)))

(defmulti remove-para!
  "Removes the paragraph with `uuid` from the DOM."
  {:arglists '([uuid editor-state prev-editor-state])}
  (fn [_ uuid _ {:keys [doc]}] (:type (get (:children doc) uuid))))

(defmethod remove-para! :default
  [editor-elem paragraph-idx _ _]
  ;; TODO: check to see if the paragraphs before and after uuid are both list paragraphs
  ;; of the same type, and merge if so
  (let [p-elem (get-paragraph-dom-elem editor-elem paragraph-idx)
        prev-elem (.-previousElementSibling p-elem)
        next-elem (.-nextElementSibling p-elem)]
    (.remove p-elem)
    (merge-list-elems-if-needed! prev-elem next-elem)))

(defn- remove-list-para!
  [editor-elem paragraph-idx]
  (let [elem (get-paragraph-dom-elem editor-elem paragraph-idx)
        prev-elem (.-previousElementSibling elem)
        next-elem (.-nextElementSibling elem)
        parent-elem (.-parentElement elem)]
    (if (and (not= parent-elem editor-elem)
             (zero? (.. parent-elem -children -length)))
      ;; Only item in the list, remove parent so there is no dangling <ul>/<ol>
      (.remove (.-parentElement elem))
      ;; Other items in list, keep <ul>/<ol>
      (.remove elem))
    (merge-list-elems-if-needed! prev-elem next-elem)))

(defmethod remove-para! :ul
  [editor-elem paragraph-idx editor-state prev-state]
  (remove-list-para! editor-elem paragraph-idx))

(defmethod remove-para! :ol
  [editor-elem paragraph-idx editor-state prev-state]
  (remove-list-para! editor-elem paragraph-idx))

;; Currently all paragraph types update the same, no special logic/multimethod needed
(defn update-para! [editor-elem paragraph-idx viewmodel editor-state prev-state]
  (let [paragraph-elem (get-paragraph-dom-elem editor-elem paragraph-idx)
        type-changed? (not= (-> editor-state :doc :children (get paragraph-idx) :type)
                            (-> prev-state :doc :children (get paragraph-idx) :type))]
    (if type-changed?
      (do
        (remove-para! editor-elem paragraph-idx editor-state prev-state)
        (insert-para! editor-elem paragraph-idx viewmodel editor-state))
      (set! (.-outerHTML paragraph-elem) (vm-para->dom viewmodel (:selection editor-state))))))

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
        vm (get viewmodels (sel/caret-para selection)) ;; TODO: should arg be (sel/caret-para selection) instead?
        within-line? #(and (>= caret (:start-offset %)) (< caret (:end-offset %)))
        at-para-end? #(and (= caret (:end-offset %)) (= caret (:length vm)))
        lines (:lines vm)]
    (loop [i 0]
      (when (> i (count lines)) (throw (js/Error. "Did not find line with caret inside it!")))

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
  "Returns the line in the viewmodel immediately below the line with the caret inside of it.
   If there is no line below the current line, returns nil."
  [viewmodels selection]
  (let [lines (:lines (viewmodels (sel/caret-para selection)))
        line-idx (inc (caret-line-idx viewmodels selection))]
    (get lines line-idx)))

(defn caret-px
  "Returns the horizontal offset of the text caret from the document's edge, in pixels."
  ([selection line paragraph-type measure-fn]
   (let [spans-before-caret (spans-before-offset line (sel/caret selection))]
     (reduce (fn [width span]
               (+  width (measure-fn (:text span) (:formats span) paragraph-type)))
             0 spans-before-caret)))
  ([{:keys [selection doc]} viewmodels measure-fn]
   (let [caret-paragraph (get (:children doc) (sel/caret-para selection))
         caret-line (line-with-caret viewmodels (sel/smart-collapse selection))]
     (caret-px selection caret-line (:type caret-paragraph) measure-fn))))

#_(defn chars-and-formats [span] (map #(hash-map :grapheme %, :formats (:formats span)) (:text span)))

(defn nearest-line-offset-to-pixel
  "Takes a viewmodel Line and a distance from the left side of the editor-elem in pixels,
   and returns the paragraph offset that is closest to that pixel position.

   Arguments:
   - `line`: viewmodel Line object
   - `target-px`: target distance from left bound of editor element, in pixels
   - `last-line-in-paragraph?`: bool, should be true if this is the last line in the paragraph,
      as that will affect the logic somewhat.
   - `measure-fn`: measure function to use
   - `paragraph-type`: type of paragraph (:h1, :h2, :olist, :ulist)"
  [& {:keys [line target-px last-line-in-paragraph? measure-fn paragraph-type]}]
  (let [line-text (->> (:spans line) (map :text) (apply str))
        line-graphemes (mapv #(update % :offset + (:start-offset line)) (sl/graphemes line-text))]
    (loop [i 0
           offset-px 0]
      (if (= i (count line-graphemes))
        ;; End reached of line reached
        (let [last-grapheme (peek line-graphemes)]
          (if-not last-line-in-paragraph?
            ;; There is an invisible space at the end of each line - place the text caret directly in
            ;; after it, and the caret will be rendered at the start of the next line. However, it
            ;; would be visually confusing to click _past the right edge_ of a line and have your cursor
            ;; show up on the next line, so we get the 2nd-to-last offset in the line.
            (:offset last-grapheme)
            ;; ...EXCEPT on the last line of the paragraph, where there is no invisible space
            (:end-offset line)))
        (let [{grapheme :grapheme, offset :offset} (nth line-graphemes i)
              grapheme-width (measure-fn grapheme (vm/formats-at line offset) paragraph-type)
              new-offset-px (+ offset-px grapheme-width)
              delta-from-grapheme-left (abs (- offset-px target-px))
              delta-from-grapheme-right (abs (- new-offset-px target-px))]
          (cond
            (> delta-from-grapheme-right delta-from-grapheme-left)
            offset

            :else
            (recur (inc i) new-offset-px)))))))

(defn down-selection
  "Move the caret down into the next line. Returns a new Selection."
  [editor-state viewmodels editor-elem measure-fn horizontal-start-pos]
  (let [{:keys [selection doc]} editor-state
        collapsed-sel (sel/smart-collapse selection)
        para-idx (sel/caret-para collapsed-sel)
        para (get (:children doc) para-idx)
        viewmodel (get viewmodels para-idx)
        caret-line (line-with-caret viewmodels collapsed-sel)
        ;; Caret on last line in paragraph?
        caret-in-last-line? (= caret-line (peek (:lines viewmodel)))
        last-para? (= para-idx (dll/last-index (:children doc)))]
    (if (and caret-in-last-line? last-para?)
      collapsed-sel
      (let [destination-para-idx (if (or last-para? (not caret-in-last-line?))
                                   para-idx
                                   (dll/next-index (:children doc) para-idx))
            destination-para (get (:children doc) destination-para-idx)
            next-line (if (not caret-in-last-line?)
                        (line-below-caret viewmodels collapsed-sel)
                        (-> (get viewmodels destination-para-idx) :lines first))
            next-line-vm-paragraph (if (not caret-in-last-line?)
                                     viewmodel
                                     (-> (:children doc) (dll/next-index para-idx) (viewmodels)))
            initial-para-margin (:left (margins (get-paragraph-dom-elem editor-elem para-idx)))
            dest-para-margin (:left (margins (get-paragraph-dom-elem editor-elem destination-para-idx)))
            caret-offset-px (or horizontal-start-pos
                                (+ initial-para-margin (caret-px collapsed-sel caret-line (:type para) measure-fn)))
            next-line-offset (nearest-line-offset-to-pixel :line next-line
                                                           :target-px (- caret-offset-px dest-para-margin)
                                                           :last-line-in-paragraph? (= next-line
                                                                                       (peek (:lines next-line-vm-paragraph)))
                                                           :measure-fn measure-fn
                                                           :paragraph-type (:type destination-para))]
        (nav/autoset-formats doc (sel/selection [destination-para-idx next-line-offset]))))))

(defn down
  "Move the caret down into the next line. Returns an EditorUpdate.
   This is not in the model code because it requires the viewmodel to work."
  [editor-state viewmodels editor-elem measure-fn horizontal-start-pos]
  (let [new-selection (down-selection editor-state viewmodels editor-elem measure-fn horizontal-start-pos)]
    (es/->EditorUpdate (assoc editor-state :selection new-selection) (es/changelist))))

(defn up-selection
  "Move the caret up into the next line. Returns a new Selection."
  [editor-state viewmodels editor-elem measure-fn horizontal-start-pos]
  (let [{:keys [selection doc]} editor-state
        collapsed-sel (sel/smart-collapse selection)
        para-idx (sel/caret-para collapsed-sel)
        para (get (:children doc) para-idx)
        viewmodel (get viewmodels para-idx)
        caret-line (line-with-caret viewmodels collapsed-sel)
        ;; Caret in first line in paragraph?
        caret-in-first-line? (= caret-line (first (:lines viewmodel)))
        first-para? (= para-idx (dll/first-index (:children doc)))
        destination-para-idx (if (or first-para? (not caret-in-first-line?))
                               para-idx
                               (dll/prev-index (:children doc) para-idx))
        destination-para (get (:children doc) destination-para-idx)
        destination-para-lines (:lines (get viewmodels destination-para-idx))
        prev-line (if caret-in-first-line?
                    (if first-para?
                      (first destination-para-lines)
                      (peek destination-para-lines))
                    (line-above-caret viewmodels collapsed-sel))
        initial-para-margin (:left (margins (get-paragraph-dom-elem editor-elem para-idx)))
        dest-para-margin (:left (margins (get-paragraph-dom-elem editor-elem destination-para-idx)))
        caret-offset-px (or horizontal-start-pos
                            (+ initial-para-margin (caret-px collapsed-sel caret-line (:type para) measure-fn)))
        prev-line-offset (nearest-line-offset-to-pixel :line prev-line
                                                       :target-px (- caret-offset-px dest-para-margin)
                                                       :last-line-in-paragraph? caret-in-first-line?
                                                       :measure-fn measure-fn
                                                       :paragraph-type (:type destination-para))]
    (nav/autoset-formats doc (sel/selection [destination-para-idx prev-line-offset]))))

(defn up
  "Move the caret up into the next line. Returns an EditorUpdate.
   This is not in the model code because it requires the viewmodel to work."
  [editor-state viewmodels editor-elem measure-fn horizontal-start-pos]
  (let [new-selection (up-selection editor-state viewmodels editor-elem measure-fn horizontal-start-pos)]
    (es/->EditorUpdate (assoc editor-state :selection new-selection) (es/changelist))))

(defn shift+down
  "Move the caret down into the next line. Returns an EditorUpdate."
  [editor-state viewmodels editor-elem measure-fn horizontal-start-pos]
  ;; TODO: go to end of line if down-selection == selection (aka it's the last para)
  (let [{:keys [selection]} editor-state
        down-sel (down-selection editor-state viewmodels editor-elem measure-fn horizontal-start-pos)
        down-para-idx (sel/caret-para down-sel)
        down-offset (sel/caret down-sel)
        down-caret {:paragraph down-para-idx, :offset down-offset}
        new-selection (if (and (:backwards? selection) (sel/range? selection))
                        (if (and (= down-para-idx (-> selection :end :paragraph))
                                 (>= down-offset (-> selection :end :offset)))
                          (assoc selection :start (:end selection), :end down-caret, :backwards? false)
                          (assoc selection :start down-caret, :backwards? true))
                        (assoc selection :end down-caret, :backwards? false))]
    (es/->EditorUpdate (assoc editor-state :selection new-selection) (es/changelist))))

(defn shift+up
  "Move the caret up into the next line. Returns an EditorUpdate."
  [editor-state viewmodels editor-elem measure-fn horizontal-start-pos]
  ;; TODO: go to start of line if down-selection == selection (aka it's the first para)
  (let [{:keys [selection]} editor-state
        up-sel (up-selection editor-state viewmodels editor-elem measure-fn horizontal-start-pos)
        up-para-idx (sel/caret-para up-sel)
        up-offset (sel/caret up-sel)
        up-caret {:paragraph up-para-idx, :offset up-offset}
        new-selection (if (and (not (:backwards? selection)) (sel/range? selection))
                        (if (and (= up-para-idx (-> selection :start :paragraph))
                                 (< up-offset (-> selection :start :offset)))
                          (assoc selection :start up-caret, :end (:start selection), :backwards? true)
                          (assoc selection :end up-caret, :backwards? false))
                        (assoc selection :start up-caret, :backwards? true))]
    (es/->EditorUpdate (assoc editor-state :selection new-selection) (es/changelist))))

(defn start-of-line-selection
  "Returns a Selection that moves the cursor to the beginning of the current line."
  [{:keys [selection]} viewmodels]
  (let [new-offset (:start-offset (line-with-caret viewmodels (sel/smart-collapse selection)))]
    (sel/selection [(sel/caret-para selection) new-offset])))

(defn start-of-line
  "Returns an EditorUpdate that moves the cursor to the beginning of the current line."
  [editor-state viewmodels]
  (es/->EditorUpdate (assoc editor-state :selection (start-of-line-selection editor-state viewmodels))
                     (es/changelist)))

(defn end-of-line-selection
  "Returns a Selection that moves the cursor to the beginning of the current line."
  [{:keys [doc selection]} viewmodels]
  (let [paragraph (get (:children doc) (sel/caret-para selection))
        caret-line (line-with-caret viewmodels (sel/smart-collapse selection))
        end-offset (:end-offset caret-line)
        ;; Decrement by 1 if it's not last line to account for space at the end of each line
        new-offset (if (= end-offset (sl/len paragraph))
                     end-offset
                     (dec end-offset))]
    (sel/selection [(sel/caret-para selection) new-offset])))

(defn end-of-line
  "Returns an EditorUpdate that moves the cursor to the beginning of the current line."
  [editor-state viewmodels]
  (es/->EditorUpdate (assoc editor-state :selection (end-of-line-selection editor-state viewmodels))
                     (es/changelist)))

(defn calc-line-height
  "Returns the actual *rendered* line height of a given paragraph DOM element, in pixels."
  [paragraph-dom-elem]
  (let [first-dom-line (.querySelector paragraph-dom-elem ".line")]
    (when (nil? first-dom-line)
      (throw (js/Error. "Error in calc-line-height: the provided paragraph DOM element has no lines! This should never happen.")))

    (-> first-dom-line
        (js/getComputedStyle)
        (.getPropertyValue "height")
        (js/parseFloat))))

(defn clicked-location
  "Returns a new single selection set to where a click occured within a paragraph.
   Parameters:

   - client-x, client-y: x and y of the click relative to browser viewport
   - paragraph: the `Paragraph` (`slate.model.paragraph/Paragraph`)
   - viewmodel: viewmodel associated with that paragraph
   - paragraph-dom-elem: DOM element of the paragraph clicked
   - measure-fn: measure-fn for the document at the current font and font-size"
  [client-x client-y paragraph {:keys [lines paragraph-index]} paragraph-dom-elem measure-fn]
  (let [line-height (calc-line-height paragraph-dom-elem)
        dom-elem-rect (.getBoundingClientRect paragraph-dom-elem)
        px (- client-x (.-x dom-elem-rect))
        py (- client-y (.-y dom-elem-rect))
        line-idx (cond
                   (< py 0) 0
                   (>= py (.-height dom-elem-rect)) (dec (count lines))
                   :else (min (int (/ py line-height))
                              ;; account for paragraph bottom padding
                              (dec (count lines))))
        line (nth lines line-idx)
        offset (cond
                 (< px 0) (:start-offset line)
                 (> px (.-width dom-elem-rect)) (if (= line (peek lines))
                                                  (:end-offset line)
                                                  (dec (:end-offset line)))
                 :else (nearest-line-offset-to-pixel :line line
                                                     :target-px px
                                                     :last-line-in-paragraph? (= line (peek lines))
                                                     :measure-fn measure-fn
                                                     :paragraph-type (:type paragraph)))]
    (nav/autoset-formats paragraph (sel/selection [paragraph-index offset]))))



(defn find-overlapping-paragraph
  "Finds paragraph that client-y is overlapping with in the y-axis and returns its index."
  [paragraphs-dll editor-elem client-y shadow-root]
  (let [idx->bounds (fn [paragraph-idx]
                      (let [bounding-rect (.getBoundingClientRect (get-paragraph-dom-elem editor-elem paragraph-idx))]
                        {:top (.-top bounding-rect)
                         :bottom (.-bottom bounding-rect)}))
        first-para-bounds (-> paragraphs-dll dll/first-index idx->bounds)
        last-para-bounds (-> paragraphs-dll dll/last-index idx->bounds)]
    (cond
      ;; Above first paragraph
      (< client-y (:top first-para-bounds))
      (dll/first-index paragraphs-dll)

      ;; Below last paragraph
      (> client-y (:bottom last-para-bounds))
      (dll/last-index paragraphs-dll)

      ;; Find paragraph that client-y is overlapping with in the y axis
      :else
      (let [editor-bounds (.getBoundingClientRect editor-elem)
            center-x (+ (.-x editor-bounds) (/ (.-width editor-bounds) 2))
            elements-at-point (.elementsFromPoint shadow-root center-x client-y)
            paragraph-elem (first (filter #(.matches % ".paragraph") elements-at-point))]
        (dom-id->paragraph-index (.-id paragraph-elem))))))

(declare ^:dynamic *last-mousedown-event*)

(defn mouse-event->selection
  "Takes a MouseEvent object and the editor state and, if its clientX and clientY
   are inside a paragraph, returns a single selection set to the paragraph and offset where the
   mouse pointer is at.

   If the mouse is *outside* any paragraph, it will return whatever paragraph is closest.
   For example, clicking off the right of a paragraph P (not actually inside of it) will return
   a selection inside of P with the offset set to the end of the line you clicked to the right of.
   Likewise, clicking above or below the first or line paragraphs will return selections in the first
   or last lines, respectively, with an offset based on the x coordinate of the mouse."
  [event doc viewmodels editor-elem measure-fn shadow-root]
  (let [paragraph-in-path (match-elem-in-path event ".paragraph")
        paragraph-idx (if paragraph-in-path
                        (dom-id->paragraph-index (.-id paragraph-in-path))
                        (find-overlapping-paragraph (:children doc) editor-elem (.-y event) shadow-root))
        ;; The paragraph might have re-rendered since this MouseEvent was fired, and thus the
        ;; paragraph element in the path may not actually be present in the DOM. It's index in
        ;; the document will still be the same, however, so we can just grab the current element
        ;; like this.
        paragraph-elem (get-paragraph-dom-elem editor-elem paragraph-idx)
        paragraph (get (:children doc) paragraph-idx)
        vm (get viewmodels paragraph-idx)
        sel (clicked-location (.-x event) (.-y event) paragraph vm paragraph-elem measure-fn)]
    sel))

(defn- drag-direction
  [drag-started-at drag-currently-at]
  (if (= (sel/caret-para drag-started-at) (sel/caret-para drag-currently-at))
    (if (> (sel/caret drag-currently-at) (sel/caret drag-started-at))
      :forward
      :backward)
    (if (.gt (sel/caret-para drag-currently-at) (sel/caret-para drag-started-at))
      :forward
      :backward)))

(defn drag
  [mousemove-event doc viewmodels editor-elem measure-fn shadow-root]
  (let [started-at (mouse-event->selection *last-mousedown-event* doc viewmodels editor-elem measure-fn shadow-root)
        currently-at (mouse-event->selection mousemove-event doc viewmodels editor-elem measure-fn shadow-root)
        started-para-idx (sel/caret-para started-at)
        started-offset (sel/caret started-at)
        current-para-idx (sel/caret-para currently-at)
        current-offset (sel/caret currently-at)
        dir (drag-direction started-at currently-at)
        raw-selection (if (= dir :forward)
                        (sel/selection [started-para-idx started-offset]
                                       [current-para-idx current-offset])
                        (sel/selection [current-para-idx current-offset]
                                       [started-para-idx started-offset]
                                       :backwards? true))]
    (nav/autoset-formats doc raw-selection)))

(defn create-hidden-input!
  "Creates hidden input element used to capture keystrokes."
  [shadow-root]
  (let [hidden-input-id (str (random-uuid))
        hidden-input (js/document.createElement "input")]
    (set! (.-id hidden-input) hidden-input-id)
    (set! (.-className hidden-input) "hidden-input")
    (.setAttribute hidden-input "type" "text")
    (.appendChild shadow-root hidden-input)
    hidden-input))

(defn relocate-hidden-input!
  "Relocates the hidden input to be inside the cursor element."
  ([shadow-root hidden-input focus?]
   (let [cursor-elem (.querySelector shadow-root ".slate-text-caret")]
     (.append cursor-elem hidden-input)
     (when focus?
       (.focus hidden-input #js {:preventScroll true}))))
  ([shadow-root hidden-input] (relocate-hidden-input! shadow-root hidden-input true)))

(defn scroll-to-caret!
  "Centers the text caret in the viewport."
  [shadow-root]
  (let [cursor-elem (.querySelector shadow-root ".slate-text-caret")]
    (.scrollIntoView cursor-elem #js {:block "center"})))
