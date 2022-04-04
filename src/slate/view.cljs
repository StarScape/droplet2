(ns slate.view
  "Functions for converting from viewmodels to DOM elements (in the form of strings),
   as well as some utilities for dealing with the editor's DOM layer in general."
  (:require [clojure.string :as str]
            [slate.model.selection :as sel]
            [slate.model.common :as sl]
            [slate.model.editor-state :as es]
            [slate.model.navigation :as nav]
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

(defn split-elem-on-child!
  "Takes an element (e.g. a <ol>) and one of its children (e.g. an <li>) and clones
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

(defn font-size
  "Returns the element's font size, in pixels, _as a number_, not a string."
  [dom-elem]
  (let [style (js/getComputedStyle dom-elem)
        font-size-str (.getPropertyValue style "font-size")]
    (-> font-size-str
        (subs 0 (- (.-length font-size-str) 2))
        (js/parseInt))))

(defn margins
  "Returns the elements margins as a map of {:top, :bottom, :left, :right}."
  [dom-elem]
  (let [style (js/getComputedStyle dom-elem)]
    {:top (js/parseFloat (.-marginTop style))
     :bottom (js/parseFloat (.-marginBottom style))
     :left (js/parseFloat (.-marginLeft style))
     :right (js/parseFloat (.-marginRight style))}))

(defn paragraph-type->css-class [paragraph-type]
  (if paragraph-type
    (str (name paragraph-type) "-format")
    ""))

(defn formats->css-classes [formats]
  (map #(str (name %) "-format") formats))

;; Dynamic var to indicate whether the selection is still ongoing.
;; This is something we need to keep track of between paragraphs so it winds up a little
;; more elegant to make careful use of a dynamic var, rather than returning a selection-ongoing?
;; value up and down a bunch of different cycles of the callstack.
(declare ^:dynamic *selection-ongoing?*)

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
  "Convert viewmodel span to DOM element. Also responsible for rendering caret and range selection background.
   Returns HTML string."
  [span selection pid para-length]
  (let [format-classes
        (formats->css-classes (:formats span))

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
  "Convert viewmodel line to DOM element. Returns HTML string."
  [line selection pid para-length]
  (str "<div class='line'>"
       (apply str (map #(vm-span->dom % selection pid para-length) (:spans line)))
       "</div>"))

(declare ^:dynamic *list-ongoing?*)

(defn vm-para->dom
  "Convert viewmodel to DOM element. Returns HTML string."
  [viewmodel selection]
  (let [lines (:lines viewmodel)
        pid (-> viewmodel :paragraph :uuid)
        para (:paragraph viewmodel)]
    (binding [*selection-ongoing?* (contains? (:between selection) pid)]
      (str "<div class='paragraph " (paragraph-type->css-class (:type para)) "' id='" pid "'>"
           (apply str (map #(vm-line->dom % selection pid (sl/len para)) lines))
           "</div>"))))

(defn vm-paras->dom
  "Convert the list of [[ParagraphViewModel]]s to DOM elements and returns an HTML
   string. Selection is provided in order to render the caret and highlighted text."
  [vm-paras selection]
  (str "<div class='document'>"
       (apply str (map #(vm-para->dom % selection) vm-paras))
       "</div>"))

(defn- next-dom-paragraph-after
  "Returns the next paragraph after the one with UUID `uuid` that currently has a node in the dom.
   If no paragraphs are found at all, returns `nil`."
  [uuid paragraphs-dll]
  (loop [node (dll/next paragraphs-dll uuid)]
    (if (nil? node)
      nil
      (let [elem (js/document.getElementById (str (:uuid node)))]
        (if (some? elem)
          elem
          (recur (dll/next paragraphs-dll node)))))))

(defn- next-dom-paragraph-before
  "Returns the next paragraph before the one with UUID `uuid` that currently has a node in the dom.
   If no paragraphs are found at all, returns `nil`."
  [uuid paragraphs-dll]
  (loop [node (dll/prev paragraphs-dll uuid)]
    (if (nil? node)
      nil
      (let [elem (js/document.getElementById (str (:uuid node)))]
        (if (some? elem)
          elem
          (recur (dll/prev paragraphs-dll node)))))))

(defn- nearest-top-level-ancestor
  [elem editor-elem]
  (loop [e elem]
    (if (= (.-parentElement e) editor-elem)
      e
      (recur (.-parentElement e)))))

(defn merge-list-elems-if-needed!
  "Takes two ADJACENT elements and, if they are both of the same list container type (e.g.
   are both a separate <ul> or <ol>), merges them together."
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
   containing element, either <ul> or <ol>). Automatically wraps the element in <ul>/<ol> IF there is not already
   one present for the current run of list paragraphs, otherwise will insert into that <ul>/<ol>"
  [list-type editor-elem uuid viewmodel {:keys [doc selection] :as _es}]
  (let [tag-name (name list-type)
        rendered-paragraph (vm-para->dom viewmodel selection)
        p-elem (js/document.createElement "p")
        next-p-elem (next-dom-paragraph-after uuid (:children doc))
        next-p-inside-same-list-elem? (when next-p-elem
                                        (= (.. next-p-elem -parentNode -tagName toLowerCase) tag-name))
        prev-p-elem (next-dom-paragraph-before uuid (:children doc))
        prev-p-inside-same-list-elem? (when prev-p-elem
                                        (= (.. prev-p-elem -parentNode -tagName toLowerCase) tag-name))
        p-outer-html (if (or next-p-inside-same-list-elem?
                             prev-p-inside-same-list-elem?)
                       rendered-paragraph
                       (str "<" tag-name ">" rendered-paragraph "</" tag-name ">"))]
    (cond
      ;; insert into UL before next-p-elem
      (and next-p-elem next-p-inside-same-list-elem?
           prev-p-elem prev-p-inside-same-list-elem?)
      (.insertBefore (.-parentNode next-p-elem) p-elem next-p-elem)

      ;; insert into UL before next-p-elem
      (and next-p-elem next-p-inside-same-list-elem?)
      (.insertBefore (.-parentNode next-p-elem) p-elem next-p-elem)

      ;; append to prev-p-elem's parent <ul>/<ol>
      (and prev-p-elem prev-p-inside-same-list-elem?)
      (.append (.-parentNode prev-p-elem) p-elem)

      ;; neither prev nor next is a list paragraph but there is a next paragraph in the DOM currently
      ;; insert directly before the _nearest ancestor_ of next-p-elem that is top-level within document elem
      (some? next-p-elem)
      (.insertBefore editor-elem p-elem (nearest-top-level-ancestor next-p-elem editor-elem))

      :else
      ;; No next elem, append to document end
      (.append editor-elem p-elem))
    ;; Must be placed in DOM before outerHTML can be set
    (set! (.-outerHTML p-elem) p-outer-html)))

(defn remove-list-para!
  [list-type editor-elem uuid editor-state prev-state]
  (let [elem (js/document.getElementById (str uuid))
        prev-elem (.-previousElementSibling elem)
        next-elem (.-nextElementSibling elem)
        old-children (-> prev-state :doc :children)
        new-children (-> editor-state :doc :children)
        prev-para-uuid (:uuid (dll/prev old-children uuid))
        next-para-uuid (:uuid (dll/next old-children uuid))
        prev-para-type (:type (get new-children prev-para-uuid))
        next-para-type (:type (get new-children next-para-uuid))]
    (if (and (not= prev-para-type list-type)
             (not= next-para-type list-type))
      ;; Only item in the list, remove parent so there is no dangling <ul>/<ol>
      (.remove (.-parentElement elem))
      ;; Other items in list, keep <ul>/<ol>
      (.remove elem))
    (merge-list-elems-if-needed! prev-elem next-elem)))

(defmulti insert-para!
  "Inserts the paragraph with `uuid` into the DOM."
  {:arglists '([editor-elem uuid viewmodel doc selection])}
  (fn [_ _ vm _ _] (-> vm :paragraph :type)))

(defmulti remove-para!
  "Removes the paragraph with `uuid` from the DOM."
  {:arglists '([uuid editor-state prev-editor-state])}
  (fn [_ uuid _ {:keys [doc]}] (:type (get (:children doc) uuid))))

(defmethod insert-para! :default
  [editor-elem uuid viewmodel {:keys [doc selection] :as _editor-state}]
  (let [rendered-paragraph (vm-para->dom viewmodel selection)
        paragraph-elem (js/document.createElement "p")
        node-to-insert-before (when-some [next-paragraph-elem (next-dom-paragraph-after uuid (:children doc))]
                                (if (= editor-elem (.-parentNode next-paragraph-elem))
                                  next-paragraph-elem
                                  (do
                                    ;; When the next paragraph is not top level within the document (e.g. is contained)
                                    ;; inside a <ol> or <ul>, and ALSO has a previous sibling inside of the same containing
                                    ;; element, that means we are inserting a normal paragraph into the middle of a list, so
                                    ;; we need to split the <ol> or <ul> into two separate ones and insert our new para after
                                    ;; the first.
                                    (when-let [prev-element (.-previousElementSibling next-paragraph-elem)]
                                      (split-elem-on-child! (.-parentNode next-paragraph-elem) prev-element))
                                    (.-parentNode next-paragraph-elem))))]
    (if node-to-insert-before
      (.insertBefore editor-elem paragraph-elem node-to-insert-before)
      (.append editor-elem paragraph-elem))
    (set! (.-outerHTML paragraph-elem) rendered-paragraph)))

(defmethod insert-para! :ul
  [editor-elem uuid viewmodel editor-state]
  (insert-list-para! :ul editor-elem uuid viewmodel editor-state))

(defmethod insert-para! :ol
  [editor-elem uuid viewmodel editor-state]
  (insert-list-para! :ol editor-elem uuid viewmodel editor-state))

(defmethod remove-para! :default
  [_editor-elem uuid _ _]
  ;; TODO: check to see if the paragraphs before and after uuid are both list paragraphs
  ;; of the same type, and merge if so
  (let [p-elem (js/document.getElementById (str uuid))
        prev-elem (.-previousElementSibling p-elem)
        next-elem (.-nextElementSibling p-elem)]
    (.remove p-elem)
    (merge-list-elems-if-needed! prev-elem next-elem)))

(defmethod remove-para! :ul
  [editor-elem uuid editor-state prev-state]
  (remove-list-para! :ul editor-elem uuid editor-state prev-state))

(defmethod remove-para! :ol
  [editor-elem uuid editor-state prev-state]
  (remove-list-para! :ol editor-elem uuid editor-state prev-state))

;; Currently all paragraph types update the same, no special logic/multimethod needed
(defn update-para! [editor-elem uuid viewmodel editor-state prev-state]
  (let [paragraph-elem (.getElementById js/document (str uuid))
        type-changed? (not= (-> editor-state :doc :children (get uuid) :type)
                            (-> prev-state :doc :children (get uuid) :type))]
    (if type-changed?
      (do
        (remove-para! editor-elem uuid editor-state prev-state)
        (insert-para! editor-elem uuid viewmodel editor-state))
      (set! (.-outerHTML paragraph-elem) (vm-para->dom viewmodel (:selection editor-state))))))

(defn insert-all!
  "Inserts __all__ paragraphs in the list `vm-paras` into the DOM.
   Only used in a few special circumstances, like initial render, otherwise
   document should be updated piecewise using `(insert|remove|update)-para!`."
  [editor-elem vm-paras editor-state]
  (set! (.-innerHTML editor-elem) "")
  (doseq [vm (reverse vm-paras)]
    (insert-para! editor-elem (-> vm :paragraph :uuid) vm editor-state)))

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
        vm (viewmodels (-> selection :start :paragraph)) ;; TODO: should arg be (sel/caret-para selection) instead?
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
  "Returns the line in the viewmodel immediately below the line with the caret inside of it.
   If there is no line below the current line, returns nil."
  [viewmodels selection]
  (let [lines (:lines (viewmodels (sel/caret-para selection)))
        line-idx (inc (caret-line-idx viewmodels selection))]
    (get lines line-idx)))

(defn caret-px
  "Returns the horizontal offset of the text caret from the document's edge, in pixels."
  [selection line paragraph-type measure-fn]
  (let [spans-before-caret (spans-before-offset line (sel/caret selection))]
    (reduce (fn [width span]
              (+  width (measure-fn (:text span) (:formats span) paragraph-type)))
            0 spans-before-caret)))

(defn chars-and-formats [span] (map #(hash-map :char %, :formats (:formats span)) (:text span)))

(defn nearest-line-offset-to-pixel
  "Takes a viewmodel Line and a distance from the left side of the editor-element in pixels,
   and returns the paragraph offset that is closest to that pixel position.

   Arguments:
   - `line`: viewmodel Line object
   - `target-px`: target distance from left bound of editor element, in pixels
   - `last-line-in-paragraph?`: bool, should be true if this is the last line in the paragraph,
      as that will affect the logic somewhat.
   - `measure-fn`: measure function to use
   - `paragraph-type`: type of paragraph (:h1, :h2, :olist, :ulist)"
  [& {:keys [line target-px last-line-in-paragraph? measure-fn paragraph-type]}]
  (let [chars-with-formats (->> (:spans line)
                                (map chars-and-formats (:spans line))
                                (flatten))]
    (loop [chars-i 0
           offset (:start-offset line)
           offset-px 0]
      (if (= chars-i (count chars-with-formats))
        (if (not last-line-in-paragraph?)
          ;; There is an invisible space at the end of each line - place the text caret directly in
          ;; front of it, and the caret will be rendered at the start of the next line. However, it
          ;; would be visually confusing to click _past the right edge_ of a line and have your cursor
          ;; show up on the next line, so we decrement by 1.
          (dec offset)
          ;; ...EXCEPT on the last line of the paragraph, where there is no invisible space
          offset)
        (let [{:keys [char formats]} (nth chars-with-formats chars-i)
              new-offset-px (+ offset-px (measure-fn char formats paragraph-type))
              delta-from-char-left (js/Math.abs (- offset-px target-px))
              delta-from-char-right (js/Math.abs (- new-offset-px target-px))]
          (cond
            (> delta-from-char-right delta-from-char-left)
            offset

            :else
            (recur (inc chars-i) (inc offset) new-offset-px)))))))

(defn down-selection
  "Move the caret down into the next line. Returns a new Selection."
  [editor-state viewmodels measure-fn]
  (let [{:keys [selection doc]} editor-state
        collapsed-sel (sel/smart-collapse selection)
        para-uuid (sel/caret-para collapsed-sel)
        para (get (:children doc) para-uuid)
        viewmodel (get viewmodels para-uuid)
        caret-line (line-with-caret viewmodels collapsed-sel)

        ;; Caret on last line in paragraph?
        caret-in-last-line? (= caret-line (peek (:lines viewmodel)))
        last-para? (= para-uuid (:uuid (dll/last (:children doc))))
        destination-para (if (or last-para? (not caret-in-last-line?))
                                para
                                (dll/next (:children doc) para-uuid))
        next-line (if (not caret-in-last-line?)
                    (line-below-caret viewmodels collapsed-sel)
                    (->> destination-para
                        (:uuid)
                        (get viewmodels)
                        (:lines)
                        (first)))
        next-line-vm-paragraph (if (not caret-in-last-line?)
                                 viewmodel
                                 (-> (:children doc) (dll/next para-uuid) (:uuid) (viewmodels)))
        new-uuid (if caret-in-last-line?
                   (:uuid (dll/next (:children doc) para-uuid))
                   para-uuid)]
    (if next-line
      (let [initial-para-margin (:left (margins (.getElementById js/document para-uuid)))
            dest-para-margin (:left (margins (.getElementById js/document (:uuid destination-para))))
            caret-offset-px (+ initial-para-margin (caret-px collapsed-sel caret-line (:type para) measure-fn))
            next-line-offset (nearest-line-offset-to-pixel :line next-line
                                                           :target-px (- caret-offset-px dest-para-margin)
                                                           :last-line-in-paragraph? (= next-line
                                                                                       (peek (:lines next-line-vm-paragraph)))
                                                           :measure-fn measure-fn
                                                           :paragraph-type (:type destination-para))]
        (nav/autoset-formats doc (sel/selection [new-uuid next-line-offset])))
      collapsed-sel)))

(defn down
  "Move the caret down into the next line. Returns an EditorUpdate.
   This is not in the model code because it requires the viewmodel to work."
  [{:keys [selection] :as editor-state} viewmodels measure-fn]
  (let [new-selection (down-selection editor-state viewmodels measure-fn)
        changed-uuids (conj (sel/all-uuids selection) (sel/caret-para new-selection))]
    (es/->EditorUpdate (assoc editor-state :selection new-selection)
                       (es/changelist :changed-uuids changed-uuids))))

(defn up-selection
  "Move the caret up into the next line. Returns a new Selection."
  [editor-state viewmodels measure-fn]
  (let [{:keys [selection doc]} editor-state
        collapsed-sel (sel/smart-collapse selection)
        para-uuid (sel/caret-para collapsed-sel)
        para (get (:children doc) para-uuid)
        viewmodel (get viewmodels para-uuid)
        caret-line (line-with-caret viewmodels collapsed-sel)
        caret-in-first-line? (= caret-line (first (:lines viewmodel)))
        first-para? (= para-uuid (:uuid (dll/first (:children doc))))
        destination-para (if (or first-para? (not caret-in-first-line?))
                                para
                                (dll/prev (:children doc) para-uuid))
        prev-line (if (not caret-in-first-line?)
                    (line-above-caret viewmodels collapsed-sel)
                    (-> (:children doc)
                        (dll/prev para-uuid)
                        (:uuid)
                        (viewmodels)
                        (:lines)
                        (peek)))
        new-uuid (if caret-in-first-line?
                   (:uuid (dll/prev (:children doc) para-uuid))
                   para-uuid)]
    (if prev-line
      (let [initial-para-margin (:left (margins (.getElementById js/document para-uuid)))
            dest-para-margin (:left (margins (.getElementById js/document (:uuid destination-para))))
            caret-offset-px (+ initial-para-margin (caret-px collapsed-sel caret-line (:type para) measure-fn))
            prev-line-offset (nearest-line-offset-to-pixel :line prev-line
                                                           :target-px (- caret-offset-px dest-para-margin)
                                                           :last-line-in-paragraph? caret-in-first-line?
                                                           :measure-fn measure-fn
                                                           :paragraph-type (:type destination-para))]
        (nav/autoset-formats doc (sel/selection [new-uuid prev-line-offset])))
      collapsed-sel)))

(defn up
  "Move the caret up into the next line. Returns an EditorUpdate.
   This is not in the model code because it requires the viewmodel to work."
  [{:keys [selection] :as editor-state} viewmodels measure-fn]
  (let [new-selection (up-selection editor-state viewmodels measure-fn)
        changed-uuids (conj (sel/all-uuids selection) (sel/caret-para new-selection))]
    (es/->EditorUpdate (assoc editor-state :selection new-selection)
                       (es/changelist :changed-uuids changed-uuids))))

(defn shift+down
  "Move the caret down into the next line. Returns an EditorUpdate."
  [editor-state viewmodels measure-fn]
  ;; TODO: go to end of line if down-selection == selection (aka it's the last para)
  (let [{:keys [selection]} editor-state
        down-sel (down-selection editor-state viewmodels measure-fn)
        down-para (sel/caret-para down-sel)
        down-offset (sel/caret down-sel)
        down-caret {:paragraph down-para, :offset down-offset}
        new-selection (if (and (:backwards? selection) (sel/range? selection))
                        (if (and (= down-para   (-> selection :end :paragraph))
                                 (>= down-offset (-> selection :end :offset)))
                          (assoc selection :start (:end selection), :end down-caret, :backwards? false)
                          (assoc selection :start down-caret, :backwards? true))
                        (assoc selection :end down-caret, :backwards? false))
        new-selection (if (:backwards? new-selection)
                        ;; may have moved start down a paragraph and need to remove new start from :between
                        (sel/remove-ends-from-between new-selection)
                        ;; may have moved end down a paragraph and need to add previous end para to :between
                        (sel/add-to-between new-selection (sel/end-para selection)))
        changed-uuids #{(sel/caret-para selection), (sel/caret-para new-selection)}]
    (es/->EditorUpdate (assoc editor-state :selection new-selection)
                       (es/changelist :changed-uuids changed-uuids))))

(defn shift+up
  "Move the caret up into the next line. Returns an EditorUpdate."
  [editor-state viewmodels measure-fn]
  ;; TODO: go to start of line if down-selection == selection (aka it's the first para)
  (let [{:keys [selection]} editor-state
        up-sel (up-selection editor-state viewmodels measure-fn)
        up-para (sel/caret-para up-sel)
        up-offset (sel/caret up-sel)
        up-caret {:paragraph up-para, :offset up-offset}
        new-selection (if (and (not (:backwards? selection)) (sel/range? selection))
                        (if (and (= up-para   (-> selection :start :paragraph))
                                 (< up-offset (-> selection :start :offset)))
                          (assoc selection :start up-caret, :end (:start selection), :backwards? true)
                          (assoc selection :end up-caret, :backwards? false))
                        (assoc selection :start up-caret, :backwards? true))
        new-selection (if (:backwards? new-selection)
                        ;; may have moved start up a paragraph and need to add previous start para to :between
                        (sel/add-to-between new-selection (sel/start-para selection))
                        ;; may have moved end up a paragraph and need to remove new end from :between
                        (sel/remove-ends-from-between new-selection))
        changed-uuids #{(sel/caret-para selection), (sel/caret-para new-selection)}]
    (es/->EditorUpdate (assoc editor-state :selection new-selection)
                       (es/changelist :changed-uuids changed-uuids))))

(defn start-of-line-selection
  "Returns a Selection that moves the cursor to the beginning of the current line."
  [{:keys [selection]} viewmodels]
  (let [new-offset (:start-offset (line-with-caret viewmodels selection))]
    (sel/selection [(sel/caret-para selection) new-offset])))

(defn start-of-line
  "Returns an EditorUpdate that moves the cursor to the beginning of the current line."
  [{:keys [selection] :as editor-state} viewmodels]
  (es/->EditorUpdate (assoc editor-state :selection (start-of-line-selection editor-state viewmodels))
                     (es/changelist :changed-uuids (sel/all-uuids selection))))

(defn end-of-line-selection
  "Returns a Selection that moves the cursor to the beginning of the current line."
  [{:keys [doc selection]} viewmodels]
  (let [paragraph (get (:children doc) (sel/caret-para selection))
        caret-line (line-with-caret viewmodels selection)
        end-offset (:end-offset caret-line)
        ;; Decrement by 1 if it's not last line to account for space at the end of each line
        new-offset (if (= end-offset (sl/len paragraph))
                     end-offset
                     (dec end-offset))]
    (sel/selection [(:uuid paragraph) new-offset])))

(defn end-of-line
  "Returns an EditorUpdate that moves the cursor to the beginning of the current line."
  [{:keys [selection] :as editor-state} viewmodels]
  (es/->EditorUpdate (assoc editor-state :selection (end-of-line-selection editor-state viewmodels))
                     (es/changelist :changed-uuids (sel/all-uuids selection))))

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
                 :else (nearest-line-offset-to-pixel :line line
                                                     :target-px px
                                                     :last-line-in-paragraph? (= line (peek lines))
                                                     :measure-fn measure-fn
                                                     :paragraph-type (:type paragraph)))]
    (nav/autoset-formats paragraph (sel/selection [(:uuid paragraph) offset]))))

(defn find-overlapping-paragraph
  "Finds paragraph that client-y is overlapping with in the y-axis and returns its UUID."
  [paragraphs-dll client-y prev-event]
  ;; (println "Finding overlap!")
  (let [para->bounds (fn [{:keys [uuid]}]
                       (let [bounding-rect (.getBoundingClientRect (.getElementById js/document (str uuid)))]
                         {:uuid uuid
                          :top (.-top bounding-rect)
                          :bottom (.-bottom bounding-rect)}))
        first-para-bounds (-> paragraphs-dll first para->bounds)
        last-para-bounds (-> paragraphs-dll peek para->bounds)]
    (cond
      (< client-y (:top first-para-bounds))
      (-> paragraphs-dll first :uuid)

      (> client-y (:bottom last-para-bounds))
      (-> paragraphs-dll peek :uuid)

      ;; TODO: one way to further optimize this would be to cache the last drag event
      ;; and use that. I have a feeling that might be more trouble than its worth though,
      ;; unless we actually start to notice real perf problems with this.

      ;; Find paragraph that client-y is overlapping with in the y axis
      :else
      (let [overlaps? #(<= (:top %) client-y (:bottom %))
            prev-para (match-elem-in-path prev-event ".paragraph")
            ;; Start searching at either the paragraph the
            ;; previous event took place in, or the first one
            start-uuid (if prev-para (uuid (.-id prev-para)) (:uuid (first paragraphs-dll)))
            advance (if (and prev-para (neg? (- client-y (.-y prev-event))))
                         dll/prev
                         dll/next)]
        ;; TODO: okay, definitely some errors here...
        (loop [p (get paragraphs-dll start-uuid)]
          (when (nil? p) (throw "I don't think this should ever happen..."))

          (if (overlaps? (para->bounds p))
            (:uuid p)
            (recur (advance paragraphs-dll p))))))))

(declare ^:dynamic *last-mousedown-event*)

(defn mouse-event->selection
  "Takes a MouseEvent object and the editor state and, if its clientX and clientY
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
  ([event doc viewmodels measure-fn]
   (let [paragraph-in-path (match-elem-in-path event ".paragraph")
         paragraph-uuid (if paragraph-in-path
                          (.-id paragraph-in-path)
                          (find-overlapping-paragraph (:children doc) (.-y event) *last-mousedown-event*))
         ; The paragraph might have re-rendered since this MouseEvent was fired, and thus the
         ; paragraph element in the path may not actually be present in the DOM. It's ID/UUID
         ; will still be valid, however, so we can just grab the current element like this.
         paragraph-elem (.getElementById js/document paragraph-uuid)
         vm (get viewmodels (uuid (.-id paragraph-elem)))
         sel (clicked-location (.-x event) (.-y event) paragraph-elem vm measure-fn)]
     sel)))

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
  [mousemove-event doc viewmodels measure-fn]
  (let [started-at (mouse-event->selection *last-mousedown-event* doc viewmodels measure-fn)
        currently-at (mouse-event->selection mousemove-event doc viewmodels measure-fn)
        started-uuid (sel/caret-para started-at)
        started-offset (sel/caret started-at)
        current-uuid (sel/caret-para currently-at)
        current-offset (sel/caret currently-at)
        dir (drag-direction started-at currently-at *last-mousedown-event* mousemove-event)
        raw-selection (if (= dir :forward)
                        (sel/selection [started-uuid started-offset]
                                       [current-uuid current-offset]
                                       :between (set (dll/uuids-between (:children doc) started-uuid current-uuid)))
                        (sel/selection [current-uuid current-offset]
                                       [started-uuid started-offset]
                                       :backwards? true
                                       :between (set (dll/uuids-between (:children doc) current-uuid started-uuid))))]
    (nav/autoset-formats doc raw-selection)))
