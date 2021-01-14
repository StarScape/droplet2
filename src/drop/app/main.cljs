(ns drop.app.main
  (:require [clojure.string :as str]
            [drop.app.view :as view]
            [drop.editor.core :as c]
            [drop.editor.navigation :as nav]
            [drop.editor.selection :as sel]
            [drop.editor.measurement :refer [ruler-for-elem]]
            [drop.editor.viewmodel :as vm]))

;; up/down nonsense
(defn split-span
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
                (conj spans-before (nth (split-span span offset) 0))

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

;; main

(defn parse-event [e]
  (let [modifiers (cond-> (transient [])
                    (.-ctrlKey e) (conj! "ctrl")
                    (.-altKey e) (conj! "alt")
                    (.-shiftKey e) (conj! "shift")
                    :always (persistent!))
        key (case (.-key e)
              "ArrowLeft" "left"
              "ArrowRight" "right"
              "ArrowUp" "up"
              "ArrowDown" "down"
              (-> (.-key e) .toLowerCase keyword))]
    (->> (conj modifiers key)
         (str/join "+")
         (keyword))))

(def fake-editor (.getElementById js/document "fake-editor"))
(def hidden-input (.querySelector js/document "#hidden-input"))
(def measure-fn (ruler-for-elem fake-editor))
(def test-para
  (c/paragraph [(c/run "Hello world, this is an example of a paragraph ")
                (c/run "that I might want to split into lines. I'm really just typing a bunch of random stuff in here. " #{:italic})
                (c/run "Don't know what else to say. Hmmmm..." #{:bold})]))

;; TODO: maybe change this to "editor-state" and include dom references and current ruler inside it
;; TODO: hide this behind an initializer function which returns the shit we need and takes an elem as its argument
(def doc-state (atom {:doc (c/document [test-para])
                      :selection (sel/selection [(:uuid test-para) 0])
                      ;; TODO: just change to a DLL of viewmodels
                      :viewmodels {(:uuid test-para) (vm/from-para test-para 200 measure-fn)}}))

(defn sync-dom [elem doc-state measure-fn]
  (let [state @doc-state
        doc (:doc state)
        sel (:selection state)
        vm-paras (map #(vm/from-para % 200 measure-fn) (:children doc))]
    (set! (.-innerHTML elem) (view/vm-paras->dom vm-paras sel))))

(def interceptors
  {:left (fn [state _e]
           (update state :selection #(nav/prev-char (:doc state) %)))
   :right (fn [state _e]
            (update state :selection #(nav/next-char (:doc state) %)))
   :down (fn [state _e]
           (update state :selection #(down state measure-fn)))})

(defn main []
  (.addEventListener js/document "keydown"
    (fn [e]
      (when-let [interceptor-fn (get interceptors (parse-event e))]
        (.preventDefault e)
        (reset! doc-state (interceptor-fn @doc-state e)))
      (sync-dom fake-editor doc-state measure-fn)))
  (sync-dom fake-editor doc-state measure-fn))

(defn ^:dev/after-load reload []
  (sync-dom fake-editor doc-state measure-fn))

;; TODO: handle up and down events
;; TODO: Handle shifting selection left/right
;; TODO: Handle shifting selection up/down
;; TODO: handle input and deletion
