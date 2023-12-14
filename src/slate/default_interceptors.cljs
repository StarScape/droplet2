(ns slate.default-interceptors
  "Default set of interceptors that are added to a new editor.

  Interceptors are meant for extensibility, but most of the default events are also
  implemented using them, the idea being that eating our own dogfood is the only
  way to end up with a sufficiently flexible and ergonomic system."
  (:require-macros [slate.interceptors :refer [definterceptor]])
  (:require [slate.interceptors]
            [slate.model.common :as m]
            [slate.model.editor-state :as es]
            [slate.model.navigation :as nav]
            [slate.model.selection :as sel]
            [slate.utils :as utils]
            [slate.view :as view]
            [slate.clipboard :as clipboard]
            [slate.extras :as extras]))

;; TODO: for interceptors that aren't dependent on the viewmodel or DOM state, there's no reason
;; I couldn't write unit tests for them. Should probably do that, so I can quickly identify any
;; regressions. However, probably a good idea to wait until the history system is in place and
;; stable, so we can test the whole shebang.

(definterceptor insert
  [editor-state _ e]
  (es/insert editor-state (.-data e)))

(definterceptor delete
  [{:keys [doc selection] :as editor-state} _ui-state _e]
  (let [paragraph-type (:type (get (:children doc) (sel/caret-para selection)))]
    (if (and (sel/single? selection)
             (zero? (-> selection :start :offset))
             (or (= paragraph-type :ol) (= paragraph-type :ul)))
      (es/toggle-paragraph-type editor-state paragraph-type)
      (es/delete editor-state))))

(definterceptor enter
  [{:keys [doc selection] :as editor-state} _ui-state _e]
  (let [paragraph (get (:children doc) (sel/caret-para selection))
        paragraph-type (:type paragraph)]
    (if (and (sel/single? selection)
             (m/blank? paragraph)
             (or (= paragraph-type :ol) (= paragraph-type :ul)))
      (es/toggle-paragraph-type editor-state paragraph-type)
      (es/enter editor-state))))

(definterceptor tab
  [editor-state _ui-state _e]
  (es/insert editor-state "\t"))

;; Movement / navigation ;;
(definterceptor click
  {:include-in-history? false}
  [editor-state ui-state event]
  (let [new-sel (view/mouse-event->selection event
                                             (:doc editor-state)
                                             (:viewmodels ui-state)
                                             (:dom-elem ui-state)
                                             (:measure-fn ui-state)
                                             (:shadow-root ui-state))]
    (es/set-selection editor-state new-sel)))

(definterceptor double-click
  {:include-in-history? false}
  [editor-state ui-state event]
  (let [click-location (view/mouse-event->selection event
                                                    (:doc editor-state)
                                                    (:viewmodels ui-state)
                                                    (:dom-elem ui-state)
                                                    (:measure-fn ui-state)
                                                    (:shadow-root ui-state))]
    (-> (es/set-selection editor-state click-location)
        (es/select-whole-word))))

(definterceptor triple-click
  {:include-in-history? false}
  [editor-state ui-state event]
  (let [click-location (view/mouse-event->selection event
                                                    (:doc editor-state)
                                                    (:viewmodels ui-state)
                                                    (:dom-elem ui-state)
                                                    (:measure-fn ui-state)
                                                    (:shadow-root ui-state))]
    (-> (es/set-selection editor-state click-location)
        (es/select-whole-paragraph))))

(definterceptor drag
  {:include-in-history? false}
  [editor-state ui-state event]
  (es/set-selection editor-state (view/drag event
                                            (:doc editor-state)
                                            (:viewmodels ui-state)
                                            (:dom-elem ui-state)
                                            (:measure-fn ui-state)
                                            (:shadow-root ui-state))))

(definterceptor left
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/prev-char editor-state))

(definterceptor ctrl+left
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/prev-word editor-state))

(definterceptor shift+left
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/shift+left editor-state))

(definterceptor ctrl+shift+left
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/ctrl+shift+left editor-state))

(definterceptor right
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/next-char editor-state))

(definterceptor ctrl+right
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/next-word editor-state))

(definterceptor shift+right
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/shift+right editor-state))

(definterceptor ctrl+shift+right
  {:include-in-history? false}
  [editor-state _ui-state _e]
  (nav/ctrl+shift+right editor-state))

(definterceptor next-clause
  {:include-in-history? false}
  [editor-state _ _]
  (nav/next-clause editor-state))

(definterceptor expand-right-clause
  {:include-in-history? false}
  [editor-state _ _]
  (es/expand-caret-right editor-state (-> (nav/next-clause editor-state) :editor-state :selection)))

(definterceptor prev-clause
  {:include-in-history? false}
  [editor-state _ _]
  (nav/prev-clause editor-state))

(definterceptor expand-left-clause
  {:include-in-history? false}
  [editor-state _ _]
  (es/expand-caret-left editor-state (-> (nav/prev-clause editor-state) :editor-state :selection)))

(definterceptor next-sentence
  {:include-in-history? false}
  [editor-state _ _]
  (nav/next-sentence editor-state))

(definterceptor expand-right-sentence
  {:include-in-history? false}
  [editor-state _ _]
  (es/expand-caret-right editor-state (-> (nav/next-sentence editor-state) :editor-state :selection)))

(definterceptor prev-sentence
  {:include-in-history? false}
  [editor-state _ _]
  (nav/prev-sentence editor-state))

(definterceptor expand-left-sentence
  {:include-in-history? false}
  [editor-state _ _]
  (es/expand-caret-left editor-state (-> (nav/prev-sentence editor-state) :editor-state :selection)))

(definterceptor next-paragraph
  {:include-in-history? false}
  [editor-state _ _]
  (nav/next-paragraph editor-state))

(definterceptor expand-right-paragraph
  {:include-in-history? false}
  [editor-state _ _]
  (es/expand-caret-right editor-state (-> (nav/next-paragraph editor-state) :editor-state :selection)))

(definterceptor prev-paragraph
  {:include-in-history? false}
  [editor-state _ _]
  (nav/prev-paragraph editor-state))

(definterceptor expand-left-paragraph
  {:include-in-history? false}
  [editor-state _ _]
  (es/expand-caret-left editor-state (-> (nav/prev-paragraph editor-state) :editor-state :selection)))

(def vertical-nav-events #{:up :down :shift+up :shift+down})

(defn vertical-nav-remember-start-offset
  "When a user navigates up or down using the arrow keys, the editor looks for the offset in
   the line above or below the current line that is closest to that of the current caret, and
   navigates to that offset.

   When a user _continually_ travels up and down in the document using the arrow keys,
   the editor does not look for location in the next/previous line closest to the _current_
   caret, but rather closest to the offset of where the caret was when the user _began_ navigating
   up and down.

   For example, consider a document that looks like this, with the cursor at the end of the first line:

   ```
   this is the first line, a reasonably long one
   2nd
   And this is the final line.
   ```

   If the user presses down once, they will end up at the end of the second line, which is intuitive.
   If they press down again, what happens? If we look for an offset based only on the current position
   of the text caret, then we will end up something near the end of 'And' in the 3rd line, but if the user
   were repeatedly pressing down in a long document, that would be jarring considering how far to the right
   side they began. We can get around this and make the behavior more intuitive by remembering the pixel
   offset that vertical navigation began at.

   This function will do that, calling the supplied vertical navigation function, remembering the start
   offset when appropriate, and return a new EditorState"
  [vertical-nav-fn editor-state {:keys [input-history viewmodels dom-elem measure-fn] :as ui-state}]
  (let [last-event-vertical-nav? (vertical-nav-events (peek input-history))
        start-pos (when last-event-vertical-nav?
                    (extras/get ui-state :horizontal-start-pos nil))
        vertical-nav-editor-state (vertical-nav-fn editor-state viewmodels dom-elem measure-fn start-pos)]
    (when-not last-event-vertical-nav?
      (extras/set! ui-state
                   :horizontal-start-pos
                   (view/caret-px editor-state viewmodels measure-fn)))
    vertical-nav-editor-state))

(definterceptor down
  {:include-in-history? false}
  [editor-state ui-state _e]
  (vertical-nav-remember-start-offset view/down editor-state ui-state))

(definterceptor shift+down
  {:include-in-history? false}
  [editor-state ui-state _e]
  (vertical-nav-remember-start-offset view/shift+down editor-state ui-state))

(definterceptor up
  {:include-in-history? false}
  [editor-state ui-state _e]
  (vertical-nav-remember-start-offset view/up editor-state ui-state))

(definterceptor shift+up
  {:include-in-history? false}
  [editor-state ui-state _e]
  (vertical-nav-remember-start-offset view/shift+up editor-state ui-state))

(definterceptor start-of-line
  {:include-in-history? false}
  [editor-state ui-state _e]
  (view/start-of-line editor-state (:viewmodels ui-state)))

(definterceptor end-of-line
  {:include-in-history? false}
  [editor-state ui-state _e]
  (view/end-of-line editor-state (:viewmodels ui-state)))

(definterceptor expand-to-end-of-line
  {:include-in-history? false}
  [editor-state ui-state _e]
  (let [end-of-line-point (view/end-of-line-selection editor-state (:viewmodels ui-state))]
    (es/expand-caret-right editor-state end-of-line-point)))

(definterceptor expand-to-start-of-line
  {:include-in-history? false}
  [editor-state ui-state _e]
  (let [end-of-line-point (view/start-of-line-selection editor-state (:viewmodels ui-state))]
    (es/expand-caret-left editor-state end-of-line-point)))

(definterceptor start-of-doc
  {:include-in-history? false
   :scroll-to-caret? true}
  [editor-state _ _]
  (nav/start editor-state))

(definterceptor end-of-doc
  {:include-in-history? false
   :scroll-to-caret? true}
  [editor-state _ _]
  (nav/end editor-state))

(definterceptor select-all
  {:include-in-history? false}
  [editor-state _ _]
  (es/select-all editor-state))

(defn standard-auto-surround-should-fire?
  [{:keys [selection] :as editor-state}]
  (or (sel/range? selection)
      ;; The chars immediately before and after the pipe cursor must not be content chars
      (and (not (nav/content? (m/char-before editor-state)))
           (not (nav/content? (m/char-at editor-state))))))

;; Auto-surrounding ;;
(definterceptor auto-surround-double-quote
  {:add-to-history-immediately? true
   :should-fire? standard-auto-surround-should-fire?}
  [editor-state _ui-state _e]
  (es/auto-surround editor-state \"))

(definterceptor auto-surround-single-quote
  {:add-to-history-immediately? true
   :should-fire? (fn [{:keys [selection] :as editor-state}]
                   ;; Fire if the selection is a range or if it is a single
                   ;; selection with a non-blank character before and after it.
                   (or (sel/range? selection)
                       (and (not (nav/content? (m/char-before editor-state)))
                            (or (= (sel/caret selection) (m/len (es/current-paragraph editor-state)))
                                (not (nav/word? (m/char-at editor-state)))))))}
  [editor-state _ui-state _e]
  (es/auto-surround editor-state \'))

(definterceptor auto-surround-paren
  {:add-to-history-immediately? true
   :should-fire? standard-auto-surround-should-fire?}
  [editor-state _ui-state _e]
  (es/auto-surround editor-state "(" ")"))

;; Autocompletions ;;
(definterceptor transform-double-dash-to-em-dash
  {:add-to-history-immediately? true}
  [editor-state _ _]
  (-> (es/delete editor-state)
      (es/delete)
      (es/insert "—")))

(definterceptor insert-ol-at-paragraph-start
  {:add-to-history-immediately? true
   :should-fire? (fn [{:keys [selection]}]
                   (and (sel/single? selection) (= 2 (sel/caret selection))))}
  [editor-state _ui-state _e]
  (-> (es/delete editor-state)
      (es/delete)
      (es/toggle-paragraph-type :ol)))

(definterceptor insert-ul-at-paragraph-start
  {:add-to-history-immediately? true
   :should-fire? (fn [{:keys [selection]}]
                   (and (sel/single? selection) (= 1 (sel/caret selection))))}
  [editor-state _ui-state _e]
  (-> (es/delete editor-state)
      (es/toggle-paragraph-type :ul)))

;; Formatting
(definterceptor italic
  [editor-state _ _]
  (es/toggle-format editor-state :italic))

(definterceptor strikethrough
  [editor-state _ _]
  (es/toggle-format editor-state :strikethrough))

(definterceptor bold
  [editor-state _ _]
  (es/toggle-format editor-state :bold))

(definterceptor h1
  [editor-state _ _]
  (es/toggle-paragraph-type editor-state :h1))

(definterceptor h2
  [editor-state _ _]
  (es/toggle-paragraph-type editor-state :h2))

(definterceptor ulist
  {:add-to-history-immediately? true}
  [editor-state _ _]
  (es/toggle-paragraph-type editor-state :ul))

(definterceptor olist
  {:add-to-history-immediately? true}
  [editor-state _ _]
  (es/toggle-paragraph-type editor-state :ol))

;; Clipboard
(definterceptor cut
  {:add-to-history-immediately? true}
  [editor-state _ e]
  (clipboard/cut editor-state e))

(definterceptor copy
  [editor-state _ e]
  (clipboard/copy editor-state e))

(definterceptor paste
  {:add-to-history-immediately? true}
  [editor-state _ e]
  (clipboard/paste editor-state e))

(definterceptor open
  {:manual? true}
  [*ui-state _]
  ((:on-load @*ui-state) *ui-state))

(def universal-interceptors
  {:click click
   :double-click double-click
   :triple-click triple-click
   :drag drag
   :insert insert
   :delete delete
   :cut cut
   :copy copy
   :paste paste
   :enter enter
   :tab tab
   :left left
   :shift+left shift+left
   :right right
   :shift+right shift+right
   :ctrl+shift+right ctrl+shift+right
   :down down
   :shift+down shift+down
   :up up
   :shift+up shift+up
   "\"" auto-surround-double-quote
   "'" auto-surround-single-quote
   "(" auto-surround-paren
   "-- " transform-double-dash-to-em-dash
   "1. " insert-ol-at-paragraph-start
   "2. " insert-ol-at-paragraph-start
   "3. " insert-ol-at-paragraph-start
   "4. " insert-ol-at-paragraph-start
   "5. " insert-ol-at-paragraph-start
   "6. " insert-ol-at-paragraph-start
   "7. " insert-ol-at-paragraph-start
   "8. " insert-ol-at-paragraph-start
   "9. " insert-ol-at-paragraph-start
   "* " insert-ul-at-paragraph-start
   "- " insert-ul-at-paragraph-start})

(def win-linux-interceptors
  {:ctrl+left ctrl+left
   :ctrl+shift+left ctrl+shift+left
   :ctrl+right ctrl+right
   :ctrl+shift+right ctrl+shift+right
   :ctrl+a select-all
   ;; TODO: should these be in universal-interceptors?
   :pageup start-of-line
   :pagedown end-of-line
   :home start-of-doc
   :end end-of-doc

   :ctrl+0 next-paragraph
   :ctrl+shift+0 expand-right-paragraph
   :ctrl+9 prev-paragraph
   :ctrl+shift+9 expand-left-paragraph
   (keyword "ctrl+]") next-sentence
   (keyword "ctrl+shift+]") expand-right-sentence
   (keyword "ctrl+[") prev-sentence
   (keyword "ctrl+shift+[") expand-left-sentence
   (keyword "ctrl+,") prev-clause
   (keyword "ctrl+shift+,") expand-left-clause
   :ctrl+. next-clause
   :ctrl+shift+. expand-right-clause

   :ctrl+1 h1
   :ctrl+2 h2
   :ctrl+i italic
   :ctrl+t strikethrough
   :ctrl+b bold
   :ctrl+shift+u ulist
   :ctrl+shift+o olist
   :ctrl+o open})

(def mac-interceptors
  {:alt+left ctrl+left
   :alt+shift+left ctrl+shift+left
   :alt+right ctrl+right
   :alt+shift+right ctrl+shift+right
   :cmd+left start-of-line
   :cmd+right end-of-line
   :cmd+shift+left expand-to-start-of-line
   :cmd+shift+right expand-to-end-of-line
   :cmd+up start-of-doc
   :cmd+down end-of-doc
   :cmd+a select-all

   :cmd+0 next-paragraph
   :cmd+shift+0 expand-right-paragraph
   :cmd+9 prev-paragraph
   :cmd+shift+9 expand-left-paragraph
   (keyword "cmd+]") next-sentence
   (keyword "cmd+shift+]") expand-right-sentence
   (keyword "cmd+[") prev-sentence
   (keyword "cmd+shift+[") expand-left-sentence
   (keyword "cmd+,") prev-clause
   (keyword "cmd+shift+,") expand-left-clause
   :cmd+. next-clause
   :cmd+shift+. expand-right-clause

   :cmd+i italic
   :cmd+t strikethrough
   :cmd+b bold
   :cmd+1 h1
   :cmd+2 h2
   :cmd+shift+u ulist
   :cmd+shift+o olist

   :cmd+o open})

(def default-interceptors
  (merge universal-interceptors (if (utils/is-mac?)
                                  mac-interceptors
                                  win-linux-interceptors)))
