(ns slate.default-interceptors
  "Default set of interceptors that are added to a new editor.

  Interceptors are meant for extensibility, but most of the default events are also
  implemented using them, the idea being that eating our own dogfood is the only
  way to end up with a sufficiently flexible and ergonomic system."
  (:require-macros [slate.interceptors :refer [definterceptor]])
  (:require [slate.interceptors]
            [slate.model.common :as m]
            [slate.model.editor-state :as es :refer [>>=]]
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
  (m/insert editor-state (.-data e)))

(definterceptor delete
  [{:keys [doc selection] :as editor-state} _ui-state _e]
  (let [paragraph-type (:type (get (:children doc) (sel/caret-para selection)))]
    (if (and (sel/single? selection)
             (zero? (-> selection :start :offset))
             (or (= paragraph-type :ol) (= paragraph-type :ul)))
      (es/toggle-paragraph-type editor-state paragraph-type)
      (m/delete editor-state))))

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
  (m/insert editor-state "\u2003"))

;; Movement / navigation ;;
(definterceptor click
  {:include-in-history? false}
  [editor-state ui-state event]
  (let [new-sel (view/mouse-event->selection event
                                             (:doc editor-state)
                                             (:viewmodels ui-state)
                                             (:measure-fn ui-state))]
    (es/set-selection editor-state new-sel)))

(definterceptor drag
  {:include-in-history? false}
  [editor-state ui-state event]
  (es/set-selection editor-state (view/drag event
                                            (:doc editor-state)
                                            (:viewmodels ui-state)
                                            (:measure-fn ui-state))))

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

(definterceptor prev-clause
  {:include-in-history? false}
  [editor-state _ _]
  (nav/prev-clause editor-state))

(definterceptor next-sentence
  {:include-in-history? false}
  [editor-state _ _]
  (nav/next-sentence editor-state))

(definterceptor prev-sentence
  {:include-in-history? false}
  [editor-state _ _]
  (nav/prev-sentence editor-state))

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
   offset when appropriate, and return an EditorUpdate."
  [vertical-nav-fn editor-state {:keys [input-history viewmodels measure-fn] :as ui-state}]
  (let [last-event-vertical-nav? (vertical-nav-events (peek input-history))
        start-pos (when last-event-vertical-nav?
                    (extras/get ui-state :horizontal-start-pos nil))
        vertical-nav-update (vertical-nav-fn editor-state viewmodels measure-fn start-pos)]
    (when-not last-event-vertical-nav?
      (extras/set! ui-state
                   :horizontal-start-pos
                   (view/caret-px editor-state viewmodels measure-fn)))
    vertical-nav-update))

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
  {:include-in-history? false}
  [editor-state _ _]
  (nav/start editor-state))

(definterceptor end-of-doc
  {:include-in-history? false}
  [editor-state _ _]
  (nav/end editor-state))

(definterceptor select-all
  {:include-in-history? false}
  [editor-state _ _]
  (es/select-all editor-state))

;; Auto-surrounding ;;
(definterceptor auto-surround-double-quote
  {:add-to-history-immediately? true}
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
  {:add-to-history-immediately? true}
  [editor-state _ui-state _e]
  (es/auto-surround editor-state "(" ")"))

;; Autocompletions ;;
(definterceptor transform-double-dash-to-em-dash
  {:add-to-history-immediately? true}
  [editor-state _ _]
  (-> (m/delete editor-state)
      (>>= m/delete)
      (>>= m/insert "—")))

;; Formatting
(definterceptor ctrl+i
  [editor-state _ _]
  (m/toggle-format editor-state :italic))

(definterceptor ctrl+b
  [editor-state _ _]
  (m/toggle-format editor-state :bold))

(definterceptor h1
  [editor-state _ _]
  (es/toggle-paragraph-type editor-state :h1))

(definterceptor h2
  [editor-state _ _]
  (es/toggle-paragraph-type editor-state :h2))

(definterceptor ulist
  [editor-state _ _]
  (es/toggle-paragraph-type editor-state :ul))

(definterceptor olist
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

(def universal-interceptors
  {:click click
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
   "-- " transform-double-dash-to-em-dash})

(def win-linux-interceptors
  {:ctrl+left ctrl+left
   :ctrl+shift+left ctrl+shift+left
   :ctrl+right ctrl+right
   :ctrl+shift+right ctrl+shift+right
   :ctrl+a select-all
   :ctrl+i ctrl+i
   :ctrl+b ctrl+b
   ;; TODO: should these be in universal-interceptors?
   :pageup start-of-line
   :pagedown end-of-line
   :home start-of-doc
   :end end-of-doc
   :ctrl+1 h1
   :ctrl+2 h2})

(def mac-interceptors
  {:alt+left ctrl+left
   :alt+shift+left ctrl+shift+left
   :alt+right ctrl+right
   :alt+shift+right ctrl+shift+right
   :cmd+left start-of-line
   :cmd+right end-of-line
   :cmd+shift+left expand-to-start-of-line
   :cmd+shift+right expand-to-end-of-line
   :cmd+0 next-clause
   :cmd+9 prev-clause
   (keyword "cmd+]") next-sentence
   (keyword "cmd+[") prev-sentence
   :cmd+up start-of-doc
   :cmd+down end-of-doc
   :cmd+a select-all
   :cmd+i ctrl+i
   :cmd+b ctrl+b
   :cmd+1 h1
   :cmd+2 h2
   :cmd+u ulist
   :cmd+o olist})

(def default-interceptors
  (merge universal-interceptors (if (utils/is-mac?)
                                  mac-interceptors
                                  win-linux-interceptors)))
