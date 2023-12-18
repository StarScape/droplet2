(ns slate.clipboard
  (:require [slate.filetypes.html-export :refer [slate->html]]
            [slate.filetypes.html-import :refer [html->slate]]
            [slate.model.common :as model :refer [selected-content as-plain-text]]
            [slate.model.editor-state :as es]
            [slate.model.selection :as sel]))

(def ^:const mime-plaintext "text/plain")
(def ^:const mime-html "text/html")

;; JS's clipboardData stores its data as a string, so it's more efficient, when
;; pasting from Droplet straight back into Droplet, to not bother with serializing
;; and deserializing an object, and instead just stash it in a local atom. If Slate
;; sees that the data in the clipboard is from itself, it will just pull the stashed
;; content from this atom instead.
(def *clipboard (atom nil))

(defn- set-clipboard-data
  [event format data]
  (.. event -clipboardData (setData format data)))

(defn copy-to-clipboard!
  "Copies the content to the clipboard atom with a unique id matching that added to the system clipboard."
  [content event]
  (let [copy-id (str (random-uuid))]
    (set-clipboard-data event "slate-copy-id" copy-id)
    (set-clipboard-data event mime-plaintext (as-plain-text content))
    (set-clipboard-data event mime-html (slate->html content))
    (reset! *clipboard {:content content
                        :copy-id copy-id})))

(defn copy
  "Copies the currently selected content to the clipboard and returns an unchanged EditorState."
  [editor-state event]
  (let [content (selected-content editor-state)]
    (copy-to-clipboard! content event)
    editor-state))

(defn cut
  "Cuts the currently selected content to the clipboard and returns the new EditorState"
  [{:keys [selection] :as editor-state} event]
  (if (sel/range? selection)
    (-> editor-state
        (copy event)
        (es/delete))
    editor-state))

(defn paste
  "Pastes the currently selected content into the editor and returns a new EditorState."
  [editor-state event]
  (let [clipboard-data @*clipboard
        slate-copy-id (.. event -clipboardData (getData "slate-copy-id"))
        paste-from-slate? (= slate-copy-id (:copy-id clipboard-data))
        html? (.. event -clipboardData -types (includes mime-html))
        html-converted (when html?
                         (try
                           (html->slate (.. event -clipboardData (getData mime-html)))
                           (catch js/Error _e nil)))
        plain-text? (.. event -clipboardData -types (includes mime-plaintext))]
    (cond
      paste-from-slate?
      (es/insert editor-state (:content clipboard-data))

      (and html? html-converted)
      (es/insert editor-state html-converted)

      plain-text?
      (es/insert editor-state (.. event -clipboardData (getData mime-plaintext)))

      :else
      editor-state)))
