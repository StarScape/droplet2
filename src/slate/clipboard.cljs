(ns slate.clipboard
  (:require [slate.model.common :as model :refer [selected-content]]
            [slate.model.editor-state :as es :refer [>>=]]
            [slate.model.selection :as sel]))

;; JS's clipboardData store its data as a string, so it's more efficient, when
;; pasting from Droplet straight back into Droplet, to not bother with serializing
;; and deserializing an object and just stash it in a local atom. If Slate sees that the data
;; in the clipboard is from itself, it will just pull the stashed content from this atom instead.
(def *clipboard (atom nil))

(defn cut
  "Cuts the currently selected content to the clipboard and returns an EditorUpdate."
  [{:keys [selection] :as editor-state} _event]
  (if (sel/range? selection)
    (let [content (selected-content editor-state)]
      (reset! *clipboard content)
      (model/delete editor-state))
    (es/identity-update editor-state)))

(defn copy
  "Copies the currently selected content to the clipboard and returns an (empty) EditorUpdate."
  [editor-state _event]
  (let [content (selected-content editor-state)]
    (reset! *clipboard content)
    (es/identity-update editor-state)))

(defn paste
  "Pastes the currently selected content into the editor and returns an EditorUpdate."
  [editor-state _event]
  (if-let [clipboard-data @*clipboard]
    (model/insert editor-state clipboard-data)
    (es/identity-update editor-state)))
