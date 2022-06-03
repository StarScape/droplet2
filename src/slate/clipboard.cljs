(ns slate.clipboard
  (:require [clojure.string :as str]
            [slate.model.common :as model :refer [TextContainer
                                                  selected-content
                                                  text]]
            [slate.model.run :as r :refer [Run]]
            [slate.model.paragraph :as p :refer [Paragraph]]
            [slate.model.editor-state :as es :refer [>>=]]
            [slate.model.selection :as sel]))

(def ^:const mime-plaintext "text/plain")
(def ^:const mime-html "text/html")

;; JS's clipboardData store its data as a string, so it's more efficient, when
;; pasting from Droplet straight back into Droplet, to not bother with serializing
;; and deserializing an object=, and instead just stash it in a local atom. If Slate
;; sees that the data in the clipboard is from itself, it will just pull the stashed
;; content from this atom instead.
(def *clipboard (atom nil))

(defn- content-text [content]
  (let [first-child (first content)]
    (cond
      (satisfies? TextContainer content)
      (text content)

      (satisfies? TextContainer first-child)
      (cond
        (= Run (type first-child))
        (reduce str (map text content))

        (= Paragraph (type first-child))
        (str/join "\n" (map text content))))))

(defn copy-to-clipboard!
  "Copies the content to the clipboard atom with a unique id matching that added to the system clipboard."
  [content event]
  (let [copy-id (str (random-uuid))]
    (.. event -clipboardData (setData "slate-copy-id" copy-id))
    (.. event -clipboardData (setData mime-plaintext (content-text content)))
    (reset! *clipboard {:content content
                        :copy-id copy-id})))

(defn cut
  "Cuts the currently selected content to the clipboard and returns an EditorUpdate."
  [{:keys [selection] :as editor-state} event]
  (if (sel/range? selection)
    (let [content (selected-content editor-state)]
      (copy-to-clipboard! content event)
      (model/delete editor-state))
    (es/identity-update editor-state)))

(defn copy
  "Copies the currently selected content to the clipboard and returns an (empty) EditorUpdate."
  [editor-state event]
  (let [content (selected-content editor-state)]
    (copy-to-clipboard! content event)
    (es/identity-update editor-state)))

(defn paste
  "Pastes the currently selected content into the editor and returns an EditorUpdate."
  [editor-state event]
  (let [clipboard-data @*clipboard
        slate-copy-id (.. event -clipboardData (getData "slate-copy-id"))
        paste-from-slate? (= slate-copy-id (:copy-id clipboard-data))
        plain-text? (.. event -clipboardData -types (includes mime-plaintext))]
    (cond
      paste-from-slate?
      (model/insert editor-state (:content clipboard-data))

      plain-text?
      (model/insert editor-state (.. event -clipboardData (getData "Text")))

      :else
      (es/identity-update editor-state))))
