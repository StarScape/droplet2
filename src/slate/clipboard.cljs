(ns slate.clipboard
  (:require [slate.model.common :as model :refer [selected-content]]
            [slate.model.editor-state :as es :refer [>>=]]))

(def clipboard (atom nil))

(defn cut
  [editor-state _event]
  (es/identity-update editor-state))

(defn copy
  [editor-state _event]
  (let [content (selected-content editor-state)]
    (reset! clipboard content)
    (es/identity-update editor-state)))

(defn paste
  [editor-state _event]
  (if-let [clipboard-data @clipboard]
    (let [update (model/insert editor-state clipboard-data)]
      ;; #p update
      #_(frequencies (map :uuid #p (-> update :editor-state :doc :children)))
      update)
    (es/identity-update editor-state)))
