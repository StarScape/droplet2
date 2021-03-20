(ns slate.editor
  "Code for dealing with the whole of the editor state. Generally speaking most things belong in a more specific
   namespace: view, viewmodel, model, or whatever layer of Slate they most specifically belong to. A few things
   are broad enough to deserve being here, though."
  (:require [slate.view :as view]
            [slate.viewmodel :as vm]))


;; TODO: make transactions filter out certain things (like :viewmodels), and account for the :para-ids element
(defn apply-transaction
  "Applies the given transaction to the editor state and returns the new transaction"
  [editor-state transaction]
  (let [merged (merge editor-state transaction)]
    (assoc merged :viewmodels (vm/from-doc (:doc merged) 200 (:measure-fn editor-state)))))

(defn apply-transaction!
  "Side-affecting version of `apply-transaction`. Updates the state atom directly, and also returns the new state swapped in."
  [editor-state-atom transaction]
  (swap! editor-state-atom apply-transaction transaction))

;; TODO: move to view namespace?
(defn sync-dom
  "Brings the DOM up to date with the latest changes changes to the document.
   Takes the editor state as argument its argument."
  [{:keys [viewmodels doc selection dom-elem] :as _editor-state}]
  (let [vm-paras (map #(get viewmodels (:uuid %)) (:children doc))
        rendered (view/vm-paras->dom vm-paras selection)]
    (set! (.-innerHTML dom-elem) rendered)))
