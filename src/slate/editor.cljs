(ns slate.editor
  "Code for dealing with the whole of the editor state. Generally speaking most things belong in a more specific
   namespace: view, viewmodel, model, or whatever layer of Slate they most specifically belong to. A few things
   are broad enough to deserve being here, though."
  (:require [slate.map-utils :refer [remove-nil-vals-from-map]]
            [slate.view :as view]
            [slate.viewmodel :as vm]))

(defn para->vm-para
  "Converts a [[Paragraph]] to a [[ParagraphViewModel]], using the width and measure-fn present
   in the editor state supplied as the second argument."
  [paragraph editor-state]
  (vm/from-para paragraph 200 (:measure-fn editor-state)))

(defn update-viewmodels
  "Updates the editor state's viewmodels according to the transaction, deleting
   viewmodels that have been removed and updating those that have been changed or
   inserted."
  [editor-state transaction]
  (let [{:keys [viewmodels]} editor-state
        {:keys [changed-uuids inserted-uuids deleted-uuids doc]} transaction
        new-doc-children (:children doc)
        updated-vms (as-> viewmodels vms
                      (apply dissoc vms deleted-uuids)
                      (reduce (fn [new-vms changed-uuid]
                                (assoc new-vms changed-uuid (para->vm-para (get new-doc-children changed-uuid) editor-state)))
                              vms (concat inserted-uuids changed-uuids)))]
    (assoc editor-state :viewmodels updated-vms)))

;; TODO: make transactions filter out certain things (like :viewmodels), and account for the :para-ids element
(defn apply-transaction
  "Applies the given transaction to the editor state and returns the new transaction"
  [editor-state transaction]
  (let [vals-to-merge (dissoc transaction :changed-uuids :deleted-uuids :inserted-uuids) #_(select-keys transaction [:doc :selection])
        new-editor-state (merge editor-state vals-to-merge)]
    (update-viewmodels new-editor-state transaction)))

(defn apply-transaction!
  "Mutating version of `apply-transaction`. Updates the state atom directly, and also returns the new state swapped in.
   **Does not** sync the DOM."
  [editor-state-atom transaction]
  (swap! editor-state-atom apply-transaction transaction))

;; TODO: move to view namespace?
(defn sync-dom
  "Brings the DOM up to date with the latest changes changes to the document.
   If passed just the editor state, a full render of the document will be performed.
   This is mainly used when the application is first started.
   If passed an editor state *and* the last transaction, only the paragraphs that have
   changed will be updated."
  ([{:keys [viewmodels doc selection dom-elem] :as _editor-state}]
   (let [vm-paras (map #(get viewmodels (:uuid %)) (:children doc))
         rendered (view/vm-paras->dom vm-paras selection)]
     (set! (.-innerHTML dom-elem) rendered)))
  ([{:keys [viewmodels doc selection dom-elem]} transaction]
   #p "yus"
   (let [vm-paras (map #(get viewmodels (:uuid %)) (:children doc))
         rendered (view/vm-paras->dom vm-paras selection)]
     (set! (.-innerHTML dom-elem) rendered))))
