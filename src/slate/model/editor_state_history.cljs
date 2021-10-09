(ns slate.model.editor-state-history
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(defrecord EditorState [])
(s/def ::editor-state #(instance? EditorState %))

(s/def ::backstack (s/coll-of ::editor-state))
(s/def ::current-state-index nat-int?)
#_(s/def ::current-state-index (s/or int? #(= :tip %)))
(s/def ::tip (s/nilable ::editor-state))

(s/def ::editor-state-history
  (s/and (s/keys :req-un [::backstack
                          ::current-state-index
                          ::tip])
         ;; No nil tip and empty backstack at the same time.
         ;; Some sort of history has to exist at all points.
         (fn [{:keys [backstack tip]}] (or (seq backstack) (some? tip)))
         ;; If the current state is not the most recent (i.e. there are
         ;; redos available), it makes no sense for there to be a tip.
         (fn [{:keys [backstack current-state-index tip]}]
           (if (< current-state-index (count backstack))
             (= tip nil)
             true))
         ;; The current-state-index points either to the tip (indicated as len-of-backstack + 1),
         ;; or somewhere in the backstack.
         (fn [{:keys [backstack current-state-index tip]}]
           (let [expected-tip-index (count backstack)]
             (if (nil? tip)
               (< current-state-index expected-tip-index)
               (= current-state-index expected-tip-index))))))

;; invalid
(def ed1 {:backstack []
          :current-state-index 0
          :tip nil})
(def ed2 {:backstack []
          :current-state-index 0
          :tip (->EditorState)})
(def ed3 {:backstack [(->EditorState)]
          :current-state-index 1
          :tip (->EditorState)})
(def ed4 {:backstack [(->EditorState) (->EditorState) (->EditorState) (->EditorState)]
          :current-state-index 1
          :tip nil})
(def ed5 {:backstack [(->EditorState) (->EditorState) (->EditorState) (->EditorState)]
          :current-state-index 3
          :tip nil})
;; invalid
(def ed6 {:backstack [(->EditorState) (->EditorState) (->EditorState) (->EditorState)]
          :current-state-index 4
          :tip nil})
(def ed7 {:backstack [(->EditorState) (->EditorState) (->EditorState) (->EditorState)]
          :current-state-index 4
          :tip (->EditorState)})
;; invalid
(def ed8 {:backstack [(->EditorState) (->EditorState) (->EditorState) (->EditorState)]
          :current-state-index 1
          :tip (->EditorState)})

(s/valid? ::editor-state-history ed1)
(s/valid? ::editor-state-history ed2)
(s/valid? ::editor-state-history ed3)
(s/valid? ::editor-state-history ed4)
(s/valid? ::editor-state-history ed5)
(s/valid? ::editor-state-history ed6)
(s/valid? ::editor-state-history ed7)
(s/valid? ::editor-state-history ed8)

(s/fdef add-tip-to-backstack
        :args (s/cat :history ::editor-state-history)
        :ret ::editor-state-history)

(defn add-tip-to-backstack
  "Removes `history`'s tip and incorporates it into the backstack as the last element."
  [{:keys [tip] :as history}]
  (-> history
      (update :backstack conj tip)
      (update :current-state-index dec)
      (assoc :tip nil)))

(s/fdef current-state
  :args (s/cat :history ::editor-state-history)
  :ret ::editor-state)

(defn current-state
  "Returns the current state of `history` (the one that should currently be displayed on the screen).
   This should be used rather than accessing any of the fields in the `EditorStateHistory` directly."
  [{:keys [backstack current-state-index tip] :as history}]
  (if (< current-state-index (count backstack))
    (nth backstack current-state-index)
    tip))

(s/fdef prev-state
  :args (s/cat :history ::editor-state-history)
  :ret ::editor-state)

(defn prev-state
  "Returns the previous state of `history` (the one immediately preceeding the
   one currently displayed on the screen). This should be used rather than
   accessing any of the fields in the `EditorStateHistory` directly.

   **Will** return nil if there is no previous state."
  [history]
  (get (:backstack history) (dec (:current-state-index history))))

(s/fdef has-undo?
  :args (s/cat :history ::editor-state-history)
  :ret boolean?)

(defn has-undo?
  "Returns true if argument `history` has a state before the current."
  [{:keys[current-state-index backstack] :as history}]
  (pos? current-state-index))

(s/fdef has-undo?
  :args (s/cat :history ::editor-state-history)
  :ret boolean?)

(defn has-redo?
  "Returns true if argument `history` has a state after the current."
  [{:keys [current-state-index backstack] :as history}]
  (< current-state-index (dec (count backstack))))

(s/fdef undo
  :args (s/cat :history ::editor-state-history)
  :ret ::editor-state-history)

(defn undo
  "Reverts `history` to the previous state, if one exists, (identity otherwise)."
  [{:keys [tip] :as history}]
  (if (has-undo? history)
    (-> (if (nil? tip)
          history
          (add-tip-to-backstack history))
        (update :current-state-index dec))
    history))

(s/fdef redo
  :args (s/cat :history ::editor-state-history)
  :ret ::editor-state-history)

(defn redo
  "Restores `history` to the previously undone state, if one exists (identity otherwise)."
  [history]
  (if (has-redo? history)
    (update history :current-state-index inc)
    history))

(s/fdef set-tip
  :args (s/cat :history ::editor-state-history)
  :ret ::editor-state-history)

(defn set-tip
  "Sets the `tip` to the provided `EditorState`. If `has-redo?` is true, the redos will be removed.
   This is what should be called whenever you want to update the editor state without yet adding
   anything to the backstack. For adding the tip to the backstack, see `add-tip-to-backstack`."
  [history new-tip]
  (let [backstack (if (has-redo? history)
                    (subvec history 0 (:current-state-index history))
                    (:backstack history))]
    (assoc history
           :tip new-tip
           :backstack backstack
           :current-state-index (count backstack))))

(stest/instrument)
