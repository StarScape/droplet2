(ns slate.model.history
  "The history object holds a series of successive EditorStates and
   the changes between them. This namespace defines functions for manipulating
   and managing the history object.

   The history consists of a few parts: the backstack (a list of EditorStates),
   the current-state-index (which references an index into the backstack), and the
   *tip*. The tip is also an EditorState, but represents a current state that *has
   not yet been incorporated into the backstack*. The tip can be modified directly --
   and doing so will not result in a new state being added to the history. If you want
   to add the history's tip to the backstack, an explicit call to `add-tip-to-backstack`
   is needed.

   To understand the need for tip, consider the behavior of a typical text editor.
   Not EVERY single state change is preserved. For example, if you type ten characters
   rapidly and then pause, most editors will record this as a single level of undo.
   The tip is essentially saying 'here is a current state of the editor which does not
   yet merit being broken into separate levels of undo-ability.' The code in `fire-interceptor!`
   will automatically incoporate the tip into the backstack after a certain period of inactivity"
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [slate.model.editor-state :as es :refer [editor-state]])
  (:refer-clojure :exclude [next]))

(s/def ::backstack (s/coll-of ::es/editor-update))
(s/def ::current-state-index nat-int?)
(s/def ::tip (s/nilable ::es/editor-update))

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

(s/fdef add-tip-to-backstack
        :args (s/cat :history ::editor-state-history)
        :ret ::editor-state-history)

(defn add-tip-to-backstack
  "Removes `history`'s tip and incorporates it into the backstack as the last element."
  [{:keys [tip] :as history}]
  (if (nil? tip)
    history
    (-> history
        (update :backstack conj tip)
        (assoc :tip nil))))

(s/fdef current
  :args (s/cat :history ::editor-state-history)
  :ret ::es/editor-update)

(defn current
  "Returns the current entry of `history` (the one that should currently be displayed on the screen).
   This should be used rather than accessing any of the fields in the `EditorStateHistory` directly."
  [{:keys [backstack current-state-index tip] :as _history}]
  (if (< current-state-index (count backstack))
    (nth backstack current-state-index)
    tip))

(s/fdef current-state
  :args (s/cat :history ::editor-state-history)
  :ret ::es/editor-state)

(defn current-state
  "Returns the current `EditorState` of `history` (the one that should currently be displayed on the screen).
   This should be used rather than accessing any of the fields in the `EditorStateHistory` directly."
  [history]
  (:editor-state (current history)))

(s/fdef prev
  :args (s/cat :history ::editor-state-history)
  :ret ::es/editor-update)

(defn prev
  "Returns the previous entry in `history` (the one immediately preceeding the
   one currently displayed on the screen). This should be used rather than
   accessing any of the fields in the `EditorStateHistory` directly.

   **Will** return nil if there is no previous entry."
  [history]
  (get (:backstack history) (dec (:current-state-index history))))

(s/fdef prev-state
  :args (s/cat :history ::editor-state-history)
  :ret ::es/editor-state)

(defn prev-state
  "Returns the previous `EditorState` in `history` (the one immediately preceeding the
   one currently displayed on the screen). This should be used rather than
   accessing any of the fields in the `EditorStateHistory` directly.

   **Will** return nil if there is no previous state."
  [history]
  (:editor-state (prev history)))

(s/fdef next
  :args (s/cat :history ::editor-state-history)
  :ret ::es/editor-update)

(defn next
  "Returns the next entry in `history` (the one immediately succeeding the
   one currently displayed on the screen). This should be used rather than
   accessing any of the fields in the `EditorStateHistory` directly.

   **Will** return nil if there is no previous state."
  [history]
  (get (:backstack history) (inc (:current-state-index history))))

(s/fdef next-state
  :args (s/cat :history ::editor-state-history)
  :ret ::es/editor-state)

(defn next-state
  "Returns the next `EditorState` in `history` (the one immediately succeeding the
   one currently displayed on the screen). This should be used rather than
   accessing any of the fields in the `EditorStateHistory` directly.

   **Will** return nil if there is no previous state."
  [history]
  (:editor-state (next history)))

(s/fdef has-undo?
  :args (s/cat :history ::editor-state-history)
  :ret boolean?)

(defn has-undo?
  "Returns true if argument `history` has a state before the current."
  [{:keys[current-state-index] :as history}]
  (pos? current-state-index))

(s/fdef has-redo?
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
  "Reverts `history` to the previous entry, if one exists, (identity otherwise)."
  [{:keys [tip] :as history}]
  (if (has-undo? history)
    (-> (if (some? tip) (add-tip-to-backstack history) history)
        (update :current-state-index dec))
    history))

(s/fdef redo
  :args (s/cat :history ::editor-state-history)
  :ret ::editor-state-history)

(defn redo
  "Restores `history` to the previously undone entry, if one exists (identity otherwise)."
  [history]
  (if (has-redo? history)
    (update history :current-state-index inc)
    history))

(s/fdef set-tip
  :args (s/cat :history ::editor-state-history
               :tip ::es/editor-update)
  :ret ::editor-state-history)

(defn set-tip
  "Sets the `tip` to the provided val `new-tip`. If `(has-redo?)` is true, the redos will be removed.
   This is what should be called whenever you want to update the editor state without yet adding
   anything to the backstack. For adding the tip to the backstack, see `add-tip-to-backstack`."
  [history new-tip]
  (let [backstack (if (has-redo? history)
                    (subvec (:backstack history) 0 (inc (:current-state-index history)))
                    (:backstack history))]
    (assoc history
           :tip new-tip
           :backstack backstack
           :current-state-index (count backstack))))

(s/fdef init
  :args (s/cat :state-or-update (s/or :state ::es/editor-state
                                      :update ::es/editor-update))
  :ret ::editor-state-history)

(defn init
  "Returns a new history object with the tip set to the provided EditorState.

   Arguments:
   - `state-or-update`: An initial `EditorState` or `EditorUpdate` to serve as the beginning of history.
   An `EditorState` argument will be converted to an `EditorUpdate` with no changelist."
  [state-or-update]
  (let [initial (cond
                  (instance? es/EditorUpdate state-or-update)
                  state-or-update

                  (instance? es/EditorState state-or-update)
                  (es/identity-update state-or-update)

                  :else
                  (throw "Error in history/init: argument must be either an EditorUpdate or EditorState."))]
    {:tip nil, :backstack [initial], :current-state-index 0}))

;; TODO: look into orchestra en vez de usar esto
(stest/instrument)
