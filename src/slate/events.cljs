(ns slate.events
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [slate.model.common :as m]
            [slate.model.doc :as doc]
            [slate.navigation :as nav]
            [slate.model.selection :as sel]
            [slate.view :as view]))

;; Maximum number of keys typed to remember
(def ^:const max-input-history 10)

(defn add-key-to-history
  "Adds the key to the vector of input history, dropping the least recent key
   typed off the history vector if necessary, and returns the new history vector."
  [input-history key]
  (conj (if (< (count input-history) max-input-history)
          input-history
          (subvec input-history 1))
        key))

#_(defn fire-interceptor!
  "Calls the interceptor with the current editor state and the JS `Event` object as its args
   (and optionally, any additional args you wish to pass it) and re-synces the DOM.

   If no interceptor function is provided, the event will be parsed and the matching registered
   interceptor (if any) will be fired (TODO TODO TODO)."
  [interceptor-fn state-atom event & args]
  ;; These two are common-ish bugs when I refactor things around, and the error
  ;; message
  ;; is less than helpful, so it's good to just give an explicit failure here
  ;; instead.
  {:pre [(some? interceptor-fn) (some? state-atom)]}
  (let [transaction (apply interceptor-fn @state-atom event args)
        ;; TODO: have to make the interceptors return transactions
        new-state (editor/apply-transaction! state-atom transaction)]
    (editor/sync-dom new-state transaction)))




