(ns slate.editor-ui-state
  (:require-macros [slate.interceptors :refer [interceptor definterceptor]])
  (:require [clojure.spec.alpha :as s]
            [slate.interceptors :as interceptors]
            [slate.model.editor-state-history :as history]
            [slate.events :as events]
            [clojure.spec.test.alpha :as stest]
            [slate.model.editor-state :as es]))

(s/def ::id uuid?)
(s/def ::history ::history/editor-state-history)
(s/def ::dom-elem #(instance? js/HTMLElement %))
(s/def ::hidden-input ::dom-elem)
(s/def ::measure-fn (s/spec :args (s/cat :text string?
                                         :formats (s/coll-of keyword? :kind set?))
                            :ret int?))
(s/def ::input-history (s/coll-of any?))
(s/def ::viewmodels (s/coll-of any?))
(s/def ::interceptors ::interceptors/interceptor-map)

(s/def ::editor-ui-state (s/keys :req-un [::id
                                          ::history
                                          ::dom-elem
                                          ::hidden-input
                                          ::measure-fn
                                          ::input-history
                                          ::viewmodels
                                          ::interceptors]))

(defn- debounce [ms f]
  (let [timer (atom nil)]
    (fn [& xs]
      (js/clearTimeout @timer)
      (reset! timer (js/setTimeout #(apply f xs) ms)))))

;; FIX
(defn update-viewmodels-to-history-tip [x] x)

(defn sync-dom! [ui-state]
  ;; sync-dom should need: viewmodels, changelist, dom-elem, and measure-fn
  )

(defn integrate-tip! [ui-state-atom]
  (swap! ui-state-atom update :history history/add-tip-to-backstack))

(def integrate-tip-after-wait! (debounce 3000 integrate-tip!))

(defn fire-interceptor!
  [ui-state-atom interceptor event]
  (let [ui-state @ui-state-atom ; only deref once a cycle
        editor-state (history/current-state (:history ui-state))
        new-editor-state (interceptor editor-state ui-state event)
        new-ui-state (-> ui-state
                         (update :history history/set-tip new-editor-state)
                         (update :input-history events/add-key-to-history (:input-name interceptor))
                         (update-viewmodels-to-history-tip))
        new-ui-state (if (and (:add-to-history-immediately? interceptor)
                              (:include-in-history? interceptor))
                       (update new-ui-state
                               :history
                               history/add-tip-to-backstack
                               new-editor-state)
                       new-ui-state)]
    (when-not (:no-effects? interceptor)
      (sync-dom! new-ui-state))

    (reset! ui-state-atom new-ui-state)

    (when (and (:include-in-history? interceptor)
               (not (:add-to-history-immediately? interceptor)))
      ;; NOTE: make sure wait is on a per-instance basis
      (integrate-tip-after-wait! ui-state-atom))))
