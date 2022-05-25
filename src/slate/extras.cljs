(ns slate.extras
  "Way of associating additional information with an editor's UI state without
   modifying the UI state itself. Can be used inside interceptors."
  (:refer-clojure :exclude [set! get]))

(def ^:private extras-map (js/WeakMap.))

(defn init-instance-extras!
  "Initializes instance extras map in global store if not already initialized."
  [{:keys [id] :as _ui-state}]
  (when-not (.has extras-map id)
    (.set extras-map id {})))

(defn assoc-extra!
  [{:keys [id] :as _ui-state} key val]
  (.set extras-map id (assoc (.get extras-map id) key val)))

(defn get
  "Retrieves the extra field `key`, initializing it to `default-val` if not already initialized."
  [{:keys [id] :as ui-state} key default-val]
  (init-instance-extras! ui-state)

  (let [extras (.get extras-map id)]
    (if (contains? extras key)
      (clojure.core/get extras key)
      (do
        (assoc-extra! ui-state key default-val)
        default-val))))

(defn set!
  "Sets the extra field `key` to `new-val`. Returns `new-val`."
  [ui-state key new-val]
  (init-instance-extras! ui-state)
  (assoc-extra! ui-state key new-val)
  new-val)
