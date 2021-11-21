(ns slate.interceptors
  (:require-macros [slate.interceptors :refer [interceptor definterceptor]])
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [slate.model.editor-state :as es]))

(defrecord Interceptor [interceptor-fn
                        input-name
                        include-in-history?
                        add-to-history-immediately?
                        no-effects?]
  IFn
  (-invoke [this editor-state extras event-obj]
    ((:interceptor-fn this) editor-state extras event-obj)))

(comment
  (def i (interceptor
          {:input-name :click
           :include-in-history? true}
          [editor-state full-ui-state event]
          (prn editor-state full-ui-state event))))

(s/def ::pattern (s/and :string string? keyword?))
(s/def ::interceptor-fn (s/spec :args (s/cat :state ::es/editor-state
                                             :ui-state any?)
                                :ret ::es/editor-state))
(s/def ::input-name (s/nilable ::pattern))
(s/def ::include-in-history? boolean?)
(s/def ::add-to-history-immediately? boolean?)
(s/def ::interceptor (s/keys :req-un [::interceptor-fn
                                      ::input-name
                                      ::include-in-history?
                                      ::add-to-history-immediately?]
                             :opt-un [::input-name]))
(s/def ::interceptor-map (s/map-of ::pattern ::interceptor))

;; (s/fdef reg-interceptor
;;   :args (s/alt
;;          :no-input-name (s/cat :map ::interceptor-map
;;                                :pattern ::pattern
;;                                :interceptor ::interceptor-fn)
;;          :with-input-name (s/cat :map ::interceptor-map
;;                                  :pattern ::pattern
;;                                  :interceptor ::interceptor-fn
;;                                  :input-name ::input-name))
;;   :ret ::interceptor-map)

;; (defn reg-interceptor
;;   ([interceptor-map pattern interceptor-fn input-name]
;;    (assoc interceptor-map pattern {:interceptor-fn interceptor-fn
;;                                    :input-name input-name})))

;; (s/fdef unreg-interceptor
;;   :args (s/cat :map ::interceptor-map
;;                :pattern ::pattern)
;;   :ret ::interceptor-map)

;; (defn unreg-interceptor
;;   [interceptor-map pattern]
;;   (dissoc interceptor-map pattern))

(stest/instrument)
