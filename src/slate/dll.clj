(ns slate.dll)

;; TODO: get rid of this
(defmacro node-uuid [node] `(:uuid (.-value ~node)))
