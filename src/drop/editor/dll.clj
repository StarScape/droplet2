(ns drop.editor.dll)

(defmacro node-uuid [node] `(:uuid (.-value ~node)))
