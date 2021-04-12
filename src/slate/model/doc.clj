(ns slate.model.doc)

;; (defmacro transact->
;;   "Convenience macro for combining a bunch of transactions.
;;    The first arg in the initial destructuring binding of some operation
;;    that returns a transaction, for example `[{:keys [doc selection]} (insert my-doc my-sel my-content)]`.
;;    The remaining body of the macro consists of function calls which return transactions. The trick is, the
;;    same destructuring as in the initial binding will be performed on every result, and passed on to the
;;    next form. In other words, you can thread all your transaction properties through a series of operations.

;;    For example:

;;    ```
;;    (transact-> [{doc :doc, sel :selection} (sl/insert my-document my-selection \"foo!\")]
;;                 (sl/insert doc sel \"foo!\")
;;                 (sl/delete sel)
;;                 (sl/insert doc sel \", bar\"))
;;    ```

;;    This will insert foo!, backspace the !, insert ', bar', and then return the final transaction."
;;   [initial-binding & operations]
;;   (let [[destructure-map#, first-op] initial-binding]
;;     ;; current-binding => [destruct-map, function-call-form]
;;     `(loop [last-transaction# ~first-op
;;             [op# & ops#] ~operations]
;;        (if (seq ops#)
;;          (let ~(destructure [destructure-map# `last-transaction#])
;;            (let [result# op#]
;;              (recur result# ops#)))
;;          last-transaction#))))

;; #_(loop [last-transaction (sl/insert doc sel "foo!")
;;        [op & ops] [(sl/insert doc sel "foo!") (sl/delete sel) (sl/insert doc sel ", bar")]]
;;   (let [{doc :doc, sel :selection} last-transaction]
;;     (recur (op) ops)))

;; (macroexpand-1
;;  '(transact-> [{doc :doc, sel :selection} (sl/insert doc sel "foo!")]
;;               (sl/insert doc sel "foo!")
;;               (sl/delete sel)
;;               (sl/insert doc sel ", bar")))

;; (let [destruct-map {'d :doc, 's :selection}]
;;   (let (destructure [destruct-map (sl/insert doc sel "foo!")]))
;;   (loop []))

;; (with-bs
;;   )

;; #_(comment
;;   #_(transact-> [{:keys [doc, selection]} (delete doc selection)]
;;               (enter doc selection "foo!"))

;;   (let [document nil, selection nil]
;;     (transact-> [{doc :doc, sel :selection} (sl/insert document selection "foo!")]
;;                 (sl/insert doc sel "foo!")
;;                 (sl/delete sel)
;;                 (sl/insert doc sel ", bar"))))

;; #_(comment
;;   (merge-transactions {:doc :d1, :selection :s1, :changed-ids [:p1 :p2]}
;;                       {:doc :d2, :changed-ids [:p3]}))
