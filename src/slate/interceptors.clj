(ns slate.interceptors)

(defmacro interceptor
  [{:keys [input-name include-in-history?]
    :or {include-in-history? true}}
   arglist, fn-body]
  `(map->Interceptor {:input-name ~input-name
                     :include-in-history? ~include-in-history?
                     :interceptor-fn (fn [~@arglist] ~fn-body)}))

(comment
  (macroexpand
   '(interceptor
     {:input-name :click
      :include-in-history? true}
     [editor-state full-ui-state event]
     (+ 1 2 3)))

  (macroexpand
   '(definterceptor click
      {:input-name :click
       :include-in-history? true}
      [editor-state full-ui-state event]
      (+ 1 2 3))))
