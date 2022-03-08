(ns slate.interceptors)

(declare map->Interceptor)

(defmacro interceptor
  "Defines a new interceptor and returns it.
   Takes: attr-map, param-list, function-body*, for example:

   ```
   (interceptor {:input-name :click
                 :include-in-history? true}
     [editor-state full-ui-state event]
     (some-op-that-returns-new-editor-state))
   ```

   The attribute map includes the following keys:

   `:input-name`: **Required.** The input-name to be logged in the editor's input history.

   `:include-in-history?`: **Default `true`**. Whether the interceptor's return value should be
   added as a separate state in the editor's history.

   `:add-to-history-immediately?`: **Default `false`**. Whether the return value should be added to
   the editor's history immediately, without a timeout for inactivity.

   `:manual?`: **Default `false`**. If set to `true`, the typical add-to-history and DOM sync process is forgone and the interceptor
   will just be called as a normal function, **which is passed the EditorUIState atom** (not just it's contents) and the event. This exists because
   it is occasionally useful to hook into the interceptor system to execute operations that have nothing to do with Slate's editor surface
   itself, such as file-saving on a shortcut, or the undo and redo operations, which behave a little differently than the normal interceptor
   paths."
  [{:keys [input-name include-in-history? add-to-history-immediately? manual?]
    :or {include-in-history? true
         add-to-history-immediately? false
         manual? false}}
   arglist, & fn-body]
  (assert input-name "Input name is required for interceptor macro.")
  (when (not include-in-history?)
    (assert (not add-to-history-immediately?)
            "add-to-history-immediately? cannot be true if include-in-history? is false"))
  `(map->Interceptor {:input-name ~input-name
                      :manual? ~manual?
                      :include-in-history? ~include-in-history?
                      :add-to-history-immediately? ~add-to-history-immediately?
                      :interceptor-fn (fn [~@arglist] ~@fn-body)}))

;; TODO: 
(defmacro definterceptor
  "Same as `interceptor`, but `def`'s the resulting interceptor using `name`.
   
   Also has an additional arity that elides the option-map, automatically setting
   the interceptor's `:input-name` to the name used to `def` it. So this:

   ```
   (definterceptor click
     [editor-state ui-state event]
     (...))
   ```

   Is equivalent to:

   ```
   (definterceptor click
     {:input-name :click}
     [editor-state ui-state event]
     (...))
   ```

   It will likewise default :input-name to the name of the interceptor in 4-arg arity.
   However, this can be overriden by using the opts map."
  ;; [interceptor-name opts arglist & fn-body]
  ;; [interceptor-name arglist & fn-body]
  [interceptor-name & rest]
  (if (map? (first rest))
    (let [[opts arglist & fn-body] rest
          options (update opts :input-name #(or % (keyword interceptor-name)))]
      `(def ~interceptor-name (interceptor ~options ~arglist ~@fn-body)))
    (let [[arglist & fn-body] rest]
      `(definterceptor ~interceptor-name {} ~arglist ~@fn-body))))

(comment
  (macroexpand
   '(interceptor
     {:input-name :click
      :include-in-history? true}
     [editor-state full-ui-state event]
     (+ 1 2 3)
     (+ 4 5 6)))

  (macroexpand
   '(definterceptor click
      {:input-name :click
       :include-in-history? true}
      [editor-state full-ui-state event]
      (+ 1 2 3)
      (+ 4 5 6)))

  (macroexpand
   '(definterceptor click
      [editor-state full-ui-state event]
      (+ 1 2 3)
      (+ 4 5 6)))

  (macroexpand
   '(definterceptor click
      [editor-state full-ui-state event]
      (+ 1 2 3)))
  )
