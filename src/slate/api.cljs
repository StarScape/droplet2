(ns slate.api
  "Functions for invoking slate behavior from outside its module."
  (:require [slate.editor-ui-state :as ui-state]
            [slate.default-interceptors :as default-ints]))

(defn next-clause! [*ui-state]
  (ui-state/fire-interceptor! *ui-state default-ints/next-clause nil))

(defn prev-clause! [*ui-state]
  (ui-state/fire-interceptor! *ui-state default-ints/prev-clause nil))

(defn next-sentence! [*ui-state]
  (ui-state/fire-interceptor! *ui-state default-ints/next-sentence nil))

(defn prev-sentence! [*ui-state]
  (ui-state/fire-interceptor! *ui-state default-ints/prev-sentence nil))

(defn next-paragraph! [*ui-state]
  (ui-state/fire-interceptor! *ui-state default-ints/next-paragraph nil))

(defn prev-paragraph! [*ui-state]
  (ui-state/fire-interceptor! *ui-state default-ints/prev-paragraph nil))

(defn when-ready
  "Executes a provided function when the Slate editor is ready."
  [*ui-state f]
  (if (:ready? @*ui-state)
    (f)
    (let [watch-key (rand-int 100000000)]
      (add-watch *ui-state watch-key
                 (fn [_ _ _ new-state]
                   (when (:ready? new-state)
                     ; if f modifies the atom, watch needs to be removed before calling f to prevent an infinite loop
                     (remove-watch *ui-state watch-key)
                     (f)))))))
