(ns slate.moby-dick-browser-test
  (:require [cljs.test :include-macros true :refer [deftest is]]))

(defn nop [& _arg] nil)

(defn setup-slate-instance
  [*ui-state dom-elem]
  (ui-state/init! :*atom *ui-state
                  :font-family "Noto Serif"
                  :dom-elem dom-elem
                  :on-new nop
                  :on-open nop
                  :on-save nop
                  :on-save-as nop
                  :on-load-file-error nop
                  :on-focus-find nop
                  :on-doc-changed nop
                  :on-selection-changed nop
                  :should-lose-focus? (constantly false)
                  :on-ready nop))
