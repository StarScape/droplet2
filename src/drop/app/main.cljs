(ns drop.app.main
  (:require [drop.app.events]
            [drop.app.subs]
            [drop.app.effects]
            [drop.app.ipc :as ipc]
            [drop.app.components.core :as components]
            [drop.app.components.slate-editor :as slate-editor]
            [drop.utils :as utils]
            [reagent.dom :as rdom]
            [re-frame.core :as rf]
            [re-frame.db :as db]
            [orchestra-cljs.spec.test :as st]
            ["electron" :refer [ipcRenderer]]))

;; Globally print any errors with stacktrace.
;; CLJS, unfuriatingly, does not print a stacktrace automatically.
;; (set! (.. js/window -onerror)
;;       (fn [msg url line col err]
;;         (println msg)
;;         (println (str "\nat: " line ":" col "\n"))))

(when utils/DEV
  (st/instrument))

(defn mount-main-component []
  (let [elem (js/document.getElementById "reagent-main")]
    (rdom/render [components/app] elem)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn main []
  (rf/dispatch-sync [:boot])
  (ipc/init-handlers!)
  (mount-main-component))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn ^:dev/after-load reload []
  (.send ipcRenderer "-reload-last-file")
  (mount-main-component))
