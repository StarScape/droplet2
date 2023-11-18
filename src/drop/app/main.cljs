(ns drop.app.main
  #_(:require [drop.app.events] ;; These two are only required to make the compiler
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
            ["electron" :refer [ipcRenderer #_desktopCapturer]]))

#_#_(when utils/DEV
  (st/instrument))

(defn mount-main-component []
  (let [elem (js/document.getElementById "reagent-main")]
    (rdom/render [components/app] elem)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn main []
  #_#_#_(rf/dispatch-sync [:boot])
  (ipc/init-handlers!)
  (mount-main-component))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
#_(defn ^:dev/after-load reload []
  (.send ipcRenderer "-reload-last-file")
  (mount-main-component))
