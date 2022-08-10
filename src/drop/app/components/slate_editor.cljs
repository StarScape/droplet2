(ns drop.app.components.slate-editor
  (:require [drop.app.persistent-atom :refer [persistent-atom]]
            [drop.app.components.actionbar :refer [actionbar]]
            [drop.app.utils :as app-utils]
            [drop.utils :as utils]
            [slate.editor-ui-state :as ui-state]
            [slate.core :as sl]
            [slate.default-interceptors :as ints]
            [slate.utils :as slate-utils]
            [reagent.core :as r]
            ["electron" :refer [ipcRenderer]]))

;; For now, there is a single global slate instance.
;; This will change at some point when tabs are implemented.
(def *slate-instance (atom nil))
(def *full-screen? (r/atom false))

(defonce *open-file (doto (persistent-atom ::open-file {:path nil
                                                        :saved? false})
                      (add-watch :change-title (fn [_ _ _ new-val]
                                                 (app-utils/set-title! new-val)))))

(defn on-new! []
  (reset! *open-file {:path nil
                      :saved? false}))

(defn on-open! [*ui-state]
  (-> (.invoke ipcRenderer "choose-file")
      (.then (fn [[file-path contents]]
               (reset! *open-file {:path file-path
                                   :saved? true})
               (sl/load-file! *ui-state contents)))
      (.catch #(js/console.log %))))

(defn on-change! []
  (swap! *open-file assoc :saved? false))

(defn on-save-as!
  [serialized]
  (-> (.invoke ipcRenderer "save-file-as" serialized)
      (.then #(reset! *open-file {:path %, :saved? true}))
      (.catch #())))

(defn on-save! [serialized]
  (let [{open-file-path :path} @*open-file]
    (if open-file-path
      (do
        (.send ipcRenderer "save-file" open-file-path serialized)
        (swap! *open-file assoc :saved? true))
      (on-save-as! serialized))))

(defn slate-editor [{:keys [file-deserialized ui-state-atom]}]
  [:div
   {:class "react-slate-elem flex-1 overflow-y-auto"
    :ref (fn [elem]
           (when elem
             (let [*ui-state (sl/init! :*atom ui-state-atom
                                       :history file-deserialized
                                       :dom-elem elem
                                       :on-new on-new!
                                       :on-open on-open!
                                       :on-change on-change!
                                       :on-save on-save!
                                       :on-save-as on-save-as!)]
               ;; Utility for viewing editor history from console
               (when utils/DEV
                 (set! js/dumpHistory #(js/console.log (slate-utils/pretty-history-stack (:history @*ui-state))))))))}])

(defn main-editor []
  (let [active-formats (r/atom #{})
        current-file (:path @*open-file)
        deserialized-file-contents (if current-file
                                     (let [file-contents (.sendSync ipcRenderer "read-file" current-file)]
                                       (ui-state/deserialize file-contents))
                                     nil)]
    (add-watch *slate-instance :watcher (fn [_key _atom _old-state _new-state]
                                          (reset! active-formats (ui-state/active-formats @*slate-instance))))
    (fn []
      [:<>
       [:div {:class "h-screen flex flex-row justify-center"}
        [slate-editor {:file-deserialized deserialized-file-contents
                       :ui-state-atom *slate-instance}]]
       [actionbar {:class "fixed bottom-0 w-screen bg-white px-1 py-1 flex border-t border-gray-200"
                   :active-formats @active-formats
                   :on-format-toggle #(let [interceptor (case %
                                                          :italic ints/italic
                                                          :strikethrough ints/strikethrough
                                                          :bold ints/bold
                                                          :h1 ints/h1
                                                          :h2 ints/h2
                                                          :ol ints/olist
                                                          :ul ints/ulist)]
                                        (ui-state/fire-interceptor! *slate-instance interceptor (js/Event. "keydown")))}]])))

(defn on-startup
  "There is some global state and handlers in this namespace that need to be configured on startup."
  []
  (app-utils/set-title! @*open-file)

  (.on ipcRenderer "change-full-screen-status"
       (fn [_e, message-contents]
         (reset! *full-screen? message-contents)
         #_(js/console.log (str "Received, now " @*full-screen?))))

  (.on ipcRenderer "menubar-item-clicked"
       (fn [_e, message-contents]
         (case message-contents
           "new" (on-new!)
           "save" (on-save! (ui-state/serialize @*slate-instance))
           "save-as" (on-save-as! (ui-state/serialize @*slate-instance))
           "open" (on-open! *slate-instance)))))
