(ns drop.app.components.slate-editor
  (:require [drop.app.persistent-atom :refer [persistent-atom]]
            [drop.app.components.actionbar :refer [actionbar]]
            [drop.utils :as utils]
            [slate.editor-ui-state :as ui-state]
            [slate.core :as sl]
            [slate.default-interceptors :as ints]
            [slate.utils :as slate-utils]
            [reagent.core :as r]
            ["electron" :refer [ipcRenderer]]
            ["path" :as path]))

(def *full-screen? (atom false))

(.on ipcRenderer "change-full-screen-status"
     (fn [event, message-contents]
       (reset! *full-screen? message-contents)
       #_(js/console.log (str "Received, now " @*full-screen?))))

(defn set-title!
  [{:keys [path saved?] :as ar}]
  (let [file-name (when path (path/basename path))
        title (or file-name "Droplet")
        title (if (and path (not saved?))
                (str title "*")
                title)]
    (set! js/document.title title)))

;; TODO: persist dis bih
;; (defonce *open-file (atom nil))
(defonce *open-file (doto (persistent-atom ::open-file {:path nil
                                                        :saved? false})
                      (add-watch :change-title (fn [_ _ _ new-val]
                                                 (set-title! new-val)))))
(set-title! @*open-file)

(defn slate-editor [{:keys [file-deserialized ui-state-atom]}]
  [:div
   {:class "react-slate-elem flex-1 overflow-y-auto"
    :ref (fn [elem]
           (when elem
             (let [on-save-as (fn [serialized]
                                (-> (.invoke ipcRenderer "save-file-as" serialized)
                                    (.then #(reset! *open-file {:path %, :saved? true}))
                                    (.catch #())))
                   *ui-state (sl/init! :*atom ui-state-atom
                                       :history file-deserialized
                                       :dom-elem elem
                                       :on-new (fn []
                                                 (reset! *open-file {:path nil
                                                                     :saved? false}))
                                       :on-open (fn [*ui-state]
                                                  (-> (.invoke ipcRenderer "choose-file")
                                                      (.then (fn [[file-path contents]]
                                                               (reset! *open-file {:path file-path
                                                                                   :saved? true})
                                                               (sl/load-file! *ui-state contents)))
                                                      (.catch #(js/console.log %))))
                                       :on-change (fn []
                                                    (swap! *open-file assoc :saved? false))
                                       :on-save (fn [serialized]
                                                  (let [{open-file-path :path} @*open-file]
                                                    (if open-file-path
                                                      (do
                                                        (.send ipcRenderer "save-file" open-file-path serialized)
                                                        (swap! *open-file assoc :saved? true))
                                                      (on-save-as serialized))))
                                       :on-save-as on-save-as)]
               ;; Utility for viewing editor history from console
               (when utils/DEV
                 (set! js/dumpHistory #(js/console.log (slate-utils/pretty-history-stack (:history @*ui-state))))))))}])

(defn main-editor []
  (let [active-formats (r/atom #{})
        *slate-instance (atom nil)
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
