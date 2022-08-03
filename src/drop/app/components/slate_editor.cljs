(ns drop.app.components.slate-editor
  (:require [drop.app.persistent-atom :refer [persistent-atom]]
            [drop.app.components.actionbar :refer [actionbar]]
            [slate.editor-ui-state :as ui-state]
            [slate.core :as sl]
            [slate.default-interceptors :as ints]
            [slate.utils :as utils]
            [reagent.core :as r]
            ["electron" :refer [ipcRenderer]]))

;; TODO: persist dis bih
;; (defonce *open-file (atom nil))
(defonce *open-file (persistent-atom ::open-file nil))

(defn slate-editor [{:keys [file-deserialized ui-state-atom]}]
  [:div
   {:class "react-slate-elem flex-1 overflow-y-scroll"
    :ref (fn [elem]
           (when elem
             (let [on-save-as (fn [serialized]
                                (-> (.invoke ipcRenderer "save-file-as" serialized)
                                    (.then #(reset! *open-file %))
                                    (.catch #())))
                   *ui-state (sl/init! :*atom ui-state-atom
                                       :history file-deserialized
                                       :dom-elem elem
                                       :on-save (fn [serialized]
                                                  (if @*open-file
                                                    (.send ipcRenderer "save-file" @*open-file serialized)
                                                    (on-save-as serialized)))
                                       :on-save-as on-save-as
                                       :on-open (fn [*ui-state]
                                                  (-> (.invoke ipcRenderer "choose-file")
                                                      (.then (fn [[file-path contents]]
                                                               (reset! *open-file file-path)
                                                               (sl/load-file! *ui-state contents)))
                                                      (.catch #(js/console.log %)))))]
               ;; Utility for viewing editor history from console
               (set! js/dumpHistory #(js/console.log (utils/pretty-history-stack (:history @*ui-state)))))))}])

(defn main-editor []
  (let [active-formats (r/atom #{})
        *slate-instance (atom nil)
        current-file @*open-file
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
