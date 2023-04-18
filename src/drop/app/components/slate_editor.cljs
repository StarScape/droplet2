(ns drop.app.components.slate-editor
  (:require [clojure.pprint :as pprint]
            [drop.app.components.actionbar :refer [actionbar]]
            [drop.app.components.find-and-replace-popup :refer [find-and-replace-popup]]
            [drop.app.file-handling :as file-handling]
            [drop.utils :as utils]
            [slate.default-interceptors :as ints]
            [slate.editor-ui-state :as ui-state]
            [slate.filetypes.core :as filetypes]
            [slate.model.history :as history]
            [slate.utils :as slate-utils]
            [reagent.core :as r]
            [re-frame.core :as rf :refer [dispatch subscribe]]
            [re-frame.db]
            ["electron" :refer [ipcRenderer]]))

(defn find-and-replace
  [*slate-instance *find-and-replace-ref focus-find-popup!]
  (let [f+r-state (-> @*slate-instance :find-and-replace)]
    [find-and-replace-popup {:activated? (:active? f+r-state)
                             :ignore-case-toggled? (not (:ignore-case? f+r-state))
                             :current-occurrence (:current-occurrence-idx f+r-state)
                             :total-occurrences (count (:occurrences f+r-state))
                             :find-text (:find-text f+r-state)
                             :on-find-text-changed #(ui-state/set-find-text! *slate-instance %)
                             :on-find #(ui-state/find! *slate-instance)
                             :on-replace #(ui-state/replace-current! *slate-instance %)
                             :on-replace-all #(ui-state/replace-all! *slate-instance %)
                             :on-click-exit #(ui-state/cancel-find! *slate-instance)
                             :on-click-next #(ui-state/next-occurrence! *slate-instance)
                             :on-click-prev #(ui-state/prev-occurrence! *slate-instance)
                             :on-toggle-ignore-case #(ui-state/toggle-ignore-case! *slate-instance)
                             :on-key-down (fn [e]
                                            (let [ui-state @*slate-instance
                                                  matching-interceptor (ui-state/find-interceptor ui-state e)]
                                              (when (or (= ui-state/undo! matching-interceptor)
                                                        (= ui-state/redo! matching-interceptor))
                                                (ui-state/fire-interceptor! *slate-instance matching-interceptor nil)
                                                (focus-find-popup!))))
                             :search-input-ref (fn [elem] (reset! *find-and-replace-ref elem))}]))

(defn slate-editor [{:keys [file-contents *ui-state on-focus-find on-doc-changed on-selection-changed]}]
  (let [*editor-elem-ref (atom nil)]
   (r/create-class
    {:display-name "slate-editor"

     :component-did-mount
     (fn [_this]
       ;; Call init only once, when component initialized
       (ui-state/init! :*atom *ui-state
                       :save-file-contents file-contents
                       :dom-elem @*editor-elem-ref
                       :on-new file-handling/on-new!
                       :on-open file-handling/on-open!
                       :on-save file-handling/on-save!
                       :on-save-as file-handling/on-save-as!
                       :on-focus-find on-focus-find
                       :on-doc-changed on-doc-changed
                       :on-selection-changed on-selection-changed
                       :on-ready (fn []
                                   (.send ipcRenderer "slate-ready")))

       ;; Utility for viewing editor history from console
       (when utils/DEV
         (set! js/dumpHistory (fn
                                ([n]
                                 (slate-utils/pretty-history-stack (:history @*ui-state) n))
                                ([]
                                 (slate-utils/pretty-history-stack (:history @*ui-state) 10))))
         (set! js/printCurrentState #(-> (:history @*ui-state)
                                         history/current-state
                                         pprint/pprint
                                         js/console.log))))

     :reagent-render
     (fn []
       [:div
        {:class "react-slate-elem flex-1 overflow-y-auto"
         :tabIndex "-1"
         :ref (fn [elem]
                (when elem
                  (reset! *editor-elem-ref elem)))}])})))

(defn main-editor []
  (let [*active-formats (r/atom #{})
        *word-count (r/atom 0)
        *slate-instance @(subscribe [:slate-instance])
        *find-and-replace-ref (r/atom nil)
        focus-find-ref! (fn []
                          (when-let [elem @*find-and-replace-ref]
                            (.focus elem)))]
    (fn []
      [:<>
       ;; Find and replace outside of div with editor and actionbar bc it needs separate focus
       [find-and-replace *find-and-replace-ref focus-find-ref!]

       ;; Editor and actionbar
       [:div {:class "h-screen flex flex-row justify-center"
              :on-focus #(when-let [instance @*slate-instance]
                           (ui-state/focus! instance))}
        [slate-editor {:*ui-state *slate-instance
                       :on-focus-find focus-find-ref!
                       :on-doc-changed (fn []
                                         (reset! *active-formats (ui-state/active-formats @*slate-instance))
                                         (dispatch [:doc-changed]))
                       :on-selection-changed #(reset! *word-count (:word-count @*slate-instance))}]
        [actionbar {:active-formats @*active-formats
                    :word-count @*word-count
                    :*full-screen? @(subscribe [:fullscreen?])
                    :on-format-toggle #(let [interceptor (case %
                                                           :italic ints/italic
                                                           :strikethrough ints/strikethrough
                                                           :bold ints/bold
                                                           :h1 ints/h1
                                                           :h2 ints/h2
                                                           :ol ints/olist
                                                           :ul ints/ulist)]
                                         (ui-state/fire-interceptor! *slate-instance interceptor (js/Event. "keydown")))}]]])))
