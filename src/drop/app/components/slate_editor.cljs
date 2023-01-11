(ns drop.app.components.slate-editor
  (:require [clojure.pprint :as pprint]
            [drop.app.persistent-atom :refer [persistent-atom clear-persistent-atom!]]
            [drop.app.components.actionbar :refer [actionbar]]
            [drop.app.components.find-and-replace-popup :refer [find-and-replace-popup]]
            [drop.app.utils :as app-utils]
            [drop.utils :as utils]
            [slate.default-interceptors :as ints]
            [slate.editor-ui-state :as ui-state]
            [slate.filetypes.core :as filetypes]
            [slate.model.doc :as doc]
            [slate.model.history :as history]
            [slate.utils :as slate-utils]
            [reagent.core :as r]
            ["electron" :refer [ipcRenderer]]
            ["path" :as path]))

(defn- current-doc [ui-state]
  (some-> ui-state :history (history/current-state) :doc))

(declare *slate-instance)

(def *full-screen? (r/atom false))
(defonce *open-file (doto (persistent-atom ::open-file
                                           {:path nil, :last-saved-doc nil}
                                           :readers ui-state/slate-types-readers)
                      (add-watch :change-title (fn [_ _ _ new-open-file]
                                                 (app-utils/set-title! new-open-file (current-doc @*slate-instance))))))

(comment
  (clear-persistent-atom! ::open-file)
  )

;; For now, there is a single global slate instance.
;; This will change at some point when tabs are implemented.
(def *slate-instance (doto (r/atom nil)
                       (add-watch :change-title (fn [_ _ _ new-ui-state]
                                                  (app-utils/set-title! @*open-file (current-doc new-ui-state))))))
(set! js/window.globalSlateInstance *slate-instance) ; for debugging use

(defn on-new!
  "Resets the open-file information, after the editor has already been set to a new, empty document.
   This **does not** reset the editor surface in any way. For that, see `slate.editor-ui-state/new-document!`"
  [new-ui-state]
  (reset! *open-file {:path nil
                      :last-saved-doc (current-doc new-ui-state)}))

(defn open-doc!
  [*ui-state doc]
  (ui-state/load-document! *ui-state doc)
  (swap! *open-file assoc :path nil))

(defn open-file!
  [*ui-state file-path contents]
  (ui-state/load-file! *ui-state contents)
  (reset! *open-file {:path file-path
                      :last-saved-doc (current-doc @*ui-state)}))

(defn on-open! [*ui-state]
  (-> (.invoke ipcRenderer "choose-file")
      (.then (fn [[file-path contents]]
               (open-file! *ui-state file-path contents)))
      (.catch #(js/console.log %))))

(defn on-save-as!
  [serialized-history]
  (-> (.invoke ipcRenderer "save-file-as" serialized-history)
      (.then #(reset! *open-file {:path %, :last-saved-doc (current-doc @*slate-instance)}))
      (.catch #())))

(defn on-save! [serialized-history]
  (let [{open-file-path :path} @*open-file]
    (if open-file-path
      (do
        (.send ipcRenderer "save-file" open-file-path serialized-history)
        (swap! *open-file assoc :last-saved-doc (current-doc @*slate-instance)))
      (on-save-as! serialized-history))))

(defn on-export! [*ui-state export-type]
  (let [{:keys [history]} @*ui-state
        doc (:doc (history/current-state history))
        exported (filetypes/export export-type doc)
        suggested-file-name (.basename path (:path @*open-file) ".drop")]
    (.send ipcRenderer "save-exported-file-as" exported export-type suggested-file-name)))

(defn slate-editor [{:keys [file-contents ui-state-atom on-focus-find]}]
  [:div
   {:class "react-slate-elem flex-1 overflow-y-auto"
    :tabIndex "-1"
    :ref (fn [elem]
           (when elem
             (let [*ui-state (ui-state/init! :*atom ui-state-atom
                                             :save-file-contents file-contents
                                             :dom-elem elem
                                             :on-new on-new!
                                             :on-open on-open!
                                             :on-save on-save!
                                             :on-save-as on-save-as!
                                             :on-focus-find on-focus-find)]
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
                                                 js/console.log))))))}])

(defn main-editor []
  (let [*active-formats (r/atom #{})
        *word-count (r/atom 0)
        *find-and-replace-ref (r/atom nil)
        focus-find-popup! (fn []
                            (when-let [elem @*find-and-replace-ref]
                              (.focus elem)))
        current-file (:path @*open-file)
        save-file-contents (when current-file
                        (let [[error?, file-text] (.sendSync ipcRenderer "read-file" current-file)]
                          (if error?
                            (do
                              (reset! *open-file {:path nil, :last-saved-doc (doc/document)})
                              ;; Slate instance will default to an empty document
                              ;; when receiving nil, so propagating nil  works fine.
                              nil)
                            file-text)))]
    (add-watch *slate-instance :watcher (fn [_key _atom _old-state new-ui-state]
                                          (reset! *active-formats (ui-state/active-formats new-ui-state))
                                          (reset! *word-count (:word-count new-ui-state))))
    (fn []
      [:<>
       [:div {:class "h-screen flex flex-row justify-center"
              :on-focus #(when-let [instance @*slate-instance]
                           (ui-state/focus! instance))}
        [slate-editor {:file-contents save-file-contents
                       :ui-state-atom *slate-instance
                       :on-focus-find focus-find-popup!}]]
       (let [find-and-replace (-> @*slate-instance :find-and-replace)]
         [find-and-replace-popup {:activated? (:active? find-and-replace)
                                  :ignore-case-toggled? (not (:ignore-case? find-and-replace))
                                  :current-occurrence (:current-occurrence-idx find-and-replace)
                                  :total-occurrences (count (:occurrences find-and-replace))
                                  :find-text (:find-text find-and-replace)
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
                                  :search-input-ref (fn [elem] (reset! *find-and-replace-ref elem))}])
       [actionbar {:active-formats @*active-formats
                   :word-count @*word-count
                   :*full-screen? *full-screen?
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
  (app-utils/set-title! @*open-file (current-doc @*slate-instance))

  (.on ipcRenderer "change-full-screen-status"
       (fn [_e, message-contents]
         (reset! *full-screen? message-contents)))

  (.on ipcRenderer "menubar-item-clicked"
       (fn [_e, item & args]
         (case item
           "new" (do
                   (ui-state/new-document! *slate-instance)
                   (on-new! @*slate-instance))
           "save" (on-save! (ui-state/serialize @*slate-instance))
           "save-as" (on-save-as! (ui-state/serialize @*slate-instance))
           "open" (on-open! *slate-instance)
           "initiate-file-export" (apply on-export! *slate-instance args))))

  (.on ipcRenderer "open-file"
       (fn [_e, file-path, file-contents]
         ;;  (js/console.log "ipRenderer open-file!")
         ;;  (js/console.log file-path file-contents)
         (try
           (open-file! *slate-instance file-path file-contents)
           (catch :default e
             (app-utils/show-error-dialog! "Failed to open file" "File failed to open")
             (js/console.log "Error thrown in ipcRenderer open-file:" e)))))

  (.on ipcRenderer "import-file"
       (fn [_e, file-type, file-contents]
         (try
           (open-doc! *slate-instance (filetypes/import file-type file-contents))
           (catch :default e
             (app-utils/show-error-dialog! "Import Failure" "Failed to import file.")
             (js/console.log "Error thrown in ipcRenderer import-file:" e))))))
