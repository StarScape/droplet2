(ns drop.app.ipc
  "IPC handlers for communicating with the main Electron process."
  (:require [re-frame.core :as rf :refer [dispatch]]
            [re-frame.db]
            [slate.api :as slate-api]
            [slate.editor-ui-state :as ui-state]
            [slate.serialization :as slate-serialization]
            [slate.filetypes.core :as filetypes]
            [drop.app.file-handling :as file-handling]
            [drop.app.utils :as app-utils]
            [drop.app.demo :as demo]
            [promesa.core :as p]
            ["electron" :refer [ipcRenderer]])
  (:require-macros [promesa.core :as p]))

(defn init-handlers! []
  (.on ipcRenderer "change-full-screen-status"
       (fn [_e, message-contents]
         (dispatch [:set-full-screen message-contents])))

  (.on ipcRenderer "file-menu-item-clicked"
       (fn [_e, item & args]
         (let [{:keys [*slate-instance]} @re-frame.db/app-db]
           (case item
             "new" (file-handling/on-new! *slate-instance)
             "save" (file-handling/on-save! (slate-serialization/serialize @*slate-instance))
             "save-as" (file-handling/on-save-as! (slate-serialization/serialize @*slate-instance))
             "initiate-file-export" (apply file-handling/on-export! @*slate-instance args)))))

  (.on ipcRenderer "selection-menu-item-clicked"
       (fn [_e, item]
         (let [{:keys [*slate-instance]} @re-frame.db/app-db]
           (case item
             "next-clause" (slate-api/next-clause! *slate-instance)
             "prev-clause" (slate-api/prev-clause! *slate-instance)
             "next-sentence" (slate-api/next-sentence! *slate-instance)
             "prev-sentence" (slate-api/prev-sentence! *slate-instance)
             "next-paragraph" (slate-api/next-paragraph! *slate-instance)
             "prev-paragraph" (slate-api/prev-paragraph! *slate-instance)))))

  (.on ipcRenderer "load-file"
       (fn [_e, file-path, file-contents]
         (try
           (dispatch [:open-file file-path file-contents])
           (catch :default e
             (app-utils/show-error-dialog! "Failed to open file" "File failed to open")
             (js/console.log "Error thrown in ipcRenderer open-file:" e)))))

  (.on ipcRenderer "load-blank-document"
       (fn [_e]
         (let [{:keys [*slate-instance]} @re-frame.db/app-db]
           (ui-state/new-document! *slate-instance)
           (dispatch [:set-open-file-path nil]))))

  (.on ipcRenderer "import-file"
       (fn [_e, file-type, file-contents]
         (try
           (let [{:keys [*slate-instance]} @re-frame.db/app-db]
             (file-handling/open-doc! *slate-instance (filetypes/import file-type file-contents)))
           (catch :default e
             (app-utils/show-error-dialog! "Import Failure" "Failed to import file.")
             (js/console.log "Error thrown in ipcRenderer import-file:" e)))))

  (.on ipcRenderer "start-screen-recording"
       (fn [_e, source-id]
         (demo/record-main-demo! source-id)))

  (.send ipcRenderer "renderer-ipc-handlers-initialized"))
