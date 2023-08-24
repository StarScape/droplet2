(ns drop.electron.core
  (:require ["electron" :refer [app BrowserWindow Menu crashReporter ipcMain dialog]]
            ["electron-is-dev" :as is-dev?]
            ["electron-window-state" :as window-state-keeper]
            ["fs" :as fs]
            ["node:fs/promises" :refer [readFile writeFile]]
            ["path" :as path]
            [cljs.core.async :as async :refer [chan <! >!] :refer-macros [go go-loop]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [drop.electron.utils :refer [on-ipc on-ipc-once handle-ipc read-persisted! write-persisted! log]]))

(log "Evaluating main electron file...")

(goog-define DEV true)
(def is-dev? DEV)

(declare init-window!)

(def main-window-info-default {:window nil
                               :renderer-ipc-handlers-initialized? false
                               :file-opened-from-os? false})

(def *main-window-info (atom main-window-info-default))

(def file-formats-info {"rtf" {:file-type-name "RTF"
                               :file-extension ".rtf"}
                        "html" {:file-type-name "HTML"
                                :file-extension ".html"}})

(defn launch-import-dialog!
  "Launches an file dialog to import the specified format type.
   Ex format-name and file-extension: 'HTML' and 'html'. Note the lack of leading '.' for the file-extension."
  [file-type]
  (go
    (let [{:keys [file-type-name file-extension]} (get file-formats-info file-type)
          main-window (:window @*main-window-info)
          result (<p! (.showOpenDialog dialog main-window
                                       (clj->js {:title (str "Open ." file-extension " file")
                                                 :filters [{:name file-type-name
                                                            :extensions [file-extension]}]
                                                 :properties ["openFile"]})))]
      (when-not ^js (.-canceled result)
        (let [file-path (nth ^js (.-filePaths result) 0)
              file-contents (<p! (readFile file-path "utf8"))]
          (.. main-window -webContents (send "import-file" file-type file-contents)))))))

(defn launch-export-dialog! [data file-type suggested-file-name]
  (go
    (try
      (let [{:keys [file-type-name file-extension]} (get file-formats-info file-type)
            result (<p! (.showSaveDialog dialog @*main-window-info
                                         (clj->js {:title "Export As..."
                                                   :defaultPath (str suggested-file-name file-extension)
                                                   :filters [{:name file-type-name
                                                              :extensions [file-extension]}]})))]
        (when-not ^js (.-canceled result)
          (<p! (writeFile (.-filePath result) data "utf8"))))
      (catch js/Error e
        (js/console.log e)))))

(defn show-new-file-confirmation-dialog!
  "Synchronously show new file confirmation dialog. Return true if user presses confirm."
  []
  (let [result (.showMessageBoxSync dialog (clj->js {:message "Create new file? The current file will be closed."
                                                     :buttons ["Confirm", "Cancel"]}))]
    (= 0 result)))

(defn wait-for-renderer-ipc-handlers!
  "Returns a Promise that will not resolve until the renderer process's IPC handlers
   have been initialized. May resolve immediately."
  []
  (js/Promise.
   (fn [resolve-promise _reject-promise]
     (log (:renderer-ipc-handlers-initialized? @*main-window-info))
     (if (:renderer-ipc-handlers-initialized? @*main-window-info)
       (resolve-promise)
       (.once ipcMain "renderer-ipc-handlers-initialized"
              (fn []
                (swap! *main-window-info assoc :renderer-ipc-handlers-initialized? true)
                (resolve-promise)))))))

(defn save-drop-file!
  [file-path file-contents]
  (fs/writeFile file-path file-contents
                (fn [err]
                  (if err
                    (js/console.error err)
                    (write-persisted! "current-file" {:path file-path})))))

(defn open-file-in-slate!
  "Reads the data from the file and sends it to the render process to be loaded into the Slate editor.
   Saves the file path as the last file opened."
  [path]
  (log "Called open-file-in-slate!")
  (go
    ;; Initialize new-window if not already done
    (<p! (init-window!))
    (log "After init-window!")
    (<p! (wait-for-renderer-ipc-handlers!))
    (log "After wait-for-renderer-ipc-handlers!")
    (fs/readFile path "utf8" (fn [err, contents]
                               (when-not err
                                 (log (str "Read file " path " from disk, sending to renderer process\n"))
                                 ;; TODO: ensure that this is only done after channel is set up
                                 (.. (:window @*main-window-info) -webContents (send "load-file" path contents))
                                 (write-persisted! "current-file" {:path path}))))))

(defn choose-file-from-fs! []
  (-> (.showOpenDialog dialog (:window @*main-window-info)
                       (clj->js {:title "Open .drop"
                                 :filters [{:name "Droplet File"
                                            :extensions [".drop"]}]
                                 :properties ["openFile"]}))
      (.then (fn [result]
               (when-not ^js (.-canceled result)
                 (open-file-in-slate! (nth ^js (.-filePaths result) 0)))))
      (.catch #(js/console.log %))))

(defn reg-ipc-handlers! []
  (on-ipc "show-error-dialog"
          (fn [_ title dialog-text]
            (.showErrorBox dialog title dialog-text)))

  (on-ipc "save-file"
    (fn [_ file-path file-contents]
      (save-drop-file! file-path file-contents)))

  (handle-ipc "save-file-as"
    (fn [_ file-contents]
      (js/Promise.
       (fn [resolve-promise, reject-promise]
         (-> (.showSaveDialog dialog (:window @*main-window-info)
                              (clj->js {:title "Save As..."
                                        :filters [{:name "Droplet File"
                                                   :extensions [".drop"]}]}))
             (.then (fn [result]
                      (let [canceled? ^js (.-canceled result)
                            file-path ^js (.-filePath result)]
                        (if canceled?
                          (reject-promise "canceled")
                          (do
                            (save-drop-file! file-path file-contents)
                            (resolve-promise file-path))))))
             (.catch #(js/console.log %)))))))

  (on-ipc "save-exported-file-as"
          (fn [_ exported-file exported-file-type suggested-file-name]
            (launch-export-dialog! exported-file exported-file-type suggested-file-name)))

  (on-ipc "choose-file"
    (fn []
      (choose-file-from-fs!)))

  (on-ipc "new-file-confirmation-dialog"
    (fn [e]
      ;; Generic method for showing confirmation dialogs
      (set! (.-returnValue e) (show-new-file-confirmation-dialog!)))))

(defn init-app-menu [window]
  (let [template (clj->js [{:label (.-name app)
                            :submenu [{:role "about"}
                                      {:type "separator"}
                                      {:role "services"}
                                      {:type "separator"}
                                      {:role "hide"}
                                      {:role "hideOthers"}
                                      {:role "unhide"}
                                      {:type "separator"}
                                      {:role "quit"}]}
                           {:label "File",
                            :submenu [{:label "New..."
                                       :accelerator "CmdOrCtrl+N"
                                       :click #(.. window -webContents (send "file-menu-item-clicked" "new"))}
                                      {:label "Open..."
                                       :accelerator "CmdOrCtrl+O"
                                       :click #(choose-file-from-fs!)}
                                      {:label "Save"
                                       :accelerator "CmdOrCtrl+S"
                                       :click #(.. window -webContents (send "file-menu-item-clicked" "save"))}
                                      {:label "Save As..."
                                       :accelerator "CmdOrCtrl+Shift+S"
                                       :click #(.. window -webContents (send "file-menu-item-clicked" "save-as"))}
                                      {:label "Import..."
                                       :submenu [{:label "HTML"
                                                  :click #(launch-import-dialog! "html")}
                                                 {:label "RTF"
                                                  :click #(launch-import-dialog! "rtf")}]}
                                      {:label "Export As..."
                                       :submenu [{:label "HTML"
                                                  :click #(.. window -webContents (send "file-menu-item-clicked" "initiate-file-export" "html"))}
                                                 {:label "RTF"
                                                  :click #(.. window -webContents (send "file-menu-item-clicked" "initiate-file-export" "rtf"))}]}]}
                           {:role "editMenu"}
                           {:label "View",
                            :submenu [{:role "togglefullscreen"}]}
                           {:label "Selection",
                            :submenu [{:label "Next Clause"
                                       :accelerator "CmdOrCtrl+."
                                       :click #(.. window -webContents (send "selection-menu-item-clicked" "next-clause"))}
                                      {:label "Prev Clause"
                                       :accelerator "CmdOrCtrl+,"
                                       :click #(.. window -webContents (send "selection-menu-item-clicked" "prev-clause"))}
                                      {:label "Next Sentence"
                                       :accelerator "CmdOrCtrl+]"
                                       :click #(.. window -webContents (send "selection-menu-item-clicked" "next-sentence"))}
                                      {:label "Prev Sentence"
                                       :accelerator "CmdOrCtrl+["
                                       :click #(.. window -webContents (send "selection-menu-item-clicked" "prev-sentence"))}
                                      {:label "Next Paragraph"
                                       :accelerator "CmdOrCtrl+0"
                                       :click #(.. window -webContents (send "selection-menu-item-clicked" "next-paragraph"))}
                                      {:label "Prev Paragraph"
                                       :accelerator "CmdOrCtrl+9"
                                       :click #(.. window -webContents (send "selection-menu-item-clicked" "prev-paragraph"))}]}
                           {:role "windowMenu"}
                           {:label "Help"
                            :role "help"
                            :submenu [{:label "View Droplet Version",
                                       :click #(.showMessageBox dialog #js {:message (str "Droplet Version: " (.getVersion app))})}]}])]
    (when is-dev?
      (.push template (clj->js {:label "Dev"
                                :submenu [{:role "reload"}
                                          {:role "forcereload"}
                                          {:role "toggledevtools"}]})))
    (.setApplicationMenu Menu (.buildFromTemplate Menu template))))

(defn init-window!
  "Initializes the main Droplet window. Returns a Promise that resolves whenever the
   window has been initialized."
  []
  (js/Promise.
   (fn [resolve-promise _reject-promise]
     (if (:window @*main-window-info)
       (resolve-promise) ;; window already exists, resolve immediately
       (do
         (log "Initializing Electron browser window")
         (let [window-state (window-state-keeper #js {:defaultWidth 1200
                                                      :defaultHeight 900})
               window (BrowserWindow.
                       #js {:x (.-x window-state)
                            :y (.-y window-state)
                            :width (.-width window-state)
                            :height (.-height window-state)
                            :minWidth 500
                            :minHeight 500
                            :webPreferences #js {:nodeIntegration true
                                                 :contextIsolation false
                                                 ;; Enabled to get support for :has() selector in CSS.
                                                 ;; Can be disabled whenever support for that is mainlined in Chrome
                                                 :experimentalFeatures true
                                                 #_#_:preload (path/join js/__dirname "preload.js")}})
               source-path (if is-dev?
                             "http://localhost:8080"
                             (str "file://" js/__dirname "/../index.html"))]
           (when is-dev?
             (.. window -webContents (openDevTools #js {:activate false})))
           (swap! *main-window-info merge {:window window
                                           :renderer-ipc-handlers-initialized? false})
           (.manage window-state window)
           (.loadURL ^js/electron.BrowserWindow window source-path)

           (.on ^js/electron.BrowserWindow window "closed"
                #(reset! *main-window-info nil))
           (.on ^js/electron.BrowserWindow window "enter-full-screen"
                #(.. window -webContents (send "change-full-screen-status", true)))
           (.on ^js/electron.BrowserWindow window "leave-full-screen"
                #(.. window -webContents (send "change-full-screen-status", false)))

           (init-app-menu window)

           (read-persisted! "current-file"
                            {:path nil}
                            (fn [{:keys [path]}]
                              ;; On macOS, if an "open-file" event has occurred, that means a .drop file
                              ;; has been opened from Finder, by dropping into onto Droplet on the Dock,
                              ;; or with the `open -a ...` command. MacOS implements it this way rather than
                              ;; using ARGV because the application may stick around even if the window closes
                              ;; in, in contrast to Windows where the window and application instance are
                              ;; synonymous.
                              ;;
                              ;; Anyway, if the "open-file" event already happened for this window, then the
                              ;; user is opening a new file, and there is no need to reopen the last file modified.
                              (when (and path (not (:file-opened-from-os? @*main-window-info)))
                                (log (str "Opening last file modified from current-file.edn: " path))
                                (open-file-in-slate! path))))

           (log "Electron browser window initialized")
           (resolve-promise)))))))

(defn main []
  ; CrashReporter can just be omitted
  (.start crashReporter
          #js {:companyName "Droplet"
               :productName "Droplet"
               :submitURL "https://example.com/submit-url"
               :autoSubmit false})

  (.on app "open-file"
       (fn [_event path]
         (log "\"open-file\" event triggered from OS")
         (open-file-in-slate! path)
         (swap! *main-window-info assoc :file-opened-from-os? true)))

  (.on app "window-all-closed" #(if (= js/process.platform "darwin")
                                  (reset! *main-window-info main-window-info-default)
                                  (.quit app)))
  (.on app "ready" init-window!)

  (.on app "activate" (fn []
                        (when (zero? (.. BrowserWindow (getAllWindows) -length))
                          (init-window!))))

  (reg-ipc-handlers!))
