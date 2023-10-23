(ns drop.electron.core
  (:require ["electron" :refer [app BrowserWindow Menu crashReporter ipcMain dialog desktopCapturer]]
            ["electron-is-dev" :as is-dev?]
            ["electron-window-state" :as window-state-keeper]
            ["fs" :as fs]
            ["node:fs/promises" :refer [readFile writeFile]]
            ["path" :as path]
            [cljs.core.async :as async :refer [chan <! >!] :refer-macros [go go-loop]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [promesa.core :as p]
            [drop.electron.utils :refer [on-ipc on-ipc-once handle-ipc log]]
            [drop.electron.savefiles :as savefiles]
            [drop.electron.screen-recording :as screen-recording])
  (:require-macros [promesa.core]))

(log "Evaluating main electron file...")

(goog-define DEV true)
(def is-dev? DEV)
(declare init-window!)
(declare init-app-menu!)

(def main-window-info-default {:window nil
                               :promise nil
                               :renderer-ipc-handlers-initialized? false
                               :file-opened-from-os? false})

(def *main-window-info (atom main-window-info-default))

(def file-formats-info {"rtf" {:file-type-name "RTF"
                               :file-extension ".rtf"}
                        "html" {:file-type-name "HTML"
                                :file-extension ".html"}})

(savefiles/declare! :name :current-file
                    :default {:path nil}
                    :spec any?)

(savefiles/declare! :name :recently-opened
                    :default []
                    :spec any?)

(savefiles/declare! :name :theme
                    :default :light
                    :spec any?)

(defn update-recently-opened
  [recently-opened file-path now-ms]
  (as-> recently-opened $
    (filter #(not= (:file-path %) file-path) $)
    (conj $ {:file-path file-path, :time-opened now-ms})
    (sort-by :time-opened $)
    (if (<= (count $) 5)
      $
      (drop 1 $))
    (reverse $)))

(defn update-recently-opened!
  [opened-file-path]
  (p/let [recently-opened (savefiles/read! :recently-opened)
          new-recently-opened (update-recently-opened recently-opened opened-file-path (js/Date.now))]
    (savefiles/write! :recently-opened new-recently-opened)
    (init-app-menu! (:window @*main-window-info))))

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
     (if (:renderer-ipc-handlers-initialized? @*main-window-info)
       (do
         (log "IPC handlers already initialized, resolving (wait-for-renderer-ipc-handlers!) promise immediately.")
         (resolve-promise))
       (do
         (log "IPC handlers not yet initialized, waiting for signal...")
         (.once ipcMain "renderer-ipc-handlers-initialized"
                (fn []
                  (log "Resolving hanging (wait-for-renderer-ipc-handlers!) promise.")
                  (resolve-promise))))))))

(defn save-drop-file!
  [file-path file-contents]
  (fs/writeFile file-path file-contents
                (fn [err]
                  (if err
                    (js/console.error err)
                    (savefiles/write! :current-file {:path file-path})))))

(defn open-file-in-slate!
  "Reads the data from the file and sends it to the render process to be loaded into the Slate editor.
   Saves the file path as the last file opened."
  [path]
  (log "Called open-file-in-slate! for path '" path "'")
  (p/do
    ;; Initialize new window if not already done (on macOS,
    ;; the app can be open without the window being open)
    (init-window!)
    (wait-for-renderer-ipc-handlers!)
    (p/let [contents (-> (readFile path "utf8")
                         (p/catch #(log "Error reading file " path ": \n" %)))]
      (log (str "Read file " path " from disk, sending to renderer process\n"))
      (.. (:window @*main-window-info) -webContents (send "load-file" path contents))
      (savefiles/write! :current-file {:path path})
      (update-recently-opened! path))))

(defn open-last-file!
  "Opens the last opened file by Droplet."
  []
  (-> (savefiles/read! :current-file)
      (p/then (fn [{:keys [path]}]
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
                  (log "Opening last file modified from current-file.edn: " path)
                  (open-file-in-slate! path))))))

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
  (log "Registering main process IPC handlers")

  (on-ipc "renderer-ipc-handlers-initialized"
    (fn []
      (log "Renderer IPC handlers initialized.")
      (swap! *main-window-info assoc :renderer-ipc-handlers-initialized? true)))

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
      (set! (.-returnValue e) (show-new-file-confirmation-dialog!))))

  (.on ipcMain "get-theme"
       (fn [e]
         (set! (.-returnValue e) (name (savefiles/read-sync! :theme)))))

  ;; DEV ONLY: used when hot-reloading to trigger reopen of last file modified
  (on-ipc "-reload-last-file" #(open-last-file!)))

(defn init-app-menu! [window]
  (p/let [recently-opened (savefiles/read! :recently-opened)
          recently-opened-path-frequencies (set (map :file-path recently-opened))
          dedupe-recently-opened-file-path (fn [file-path]
                                             (if (< 1 (get recently-opened-path-frequencies file-path))
                                               file-path
                                               (path/basename file-path)))

          template (clj->js [{:label (.-name app)
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
                                        {:label "Open Recent"
                                         :submenu (map (fn [{:keys [file-path]}]
                                                         {:label (dedupe-recently-opened-file-path file-path)
                                                          :click #(open-file-in-slate! file-path)})
                                                       recently-opened)}
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
                              :submenu [{:role "togglefullscreen"}
                                        {:label "Toggle Light/Dark Mode"
                                         :accelerator "Ctrl+Cmd+N"
                                         :click (fn []
                                                  (.. window -webContents (send "toggle-light-or-dark-mode"))
                                                  (-> (savefiles/read! :theme)
                                                      (p/then (fn [current-theme]
                                                                (savefiles/write! :theme (if (= current-theme :light)
                                                                                           :dark
                                                                                           :light))))))}]}
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
                                          {:role "toggledevtools"}
                                          {:label "Start screen recording"
                                           :click #(screen-recording/start-demo-recording! window)}]})))
    (.setApplicationMenu Menu (.buildFromTemplate Menu template))))

(defn- -init-window! []
  (log "Initializing Electron browser window")
  (swap! *main-window-info assoc :renderer-ipc-handlers-initialized? false)
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
      (log "Opening devtools")
      (.. window -webContents (openDevTools #js {:activate false})))
    (swap! *main-window-info merge {:window window})
    (.manage window-state window)
    (.loadURL ^js/electron.BrowserWindow window source-path)
    (.on ^js/electron.BrowserWindow window "closed"
         #(reset! *main-window-info nil))
    (.on ^js/electron.BrowserWindow window "enter-full-screen"
         #(.. window -webContents (send "change-full-screen-status", true)))
    (.on ^js/electron.BrowserWindow window "leave-full-screen"
         #(.. window -webContents (send "change-full-screen-status", false)))

    ;; Open last file on manual refreshes (useful when developing)
    (.once (.-webContents window) "did-finish-load" (fn []
                                                      (.on (.-webContents window) "did-finish-load"
                                                           ;; This will only fire for subsequent loads
                                                           (fn []
                                                             (log "Renderer process has auto-reloaded, calling open-last-file! again")
                                                             (open-last-file!)))))
    (init-app-menu! window)
    (log "Electron browser window initialized, calling open-last-file!")
    (open-last-file!)

    (when (= "true" (.. js/process -env -RECORD_DEMO))
      (log "Demo recording mode initiated from ENV variable")
      (go
        (<p! (wait-for-renderer-ipc-handlers!))
        (screen-recording/start-demo-recording! window)))))

(defn init-window!
  "Initializes the main Droplet window.
   Returns a Promise that resolves whenever the window has been created."
  []
  (-> (.whenReady app)
      (.then #(or (:promise @*main-window-info)
                  (-> (swap! *main-window-info assoc :promise
                             (js/Promise. (fn [resolve _]
                                            (try
                                              (-init-window!)
                                              (catch js/Object e
                                                (js/console.log "Error initializing window" e)))
                                            (resolve))))
                      ; get promise prop of returned main-window-info
                      :promise)))))

(defn main []
  ; CrashReporter can just be omitted
  (.start crashReporter
          #js {:companyName "Droplet"
               :productName "Droplet"
               :submitURL "https://example.com/submit-url"
               :autoSubmit false})

  ;; Check for file to open passed as cmd line argument
  (when (and (not is-dev?) ; in development the args will be those passed to the `electron` binary
             (> (count (.-argv js/process)) 1))
    (let [file-path (nth (.-argv js/process) 1)]
      (log "File passed to ARGV: " (.-argv js/process))

      ;; Check if file exits
      (if (and (fs/existsSync file-path)
               (.. (fs/lstatSync file-path) (isFile)))
        (do
          (open-file-in-slate! file-path)
          (swap! *main-window-info assoc :file-opened-from-os? true))
        (do
          (js/console.log (str "File " file-path " does not exist or is not a file."))
          (.exit js/process 1)))))

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
