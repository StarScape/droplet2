(ns drop.electron.core
  (:require ["electron" :refer [app BrowserWindow Menu crashReporter ipcMain dialog]]
            ["electron-is-dev" :as is-dev?]
            ["electron-window-state" :as window-state-keeper]
            ["fs" :as fs]
            ["node:fs/promises" :refer [readFile writeFile]]
            ["path" :as path]
            [cljs.core.async :refer [go]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [drop.electron.utils :refer [on-ipc handle-ipc]]
            [drop.electron.persistent-atoms :as p-atoms]))

(js/console.log "Evaluating main electron file...")

(def file-formats-info {"rtf" {:file-type-name "RTF"
                               :file-extension ".rtf"}
                        "html" {:file-type-name "HTML"
                                :file-extension ".html"}})

(goog-define DEV true)
(def is-dev? DEV)

(def *main-window (atom nil))

(defn launch-import-dialog!
  "Launches an file dialog to import the specified format type.
   Ex format-name and file-extension: 'HTML' and 'html'. Note the lack of leading '.' for the file-extension."
  [file-type]
  (go
    (let [{:keys [file-type-name file-extension]} (get file-formats-info file-type)
          main-window @*main-window
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
            result (<p! (.showSaveDialog dialog @*main-window
                                         (clj->js {:title "Export As..."
                                                   :defaultPath (str suggested-file-name file-extension)
                                                   :filters [{:name file-type-name
                                                              :extensions [file-extension]}]})))]
        (when-not ^js (.-canceled result)
          (<p! (writeFile (.-filePath result) data "utf8"))
          #_(.. main-window -webContents (send "export-file-successful" "html"))))
      (catch js/Error e
        (js/console.log e)))))

(defn reg-ipc-handlers! []
  (p-atoms/reg-handler!)

  (on-ipc "show-error-dialog"
          (fn [_ title dialog-text]
            (.showErrorBox dialog title dialog-text)))

  (on-ipc "save-file"
    (fn [_ file-path file-contents]
      ;; #p "save-file"
      (fs/writeFile file-path file-contents
                    (fn [err] (when err (js/console.error err))))))

  (handle-ipc "save-file-as"
    (fn [_ file-contents]
      ;; #p "save-file-as"
      (js/Promise.
       (fn [resolve, reject]
         (-> (.showSaveDialog dialog @*main-window
                              (clj->js {:title "Save As..."
                                        :filters [{:name "Droplet File"
                                                   :extensions [".drop"]}]}))
             (.then (fn [result]
                      (if ^js (.-canceled result)
                        (reject "canceled")
                        (do
                          (fs/writeFileSync ^js (.-filePath result) file-contents)
                          (resolve ^js (.-filePath result))))))
             (.catch #()))))))

  (on-ipc "save-exported-file-as"
          (fn [_ exported-file exported-file-type suggested-file-name]
            (launch-export-dialog! exported-file exported-file-type suggested-file-name)))

  (handle-ipc "choose-file"
    (fn [_]
      ;; #p "choose-file"
      (js/Promise.
       (fn [resolve, reject]
         (-> (.showOpenDialog dialog @*main-window
                              (clj->js {:title "Open .drop"
                                        :filters [{:name "Droplet File"
                                                   :extensions [".drop"]}]
                                        :properties ["openFile"]}))
             (.then (fn [result]
                      (if-not ^js (.-canceled result)
                        (let [file-path (nth ^js (.-filePaths result) 0)]
                          (fs/readFile file-path "utf8" #(resolve #js [file-path %2])))
                        (reject "canceled"))))
             (.catch #()))))))

  (on-ipc "read-file"
    (fn [e file-path]
      ;; #p "read-file"
      (try
        (let [file-contents (fs/readFileSync file-path "utf8")]
          (set! (.-returnValue e) #js [false, file-contents]))
        (catch js/Error _
          (set! (.-returnValue e) #js [true, ""]))))))

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
                                       :click #(.. window -webContents (send "menubar-item-clicked" "new"))}
                                      {:label "Open..."
                                       :accelerator "CmdOrCtrl+O"
                                       :click #(.. window -webContents (send "menubar-item-clicked" "open"))}
                                      {:label "Save"
                                       :accelerator "CmdOrCtrl+S"
                                       :click #(.. window -webContents (send "menubar-item-clicked" "save"))}
                                      {:label "Save As..."
                                       :accelerator "CmdOrCtrl+Shift+S"
                                       :click #(.. window -webContents (send "menubar-item-clicked" "save-as"))}
                                      {:label "Import..."
                                       :submenu [#_{:label "HTML"
                                                  :click #(launch-import-dialog! "html")}
                                                 {:label "RTF"
                                                  :click #(launch-import-dialog! "rtf")}]}
                                      {:label "Export As..."
                                       :submenu [{:label "HTML"
                                                  :click #(.. window -webContents (send "menubar-item-clicked" "initiate-file-export" "html"))}
                                                 {:label "RTF"
                                                  :click #(.. window -webContents (send "menubar-item-clicked" "initiate-file-export" "rtf"))}]}]}
                           {:role "editMenu"}
                           {:label "View",
                            :submenu [{:role "togglefullscreen"}]}
                           {:role "windowMenu"}])]
    (when is-dev?
      (.push template (clj->js {:label "Dev"
                                :submenu [{:role "reload"}
                                          {:role "forcereload"}
                                          {:role "toggledevtools"}]})))

    (.setApplicationMenu Menu (.buildFromTemplate Menu template))))

(defn init-window []
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
      (.. window -webContents (openDevTools)))
    (reset! *main-window window)
    (.manage window-state window)
    (.loadURL ^js/electron.BrowserWindow window source-path)

    (.on ^js/electron.BrowserWindow window "closed"
         #(reset! *main-window nil))

    (.on ^js/electron.BrowserWindow window "enter-full-screen"
         #(.. window -webContents (send "change-full-screen-status", true)))
    (.on ^js/electron.BrowserWindow window "leave-full-screen"
         #(.. window -webContents (send "change-full-screen-status", false)))
    (.on app "open-file"
         (fn [event path]
           (fs/readFile path "utf8" (fn [err, contents]
                                      (.. window -webContents (send "open-file" path contents))))))

    (init-app-menu window)

    (js/console.log "Initialized Electron browser window")))

(defn main []
  ; CrashReporter can just be omitted
  (.start crashReporter
          #js {:companyName "Droplet"
               :productName "Droplet"
               :submitURL "https://example.com/submit-url"
               :autoSubmit false})

  (.on app "window-all-closed" #(when-not (= js/process.platform "darwin") (.quit app)))
  (.on app "ready" init-window)
  #_(.. app (whenReady) (then (fn []
                                (.on app "activate" #(js/console.log "activate!")))))
  (.on app "activate" (fn []
                        (when (zero? (.. BrowserWindow (getAllWindows) -length))
                          (init-window))))

  (reg-ipc-handlers!))
