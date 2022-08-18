(ns drop.electron.core
  (:require ["electron" :refer [app BrowserWindow Menu crashReporter ipcMain dialog]]
            ["electron-is-dev" :as is-dev?]
            ["electron-window-state" :as window-state-keeper]
            ["fs" :as fs]
            ["path" :as path]
            [drop.electron.utils :refer [on-ipc handle-ipc]]
            [drop.electron.persistent-atoms :as p-atoms]))

(def is-dev? true)

(js/console.log "Evaluating main electron file...")

(def main-window (atom nil))

(defn reg-ipc-handlers! []
  (p-atoms/reg-handler!)

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
         (-> (.showSaveDialog dialog @main-window
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

  (handle-ipc "choose-file"
    (fn [_]
      ;; #p "choose-file"
      (js/Promise.
       (fn [resolve, reject]
         (-> (.showOpenDialog dialog @main-window
                              (clj->js {:title "Open .drop"
                                        :filters [{:name "Droplet File"
                                                   :extensions [".drop"]}]
                                        :properties ["openFile"]}))
             (.then (fn [result]
                      #_(js/console.log result)
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
                            :submenu [{:label "New"
                                       :accelerator "CmdOrCtrl+N"
                                       :click #(.. window -webContents (send "menubar-item-clicked", "new"))}
                                      {:label "Save"
                                       :accelerator "CmdOrCtrl+S"
                                       :click #(.. window -webContents (send "menubar-item-clicked", "save"))}
                                      {:label "Save As..."
                                       :accelerator "CmdOrCtrl+Shift+S"
                                       :click #(.. window -webContents (send "menubar-item-clicked", "save-as"))}
                                      {:label "Open..."
                                       :accelerator "CmdOrCtrl+Shift+O"
                                       :click #(.. window -webContents (send "menubar-item-clicked", "open"))}]}
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
    (reset! main-window window)
    (.manage window-state window)
    (.loadURL ^js/electron.BrowserWindow window source-path)

    (.on ^js/electron.BrowserWindow window "closed"
         #(reset! main-window nil))

    (.on ^js/electron.BrowserWindow window "enter-full-screen"
         #(.. window -webContents (send "change-full-screen-status", true)))
    (.on ^js/electron.BrowserWindow window "leave-full-screen"
         #(.. window -webContents (send "change-full-screen-status", false)))

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
                                (js/console.log "whenReady")
                                (.on app "activate" #(js/console.log "activate!")))))
  (.on app "activate" (fn []
                        (when (zero? (.. BrowserWindow (getAllWindows) -length))
                          (init-window))))
  (reg-ipc-handlers!))
