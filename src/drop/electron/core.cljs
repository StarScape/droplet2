(ns drop.electron.core
  (:require ["electron" :refer [app BrowserWindow crashReporter ipcMain dialog]]
            ["electron-is-dev" :as is-dev?]
            ["electron-window-state" :as window-state-keeper]
            ["fs" :as fs]
            [drop.electron.utils :refer [on-ipc handle-ipc]]
            [drop.electron.persistent-atoms :as p-atoms]))

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
      (let [file-contents (fs/readFileSync file-path "utf8")]
        (js/console.log file-contents)
        (set! (.-returnValue e) file-contents)))))

(defn init-browser []
  (let [window-state (window-state-keeper #js {:defaultWidth 1200
                                               :defaultHeight 900})
        window (BrowserWindow.
                #js {:x (.-x window-state)
                     :y (.-y window-state)
                     :width (.-width window-state)
                     :height (.-height window-state)
                     :webPreferences #js {:nodeIntegration true
                                          :contextIsolation false}})
        source-path (if is-dev?
                      "http://localhost:8080"
                      (str "file://" js/__dirname "/../index.html"))]
    (when is-dev?
      (.. window -webContents (openDevTools)))
    (reset! main-window window)
    (.manage window-state window)
    (.loadURL ^js/electron.BrowserWindow window source-path)
    (.on ^js/electron.BrowserWindow @main-window "closed" #(reset! main-window nil))
    (js/console.log "Initialized Electron browser window")))

(defn main []
  ; CrashReporter can just be omitted
  (.start crashReporter
          #js {:companyName "Droplet"
               :productName "Droplet"
               :submitURL "https://example.com/submit-url"
               :autoSubmit false})

  (.on app "window-all-closed" #(when-not (= js/process.platform "darwin") (.quit app)))
  (.on app "ready" init-browser)
  (reg-ipc-handlers!))
