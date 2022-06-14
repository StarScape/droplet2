(ns drop.electron.core
  (:require ["electron" :refer [app BrowserWindow crashReporter ipcMain]]
            ["electron-is-dev" :as is-dev?]
            ["electron-window-state" :as window-state-keeper]
            ["fs" :as fs]
            ["path" :as path]
            [clojure.edn :as edn]))

(js/console.log "Evaluating main electron file...")

(def main-window (atom nil))

(defn on-ipc [channel handler]
  (.on ipcMain channel handler))

(on-ipc "save-file"
  (fn [_ file-contents]
    (fs/writeFileSync "/Users/jack/Desktop/test.drop" file-contents)))

(on-ipc "open-file"
  (fn [e file-path]
    (js/console.log "open-file called")
    (let [file-contents (fs/readFileSync file-path "utf8")]
      (js/console.log file-contents)
      (set! (.-returnValue e) file-contents))))

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

    (.manage window-state window)
    ;; TODO: open dev tools when is-dev? is true

    (reset! main-window window)
    (.loadURL ^js/electron.BrowserWindow window source-path)
    ; Path is relative to the compiled js file (main.js in our case)
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
  (.on app "ready" init-browser))
