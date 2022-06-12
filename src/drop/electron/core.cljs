(ns drop.electron.core
  (:require ["electron" :refer [app BrowserWindow crashReporter]]
            ["electron-is-dev" :refer [isDev]]))

(js/console.log "Initializing electron...")

(def main-window (atom nil))

(defn init-browser []
  (let [window (BrowserWindow.
                (clj->js {:width 1200
                          :height 900
                          :webPreferences
                          {:nodeIntegration true}}))
        source-path (if isDev
                      "http://localhost:8080"
                      (str "file://" js/__dirname "/../index.html"))]
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

  (.on app "window-all-closed" #(when-not (= js/process.platform "darwin")
                                  (.quit app)))
  (.on app "ready" init-browser))

(try
  ((js/require "electron-reloader") js/module)
  (catch js/Object e
       (.log js/console e)))
