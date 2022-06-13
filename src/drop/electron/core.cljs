(ns drop.electron.core
  (:require ["electron" :refer [app BrowserWindow crashReporter]]
            ["electron-is-dev" :as is-dev?]
            ["electron-window-state" :as window-state-keeper]))

(js/console.log "Initializing electron...")

(def main-window (atom nil))

(defn init-browser []
  (let [window-state (window-state-keeper #js {:defaultWidth 1200
                                               :defaultHeight 900})
        window (BrowserWindow.
                #js {:x (.-x window-state)
                     :y (.-y window-state)
                     :width (.-width window-state)
                     :height (.-height window-state)
                     :webPreferences {:nodeIntegration true}})
        source-path (if is-dev?
                      "http://localhost:8080"
                      (str "file://" js/__dirname "/../index.html"))]
    (.manage window-state window)
    ;; TODO: open dev tools when is-dev? is true

    (reset! main-window window)
    (.loadURL ^js/electron.BrowserWindow window source-path)
    ; Path is relative to the compiled js file (main.js in our case)
    (.on ^js/electron.BrowserWindow @main-window "closed" #(reset! main-window nil))

    (js/console.log "Initialized Electron browser window")))

#_(defn ^:dev/after-load reload []
  (js/console.log "relaunching...")
  (.relaunch app)
  (.quit app))

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
