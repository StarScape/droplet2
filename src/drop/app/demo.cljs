(ns drop.app.demo
  "Code for automated recording of Droplet demo video(s)."
  (:require [re-frame.db]
            [re-frame.core :as rf]
            [promesa.core :as p]
            ["electron" :refer [ipcRenderer]]
          [slate.editor-ui-state :as ui-state])
  (:require-macros [promesa.core :as p]))

(defn screen-record!
  "Begins a screen recording, then calls callback, which should return a Promise that resolves when recording is ready to be ended."
  ([source-id setup]
   (p/let [dpr (.-devicePixelRatio js/window)
           width (*  dpr (.-outerWidth js/window))
           height (* dpr (.-outerHeight js/window))
           #_#_height-of-window-bar (- height (* dpr (.-innerHeight js/window)))
           stream (.. js/navigator -mediaDevices (getUserMedia (clj->js {:audio false,
                                                                         :video {:mandatory {:chromeMediaSource "desktop",
                                                                                             :chromeMediaSourceId source-id,
                                                                                             :minWidth width,
                                                                                             :minHeight height}}})))
           recorder (js/MediaRecorder. stream)]
     (setup)
     (.start recorder)
     (.send ipcRenderer "recording-started")
     (.once ipcRenderer "stop-screen-recording"
            (fn []
              (.addEventListener recorder "dataavailable"
                                 (fn [e]
                                   (p/let [array-buffer (.arrayBuffer (.-data e))]
                                     (.send ipcRenderer "save-video-file-and-exit" array-buffer)))
                                 #js {:once true})
              (.requestData recorder)
              (.stop recorder))))))

(defn record-main-demo! [source-id]
  (let [*slate-instance (:*slate-instance @re-frame.db/app-db)
        setup (fn []
                (ui-state/new-document! *slate-instance)
                (rf/dispatch [:set-open-file-path nil]))]
    (screen-record! source-id setup)))
