(ns drop.electron.screen-recording
  (:require ["electron" :refer [app desktopCapturer]]
            ["node:fs/promises" :refer [writeFile]]
            [drop.electron.utils :refer [on-ipc on-ipc-once handle-ipc read-persisted! write-persisted! log]]
            [promesa.core :as p])
  (:require-macros [promesa.core :as p]))

(def typing-delay 50)

(defn enter-key!
  ([window modifiers key]
   (.. window -webContents (sendInputEvent (clj->js {:keyCode key, :modifiers modifiers :type "keyDown"})))
   (.. window -webContents (sendInputEvent (clj->js {:keyCode key, :modifiers modifiers :type "char"})))
   (.. window -webContents (sendInputEvent (clj->js {:keyCode key, :modifiers modifiers :type "keyUp"}))))

  ([window key]
   (enter-key! window [] key)))

(defn delay-and-enter-key!
  ([window modifiers key wait]
   (-> (p/delay wait)
       (p/then #(enter-key! window modifiers key))))
  ([window key wait] (delay-and-enter-key! window [] key wait)))

(defn enter-string! [window text]
  (reduce (fn [promise char]
            (p/then promise #(delay-and-enter-key! window char typing-delay)))
          (p/resolved nil) text))

(def shorthand-table
  {:enter {:key-code "Enter"}
   :backspace {:key-code "Backspace"}
   :cmd+i {:modifiers ["Command"] :key-code "I"}})

(defn play-script-item! ;returns promise
  [window item]
  (cond
    (string? item)
    (enter-string! window item)

    (keyword? item)
    (play-script-item! window (get shorthand-table item))

    (number? item)
    (p/delay item)

    (contains? item :modifiers)
    (delay-and-enter-key! window (:modifiers item) (:key-code item) typing-delay)

    (contains? item :key-code)
    (delay-and-enter-key! window (:key-code item) typing-delay)))

(defn play-script!
  "Convenience function to play a demo 'script', a data structure allowing you to easily declare
   What keystrokes to enter during the demo (see below for example)."
  [window script]
  (reduce (fn [promise script-item]
            (p/then promise #(play-script-item! window script-item)))
          (p/resolved nil) script))

(def demo-script
  [500
   {:modifiers ["Command"] :key-code "1"}
   "Droplet"
   :enter
   :cmd+i
   "Your new digital sheet of paper"
   :cmd+i
   :enter
   :enter
   "Droplet is a prose editor for writers"
   500
   {:modifiers ["Alt" "Shift"] :key-code "Left"}
   100
   :backspace

   "novelists"
   500
   {:modifiers ["Alt" "Shift"] :key-code "Left"}
   100
   :backspace

   "journalists"
   500
   {:modifiers ["Alt" "Shift"] :key-code "Left"}
   100
   :backspace

   "bloggers"
   500
   {:modifiers ["Alt" "Shift"] :key-code "Left"}
   100
   :backspace

   "students"
   500
   {:modifiers ["Alt" "Shift"] :key-code "Left"}
   100
   :backspace

   "you."
   1500
   " Its beautiful, minimal interface gives you the full power of rich text editing "
   :cmd+i "without" :cmd+i " the complexity of a spaceship's control panel."
   :enter
   :enter
   "So you can focus on your words, " :cmd+i "not" :cmd+i " your word processor."
   3000])

(defn start-demo-recording! [window]
  (p/let [window-title (.getTitle window)
          sources (.getSources desktopCapturer (clj->js {:types ["window" "screen"]}))
          source (first (filter #(= window-title (.-name %)) sources))]
    (.setContentSize window 1000 700)
    (.. window -webContents (send "start-screen-recording" (.-id source)))
    (on-ipc-once "recording-started"
                 (fn []
                   (-> (play-script! window demo-script)
                       (p/then #(.. window -webContents (send "stop-screen-recording"))))))))

(on-ipc "save-video-file-and-exit"
        (fn [_ video-array-buffer]
          (log "Saving video file")
          ;; The window bar needs to be cropped out of the final video, save it
          ;; to a temp file so the script can read it and crop it out with ffmpeg.
          (-> (p/all [#_(writeFile "./topbarheight.temp" (str top-bar-height))
                      (writeFile "./demo.webm" (js/Buffer.from video-array-buffer))])
              (p/then (fn []
                        (log "Saved demo video in Droplet main proc, exiting.")
                        (.quit app)))
              (p/catch (fn [err] (js/console.log err))))))
