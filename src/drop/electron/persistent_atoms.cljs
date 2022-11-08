(ns drop.electron.persistent-atoms
  (:require ["fs" :as fs]
            ["path" :as path]
            ["electron" :refer [app]]
            [drop.electron.utils :refer [on-ipc]]))

(def *writer-queues (atom {}))
(def *currently-writing (atom #{}))

(defn persistent-atom-name->file-path
  [atom-name]
  (let [file-name (-> (str atom-name)
                      (.substring 1)
                      (.replaceAll "/" "_")
                      (str ".edn"))]
    (path/join (.getPath app "userData") file-name)))

(comment
  (persistent-atom-name->file-path ::foo-bar-buzz)
  )

(defn enqueue-or-create [queue edn-string]
  (if queue
    (conj queue edn-string)
    #queue [edn-string]))

(defn write-queued!
  "Writes all the updates queued for a specific persistent-atom to its file,
   in order, each write waiting for the last to complete before starting."
  [atom-name]
  (when-not (@*currently-writing atom-name)
    (swap! *currently-writing conj atom-name)
    (let [queue (get @*writer-queues atom-name)
          edn-string (peek queue)]
      (when edn-string
        (swap! *writer-queues update atom-name pop)
        (fs/writeFile (persistent-atom-name->file-path atom-name) edn-string
                      (fn [_]
                        (swap! *currently-writing disj atom-name)
                        (if (empty? (get @*writer-queues atom-name))
                          (swap! *writer-queues dissoc atom-name) ; nothing queued, remove queue
                          (write-queued! atom-name))))))))

(defn reg-handler!
  "Register IPC events for handling of persistent atoms."
  []
  (on-ipc "persist-atom"
          (fn [_ atom-name edn-string]
            (swap! *writer-queues update atom-name enqueue-or-create edn-string)
            (write-queued! atom-name)))

  (on-ipc "get-atom-value"
          (fn [event atom-name]
            (let [file-path (persistent-atom-name->file-path atom-name)
                  return-val (if (fs/existsSync file-path)
                               #js [true, (fs/readFileSync file-path "utf8")]
                               #js [false, nil])]
              (set! (.-returnValue event) return-val)))))