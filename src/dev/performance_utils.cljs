(ns dev.performance-utils)

(def registry (atom {}))

(defn update-total
  [{:keys [most-recent-start-time total] :as entry}]
  (assoc entry :total (+ total (- (js/performance.now) most-recent-start-time))))

(defn measurement-started?
  "Returns true if the measurement registered with `name` has been started. May or may not be paused."
  [name]
  (some? (get @registry name)))

(defn start-time-measurement!
  [name]
  (swap! registry assoc name {:most-recent-start-time (js/performance.now)
                              :total 0
                              :paused? false}))

(defn pause-time-measurement! [name]
  (swap! registry update name #(-> %
                                   (update-total)
                                   (assoc :paused? true))))

(defn continue-time-measurement! [name]
  (let [entry (get @registry name)
        new-entry (if (:paused? entry)
                    (assoc entry
                           :most-recent-start-time (js/performance.now)
                           :paused? false)
                    entry)]
    (swap! registry assoc name new-entry)))

(defn stop-time-measurement! [name]
  (let [{:keys [paused? total] :as entry} (get @registry name)
        result (if-not paused?
                 (:total (update-total entry))
                 total)]
    (swap! registry dissoc name)
    result))

(def ^:private *last-time-called (atom nil))

(defn print-time-since-last!
  "Prints the time since the last time this function was called.
   Useful for narrowing down where bottlenecks are."
  []
  (let [last-time-called @*last-time-called
        now (js/Date.now)]
    (if last-time-called
      (js/console.log (str (- now last-time-called) "ms"))
      (js/console.log "No previous call."))
    (reset! *last-time-called now)))

(comment
  (start-time-measurement! "foobar")
  (pause-time-measurement! "foobar")
  (continue-time-measurement! "foobar")
  (stop-time-measurement! "foobar")

  (print-time-since-last!)

  )