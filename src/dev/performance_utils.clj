(ns dev.performance-utils)

(defmacro inside-time-measurement! [measurement-name expr]
  `(do
     (if (measurement-started? ~measurement-name)
       (continue-time-measurement! ~measurement-name)
       (do
         (start-time-measurement! ~measurement-name)
         (pause-time-measurement! ~measurement-name)))
     (let [ret# ~expr]
       (pause-time-measurement! ~measurement-name)
       ret#)))

(comment
  (macroexpand
   '(inside-measurement! "get-words" (+ 1 1)))
  )