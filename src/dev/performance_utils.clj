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

(defmacro measure-time-and-print! [measurement-name expr]
  `(do
     (js/setTimeout (fn []
                      (js/console.log (str ~measurement-name " took " (stop-time-measurement! ~measurement-name) "ms")))
                    0)
     (inside-time-measurement! ~measurement-name ~expr)))

(comment
  (macroexpand
   '(inside-time-measurement! "get-words" (+ 1 1)))
  (macroexpand
   '(measure-time-and-print! "foo!" (+ 1 1)))
  )
