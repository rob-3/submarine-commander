(ns dev.rob-3.submarine-commander.error)

(defn err? [x]
  (boolean
   (and (keyword? x)
        (= "err" (namespace x)))))
