(ns dev.rob-3.submarine-commander.systems
  (:require
   [clojure.set :refer [intersection]]))

(def color->colorset
  {:green #{:green1 :green2 :green3 :green4 :green5 :green6}
   :red #{:red1 :red2 :red3 :red4 :red5 :red6}
   :yellow #{:yellow1 :yellow2 :yellow3 :yellow4 :yellow5 :yellow6}})

(defn broken? [breakdowns color]
  {:pre [(#{:green :red :yellow} color)]}
  (boolean (seq (intersection breakdowns (color->colorset color)))))

(comment
  (broken? #{:green1}
           :green)
  (broken? #{:red1}
           :green))
