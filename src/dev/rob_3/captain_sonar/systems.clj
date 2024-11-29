(ns dev.rob-3.captain-sonar.systems
  (:require
   [clojure.set :refer [intersection union]]))

(def color->colorset
  {:green #{:green :green1 :green2}
   :red #{:red :red1 :red2}
   :yellow #{:yellow :yellow1 :yellow2}})

(defn broken? [{:keys [west north south east]} color]
  {:pre [(#{:green :red :yellow} color)]}
  (let [marked-systems (union west north south east)]
    (boolean (seq (intersection marked-systems (color->colorset color))))))

(comment
  (broken? {:west #{:green}
            :north #{}
            :south #{}
            :east #{}}
           :green)
  (broken? {:west #{:red}
            :north #{}
            :south #{}
            :east #{}}
           :green))
