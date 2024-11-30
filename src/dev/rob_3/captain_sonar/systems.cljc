(ns dev.rob-3.captain-sonar.systems
  (:require
   [clojure.set :refer [intersection union]]))

(def color->colorset
  {:green #{:green1 :green2 :green3 :green4 :green5 :green6}
   :red #{:red1 :red2 :red3 :red4 :red5 :red6}
   :yellow #{:yellow1 :yellow2 :yellow3 :yellow4 :yellow5 :yellow6}})

(defn broken? [{:keys [west north south east]} color]
  {:pre [(#{:green :red :yellow} color)]}
  (let [marked-systems (union west north south east)]
    (boolean (seq (intersection marked-systems (color->colorset color))))))

(comment
  (broken? {:west #{:green1}
            :north #{}
            :south #{}
            :east #{}}
           :green)
  (broken? {:west #{:red1}
            :north #{}
            :south #{}
            :east #{}}
           :green))
