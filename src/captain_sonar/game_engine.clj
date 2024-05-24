(ns captain-sonar.game-engine
  (:require
   [clojure.set :refer [intersection union]]))

(def state
  {:red {:location [1 1]
         :health 4
         :systems {:torpedo 0
                   :mine 0
                   :drone 0
                   :sonar 0
                   :silence 0}
         ;; the presence of a keyword means the box is marked
         :breakdowns {:west #{:red :yellow :green1 :green2 :reactor1 :reactor2}
                      :north #{:yellow1 :yellow2 :red1 :green1 :red2 :reactor}
                      :south #{:green :yellow1 :red1 :red2 :reactor :yellow2}
                      :east #{:green1 :yellow :red :reactor :green2 :reactor2}}
         :surfaced false}
   :blue {:location [15 15]
          :health 4
          :systems {:torpedo 0
                    :mine 0
                    :drone 0
                    :sonar 0
                    :silence 0}
          :breakdowns {:west #{:red :yellow :green1 :green2 :reactor1 :reactor2}
                       :north #{:yellow1 :yellow2 :red1 :green1 :red2 :reactor}
                       :south #{:green :yellow1 :red1 :red2 :reactor :yellow2}
                       :east #{:green1 :yellow :red :reactor :green2 :reactor2}}
          :surfaced false}})

(defn green-broken? [{:keys [west north south east]}]
  (let [marked-systems (union west north south east)]
    (boolean (seq (intersection marked-systems #{:green :green1 :green2})))))

(defn red-broken? [{:keys [west north south east]}]
  (let [marked-systems (union west north south east)]
    (boolean (seq (intersection marked-systems #{:red :red1 :red2})))))

(defn yellow-broken? [{:keys [west north south east]}]
  (let [marked-systems (union west north south east)]
    (boolean (seq (intersection marked-systems #{:yellow :yellow1 :yellow2})))))

(defn torpedo-online? [{:keys [systems breakdowns]}]
  (and (>= (:torpedo systems) 3)
       (not (red-broken? breakdowns))))

(defn mine-online? [{:keys [systems breakdowns]}]
 (and (>= (:mine systems) 3)
      (not (red-broken? breakdowns))))

(defn drone-online? [{:keys [systems breakdowns]}]
 (and (>= (:drone systems) 4)
      (not (green-broken? breakdowns))))

(defn sonar-online? [{:keys [systems breakdowns]}]
 (and (>= (:sonar systems) 3)
      (not (green-broken? breakdowns))))

(defn silence-online? [{:keys [systems breakdowns]}]
 (and (>= (:silence systems) 6)
      (not (yellow-broken? breakdowns))))

(comment
  (green-broken? {:west #{:green}
                  :north #{}
                  :south #{}
                  :east #{}})
  (green-broken? {:west #{:red}
                  :north #{}
                  :south #{}
                  :east #{}}))
