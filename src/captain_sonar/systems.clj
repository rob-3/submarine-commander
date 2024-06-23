(ns captain-sonar.systems
  (:require
   [clojure.set :refer [intersection union]]))

(defn green-broken? [{:keys [west north south east]}]
  (let [marked-systems (union west north south east)]
    (boolean (seq (intersection marked-systems #{:green :green1 :green2})))))

(defn red-broken? [{:keys [west north south east]}]
  (let [marked-systems (union west north south east)]
    (boolean (seq (intersection marked-systems #{:red :red1 :red2})))))

(defn yellow-broken? [{:keys [west north south east]}]
  (let [marked-systems (union west north south east)]
    (boolean (seq (intersection marked-systems #{:yellow :yellow1 :yellow2})))))

(defn torpedo-charged? [systems]
  (boolean (< (:torpedo systems) 3)))

(defn mine-charged? [systems]
  (boolean (< (:mine systems) 3)))

(defn drone-charged? [systems]
  (boolean (< (:drone systems) 4)))

(defn sonar-charged? [systems]
  (boolean (< (:sonar systems) 3)))

(defn silence-charged? [systems]
  (boolean (< (:silence systems) 6)))

(comment
  (green-broken? {:west #{:green}
                  :north #{}
                  :south #{}
                  :east #{}})
  (green-broken? {:west #{:red}
                  :north #{}
                  :south #{}
                  :east #{}}))
