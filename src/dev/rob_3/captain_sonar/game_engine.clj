(ns dev.rob-3.captain-sonar.game-engine
  (:require
   [dev.rob-3.captain-sonar.actions :refer [location?]]))

(def state
  {:teams {:team/red {:trail [[1 1]]
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
                      :surfaced false
                      :mines #{}}
           :team/blue {:trail [[15 15]]
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
                       :surfaced false
                       :mines #{}}}
   :events []})

(defn create-team [start]
  {:pre [(location? start)]}
  {:trail [start]
   :health 4
   :systems {:torpedo 0
             :mine 0
             :drone 0
             :sonar 0
             :silence 0}
   :breakdowns {:west #{}
                :north #{}
                :south #{}
                :east #{}}
   :surfaced false
   :mines #{}})

(defn create-game [& {:keys [teams]}]
  (let [teams (reduce (fn [acc {:keys [start color]}]
                        (assoc acc color (create-team start)))
                      {}
                      teams)]
    {:teams teams
     :events []}))

(comment
  (create-game :teams [{:color :team/blue :start [1 1]}
                       {:color :team/red :start [15 15]}]))
