(ns captain-sonar.game-engine)

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
