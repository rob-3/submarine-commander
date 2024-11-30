(ns dev.rob-3.submarine-commander.game-engine
  (:require
   [dev.rob-3.submarine-commander.actions :refer [location?]]))

(def state
  {:players {"d0869bad-8696-4405-ab35-8c34b2e96d33" [:team/red :captain]
             "e7e1966d-a54c-4df3-8f54-2162d4c5f8c0" [:team/red :first-mate]
             "2fd7c8be-482f-4ef0-804a-d110d14b7383" [:team/red :radio-operator]
             "798987b4-617a-4534-ba01-ddacc5aa8509" [:team/red :engineer]
             "3214c318-c497-43d7-a92d-10eb151a844b" [:team/blue :captain]
             "c46bcaa8-a2cf-4b4f-8458-6e7dd99163b4" [:team/blue :first-mate]
             "ac57b545-252e-49e1-8d4f-5f25de66bf7d" [:team/blue :radio-operator]
             "eaee2693-6afa-41d5-b5e6-094c5745f8a9" [:team/blue :engineer]}
   :teams {:team/red {:trail [[1 1]]
                      :health 4
                      :systems {:torpedo 0
                                :mine 0
                                :drone 0
                                :sonar 0
                                :silence 0}
                      ;; the presence of a keyword means the box is marked
                      :breakdowns {:west #{:red1 :yellow1 :green1 :green2 :reactor1 :reactor2}
                                   :north #{:yellow2 :yellow3 :red2 :green3 :red3 :reactor3}
                                   :south #{:green4 :yellow4 :red4 :red5 :reactor4 :yellow5}
                                   :east #{:green5 :yellow6 :red6 :reactor5 :green6 :reactor6}}
                      :surfaced false
                      :mines #{}
                      :roles {:captain "d0869bad-8696-4405-ab35-8c34b2e96d33"
                              :first-mate "e7e1966d-a54c-4df3-8f54-2162d4c5f8c0"
                              :radio-operator "2fd7c8be-482f-4ef0-804a-d110d14b7383"
                              :engineer "798987b4-617a-4534-ba01-ddacc5aa8509"}
                      :orders {:captain :north
                               :first-mate nil
                               :engineer nil}}
           :team/blue {:trail [[15 15]]
                       :health 4
                       :systems {:torpedo 0
                                 :mine 0
                                 :drone 0
                                 :sonar 0
                                 :silence 0}
                       :breakdowns {:west #{:red1 :green1 :green2 :reactor1 :reactor2}
                                    :north #{:yellow2 :yellow3 :red2 :green3 :red3 :reactor3}
                                    :south #{:green4 :yellow4 :red4 :red5 :reactor4 :yellow5}
                                    :east #{:green5 :yellow6 :red6 :reactor5 :green6 :reactor6}}
                       :surfaced false
                       :mines #{}
                       :roles {:captain "3214c318-c497-43d7-a92d-10eb151a844b"
                               :first-mate "c46bcaa8-a2cf-4b4f-8458-6e7dd99163b4"
                               :radio-operator "ac57b545-252e-49e1-8d4f-5f25de66bf7d"
                               :engineer "eaee2693-6afa-41d5-b5e6-094c5745f8a9"}
                       :orders {:captain :west
                                :first-mate :torpedo
                                :engineer :yellow1}}}
   :events []})

(defn create-team [start roles]
  {:pre [(location? start) roles]}
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
   :mines #{}
   :roles roles})

(defn create-game [& {:keys [teams]}]
  (let [teams' (reduce (fn [acc {:keys [start color roles]}]
                         (assoc acc color (create-team start roles)))
                       {}
                       teams)
        players (reduce (fn [acc {:keys [color roles]}]
                          (merge acc (reduce (fn [acc2 [role player-id]]
                                               (assoc acc2 player-id [color role]))
                                             {}
                                             roles)))
                        {}
                        teams)]
    {:players players
     :teams teams'
     :events []}))

(comment
  (create-game :teams [{:color :team/blue
                        :start [1 1]
                        :roles {:captain "52A2FAEF-4371-42D2-ADD4-9F5EBF545728"
                                :first-mate "160CE7D8-47A1-471A-A447-D8080B25A5C6"
                                :engineer "047C4F3D-C11A-4CF3-BFC1-037EB554F011"
                                :radio-operator "A53A0B18-DEFC-45D6-A438-0B5083AA0536"}}
                       {:color :team/red
                        :start [15 15]
                        :roles {:captain "034F3745-2C2B-41FD-9C3D-7021D487C55F"
                                :first-mate "3357833D-7F68-4A5D-B45F-EF5028B15395"
                                :engineer "085CC641-5306-4713-826E-588CDEED64F6"
                                :radio-operator "B1A8E5F2-326D-4FB5-9F16-E47A63F2603B"}}]))
