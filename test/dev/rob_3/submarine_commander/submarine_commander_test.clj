(ns dev.rob-3.submarine-commander.submarine-commander-test
  (:require
   [clojure.test :refer [deftest is run-tests testing]]
   [dev.rob-3.submarine-commander.actions :refer [tick]]
   [dev.rob-3.submarine-commander.game-engine :refer [create-game]]
   [dev.rob-3.submarine-commander.lenses :refer [blue-health blue-mine-charge
                                                 blue-mines blue-torp-charge
                                                 blue-trail red-orders]]
   [dev.rob-3.submarine-commander.maps :as maps]
   [dev.rob-3.submarine-commander.systems :refer [broken?]]))

(deftest system-breakdown-functions
  (testing "Broken systems are found"
    (is (broken? {:west #{} :north #{:red2} :south #{} :east #{}} :red))
    (is (broken? {:west #{} :north #{:yellow1} :south #{} :east #{}} :yellow))
    (is (broken? {:west #{} :north #{} :south #{} :east #{:green1}} :green)))
  (testing "Unbroken systems are not found"
    (is (not (broken? {:west #{:yellow :green1} :north #{} :south #{:green} :east #{:yellow}} :red)))
    (is (not (broken? {:west #{:red} :north #{} :south #{:red1} :east #{:green1}} :yellow)))
    (is (not (broken? {:west #{:red} :north #{:yellow1 :red1} :south #{:yellow1} :east #{}} :green)))))

(defn new-game [starts map]
  (create-game 
    :map map
    :teams [{:color :team/blue
             :start (:blue starts)
             :roles {:captain "52A2FAEF-4371-42D2-ADD4-9F5EBF545728"
                     :first-mate "160CE7D8-47A1-471A-A447-D8080B25A5C6"
                     :engineer "047C4F3D-C11A-4CF3-BFC1-037EB554F011"
                     :radio-operator "A53A0B18-DEFC-45D6-A438-0B5083AA0536"}}
            {:color :team/red
             :start (:red starts)
             :roles {:captain "034F3745-2C2B-41FD-9C3D-7021D487C55F"
                     :first-mate "3357833D-7F68-4A5D-B45F-EF5028B15395"
                     :engineer "085CC641-5306-4713-826E-588CDEED64F6"
                     :radio-operator "B1A8E5F2-326D-4FB5-9F16-E47A63F2603B"}}]))

(deftest test-tick
  (let [game (-> (new-game {:blue [1 1] :red [15 15]} maps/alpha)
                 (tick :action :order/captain
                       :direction :east
                       :team :team/blue)
                 (tick :action :order/first-mate
                       :system :torpedo
                       :team :team/blue)
                 (tick :action :order/engineer
                       :breakdown :yellow6
                       :team :team/blue))]
    (is (= (last (blue-trail game))
           [2 1]))))

(defn integration-test [& {:keys [starts map moves]
                           :or {map maps/alpha
                                starts {:blue [1 1] :red [15 15]}}}]
  (loop [game (new-game starts map)
         [move & moves] moves]
    (if (nil? move)
      game
      (let [[color action data1 data2] move
            color (keyword "team" (name color))
            game (case action
                   (:north
                    :south
                    :east
                    :west) (-> game
                               (tick :action :order/captain
                                     :direction action
                                     :team color)
                               (tick :action :order/first-mate
                                     :system data1
                                     :team color)
                               (tick :action :order/engineer
                                     :breakdown data2
                                     :team color))
                   :torpedo (tick game
                                  :action :order/torpedo
                                  :target data1
                                  :team color)
                   :mine (tick game
                               :action :order/mine
                               :target data1
                               :team color)
                   :detonate (tick game
                                   :action :order/detonate
                                   :mine data1
                                   :team color)
                   :sonar (tick game
                                :action :order/sonar
                                :team color
                                :target-team data1)
                   :drone (tick game
                                :action :order/drone
                                :target-team data1
                                :guess data2
                                :team color)
                   :silence (tick game
                                  :action :order/silence
                                  :move data1
                                  :team color))]
        (recur game moves)))))

(deftest integration
  (let [game (-> (new-game {:blue [1 1] :red [15 15]} maps/alpha)
                 (tick :action :order/captain
                       :direction :east
                       :team :team/blue)
                 (tick :action :order/first-mate
                       :system :torpedo
                       :team :team/blue)
                 (tick :action :order/engineer
                       :breakdown :yellow6
                       :team :team/blue))]
    (is (= (last (blue-trail game))
           [2 1]))))

(deftest integration2
  (let [game (integration-test :moves [[:blue :east :torpedo :yellow6]
                                       ;; FIXME finish test format
                                       [:blue :south :torpedo :yellow4]
                                       [:blue :west :torpedo :yellow1]
                                       [:red :west :drone :yellow1]
                                       [:red :west :drone :red1]
                                       [:red :west :drone :reactor1]
                                       [:red :west :drone :reactor2]
                                       [:red :drone :team/blue :sector/five]])
        orders (red-orders game)]
    (is (map? orders))))

(deftest system-charges
  (let [game (integration-test :moves [[:blue :east :torpedo :yellow6]
                                       [:blue :east :torpedo :red6]
                                       [:blue :east :torpedo :reactor6]])
        charges (blue-torp-charge game)]
    (is (= 3 charges))))

(deftest wrong-path
  (let [game (integration-test :moves [[:blue :east :torpedo :yellow6]
                                       [:blue :south :torpedo :yellow4]])
        blue-location (last (blue-trail game))
        blue-torpedo-charge (blue-torp-charge game)]
    (is (= [2 1] blue-location))
    (is (= 1 blue-torpedo-charge))))

(deftest fire-torpedo
  (let [game (integration-test :starts {:blue [1 1] :red [7 1]}
                               :moves [[:red :west :torpedo :yellow1]
                                       [:red :west :torpedo :reactor1]
                                       [:red :west :torpedo :reactor2]
                                       [:red :torpedo [1 1]]]) 
        blue-health (blue-health game)]
    (is (= 2 blue-health))
    (is (nil? (:error game)))))

(deftest illegal-lay-mine
  (let [g (integration-test :moves [[:blue :east :mine :yellow6]
                                    [:blue :east :mine :reactor5]
                                    [:blue :east :mine :reactor6]
                                    ;; flipped x/y from what is possible
                                    [:blue :mine [2 4]]])]
    (is (= (blue-mines g) #{}))
    (is (= (blue-mine-charge g) 3))
    (is (:error g))))

(deftest lay-a-mine
  (let [g (integration-test :moves [[:blue :east :mine :yellow6]
                                    [:blue :east :mine :reactor5]
                                    [:blue :east :mine :reactor6]
                                    [:blue :mine [4 2]]])]
    (is (= (blue-mines g) #{[4 2]}))
    (is (= (blue-mine-charge g) 0))
    (is (nil? (:error g)))))

(comment
  (run-tests 'dev.rob-3.submarine-commander.submarine-commander-test))
