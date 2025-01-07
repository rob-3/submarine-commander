(ns dev.rob-3.submarine-commander.submarine-commander-test
  (:require
   [clojure.test :refer [deftest is run-tests testing]]
   [dev.rob-3.submarine-commander.actions :refer [tick]]
   [dev.rob-3.submarine-commander.game-engine :refer [create-game]]
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

(defn new-game []
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

(deftest integration
  (let [game (-> (new-game)
                 (tick :action :order/captain
                       :direction :east
                       :team :team/blue)
                 (tick :action :order/first-mate
                       :system :torpedo
                       :team :team/blue)
                 (tick :action :order/engineer
                       :breakdown :yellow6
                       :team :team/blue))]
    (is (= (last (get-in game [:teams :team/blue :trail]))
           [2 1]))))

(comment
  (run-tests 'dev.rob-3.submarine-commander.submarine-commander-test))
