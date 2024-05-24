(ns captain-sonar.captain-sonar-test
  (:require
   [captain-sonar.game-engine :refer [drone-online? green-broken? mine-online?
                                      red-broken? silence-online?
                                      sonar-online? torpedo-online?
                                      yellow-broken?]]
   [clojure.test :refer [deftest is run-tests testing]]))

(deftest system-breakdown-functions
  (testing "Broken systems are found"
    (is (red-broken? {:west #{} :north #{:red2} :south #{} :east #{}}))
    (is (yellow-broken? {:west #{} :north #{:yellow1} :south #{} :east #{}}))
    (is (green-broken? {:west #{} :north #{} :south #{} :east #{:green1}})))
  (testing "Unbroken systems are not found"
    (is (not (red-broken? {:west #{:yellow :green1} :north #{} :south #{:green} :east #{:yellow}})))
    (is (not (yellow-broken? {:west #{:red} :north #{} :south #{:red1} :east #{:green1}})))
    (is (not (green-broken? {:west #{:red} :north #{:yellow1 :red1} :south #{:yellow1} :east #{}})))))

(deftest system-online-functions
  (testing "Systems are offline due to charge"
    (is (not (torpedo-online? {:systems {:torpedo 2} :breakdowns {:west #{} :north #{} :south #{} :east #{}}})))
    (is (not (mine-online? {:systems {:mine 2} :breakdowns {:west #{} :north #{} :south #{} :east #{}}})))
    (is (not (drone-online? {:systems {:drone 3} :breakdowns {:west #{} :north #{} :south #{} :east #{}}})))
    (is (not (sonar-online? {:systems {:sonar 2} :breakdowns {:west #{} :north #{} :south #{} :east #{}}})))
    (is (not (silence-online? {:systems {:silence 5} :breakdowns {:west #{} :north #{} :south #{} :east #{}}}))))
  (testing "Systems are online"
    (is (torpedo-online? {:systems {:torpedo 3} :breakdowns {:west #{} :north #{} :south #{} :east #{}}}))
    (is (mine-online? {:systems {:mine 3} :breakdowns {:west #{} :north #{} :south #{} :east #{}}}))
    (is (drone-online? {:systems {:drone 4} :breakdowns {:west #{} :north #{} :south #{} :east #{}}}))
    (is (sonar-online? {:systems {:sonar 3} :breakdowns {:west #{} :north #{} :south #{} :east #{}}}))
    (is (silence-online? {:systems {:silence 6} :breakdowns {:west #{} :north #{} :south #{} :east #{}}}))))

(comment
  (run-tests 'captain-sonar.captain-sonar-test))
