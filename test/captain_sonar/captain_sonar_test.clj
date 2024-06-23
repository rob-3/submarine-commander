(ns captain-sonar.captain-sonar-test
  (:require
   [captain-sonar.systems :refer [drone-charged? green-broken? mine-charged?
                                  red-broken? silence-charged?
                                  sonar-charged? torpedo-charged?
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
    (is (not (torpedo-charged? {:systems {:torpedo 2}})))
    (is (not (mine-charged? {:systems {:mine 2}})))
    (is (not (drone-charged? {:systems {:drone 3}})))
    (is (not (sonar-charged? {:systems {:sonar 2}})))
    (is (not (silence-charged? {:systems {:silence 5}}))))
  (testing "Systems are charged"
    (is (torpedo-charged? {:systems {:torpedo 3}}))
    (is (mine-charged? {:systems {:mine 3}}))
    (is (drone-charged? {:systems {:drone 4}}))
    (is (sonar-charged? {:systems {:sonar 3}}))
    (is (silence-charged? {:systems {:silence 6}}))))

(comment
  (run-tests 'captain-sonar.captain-sonar-test))
