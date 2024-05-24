(ns captain-sonar.captain-sonar-test
  (:require
   [captain-sonar.game-engine :refer [green-broken? red-broken? yellow-broken?]]
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

(comment
  (run-tests 'captain-sonar.captain-sonar-test))
