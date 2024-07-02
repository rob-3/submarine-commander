(ns captain-sonar.captain-sonar-test
  (:require
   [captain-sonar.systems :refer [broken?]]

   [clojure.test :refer [deftest is run-tests testing]]))

(deftest system-breakdown-functions
  (testing "Broken systems are found"
    (is (broken? {:west #{} :north #{:red2} :south #{} :east #{}} :red))
    (is (broken? {:west #{} :north #{:yellow1} :south #{} :east #{}} :yellow))
    (is (broken? {:west #{} :north #{} :south #{} :east #{:green1}} :green)))
  (testing "Unbroken systems are not found"
    (is (not (broken? {:west #{:yellow :green1} :north #{} :south #{:green} :east #{:yellow}} :red)))
    (is (not (broken? {:west #{:red} :north #{} :south #{:red1} :east #{:green1}} :yellow)))
    (is (not (broken? {:west #{:red} :north #{:yellow1 :red1} :south #{:yellow1} :east #{}} :green)))))

(comment
  (run-tests 'captain-sonar.captain-sonar-test))
