(ns lhrb.browsergame-test
  (:require [clojure.test :refer :all]
            [lhrb.browsergame :refer :all]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


(deftest calc-dmg
  (testing "dmg calculation"
    (is (= 0 (dmg {:turn/evasion :success})))
    (is (= 0 (dmg {:turn/strike :fail})))
    (is (= 0 (dmg {:turn/evasion :fail
                   :turn/strike :fail})))))
