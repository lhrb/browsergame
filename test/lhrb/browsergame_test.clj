(ns lhrb.browsergame-test
  (:require [clojure.test :refer :all]
            [lhrb.browsergame :as sut]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


(deftest calc-dmg
  (testing "dmg calculation"
    (is (= 0 (sut/calc-dmg {:turn/evasion :success})))
    (is (= 0 (sut/calc-dmg {:turn/strike :fail})))
    (is (= 0 (sut/calc-dmg {:turn/evasion :fail
                            :turn/strike :fail})))))
