(ns lhrb.player-test
  (:require
   [clojure.test :refer [deftest is]]
   [lhrb.player :as subject]))

(deftest distribution-test
  (is (= 20 (apply + (subject/distribution 20 5)))))

(deftest production-from-buildings-test
  (let [buildings [#:building{:name :building/greenhouse,
                              :worker
                              [#:worker{:name "Ursel",
                                        :skills
                                        #:skill{:green-thumb 2,
                                                :sorcery 9,
                                                :experimentation 3,
                                                :storytelling 5,
                                                :preaching 1}}
                               #:worker{:name "Jasmin",
                                        :skills
                                        #:skill{:green-thumb 1,
                                                :sorcery 1,
                                                :experimentation 2,
                                                :storytelling 1,
                                                :preaching 15}}]}
                   #:building{:name :building/mageguild,
                              :worker
                              [#:worker{:name "Peter",
                                        :skills
                                        #:skill{:green-thumb 9,
                                                :sorcery 2,
                                                :experimentation 1,
                                                :storytelling 7,
                                                :preaching 1}}
                               #:worker{:name "Eskarina",
                                        :skills
                                        #:skill{:green-thumb 2,
                                                :sorcery 11,
                                                :experimentation 1,
                                                :storytelling 1,
                                                :preaching 5}}]}
                   #:building{:name :building/cathedral}]]
    (is (= #:resource{:food 3, :mana 13, :religion 0}
           (subject/production-from-buildings buildings)))))

