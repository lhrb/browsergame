(ns lhrb.auth-test
  (:require [lhrb.auth :as sut]
            [clojure.test :refer [deftest is]]
            [datalevin.core :as d]
            [lhrb.db :as db]))

(def conn (d/create-conn "" db/schema))
(db/create-account! conn {:name "John" :password "Doe"})

(deftest login-test
  (is (= 303 (:status (sut/login {:form-params {:name "John" :password "Doe"}
                                  :db conn}))))
  (is (= 404 (:status (sut/login {:db conn})))))
