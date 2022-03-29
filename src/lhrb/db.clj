(ns lhrb.db
  (:require [datalevin.core :as d]))

(def schema {:account/name {:db/valueType :db.type/string
                              :db/cardinality :db.cardinality/one
                              :db/unique    :db.unique/identity}
               :account/password {:db/valueType :db.type/string
                                  :db/cardinality :db.cardinality/one}
               :player/name {:db/valueType :db.type/string
                             :db/cardinality :db.cardinality/one
                             :db/unique :db.unique/identity}
               :player/worker {:db/valueType :db.type/ref
                               :db/cardinality :db.cardinality/many}
               :player/buildings {:db/valueType :db.type/ref
                                  :db/cardinality :db.cardinality/many}
               :worker/name {:db/valueType :db.type/string
                             :db/cardinality :db.cardinality/one}
               :building/name {:db/valueType :db.type/keyword
                               :db/cardinality :db.cardinality/one}
               :building/worker {:db/valueType :db.type/ref
                                 :db/cardinality :db.cardinality/many}})
(comment

  (def conn (d/get-conn "/tmp/datalevin/mydb3" schema))
  (d/clear conn)


  (d/transact! conn [{:account/name "Peter" :account/password "1234"}])

  (d/transact! conn [{:building/name "Cathedral"
                      :building/worker [2 3]}])

  (d/transact! conn [[:db/retract 4 :building/worker 2]])

  (d/q '[:find [(pull ?e [*])]
         :where [?e :worker/name  "Jasmin"]]
       (d/db conn))

  (d/pull-many (d/db conn) '[*] [1 2 3 4])

  (d/q '[:find [(pull ?e [*])]
         :where [?e :account/name "Peter"]]
       (d/db conn))

  (d/q '[:find ?w
         :where
         [?e :player/name "horst"]
         [?e :player/worker ?w]]
       (d/db conn))
  ;; Query the data
  (d/q '[:find ?nation
         :in $ ?alias
         :where
         [?e :aka ?alias]
         [?e :nation ?nation]]
       (d/db conn)
       "fred")


  (d/listen! conn :log
             (fn [tx-report]
               (clojure.pprint/pprint tx-report)))

  )
