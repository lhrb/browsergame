(ns lhrb.player)

(defn distribution
  "generate a distribution of n points to the amount of given bins
  every bin has at least one point in it
  Note: use spec instead?"
  [n bins]
  (loop [bins-left bins
         distribution '()]
    (if (= 1 bins-left)
      (shuffle (conj distribution (- n (apply + distribution))))
      (recur (dec bins-left)
             (conj distribution
                   (+ 1
                      (rand-int (- n (apply + distribution) bins-left))))))))

(defn worker [name skillpoints]
  {:worker/name name
   :worker/skills (zipmap [:skill/green-thumb
                           :skill/sorcery
                           :skill/experimentation
                           :skill/storytelling
                           :skill/preaching]
                          (distribution skillpoints 5))})




(comment
  (= 20 (apply + (distribution 20 5)))
  (worker "X" 20)


  (require '[datalevin.core :as d])


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
               :building/name {:db/valueType :db.type/string
                               :db/cardinality :db.cardinality/one}
               :building/worker {:db/valueType :db.type/ref
                                 :db/cardinality :db.cardinality/many}})


  (def conn (d/get-conn "/tmp/datalevin/mydb3" schema))


  (d/transact! conn [{:account/name "Peter" :account/password "1234"}])

  (d/transact! conn [(worker "Dieter" 20)])

  (d/transact! conn [{:player/name "horst"
                      :player/worker [(worker "Holga" 20) (worker "Jasmin" 20)]}])


  (d/q '[:find [(pull ?e [*])]
         :where [?e :worker/name  "Jasmin"]]
       (d/db conn))

  (d/pull-many (d/db conn) '[*] [1 2 3])

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



  ,)
