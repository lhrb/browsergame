(ns lhrb.player
  (:require [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]))

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

(s/def :building/name #{:building/cathedral
                        :building/greenhouse
                        :building/mageguild
                        :building/theater
                        :building/laboratory})

(def skill-for-building {:building/greenhouse :skill/green-thumb
                         :building/mageguild :skill/sorcery
                         :building/laboratory :skill/experimentation
                         :building/theater :skill/storytelling
                         :building/cathedral :skill/preaching})

(def resource-from-building {:building/greenhouse :resource/food
                             :building/mageguild :resource/mana
                             :building/laboratory :resource/technology
                             :building/theater :resource/culture
                             :building/cathedral :resource/religion})

(defn production
  "Calculates the resource production for the given building."
  [building]
  (let [name (get-in building [:building/name])
        skill (get skill-for-building name)
        resource (get resource-from-building name)]
    {resource (->> (get building :building/worker)
                   (map (fn [w] (get-in w [:worker/skills skill])))
                   (apply +))}))

(defn production-from-buildings
  "Calculates the resource production from all buildings"
  [buildings]
  (reduce (fn [m b] (merge m (production b))) {} buildings))


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
  (d/clear conn)


  (d/transact! conn [{:account/name "Peter" :account/password "1234"}])

  (d/transact! conn [(worker "Dieter" 20)])

  (d/transact! conn [{:player/name "horst"
                      :player/worker [(worker "Holga" 20) (worker "Jasmin" 20)]}])

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

  (def building
    {:building/name :building/mageguild
     :building/worker [(worker "Peter" 20) (worker "Eskarina" 20)]})

  (def buildings
    [#:building{:name :building/greenhouse,
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
     #:building{:name :building/cathedral}])

  (production building)

  (production-from-buildings buildings)





  ,)
