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

;; account

(defn insert-account!
  [db {:keys [name password] :as account}]
  (d/transact! db [{:account/name name :account/password password}]))

(defn- create-account!
  "encrypts the password"
  [db {:keys [name password]}]
  (d/transact! db [{:account/name name
                    :account/password (buddy.hashers/derive password {:alg :bcrypt+sha512})}]))

(defn account?
  [db {:keys [name password]}]
  (d/q
   '[:find [?acc]
     :in $ ?name ?pw
     :where
     [?acc :account/name ?alias]
     [?acc :account/password ?enc]
     [(buddy.hashers/check ?pw ?enc)]]
   (d/db db)
   name password))

(def conn (d/get-conn "/tmp/datalevin/bg1" schema))

;(create-account! conn {:name "Hans" :password "12343"})
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
         :where [?e :account/name "Hans"]]
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


  (account? conn {:name "admin" :password "1234"})

(create-account! conn {:name "admin" :password "1234"})

(def password "1234")

(d/q
   '[:find [?acc ?p]
     :in $ ?name ?pw
     :where
     [?acc :account/name ?alias]
     [?acc :account/password ?p]]
   (d/db conn)
   "admin" "pw")

(d/q
 '[:find [?acc]
   :in $ ?name ?pw
   :where
   [?acc :account/name ?alias]
   [?acc :account/password ?enc]
   [(buddy.hashers/check ?pw ?enc)]]
 (d/db conn)
 "admin" "1234")

(insert-account! conn {:name "Hans" :password "12343"})

  )
