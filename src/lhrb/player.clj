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

(defn production-player
  "Calculates the resources and currency from the player for this round"
  [player]
  (let [produced-resources (production-from-buildings (:player/buildings player))
        combined (merge-with + produced-resources (:player/resources player))
        civ (apply min (vals combined))]
    (-> player
        (update-in [:player/currency :currency/civ] #(+ % civ))
        (assoc-in [:player/resources]
                  (reduce-kv (fn [m k v] (assoc m k (- v civ))) {} combined)))))

;; create a player

(defn create-player [{:keys [name worker]
                      :as params}]

  {:player/name name
   :player/resources {:resource/food 0
                      :resource/mana 0
                      :resource/technology 0
                      :resource/culture 0
                      :resource/religion 0}
   :player/currency {:currency/gold 10000
                     :currency/civ 0}
   :player/worker worker
   :player/buildings [{:building/name :building/greenhouse}
                      {:building/name :building/laboratory}
                      {:building/name :building/cathedral}
                      {:building/name :building/theater}
                      {:building/name :building/mageguild}]})

(comment
  (= 20 (apply + (distribution 20 5)))
  (worker "X" 20)

  (def player
    {:player/resources #:resource{:food 5, :mana 0, :technology 1, :culture 2, :religion 4}
     :player/currency #:currency{:gold 345, :civ 3}
     :player/buildings [#:building{:name :building/greenhouse,
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
                        #:building{:name :building/cathedral}]})

  (production-player player)
  (production-player {:player/resources #:resource{:food 5, :mana 0, :technology 1, :culture 2, :religion 4}
                      :player/currency #:currency{:gold 345, :civ 3}})

  ,)
