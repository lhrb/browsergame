(ns lhrb.resources)

(def buildings
  ;; TODO maybe add smth like :building/consumption (negative production)
  [{:building/name "lumber"
    :building/cost-build {:resource/wood 10
                          :resource/food 1
                          :resource/site 1}
    :building/production [{:production/product :resource/wood
                           :production/base 10
                           :production/multiplier 1.5}]
    :building/cost-upgrade {:resource/gold 10}}

   {:building/name "farm"
    :building/cost-build {:resource/wood 10
                          :resource/food 1
                          :resource/site 1}
    :building/production [{:production/product :resource/food
                           :production/base 10
                           :production/multiplier 1.5}]
    :building/cost-upgrade {:resource/gold 10}}

   {:building/name "quarry"
    :building/cost-build {:resource/wood 10
                          :resource/food 1
                          :resource/site 1}
    :building/production [{:production/product :resource/food
                           :production/base 10
                           :production/multiplier 1.5}]
    :building/cost-upgrade {:resource/gold 10}}

   {:building/name "ironmine"
    :building/cost-build {:resource/wood 10
                          :resource/food 1
                          :resource/site 1}
    :building/production [{:production/product :resource/food
                           :production/base 10
                           :production/multiplier 1.5}]
    :building/cost-upgrade {:resource/gold 10}}

   {:building/name "goldmine"
    :building/cost-build {:resource/wood 10
                          :resource/food 1
                          :resource/site 1}
    :building/production [{:production/product :resource/food
                           :production/base 10
                           :production/multiplier 1.5}]
    :building/cost-upgrade {:resource/gold 10}}])

(defn index-by-name [buildings]
  (reduce (fn [acc elem]
            (assoc acc (:building/name elem) elem))
          {} buildings))


(defn charge-cost
   "calculates the resources after decreasing by the given cost"
   [resources cost]
   (reduce-kv (fn [m k v]
                (update m k #(- % v))) resources cost))

 (defn able-to-pay?
   "determines if the player is able to pay the cost"
   [resources cost]
   (->> (charge-cost resources cost)
        (vals)
        (every? nat-int?)))

 (defn build
   "adds a new building to the player
  TODO check able-to-pay?"
   [building player]
   (let [resources (charge-cost (:player/resources player)
                                (:building/cost-build building))
         new-building (merge
                      (select-keys building [:building/name])
                      {:building/level 1})]
     (-> player
         (update :player/buildings conj new-building)
         (assoc :player/resources resources))))




(comment
 ;; currently supported resources
  {:resource/wood 0
  :resource/stone 0
  :resource/food 0
  :resource/metal 0
   :resource/gold 0}

  (def player
   {:player/resources {:resource/wood 0
                       :resource/stone 0
                       :resource/food 0
                       :resource/metal 0
                       :resource/gold 0
                       :resource/site 100 ;; building ground
                       }
    :player/buildings [{:building/id 0 :building/name "lumber" :building/level 1}
                       {:building/id 1 :building/name "hunter" :building/level 1}]})

   (build (first buildings) player)
  *e)
