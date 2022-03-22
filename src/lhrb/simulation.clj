(ns lhrb.simulation
  (:require [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]))

(s/def :resource/a (s/and nat-int? #(< % 10)))
(s/def :resource/b (s/and nat-int? #(< % 10)))
(s/def :resource/c (s/and nat-int? #(< % 10)))
(s/def :resource/d (s/and nat-int? #(< % 10)))
(s/def :resource/e (s/and nat-int? #(< % 10)))

(defn resources []
 (gen/generate (s/gen (s/keys :req [:resource/a :resource/b :resource/c :resource/d :resource/e]))))

(defn spawn-agent []
 {:agent/resources (resources)
  :agent/money 1000})

(defn max-revenue
  "max revenue a agent can get, when she sells everything"
  [prices agent]
  (reduce-kv (fn [acc k v] (+ acc (* (k prices) v)))
             0
             (:agent/resources agent)))

(defn market-volume
  "adds the agent inventory to the market"
  [market agent]
  (reduce-kv (fn [m k v] (update m k (fnil + 0) v))
             market
             (:agent/resources agent)))

(defn count-full-sets
  "how many full sets are for sale?"
  [market]
  (apply min (vals market)))

(defn set-price
  "price for a full set"
  [price-table]
  (apply + (vals price-table)))



(comment

  (def price #:resource{:a 20, :b 20, :c 20, :d 20, :e 20})
  (def agent1 (spawn-agent))
  (def market (market-volume {} agent1))

  (count-full-sets market)
  (set-price price)

  (def agents
    (take 3 (repeatedly spawn-agent)))

  (def volume (reduce (fn [m a] (market-volume m a)) {} agents))

  (count-full-sets volume)
 ,)
