(ns lhrb.market
  (:require [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]))

(s/def :resource/food (s/and nat-int? #(< % 10)))
(s/def :resource/mana (s/and nat-int? #(< % 10)))
(s/def :resource/technology (s/and nat-int? #(< % 10)))
(s/def :resource/culture (s/and nat-int? #(< % 10)))
(s/def :resource/religion (s/and nat-int? #(< % 10)))

(s/def :vendor/resources (s/keys :req [:resource/food
                                       :resource/mana
                                       :resource/technology
                                       :resource/culture
                                       :resource/religion]))

(s/def :currency/gold (s/and nat-int? #(< % 1000)))
(s/def :currency/civ (s/and nat-int? #(< % 10)))
(s/def :vendor/currency (s/keys :req [:currency/gold :currency/civ]))

(defn gen-vendor []
  (gen/generate (s/gen (s/keys :req [:vendor/resources :vendor/currency]))))

(defn max-revenue
  "max revenue a agent can get, when she sells everything"
  [prices vendor]
  (reduce-kv (fn [acc k v] (+ acc (* (k prices) v)))
             0
             (:vendor/resources vendor)))

(defn add-to-volume
  "adds the agent inventory to the market"
  [market vendor]
  (reduce-kv (fn [m k v] (update m k (fnil + 0) v))
             market
             (:vendor/resources vendor)))

(defn market-volume
  [vendors]
  (reduce (fn [m a] (add-to-volume m a)) {} vendors))

(defn count-full-sets
  "how many full sets are for sale?"
  [market]
  (apply min (vals market)))

(defn set-price
  "price for a full set"
  [price-table]
  (apply + (vals price-table)))

;; -------------------------------------
;; Buy resources from the vendors
;; -------------------------------------

(defn- buy-resource
  [price-table resource vendor]
  (-> vendor
   (update-in [:vendor/resources resource] (fnil dec 0))
   (update-in [:vendor/currency :currency/gold] (fnil #(+ (get price-table resource) %) 0))))

(defn out-of-stock?
  "test if the given resource is out of stock"
  [resource vendor]
  (= 0 (get-in vendor [:vendor/resources resource])))

(defn in-stock?
  "test if the given resource is in stock"
  [resource vendor]
  (< 0 (get-in vendor [:vendor/resources resource])))

(defn- buy-from-vendors
  "get the given amount of the resource from the list of vendors
  - shuffles the vendor list before iterating for fairness
  - the amount has to be determinded before see #count-full-sets
  notes: extract price-table + resouce,"
  [price-table resource amount vendors]
  (let [vendors (shuffle vendors)]
   (loop [n amount
          v (filter (partial in-stock? resource) vendors)
          a (filter (partial out-of-stock? resource) vendors)]
     (if (<= n 0)
       (concat v a)
       (let [[vendor & rst] v
             vendor' (buy-resource price-table resource vendor)]
         (recur (dec n)
                (if (in-stock? resource vendor')
                  (conj (vec rst) vendor')
                  rst)
                (if (in-stock? resource vendor')
                  a
                  (conj a vendor'))))))))

(defn buy-resources
  "to assemble a building part every resource is needed once.
  The amount to buy is the minimal count of a resource"
  [price-table vendors]
 (let [volume (market-volume vendors)
       amount (count-full-sets volume)]
   (reduce (fn [v res] (buy-from-vendors price-table res amount v))
           vendors
           [:resource/food :resource/mana :resource/technology :resource/culture :resource/religion])))

;; -------------------------------------
;; Sell the assembled parts
;; TODO better name for "building block"
;; -------------------------------------

(defn- sell-civ-tokens
  "add building block to the vendor inventory and pay the price
  does not check if vendor has enough money"
  [price vendor]
  (-> vendor
   (update-in [:vendor/currency :currency/civ] (fnil inc 0))
   (update-in [:vendor/currency :currency/gold] (fnil #(- % price) 0))))

(defn enough-money-for-civ-token?
  "tests if the vendor has enough money to pay the price"
  [price vendor]
  (<= price (get-in vendor [:vendor/currency :currency/gold])))

(defn sell-tokens
  "iterate through the vendors and sell them the 'building blocks'
  - shuffle the vendor list for fairness"
  [price-table amount vendors]
 (let [vendors (shuffle vendors)
       price (set-price price-table)
       enough-money? (partial enough-money-for-civ-token? price)]
   (loop [n amount
          v (filter enough-money? vendors)
          a (remove enough-money? vendors)]
     (if (or (= n 0) (empty? v))
       {:sell-result/amount-left n
        :sell-result/vendors (concat v a)}
       (let [[vendor & rst] v
             vendor' (sell-civ-tokens price vendor)]
         (recur (dec n)
                (if (enough-money? vendor')
                  (conj (vec rst) vendor')
                  rst)
                (if (enough-money? vendor')
                  a
                  (conj a vendor'))))))))

;; -------------------------------------
;; Update price-table
;; -------------------------------------

(defn price-change
  "if a vendor sold all resources the price goes up,
  else it goes down"
  [leftover]
  (->> leftover (map (fn [x] (case x 0 1 -1))) (apply +)))

(defn add-with-min-val
  "add x and y and choose the max between the result and 1"
  [x y]
  (max 1 (+ x y)))

(defn calculate-new-price-table
  "after the trade every vendor has either sold everything
  she got from the particular resource or has some leftovers.
  For every vendor who have sold everything the price goes one up
  and for every vendor who has some leftovers the price goes one down."
  [price-table vendors]
  (->> vendors
       (map :vendor/resources)
       (map (fn [m] (select-keys m [:resource/food
                                   :resource/mana
                                   :resource/technology
                                   :resource/culture
                                   :resource/religion])))
       (apply merge-with
              (fn [x y] (if (coll? x) (cons y x) (list y x))))
       (reduce-kv (fn [m k v]
                    (assoc m k (price-change v))) {})
       (merge-with add-with-min-val price-table)))

(comment

  (def price-table #:resource{:food 20, :mana 20, :technology 20, :culture 20, :religion 20})
  (def vendor (gen-vendor))
  (def broker {})
  (def resource :resource/food)
  (def vendors [(gen-vendor) (gen-vendor) (gen-vendor)])
  (def market (market-volume vendors))
  (count-full-sets market)
  (def price (set-price price-table))

  (buy-from-vendors price-table resource (count-full-sets market) vendors)

  (buy-resource price-table :resource/food vendor)


  (->> vendors
       (buy-resources price-table)
       (sell-tokens price-table (count-full-sets market))
       :sell-result/vendors
       (calculate-new-price-table price-table))



  ,)
