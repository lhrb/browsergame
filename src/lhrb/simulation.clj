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

(s/def :vendor/inventory (s/keys :req [:resource/a :resource/b :resource/c :resource/d :resource/e]))
(s/def :vendor/money nat-int?)

(defn gen-vendor []
  (gen/generate (s/gen (s/keys :req [:vendor/inventory :vendor/money]))))

(defn max-revenue
  "max revenue a agent can get, when she sells everything"
  [prices vendor]
  (reduce-kv (fn [acc k v] (+ acc (* (k prices) v)))
             0
             (:vendor/inventory vendor)))

(defn add-to-volume
  "adds the agent inventory to the market"
  [market vendor]
  (reduce-kv (fn [m k v] (update m k (fnil + 0) v))
             market
             (:vendor/inventory vendor)))

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

;; ------------------------------------
;; Buy resources from the vendor
;; ------------------------------------

(defn- buy-resource
  [price-table resource vendor]
  (-> vendor
   (update-in [:vendor/inventory resource] (fnil dec 0))
   (update-in [:vendor/money] (fnil #(+ (get price-table resource) %) 0))))

(defn out-of-stock?
  "test if the given resource is out of stock"
  [resource vendor]
  (= 0 (get-in vendor [:vendor/inventory resource])))

(defn in-stock?
  "test if the given resource is in stock"
  [resource vendor]
  (< 0 (get-in vendor [:vendor/inventory resource])))

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
           [:resource/a :resource/b :resource/c :resource/d :resource/e])))

;; -------------------------------------
;; Sell the assembled parts
;; -------------------------------------

(defn- sell-building-block
  [price vendor]
  (-> vendor
   (update-in [:vendor/inventory :building-block] (fnil inc 0))
   (update-in [:vendor/money] (fnil #(- % price) 0))))

(defn enough-money-for-building-block?
  [price vendor]
  (<= price (:vendor/money vendor)))

(defn sell-blocks
  [price-table amount vendors]
 (let [vendors (shuffle vendors)
       price (set-price price-table)
       enough-money? (partial enough-money-for-building-block? price)]
   (loop [n amount
          v (filter enough-money? vendors)
          a (remove enough-money? vendors)]
     (if (or (= n 0) (empty? v))
       {:amount-left n :vendors (concat v a)}
       (let [[vendor & rst] v
             vendor' (sell-building-block price vendor)]
         (recur (dec n)
                (if (enough-money? vendor')
                  (conj (vec rst) vendor')
                  rst)
                (if (enough-money? vendor')
                  a
                  (conj a vendor'))))))))


(comment

  (def price-table #:resource{:a 20, :b 20, :c 20, :d 20, :e 20})
  (def vendor (gen-vendor))
  (def broker {})
  (def resource :resource/a)
  (def vendors [(gen-vendor) (gen-vendor) (gen-vendor)])
  (def market (market-volume vendors))
  (count-full-sets market)
  (def price (set-price price-table))

  (buy-from-vendors price-table resource (count-full-sets market) vendors)

  (<= 0 0)

  (let [[h & rst] [1 2 3 4 5 6]]
    (conj (vec rst) h))

  (conj [1 2 3 4 5 6 7 7 8 8 9 2 3 4 5 1 21 2 1] 31)

  (concat '(1 2 3) '(4 5))
  (buy-resource price-table :resource/a vendor)
  ,
  )
