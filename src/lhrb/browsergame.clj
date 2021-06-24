(ns lhrb.browsergame
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [clojure.core.match :refer [match]]
   [com.rpl.specter :as sp])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  "hi")

(def percent?
  (->> (range 0 101)
       (map (fn [x] (double (/ x 100))))
       (into #{})))

(def percent-multiplier?
  (->> (range 100 201)
       (map (fn [x] (double (/ x 100))))
       (into #{})))

(def unit-class
  [:rock :paper :scissors :lizard :spock])

(defn rock-paper-scissor
  "rock-paper-scissor-lizard-spock

  wikipedia: modeled by a comparison of the parity
  of the two choices. If it is the same (two
  odd-numbered moves or two even-numbered ones)
  then the lower number wins, while if they are
  different (one odd and one even) the higher wins."
  [class-a class-b]
  (if (= class-a class-b)
    :none
    (let [a# (.indexOf unit-class class-a)
          b# (.indexOf unit-class class-b)]
      (if (or
           (and (even? a#) (even? b#))
           (and (odd? a#) (odd? b#)))
        (if (< a# b#) class-a class-b)
        (if (< a# b#) class-b class-a)))))

(s/def :unit/id (s/and string? #(>= (.length %) 8)))
(s/def :unit/name string?)
(s/def :unit/health (s/int-in 100 1001))
(s/def :unit/damage (s/int-in 20 200))
(s/def :unit/speed pos-int?)
(s/def :unit/accuracy (s/and percent? #(<= 0.8 %)))
(s/def :unit/critical-chance percent?)
(s/def :unit/critical-multiplier percent-multiplier?)
(s/def :unit/evasion (s/and percent? #(>= 0.2 %)))
(s/def :unit/survival percent?)
(s/def :unit/mitigation percent?)
(s/def :unit/class (into #{} unit-class))
(comment
 ;; in combat specs
  (s/def :unit/faction #{:attacker :defender})
  ,)

(s/def :unit/unit
  (s/keys
   :req
   [:unit/id
    :unit/name
    :unit/health
    :unit/damage
    :unit/speed
    :unit/accuracy
    :unit/critical-chance
    :unit/critical-multiplier
    :unit/evasion
    :unit/survival
    :unit/mitigation
    :unit/class]))

(defn roll-dice
  [success-prob]
  ;; TODO implement own version of rand: seed + (rand) -> [0,1]
  ;; currently (rand) -> [0,1)
  (if (<= (rand) success-prob)
    :success
    :fail))

(defn bonus [class-a class-b]
  (if (= (rock-paper-scissor class-a class-b) class-a)
    :success
    :fail))

(def mfx
  {:roll-dice   #'roll-dice
   :bonus       #'bonus})

(defn turn-data
  [unit-a unit-b]
  #:turn{:from      (:unit/id unit-a)
         :to        (:unit/id unit-b)
         :strike    [:roll-dice (:unit/accuracy unit-a)]
         :damage    (:unit/damage unit-a)
         :damage-bonus [:bonus
                        (:unit/class unit-a)
                        (:unit/class unit-b)]
         :critical  [:roll-dice (:unit/critical-chance unit-a)]
         :critical-multiplier (:unit/critical-multiplier unit-a)
         :mitigation (:unit/mitigation unit-b)
         :mitigation-bonus [:bonus
                            (:unit/class unit-b)
                            (:unit/class unit-a)]
         :evasion [:roll-dice (:unit/evasion unit-b)]})

(defn apply-fx
  [mfx turn]
  (reduce-kv
    (fn [m k v]
      (assoc m k
       (match [v]
              [[(f :guard #(get mfx %)) & n]] (apply (f mfx) n)
              :else v)))
    {}
    turn))

(defn calc-dmg
  [{:keys [:turn/strike
           :turn/damage
           :turn/damage-bonus
           :turn/critical
           :turn/critical-multiplier
           :turn/evasion
           :turn/mitigation
           :turn/mitigation-bonus]}]
  (if (or
       (= strike :fail)
       (= evasion :success))
    0
    (* damage
       (if (= damage-bonus :success) 1.5 1)
       (if (= critical :success) critical-multiplier 1)
       (if (= mitigation-bonus :success) 0.5 1)
       (- 1 mitigation))))

(comment
  (apply-fx
     mfx
     {:bonus [:roll-dice 1]
      :test2 [:bonus :lizard :rock]
      :test 15})

  (calc-dmg
   #:turn{:evasion :fail,
          :mitigation 0.69,
          :damage 63,
          :strike :success,
          :from "3By45VUNCNlG3gge9E",
          :damage-bonus :fail,
          :mitigation-bonus :fail,
          :critical :fail,
          :to "k",
          :critical-multiplier 1.5})

  (let [u1 (gen/generate (s/gen :unit/unit))
        u2 (gen/generate (s/gen :unit/unit))]
    (->>
     (turn-data u1 u2)
     (apply-fx mfx)
     (calc-dmg)))
  ,)

;; ------------------------------------------------------------;;
;; combat turns
;; calculates the order of unit actions
;; ------------------------------------------------------------;;

(defn speed->initative
  [speed]
  (/ 1 speed))

(defn unit-timeline
  "creates a lazy sequence based on the unit speed
  e.g a timeline of this unit"
  [unit]
   (iterate
    (fn [x]
      (update x :time
       (fn [t]
         (+ t
            (speed->initative
             (get-in x [:unit :unit/speed]))))))
    {:time (speed->initative (:unit/speed unit))
     :unit unit}))

(defn merge-by
  "yields a function which merges infinite lazy sequences
  based on the given comperator"
  [comp]
  (fn merge-timelines
    [& timelines]
    (let [idx (->> timelines
                   (reduce
                    (fn [acc elem]
                      (if (comp acc elem) acc elem)))
                   (.indexOf timelines))]
      (cons
       (first (.get timelines idx))
       (lazy-seq
        (apply merge-timelines
               (update (into [] timelines) idx rest)))))))

(defn get-timeline
  [units]
  (apply
   (merge-by
    (fn [a b]
      (<
       (get (first a) :time)
       (get (first b) :time))))
   (map unit-timeline units)))

(comment
  (take 10
   ((merge-by (fn [acc elem] (< (first acc) (first elem))))
    (iterate (partial + 0.25) 0.25)
    (iterate (partial + 0.5) 0.5)
    (iterate (partial + 0.3) 0.3)))

  (take 10
   (apply (merge-by (fn [acc elem] (< (first acc) (first elem))))
    (map
     (fn [x] (iterate (partial + x) x))
     '(0.25 0.5 0.3))))

  (let [units [{:id 1
               :unit/speed 2}
              {:id 2
               :unit/speed 4}]]
   (take 15
    (apply
     (merge-by
      (fn [a b]
        (<
         (get (first a) :time)
         (get (first b) :time))))
     (map unit-timeline units))))
  ,)

(defn prepare-for-combat
    "add a faction (:attacker or :defender) and
  current health prop based on the health prop"
    [faction unit]
    (-> unit
        (assoc :unit/faction faction)
        (assoc :unit/current-health (:unit/health unit))))

(defn init-combat [attacker defender]
    {:units (into
             []
             (concat
              (map
               (partial prepare-for-combat :attacker)
               attacker)
              (map
               (partial prepare-for-combat :defender)
               defender)))
     :log []
     :defeated #{}})

(defn faction?
  "binds given faction to a predicate, which checks if an entity is the same"
  [faction]
  (fn is-faction? [x]
    (= faction (:unit/faction x))))

(defn faction-defeated?
  "checks if all units of one faction have zero or less current-health"
  [units]
  (let [all-defeated
        (fn [faction units]
          (->> units
               (filter (faction? faction))
               (map :unit/current-health)
               (every? #(<= % 0))))]
    (or
     (all-defeated :defender units)
     (all-defeated :attacker units))))

(defn find-target
  "current strategy: find unit with lowest health.
  TODO implement more sophisticated strategy"
  [faction units]
  (->> units
       (filter (faction? faction))
       (remove
        (fn [x] (>= 0 (:unit/current-health x))))
       (apply min-key :unit/current-health)))

(defn opponent-faction
  [faction]
  (if (= :attacker faction)
    :defender
    :attacker))

(defn defeated? [unit]
  (>= 0 (:unit/current-health unit)))

(defn calc-turn
  [turn combat]
  (->
   (sp/transform
    [:units sp/ALL #(= (:unit/id %) (:turn/to turn))]
    #(update % :unit/current-health
             (fn [x] (- x (calc-dmg turn))))
    combat)
   (update :log conj turn)
   ((fn [cmb]
      (assoc cmb :defeated
             (->> (:units cmb)
                  (filter defeated?)
                  (map :unit/id)
                  (into #{})))))))

(defn simulate-combat
  [attacker defender]
  (loop [combat (init-combat attacker defender)
         timeline (get-timeline (:units combat))]
    (if (faction-defeated? (:units combat))
      combat
      (let [unit (:unit (first timeline))
            target (find-target
                    (opponent-faction
                     (:unit/faction unit))
                    (:units combat))
            turn  (->>
                   (turn-data unit target)
                   (apply-fx mfx))
            combat' (calc-turn turn combat)]
        (recur combat'
               (rest
                (remove
                 (fn [unit]
                   ((:defeated combat) (:unit/id unit)))
                 timeline)))))))

(comment
  (gen/generate (s/gen :unit/unit))
  (require 'sc.api)
  
  (simulate-combat
   (gen/sample (s/gen :unit/unit) 2)
   (gen/sample (s/gen :unit/unit) 1))

  ,)
