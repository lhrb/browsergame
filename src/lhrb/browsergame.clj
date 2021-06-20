(ns lhrb.browsergame
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [clojure.core.match :refer [match]])
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

(defn bonus?
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

(s/def :unit/name string?)
(s/def :unit/health nat-int?)
(s/def :unit/damage nat-int?)
(s/def :unit/speed nat-int?)
(s/def :unit/accuracy percent?)
(s/def :unit/critical-chance percent?)
(s/def :unit/critical-multiplier percent-multiplier?)
(s/def :unit/evasion percent?)
(s/def :unit/survival percent?)
(s/def :unit/mitigation percent?)
(s/def :unit/class (into #{} unit-class))

(s/def :unit/unit
  (s/keys
   :req
   [:unit/name
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

(gen/generate (s/gen :unit/unit))

(defn roll-dice
  [success-prob]
  ;; TODO implement own version of rand: seed + (rand) -> [0,1]
  ;; currently (rand) -> [0,1)
  (if (<= (rand) success-prob)
    :success
    :fail))

(defn calc-turn
  [unit-a unit-b]
  #:turn{:from      (:unit/name unit-a)
         :to        (:unit/name unit-b)
         :strike    [:roll-dice (:unit/accuracy unit-a)]
         :damage    (:unit/damage unit-a)
         :bonus     (bonus?
                     (:unit/class unit-a)
                     (:unit/class unit-b))
         :critical  [:roll-dice (:unit/critical-chance unit-a)]
         :critical-multiplier (:unit/critical-multiplier unit-a)
         :mitigation (:unit/mitigation unit-b)
         :evasion [:roll-dice (:unit/evasion unit-b)]})

(defn apply-fx
  [mfx turn]
  (reduce-kv
    (fn [m k v]
      (assoc m k
       (match [v]
              [[(f :guard #(get mfx %)) n]] ((f mfx) n)
              :else v)))
    {}
    turn))

(comment
  (let [mfx {:roll-dice #'roll-dice}]
    (apply-fx
     mfx
     {:bonus [:roll-dice 1]
      :test 15}))
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
             (get-in x [:unit :speed]))))))
    {:time (speed->initative (:speed unit))
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
               :speed 2}
              {:id 2
               :speed 4}]]
   (take 15
    (apply
     (merge-by
      (fn [a b]
        (<
         (get (first a) :time)
         (get (first b) :time))))
     (map unit-timeline units))))
  ,)


(defn atk-dmg
  [{:keys [:unit/accuracy
           :unit/damage
           :unit/critical-chance
           :unit/critical-multiplier]}]
  (*
   (if (<= (rand) accuracy)
     1 0)
   damage
   (if (<= (rand) critical-chance)
     critical-multiplier 1)))

(comment

 (atk-dmg
  (gen/generate
   (s/gen :unit/unit)))

 (gen/generate (s/gen :unit/class))

 (def a {:unit/speed 2
         :unit/health 100})

 (def b {:unit/speed 4
         :unit/health 50})






 ,)
