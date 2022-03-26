(ns lhrb.player)

(defn distribution
  "generate a distribution of n points to the amount of given bins
  every bin has at least one point in it
  Note: use spec instead?"
  [n bins]
  (loop [bins-left bins
         distribution '()]
    (if (= 1 bins-left)
      (conj distribution (- n (apply + distribution)))
      (recur (dec bins-left)
             (conj distribution
                   (+ 1
                      (rand-int (- n (apply + distribution) bins-left))))))))


(zipmap [:resource/a :resource/b :resource/c :resource/d :resource/e]
        (distribution 20 5))

(comment
  (= 20 (apply + (distribution 20 5)))
  ,)
