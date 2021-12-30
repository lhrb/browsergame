(ns lhrb.schedule
  (:import [java.util.concurrent ScheduledExecutorService
            Executors ExecutorService
            TimeUnit ScheduledFuture]))

(set! *warn-on-reflection* true)

(defn scheduled-executor
  ^ScheduledExecutorService [threads]
  (Executors/newScheduledThreadPool threads))

(defn periodically
  ^ScheduledFuture
  [^ScheduledExecutorService e f inital-delay period]
  (.scheduleAtFixedRate e f inital-delay period TimeUnit/MILLISECONDS))

(defn shutdown-executor
  [^ExecutorService e]
  (.shutdown e))


(comment
 (def ten-sec (* 10 1000))

 (defn simple-log []
   (println (str "hallo at" " " (java.time.LocalDateTime/now))))


 (def ^ScheduledExecutorService executor (scheduled-executor 1))

 (periodically executor simple-log 0 ten-sec)

 (shutdown-executor executor)


 (def db (atom {:money 0
                :inc 10}))

 (defn update-money [m]
   (update m :money (fn [e] (+ e (:inc m)))))



 (defn next-turn-resources [db]
   (fn []
     (swap! db update-money)
     (println "current: " (:money @db))))

 (periodically executor (next-turn-resources db) 0 ten-sec)


)
