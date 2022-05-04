(ns lhrb.system
  (:require [donut.system :as ds]
            [io.pedestal.http :as server]
            [io.pedestal.http.route :as route]
            [lhrb.service :as service]
            [lhrb.db :as db]))

(def system
  {::ds/defs
   {:app
    {:db {:start (fn [{:keys [db-path]} _ _]
                   (db/create-db db-path))
          :after-start (fn [_ conn _]
                         (db/create-account! conn {:name "admin" :password "1234"}))
          :stop (fn [_ conn _]
                  (db/close conn))
          :conf {:db-path "/tmp/datalevin/testdb"}}
     :server {:start (fn [_ _ _]
                       (-> service/service ;; start with production configuration
                           (merge {:env :dev
                                   ;; do not block thread that starts web server
                                   ::server/join? false
                                   ;; Routes can be a function that resolve routes,
                                   ;;  we can use this to set the routes to be reloadable
                                   ::server/routes #(route/expand-routes (deref #'service/routes))
                                   ;; all origins are allowed in dev mode
                                   ::server/allowed-origins {:creds true :allowed-origins (constantly true)}
                                   ;; Content Security Policy (CSP) is mostly turned off in dev mode
                                   ::server/secure-headers {:content-security-policy-settings {:object-src "'none'"}}})
                           ;; Wire up interceptor chains
                           server/default-interceptors
                           server/dev-interceptors
                           server/create-server
                           server/start))
              :stop (fn [_ instance _] (server/stop instance))}}}})


(comment
  (def running-system (ds/signal system :start))

  (ds/signal running-system :stop)

  (db/account?
   (get-in (::ds/instances running-system) [:app :db])
   {:name "admin" :password "1234"})




  ,)
