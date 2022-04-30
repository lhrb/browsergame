(ns lhrb.service
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.http.ring-middlewares :as middlewares]
            [ring.util.response :as ring-resp]
            [ring.middleware.session.cookie :as cookie]
            [buddy.auth :as auth]
            [buddy.hashers :as hashers]
            [hiccup2.core :as hiccup]
            [hiccup.page :refer [doctype]]
            [io.pedestal.http.sse :as sse]
            [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as chan]
            [lhrb.auth :refer [login-page login authentication-interceptor authorization-interceptor]]))

(def hash-alg {:alg :bcrypt+sha512})

(def db {:bla 1})

(def users
  "A sample user store."
  {:aaron {:display-name "Aaron Aardvark"
           :password     "secret"
           :role         :user}
   :gmw   {:display-name "Gerald M. Weinberg"
           :password     "rutabaga"
           :role         :admin}})

(defn home
  "Given a request, returns a user-specific response if authenticated,
  otherwise the response is anonymous."
  [request]
  (if (auth/authenticated? request)
    (ring-resp/response (str "Hello " (:display-name (get users (:identity request)))))
    {:headers {}
     :status 200
     :body "hallo neu laden"}))

(defn admin
  "Returns a 200 response for authorized users, otherwise throws a buddy-auth
  'unauthorized' exception."
  [request]
  (let [known-user (get users (keyword (:identity request)))]
    (if (= :admin (:role known-user))
      (ring-resp/response  "Only admins can see this!")
      (buddy.auth/throw-unauthorized))))

(defn hiccup-page [request]
  (ring-resp/response
   (str
    (hiccup/html
     {:mode :html}
     (doctype :html5)
     [:html
      [:head
       [:meta {:charset "UTF-8"}]
       [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
       [:meta {:http-equiv "Content-Security-Policy"
               :content "script-src 'self' unpkg.com cdn.tailwindcss.com;"}]
       [:script {:src "https://cdn.tailwindcss.com"}]
       [:script {:src "https://unpkg.com/htmx.org@1.7.0"}]]
      [:body
       [:h1 {:class "text-3xl"} "Hiccup"]
       [:hr]
       [:p "This page was rendered with Hiccup!"]
       [:button {:hx-post "/clicked" :hx-swap "outerHTML"} "click me"]
       [:br]
       [:p {:id "the-text"}   "Hello from the Hiccup demo. Do you need a glass of water?"]
       [:p {:id "the-date"}  "today"]
       #_[:div {:hx-sse "connect:/events"}
        [:div {:hx-sse "swap:counter"}]]]]))))

(defn btn [request]
  (ring-resp/response
   (str (hiccup/html [:h1 "toll"]))))

(defn db-interceptor [db]
  (interceptor/interceptor
   {:name ::db
    :enter (fn [ctx] (update ctx :request assoc :db db))}))

(defn stream-ready [event-chan context]
  (dotimes [i 100]
    (when-not (chan/closed? event-chan)
      (async/>!! event-chan {:name "counter" :data (str (hiccup/html [:h2 (str "resource: " i)]))})
      (Thread/sleep 5000)))
  (async/close! event-chan))

(def events-chan (async/chan))

(defn stream-ready2 [events-chan]
  (fn [event-chan context]
    (async/go-loop []
      (let [rdr (async/<!! (async/go (async/<! events-chan)))]
       (if (= :close rdr)
         (async/close! event-chan)
         (do (async/>!! event-chan rdr) (recur)))))))

(def common-interceptors [(body-params/body-params)
                          http/html-body
                          (middlewares/session {:store (cookie/cookie-store)})
                          authentication-interceptor
                          authorization-interceptor
                          (db-interceptor db)])

(def routes #{["/" :get (conj common-interceptors `home)]
              ["/admin" :get (conj common-interceptors `admin)]
              ["/site" :get (conj common-interceptors `hiccup-page)]
              ["/clicked" :post (conj common-interceptors `btn)]
              ["/events" :get (sse/start-event-stream stream-ready)]
              ["/events2" :get (sse/start-event-stream (stream-ready2 events-chan))]
              ["/login" :get (conj common-interceptors `login-page)]
              ["/login" :post (conj common-interceptors `login)]})

(def service {:env                     :prod
              ::http/routes            routes
              ::http/resource-path     "/public"
              ::http/type              :jetty
              ::http/port              8080
              ::http/container-options {:h2c? true
                                        :h2?  false
                                        :ssl? false}})


(comment

  (hashers/derive "secret" hash-alg)

  (let [c (async/chan)]
    (async/go (async/>! c "hello"))
    (println (async/<!! (async/go (async/<! c))))
    (async/close! c))


  (async/go (async/>! events-chan "hallo"))

  ,)
