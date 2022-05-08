(ns lhrb.auth
  (:require [ring.util.response :as ring-resp]
            [hiccup2.core :as hiccup]
            [hiccup.page :refer [doctype]]
            [buddy.auth.backends :as auth.backends]
            [buddy.auth.middleware :as auth.middleware]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.interceptor.chain :as interceptor.chain]
            [io.pedestal.interceptor.error :refer [error-dispatch]]
            [lhrb.db :refer [account?]]))

(def auth-backend (auth.backends/session))

(def authentication-interceptor
  "Port of buddy-auth's wrap-authentication middleware."
  (interceptor/interceptor
   {:name ::authenticate
    :enter (fn [ctx]
             (update ctx :request auth.middleware/authentication-request auth-backend))}))

(def authorization-interceptor
  "Port of buddy-auth's wrap-authorization middleware."
  (error-dispatch [ctx ex]
                  [{:exception-type :clojure.lang.ExceptionInfo :stage :enter}]
                  (try
                    (assoc ctx
                           :response
                           (auth.middleware/authorization-error (:request ctx)
                                                                ex
                                                                auth-backend))
                    (catch Exception e
                      (assoc ctx ::interceptor.chain/error e)))

                  :else (assoc ctx ::interceptor.chain/error ex)))

(defn login-page
  [request]
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
       [:h1 (str "Session: " (:session request))]
       [:h1 {:class "text-3xl"} "Login"]
       [:form {:action "/login" :method "post"}
        [:label {:for "name"} "First name:"]
        [:br]
        [:input {:type "text", :id "name", :name "name", :value "John"}]
        [:br]
        [:label {:for "password"} "Password:"]
        [:br]
        [:input {:type "password", :id "password", :name "password", :value "Doe"}]
        [:br]
        [:br]
        [:input {:type "submit", :value "Submit"}]]
       [:p {:id "the-date"}  "today"]]]))))

(defn login
  [{:keys [db form-params]}]
  (if-let [account-id (account? db form-params)]
    {:status 303
     :headers {"Location" "/login"}
     :body "See Other"
     :session {:identity account-id}}
    (ring-resp/not-found "not found")))
