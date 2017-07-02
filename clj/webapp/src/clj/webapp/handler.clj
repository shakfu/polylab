(ns webapp.handler
  (:require [compojure.core :refer [routes wrap-routes]]
            [webapp.layout :refer [error-page]]
            [webapp.routes.home :refer [home-routes]]
            [compojure.route :as route]
            [webapp.env :refer [defaults]]
            [mount.core :as mount]
            [webapp.middleware :as middleware]))

(mount/defstate init-app
                :start ((or (:init defaults) identity))
                :stop  ((or (:stop defaults) identity)))

(def app-routes
  (routes
    (-> #'home-routes
        (wrap-routes middleware/wrap-csrf)
        (wrap-routes middleware/wrap-formats))
    (route/not-found
      (:body
        (error-page {:status 404
                     :title "page not found"})))))


(defn app [] (middleware/wrap-base #'app-routes))
