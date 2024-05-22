(ns captain-sonar.captain-sonar
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [hiccup2.core :as h]))

(defmacro html [x]
  `(str (h/html ~x)))

(defn app [request]
  (case [(:request-method request) (:uri request)]
    [:get "/"] {:status 200 :headers {} :body "howdy"}
    [:post "/"] {:status 200 :headers {} :body "post"}
    [:get "/hello"] {:status 200 :headers {} :body "world"}
    {:status 404 :headers {} :body (html [:h1 "404 Not Found"])}))

(defn -main
  "Start here!"
  [& _args]
  (jetty/run-jetty #(app %) {:port 3000 :join? false}))

(comment
  (-main))
