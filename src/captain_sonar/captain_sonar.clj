(ns captain-sonar.captain-sonar
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [hiccup2.core :as h]
            [hiccup.page :as page]))

(defmacro html [& args]
  `(str (h/html ~@args)))

(defn file-rsp [f]
  {:status 200 :headers {} :body (slurp f)})

(defn index []
  (page/html5 (page/include-css "/index.css")
              [:script {:type "text/javascript"
                        :src "https://unpkg.com/htmx.org@1.9.12"
                        :integrity "sha384-ujb1lZYygJmzgSwoxRggbCHcjc0rB2XoQrxeTUQyRjrOnlCoYta87iKBWq3EsdM2"
                        :crossorigin "anonymous"}]
              (h/html [:div.container
                        [:h1.title "Captain Sonar"]
                        [:button.button {:hx-get "/create-room"
                                         :hx-swap "outerHTML"} "Create a room"]])))

(defn app [request]
  (case [(:request-method request) (:uri request)]
    [:get "/"] {:status 200 :headers {} :body (index)}
    [:get "/index.css"] (file-rsp "resources/index.css")
    [:post "/"] {:status 200 :headers {} :body "post"}
    [:get "/create-room"] {:status 200 :headers {} :body (html [:h1 "TODO"])}
    {:status 404 :headers {} :body (html [:h1 "404 Not Found"])}))

(defn -main
  "Start here!"
  [& _args]
  (jetty/run-jetty #(app %) {:port 3000 :join? false}))

(comment
  (-main)
  (html [:h1.hi "hi"] [:h1 "hi"])
  (page/html5 [:h1 "hi"]))
