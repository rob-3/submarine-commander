(ns captain-sonar.captain-sonar
  (:gen-class)
  (:require
   [clojure.walk :refer [keywordize-keys]]
   [hiccup.page :as page]
   [hiccup2.core :as h]
   [ring.adapter.jetty :as jetty]
   [ring.util.codec :refer [form-decode]]
   [ring.websocket :as ws]))

(defmacro html [& args]
  `(str (h/html ~@args)))

(defn file-rsp [f]
  {:status 200 :headers {} :body (slurp f)})

(defn query-params [request]
  (-> request :query-string form-decode keywordize-keys))

(defn index [uuid]
  (let [url (str "/room?id=" uuid)]
    [:div.container
     [:h1.title "Captain Sonar"]
     [:button.button {:hx-get url
                      :hx-push-url "true"
                      :hx-swap "outerHTML"} "Create a room"]]))

(defn request-type [r]
  (let [headers (:headers r)
        htmx? (get headers "hx-request")]
    (if (= htmx? "true")
      :htmx
      :default)))

(defn page-skeleton [children]
  (page/html5 (page/include-css "/index.css")
              [:script {:type "text/javascript"
                        :src "https://unpkg.com/htmx.org@1.9.12"
                        :integrity "sha384-ujb1lZYygJmzgSwoxRggbCHcjc0rB2XoQrxeTUQyRjrOnlCoYta87iKBWq3EsdM2"
                        :crossorigin "anonymous"}]
              [:script {:src "https://unpkg.com/htmx.org@1.9.12/dist/ext/ws.js"}]
              (h/html children)))

(ns-unmap *ns* 'index-page)
(defmulti index-page request-type)
(defmethod index-page :default [_request]
  (let [uuid (random-uuid)]
    (page-skeleton (index uuid))))

(defn room [uuid]
  [:div (str "Room " uuid)
   [:div#ws {:hx-ext "ws" :ws-connect "/ws"}
    [:div {:hx-trigger "click" :ws-send "" :id "howdy"} "hi"]]
   [:div#messages]
   [:button {:hx-get "/delete" :hx-target "#ws" :hx-swap "outerHTML"} "Delete"]])

(ns-unmap *ns* 'room-handler)
(defmulti room-handler request-type)
(defmethod room-handler :htmx [request]
  (let [uuid (:id (query-params request))]
    (h/html (room uuid))))

(defmethod room-handler :default [request]
  (let [uuid (:id (query-params request))]
    (page-skeleton [:div.container
                    [:h1.title "Captain Sonar"]
                    (room uuid)])))

(defn extract-token [auth-string]
  (second (re-find #"^Bearer (.*)" auth-string)))

(def sockets (atom {}))
(defn ws-handler [request]
  (if (ws/upgrade-request? request)
    {::ws/listener
     {:on-open (fn [socket]
                 (println "connected")
                 (ws/send socket (html [:div#messages {:hx-swap-oob "beforeend"} [:div "Howdy!"]]))
                 (let [auth-header (or (get-in request [:headers "Authorization"]) "")
                       token (extract-token auth-header)]
                   (swap! sockets assoc socket token))
                 (let [keep-alive (fn []
                                    (while (ws/open? socket)
                                      (ws/ping socket)
                                      (Thread/sleep 10000)))]
                   (future (keep-alive))))
      :on-pong (fn [_socket _buffer] (println "pong received"))
      :on-message (fn [_socket message] (println message))
      :on-close (fn [_socket _code _reason] (println "Goodbye!"))}}
    {:status 400 :headers {} :body "Websocket upgrade requests only!"}))

(defn app [request]
  (case [(:request-method request) (:uri request)]
    [:get "/"] {:status 200 :headers {} :body (index-page request)}
    [:get "/index.css"] (file-rsp "resources/index.css")
    [:post "/"] {:status 200 :headers {} :body "post"}
    [:get "/room"] {:status 200 :headers {} :body (str (room-handler request))}
    [:get "/delete"] {:status 200 :headers {} :body ""}
    [:get "/ws"] (ws-handler request)
    {:status 404 :headers {} :body (html [:h1 "404 Not Found"])}))

(defn -main
  "Start here!"
  [& _args]
  (jetty/run-jetty #(app %) {:port 3000 :join? false}))

(comment
  (-main)
  (html [:h1.hi "hi"] [:h1 "hi"])
  (page/html5 [:h1 "hi"]))
