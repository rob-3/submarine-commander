(ns captain-sonar.captain-sonar
  (:gen-class)
  (:require
   [clojure.walk :refer [keywordize-keys]]
   [hiccup.page :as page]
   [hiccup2.core :as h]
   [ring.adapter.jetty :as jetty]
   [ring.middleware.cookies :as middleware]
   [ring.util.codec :refer [form-decode]]
   [ring.websocket :as ws]))

(defmacro html [& args]
  `(str (h/html ~@args)))

(defn file-rsp [f]
  {:status 200 :headers {"vary" "hx-request"} :body (slurp f)})

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

;; FIXME we should probably maintain a user data map that allows user->room too
;; That way we can prevent people from joining multiple rooms by mistake.
(def rooms (atom {}))

(defn room-html [uuid admin?]
  [:div#ws {:hx-ext "ws" :ws-connect "/ws"}
   [:div (str "Room " uuid)]
   (when admin? [:div (str "You are the admin.")])
   [:div {:hx-trigger "click" :ws-send "" :id "howdy"} "hi"]
   [:div#messages]
   [:button {:hx-get "/delete" :hx-target "#ws" :hx-swap "outerHTML"} "Delete"]])

(defn join-room [room player]
  (swap! rooms update room (fnil #(update % :players conj player)
                                 {:admin player :players #{player}})))

(ns-unmap *ns* 'room-handler)
(defmulti room-handler request-type)
(defmethod room-handler :htmx [request]
  (let [room (:id (query-params request))
        player (:value (get (:cookies request) "id"))]
    (swap! rooms update room (fnil #(update % :players conj player)
                                   {:admin player :players #{player}}))
    (h/html (room-html room (= player (get-in @rooms [room :admin]))))))

(defmethod room-handler :default [request]
  (let [room (:id (query-params request))
        player (:value (get (:cookies request) "id"))]
    (join-room room player)
    (page-skeleton [:div.container
                    [:h1.title "Captain Sonar"]
                    (room-html room (= player (get-in @rooms [room :admin])))])))

(def sockets (atom {}))
(defn ws-handler [request]
  (if (ws/upgrade-request? request)
    {::ws/listener
     {:on-open (fn [socket]
                 (println (:remote-addr request) "connected")
                 (ws/send socket (html [:div#messages {:hx-swap-oob "beforeend"} [:div "Howdy!"]]))
                 (let [token (or (get-in request [:cookies "id" :value]) "")]
                   (swap! sockets assoc token socket))
                 (let [keep-alive (fn []
                                    (while (ws/open? socket)
                                      (ws/ping socket)
                                      (Thread/sleep 1000)))]
                   (future (keep-alive))))
      :on-pong (fn [_socket _buffer] (println (:remote-addr request) "pong"))
      :on-message (fn [_socket message] (println message))
      :on-close (fn [_socket _code _reason] (println (:remote-addr request) "disconnected"))}}
    {:status 400 :headers {"vary" "hx-request"} :body "Websocket upgrade requests only!"}))

(defn app [request]
  (case [(:request-method request) (:uri request)]
    [:get "/"] {:status 200 :headers {"vary" "hx-request"} :body (index-page request)}
    [:get "/index.css"] (file-rsp "resources/index.css")
    [:post "/"] {:status 200 :headers {"vary" "hx-request"} :body "post"}
    [:get "/room"] (merge {:status 200 :headers {"vary" "hx-request"} :body (str (room-handler request))}
                          (when-not (get-in request [:cookies "id"])
                            {:cookies {"id"
                                       {:value (str (random-uuid))
                                        :secure true
                                        :http-only true
                                        :same-site :strict
                                        :max-age 86400
                                        :path "/"}}}))
    [:get "/delete"] {:status 200 :headers {"vary" "hx-request"} :body ""}
    [:get "/ws"] (ws-handler request)
    {:status 404 :headers {"vary" "hx-request"} :body (html [:h1 "404 Not Found"])}))

(defn -main
  "Start here!"
  [& _args]
  (jetty/run-jetty (middleware/wrap-cookies #(app %)) {:port 3000 :join? false}))

(comment
  (-main)
  (html [:h1.hi "hi"] [:h1 "hi"])
  (page/html5 [:h1 "hi"]))
