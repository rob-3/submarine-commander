(ns captain-sonar.captain-sonar
  (:gen-class)
  (:require
   [cheshire.core :as json]
   [clojure.walk :refer [keywordize-keys]]
   [hiccup.page :as page]
   [hiccup2.core :as h]
   [ring.adapter.jetty :as jetty]
   [ring.middleware.cookies :as middleware]
   [ring.util.codec :refer [form-decode]]
   [ring.websocket :as ws]
   [clojure.core.async :refer [<!! >!!] :as a]))

(defmacro html [& args]
  `(str (h/html ~@args)))

(defn file-rsp [f]
  {:status 200 :headers {"vary" "hx-request" "cache-control" "no-store"} :body (slurp f)})

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
              (h/html [:div#ws.container {:hx-ext "ws" :ws-connect "/ws"}
                       children])))

(ns-unmap *ns* 'index-page)
(defmulti index-page request-type)
(defmethod index-page :default [_request]
  (let [uuid (random-uuid)]
    (page-skeleton (index uuid))))

;; FIXME we should probably maintain a user data map that allows user->room too
;; That way we can prevent people from joining multiple rooms by mistake.
(defonce state (atom {:rooms {} :users {}}))

(defn room-html [room-id player-id {:keys [admin players]}]
  {:pre [room-id player-id (not (nil? admin)) players]}
  (let [in-room? (contains? players player-id)
        admin? (= player-id admin)]
    [:div#app.container
     (when admin? [:div "You are the admin. (" player-id ")"])
     (for [player players]
       (if (= player player-id)
         (when-not admin? [:div (str "You (" player ") are in the room.")])
         [:div (str "Player " player " is in the room.")]))
     (if in-room?
       [:button.button {:hx-push-url "true" :hx-post "/leave-room"} "Leave Room"]
       [:form {:ws-send "" :id "join-form"}
        [:input {:type "hidden" :name "room" :value room-id}]
        [:input {:type "text" :name "username" :required ""}]
        [:button {:type "submit" :name "event" :value "join-room"} "Join room"]])]))

(defn add-to-room [old room player] (update old room (fnil #(update % :players conj player)
                                                           {:admin player :players #{player}})))

(defn remove-from-all-rooms [rooms player]
  (reduce (fn [acc [room-id {:keys [admin players]}]]
            (let [players' (disj players player)
                  admin' (if (= player admin) (first players') admin)]
              (if admin' (assoc acc room-id {:admin admin' :players players'}) acc)))
          {}
          rooms))

;; FIXME these are terrible names
(defonce c (a/chan (a/sliding-buffer 100)))
(defn linearize! [f]
  (>!! c f))

(defn join-room [rooms room player]
  (-> rooms
      (remove-from-all-rooms player)
      (add-to-room room player)))

(defn leave-room [rooms player]
  (remove-from-all-rooms rooms player))

(defn broadcast-update! [{:keys [rooms users]} room-id]
  (let [room (get rooms room-id)]
    (doseq [player-id (shuffle (:players room))
            :let [socket (get-in users [player-id :socket])]]
      (ws/send socket (html (room-html room-id player-id room))))))

(ns-unmap *ns* 'room-handler)
(defmulti room-handler request-type)
(defmethod room-handler :htmx [request]
  (let [room-id (:id (query-params request))
        player-id (:value (get (:cookies request) "id"))
        rooms (:rooms @state)
        room (or (get rooms room-id) {:admin false :players #{}})]
    (h/html (room-html room-id player-id room))))

(defmethod room-handler :default [request]
  (let [room-id (:id (query-params request))
        player-id (:value (get (:cookies request) "id"))
        _ (assert (not (nil? player-id)) "player-id is nil!")
        rooms (:rooms @state)
        room (or (get rooms room-id) {:admin false :players #{}})]
    (page-skeleton [:div.container
                    [:h1.title "Captain Sonar"]
                    (room-html room-id player-id room)])))

(defn on-message [_socket message player-id]
  (let [{event "event" room-id "room"} (json/parse-string message)]
    (prn message)
    (case event
      "join-room" (linearize! #(let [state' (swap! state (fn [{:keys [rooms users]}]
                                                           (let [rooms' (join-room rooms room-id player-id)]
                                                             {:rooms rooms' :users users})))]
                                 (broadcast-update! state' room-id))))))

(defn ws-handler [request]
  (if (ws/upgrade-request? request)
    (let [user-id (or (get-in request [:cookies "id" :value]) "")]
      {::ws/listener
       {:on-open (fn [socket]
                   (println (:remote-addr request) "connected")
                   ;(ws/send socket (html [:div#messages {:hx-swap-oob "beforeend"} [:div "Howdy!"]]))
                   (let [current-time (System/currentTimeMillis)]
                     (swap! state assoc-in [:users user-id] {:socket socket :last-ping current-time}))
                   (let [keep-alive (fn []
                                      (while (ws/open? socket)
                                        (ws/ping socket)
                                        (Thread/sleep 1000)))]
                     (future (keep-alive))))
        :on-pong (fn [_socket _buffer]
                   (println (:remote-addr request) "pong")
                   (let [current-time (System/currentTimeMillis)]
                     (swap! state assoc-in [:users user-id :last-ping] current-time)))
        :on-message #(on-message %1 %2 user-id)}})
        ;:on-close (fn [_socket _code _reason]
        ;            (println (:remote-addr request) "disconnected")
        ;            (println _reason)
        ;            (let [player (get-in request [:cookies "id" :value])]
        ;              (future ((fn []
        ;                         (Thread/sleep 5000)
        ;                         (let [last-ping (get-in @users [user-id :last-ping])
        ;                               current-time (System/currentTimeMillis)]
        ;                           (if (> (- current-time 3000) last-ping)
        ;                             (do
        ;                               (leave-room! player)
        ;                               (println "left"))
        ;                             (println "didn't leave")))))))}})
    {:status 400 :headers {"vary" "hx-request" "cache-control" "no-store"} :body "Websocket upgrade requests only!"}))

(defn index-handler [request]
  {:status 200
   :headers {"vary" "hx-request" "cache-control" "no-store"}
   :body (index-page request)})

(defn leave-room-handler [request]
  (let [player-id (get-in request [:cookies "id" :value])]
    (linearize! #(let [state' (swap! state (fn [state] (leave-room state player-id)))]
                   ;; FIXME this should be more granular to the specific room
                   (doseq [room-id (keys (:rooms state'))]
                     (broadcast-update! state' room-id))))
    {:status 200
     :headers {"vary" "hx-request"
               "cache-control" "no-store"
               "hx-redirect" "/"}
     :body nil}))

(defn app [request]
  (case [(:request-method request) (:uri request)]
    [:get "/"] (index-handler request)
    [:get "/index.css"] (file-rsp "resources/index.css")
    [:post "/"] {:status 200 :headers {"vary" "hx-request" "cache-control" "no-store"} :body "post"}
    [:get "/room"] (let [id (get-in request [:cookies "id"])
                         cookie {:value (str (random-uuid))
                                 :secure true
                                 :http-only true
                                 :same-site :strict
                                 :max-age 86400
                                 :path "/"}
                         request' (update-in request [:cookies "id"] #(if (nil? %) cookie %))]
                     (merge {:status 200
                             :headers {"vary" "hx-request"
                                       "cache-control" "no-store"}
                             :body (str (room-handler request'))}
                            (when-not id {:cookies {"id" cookie}})))

    [:get "/ws"] (ws-handler request)
    [:post "/leave-room"] (leave-room-handler request)
    {:status 404 :headers {"vary" "hx-request" "cache-control" "no-store"} :body (html [:h1 "404 Not Found"])}))

(defn -main
  "Start here!"
  [& _args]
  (a/thread
    (loop []
      (let [f (<!! c)]
        (f)
        (recur))))
  (jetty/run-jetty (middleware/wrap-cookies #(app %)) {:port 3000 :join? false}))

(comment
  (-main)
  (html [:h1.hi "hi"] [:h1 "hi"])
  (page/html5 [:h1 "hi"]))
