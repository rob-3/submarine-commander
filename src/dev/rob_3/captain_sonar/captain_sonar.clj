(ns dev.rob-3.captain-sonar.captain-sonar
  (:gen-class)
  (:require
   [dev.rob-3.captain-sonar.actions :refer [teams]]
   [cheshire.core :as json]
   [clojure.core.async :refer [<!! >!!] :as a]
   [clojure.string :as string]
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
                        :src "/htmx.js"}]
              [:script {:src "/ws.js"}]
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
  (let [playing? (contains? players player-id)
        admin? (= player-id admin)]
    [:div#app.container (when (not playing?) {:ws-send ""
                                              :hx-vals (json/generate-string {"event" "join-room"
                                                                              "room" room-id})
                                              :hx-trigger "load delay:1ms"})
     (when admin? [:div "You are the admin. (" player-id ")"])
     (for [player players]
       (if (= player player-id)
         (when-not admin? [:div (str "You (" player ") are in the room.")])
         [:div (str "Player " player " is in the room.")]))
     (when admin?
       [:form.form
        {:style "padding: 1em"}
        (for [team teams
              :let [team-name (string/capitalize (name team))]]
          (list
           [:h1.title.two-col (str team-name " Team")]
           (for [[role role-id] {"Captain" "captain"
                                 "First Mate" "first-mate"
                                 "Radio Operator" "radio-operator"
                                 "Engineer" "engineer"}]
             (let [id (str team "-" role-id "-select")]
               (list
                [:label {:for id} role]
                [:select {:id id :name role-id}
                 (for [player players] [:option {:value player} player])])))))
        [:input.button.submit.two-col {:type "submit" :value "Start Game"}]])
     (when playing? [:button.button {:hx-push-url "true" :hx-post "/leave-room"} "Leave Room"])]))

(defn add-to-room [state room player]
  {:pre [state room player]}
  (-> state
      (update-in [:rooms room] (fnil #(update % :players conj player)
                                     (hash-map :admin player :players #{player})))
      (assoc-in [:users player :room] room)))

(defn remove-from-all-rooms [state player-id]
  (let [old-room (get-in state [:users player-id :room])]
    (if old-room
      (-> state
          (update-in [:users player-id] dissoc :room)
          (update :rooms (fn [rooms]
                           (let [{:keys [admin players]} (get rooms old-room)
                                 players' (disj players player-id)
                                 admin' (if (= admin player-id)
                                          (first (shuffle players'))
                                          admin)]
                             (if (nil? admin')
                               (dissoc rooms old-room)
                               (assoc rooms old-room {:admin admin' :players players'}))))))
      state)))

(comment
  (add-to-room {:rooms {"room1" {:admin "p" :players #{"p"}}} :users {}} "room1" "p2")
  (remove-from-all-rooms {:rooms {"room1" {:admin "p" :players #{"p"}}} :users {"p" {:room "room1"}}} "p"))

;; FIXME these are terrible names
(defonce c (a/chan (a/sliding-buffer 100)))
(defn linearize! [f]
  (assert (>!! c f) "put should succeed"))

(defn join-room [state room player]
  {:pre [state room player]
   :post [(= (get-in state [:rooms :rooms]) nil)]}
  (-> state
      (remove-from-all-rooms player)
      (add-to-room room player)))

(defn leave-room [rooms player]
  (remove-from-all-rooms rooms player))

(defn broadcast-update! [{:keys [rooms users]} room-id]
  {:pre [rooms users room-id]}
  (let [room (get rooms room-id)]
    (assert room)
    (doseq [player-id (shuffle (:players room))
            :let [socket (get-in users [player-id :socket])]]
      (ws/send socket (html (room-html room-id player-id room))))))

(ns-unmap *ns* 'room-handler)
(defmulti room-handler request-type)
(defmethod room-handler :htmx [request]
  (let [room-id (:id (query-params request))
        player-id (:value (get (:cookies request) "id"))
        _ (assert (not (nil? player-id)) "player-id is nil!")
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
      "join-room" (linearize! (fn [] (let [state' (swap! state #(join-room % room-id player-id))]
                                       (broadcast-update! state' room-id)))))))

(defn ws-handler [request]
  (if (ws/upgrade-request? request)
    (let [user-id (get-in request [:cookies "id" :value])]
      (assert user-id)
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
        :on-message #(on-message %1 %2 user-id)
        :on-close (fn [socket _code _reason]
                    (linearize!
                     #((let [state' (swap! state
                                           (fn [s]
                                             (let [reconnected? (= (get-in s [:users user-id :socket])
                                                                   socket)]
                                               (-> s
                                                   (remove-from-all-rooms user-id)
                                                   (cond-> reconnected? (update :users dissoc user-id))))))]
                          ;; FIXME this should be more granular to the specific room
                         (doseq [room-id (keys (:rooms state'))]
                           (broadcast-update! state' room-id))))))}})
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
    [:get "/htmx.js"] (file-rsp "resources/htmx.js")
    [:get "/ws.js"] (file-rsp "resources/ws.js")
    [:post "/"] {:status 200 :headers {"vary" "hx-request" "cache-control" "no-store"} :body "post"}
    [:get "/room"] {:status 200
                    :headers {"vary" "hx-request"
                              "cache-control" "no-store"}
                    :body (str (room-handler request))}
    [:get "/ws"] (ws-handler request)
    [:post "/leave-room"] (leave-room-handler request)
    {:status 404 :headers {"vary" "hx-request" "cache-control" "no-store"} :body (html [:h1 "404 Not Found"])}))

(defn make-id-cookie []
  {:value (str (random-uuid))
   :secure true
   :http-only true
   :same-site :strict
   :max-age 86400
   :path "/"})

(defn user-id-middleware [handler]
  (fn [request]
    (let [id (get-in request [:cookies "id" :value])]
      (if id
        (handler request)
        (let [cookie (make-id-cookie)
              request' (assoc-in request [:cookies "id"] cookie)
              response (handler request')]
          (assoc-in response [:cookies "id"] cookie))))))

(defn -main
  "Start here!"
  [& _args]
  (a/thread
    (loop []
      (let [f (<!! c)]
        (try (f)
             (catch Exception e (str "caught: " (.getMessage e))))
        (recur))))
  (jetty/run-jetty (middleware/wrap-cookies (user-id-middleware #(app %))) {:port 3000 :join? false}))

(comment
  (do
    (.stop server)
    (a/close! c)
    (def c (a/chan (a/sliding-buffer 100)))
    (reset! state {:rooms {} :users {}})
    (def server (-main))))

(comment
  (-main)
  (html [:h1.hi "hi"] [:h1 "hi"])
  (page/html5 [:h1 "hi"]))
