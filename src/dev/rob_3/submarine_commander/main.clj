(ns dev.rob-3.submarine-commander.main
  (:gen-class)
  (:require
   [cheshire.core :as json]
   [clojure.core.async :refer [<!! >!!] :as a]
   [clojure.string :as string]
   [clojure.walk :refer [keywordize-keys]]
   [com.rpl.specter :refer [transform]]
   [dev.rob-3.submarine-commander.actions :refer [teams tick]]
   [dev.rob-3.submarine-commander.game-engine :refer [create-game]]
   [dev.rob-3.submarine-commander.lenses :refer [board-of team-of]]
   [dev.rob-3.submarine-commander.maps :as maps]
   [hiccup.page :as page]
   [hiccup2.core :as h]
   [ring.adapter.jetty :as jetty]
   [ring.middleware.cookies :refer [wrap-cookies]]
   [ring.middleware.params :refer [wrap-params]]
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
              [:head [:meta {:charset "UTF-8"}]]
              [:script {:type "text/javascript"
                        :src "/htmx.js"}]
              [:script {:src "/ws.js"}]
              (h/html [:div#ws.container {:hx-ext "ws" :ws-connect "/ws"}
                       children])))

(defmulti index-page request-type)
(defmethod index-page :default [_request]
  (let [uuid (random-uuid)]
    (page-skeleton (index uuid))))

;; FIXME we should probably maintain a user data map that allows user->room too
;; That way we can prevent people from joining multiple rooms by mistake.
(defonce state (atom {:rooms {} :users {}}))

(defn board-html [board]
  [:div {:style {:display "grid"
                 :grid-template-columns "repeat(15, 20px)"
                 :text-align "center"}}
   (map #(vector :span {:style {:height "20px" :width "20px"}}
                 (case %
                   :location \X
                   :mine \M
                   :trail \'
                   :island \#
                   :empty \·)) (flatten board))])

(ns-unmap *ns* 'player-html)
(defmulti player-html (fn [{:keys [game player-id]}]
                        (get-in game [:players player-id :role])))
(defmethod player-html :captain [{:keys [room-id game player-id]}] 
  [:div.container
   ;; FIXME (board-of game player-id) should exist
   (board-html (board-of game (team-of game player-id)))
   [:div {:style {:display "grid"
                  :grid-template-columns "50px 50px 50px"
                  :grid-template-rows "auto"
                  :grid-template-areas (str "\".   north    .\""
                                            "\"west  .  east \""
                                            "\".   south    .\"")}}
     ;; FIXME componentize
    [:button {:style {:grid-area "north"}
              :ws-send ""
              :hx-vals (json/encode {"event" "order/captain"
                                     "direction" "north"
                                     "room" room-id})} "North"]
    [:button {:style {:grid-area "east"}
              :ws-send ""
              :hx-vals (json/encode {"event" "order/captain"
                                     "direction" "east"
                                     "room" room-id})} "East"]
    [:button {:style {:grid-area "south"}
              :ws-send ""
              :hx-vals (json/encode {"event" "order/captain"
                                     "direction" "south"
                                     "room" room-id})} "South"]
    [:button {:style {:grid-area "west"}
              :ws-send ""
              :hx-vals (json/generate-string {"event" "order/captain"
                                              "direction" "west"
                                              "room" room-id})} "West"]]])
(defmethod player-html :radio-operator [_] [:div "radio-operator"])
(defmethod player-html :first-mate [{room-id :room-id}] [:div "first-mate"]
  [:div
   [:button {:ws-send ""
             :hx-vals (json/encode {"event" "order/first-mate"
                                             "system" "mine"
                                             "room" room-id})} "Mine"]
   [:button {:ws-send ""
             :hx-vals (json/encode {"event" "order/first-mate"
                                             "system" "torpedo"
                                             "room" room-id})} "Torpedo"]
   [:button {:ws-send ""
             :hx-vals (json/encode {"event" "order/first-mate"
                                             "system" "drone"
                                             "room" room-id})} "Drone"]
   [:button {:ws-send ""
             :hx-vals (json/encode {"event" "order/first-mate"
                                             "system" "sonar"
                                             "room" room-id})} "Sonar"]
   [:button {:ws-send ""
             :hx-vals (json/encode {"event" "order/first-mate"
                                    "system" "silence"
                                    "room" room-id})} "Silence"]])
(defmethod player-html :engineer [{room-id :room-id}]
  [:div
   [:div
     "West"
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "red1"
                                               "room" room-id})} "red1"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "yellow1"
                                               "room" room-id})} "yellow1"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "green1"
                                               "room" room-id})} "green1"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "green2"
                                               "room" room-id})} "green2"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "reactor1"
                                               "room" room-id})} "reactor1"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "reactor2"
                                               "room" room-id})} "reactor2"]]
   [:div
     "North"
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "yellow2"
                                               "room" room-id})} "yellow2"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "yellow3"
                                               "room" room-id})} "yellow3"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "red2"
                                               "room" room-id})} "red2"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "green3"
                                               "room" room-id})} "green3"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "red3"
                                               "room" room-id})} "red3"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "reactor3"
                                               "room" room-id})} "reactor3"]]
   [:div
     "South"
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "green4"
                                               "room" room-id})} "green4"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "yellow4"
                                               "room" room-id})} "yellow4"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "red4"
                                               "room" room-id})} "red4"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "red5"
                                               "room" room-id})} "red5"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "reactor4"
                                               "room" room-id})} "reactor4"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "yellow5"
                                               "room" room-id})} "yellow5"]]
   [:div
     "East"
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "green5"
                                               "room" room-id})} "green5"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "yellow6"
                                               "room" room-id})} "yellow6"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "red6"
                                               "room" room-id})} "red6"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "reactor5"
                                               "room" room-id})} "reactor5"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "green6"
                                               "room" room-id})} "green6"]
     [:button {:ws-send ""
               :hx-vals (json/generate-string {"event" "order/engineer"
                                               "breakdown" "reactor6"
                                               "room" room-id})} "reactor6"]]])
(defmethod player-html :default [obj]
  (let [{:keys [game player-id]} obj
        roles (get-in game [:players player-id :role])
        sub-htmls (map #(player-html (assoc-in obj [:game :players player-id :role] %)) roles)]
    [:div sub-htmls]))

(defn game-html [player-id game room-id]
  [:div#app.container
   ;; FIXME should probably be a lens here
   (player-html {:player-id player-id :room-id room-id :game game})])

(defn room-html [room-id player-id {:keys [admin players game]}]
  {:pre [room-id player-id (not (nil? admin)) players]}
  (let [started? (boolean game)
        playing? (contains? players player-id)
        admin? (= player-id admin)]
    (if started? (game-html player-id game room-id)
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
            {:style "padding: 1em"
             :hx-vals (json/generate-string {"event" "start-game" "room" room-id})
             :ws-send ""}
            (for [team teams
                  :let [color (name team)
                        team-name (string/capitalize color)]]
              (list
               [:h1.title.two-col (str team-name " Team")]
               (for [[role role-id] {"Captain" "captain"
                                     "First Mate" "first-mate"
                                     "Radio Operator" "radio-operator"
                                     "Engineer" "engineer"}]
                 (let [id (str color "-" role-id "-select")
                       name (str color "-" role-id)]
                   (list
                    [:label {:for id} role]
                    [:select {:id id :name name}
                     (for [player players] [:option {:value player} player])])))))
            [:input.button.submit.two-col {:type "submit" :value "Start Game"}]])
         (when playing? [:button.button {:hx-push-url "true" :hx-post "/leave-room"} "Leave Room"])])))

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

(defn broadcast-err! [{:keys [rooms users]} room-id err]
  {:pre [rooms users room-id]}
  (let [room (get rooms room-id)]
    (assert room)
    (doseq [player-id (shuffle (:players room))
            :let [socket (get-in users [player-id :socket])]]
      (ws/send socket (html [:div#app.container (str err)])))))

(defn broadcast-update! [{:keys [rooms users]} room-id]
  {:pre [rooms users room-id]}
  (let [room (get rooms room-id)]
    (assert room)
    (doseq [player-id (shuffle (:players room))
            :let [socket (get-in users [player-id :socket])]]
      (ws/send socket (try (html (room-html room-id player-id room))
                           (catch Exception e
                             (html [:div#app.container (str e)])))))))

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

(defn start-game [room-id team->roles]
  (let [game (create-game
              :map maps/alpha
              :teams [{:color :team/blue
                        ;; FIXME this should not be hard-coded.
                        ;; We need an extra screen where each captain chooses
                        ;; their location
                       :start [1 1]
                       :roles (:team/blue team->roles)}
                      {:color :team/red
                       :start [1 1]
                       :roles (:team/red team->roles)}])
    ;; save user data for game and create object
        state' (swap! state assoc-in [:rooms room-id :game] game)]
    ;; set up game map in global state
    ;; ask captain for starting location
    (broadcast-update! state' room-id)))

(defn get-team [player-id]
  (let [state @state
        room-id (get-in state [:users player-id :room])
        [team _] (get-in state [:rooms room-id :game :players player-id])]
    team))

(defn on-message [_socket message player-id]
  (prn message)
  (let [{event "event"
         room-id "room"
         direction "direction"
         system "system"
         breakdown "breakdown"} (json/parse-string message)]
    (try (case event
          "join-room" (linearize! (fn [] (let [state' (swap! state #(join-room % room-id player-id))]
                                           (broadcast-update! state' room-id))))
          "start-game" (let [teams
                             ;; TODO we should eventually do some common sense checks to,
                             ;; for example, avoid a player who is on both teams.
                             (->> message
                                  json/parse-string
                                  (reduce (fn [acc [k v]]
                                            (let [[color role] (string/split k #"-" 2)]
                                              (if (and role (re-find #"^captain|first-mate|radio-operator|engineer$" role))
                                                (assoc-in acc [(keyword "team" color) (keyword role)] v)
                                                acc)))
                                          {}))]
                         (start-game room-id teams))
          "order/captain" (let [state' (swap! state
                                              (fn [s]
                                                (transform
                                                  [:rooms room-id :game]
                                                  (fn [s]
                                                    (tick s
                                                          :action :order/captain
                                                          ;; FIXME
                                                          :direction (keyword direction)
                                                          :team (team-of s player-id)))
                                                  s)))]
                             (broadcast-update! state' room-id))
          "order/first-mate" (linearize!
                              (fn []
                                (let [state' (swap! state
                                                    (fn [s]
                                                      (transform
                                                        [:rooms room-id :game]
                                                        (fn [s]
                                                         (tick s
                                                               :action :order/first-mate
                                                               ;; FIXME
                                                               :system (keyword system)
                                                               :team (team-of s player-id)))
                                                        s)))]
                                  (broadcast-update! state' room-id))))
          "order/engineer" (linearize!
                            (fn []
                              (let [state' (swap! state
                                                  (fn [s]
                                                    (transform
                                                      [:rooms room-id :game]
                                                      (fn [s]
                                                        (tick s
                                                              :action :order/engineer
                                                              ;; FIXME
                                                              :breakdown (keyword breakdown)
                                                              :team (team-of s player-id)))
                                                      s)))]
                                (broadcast-update! state' room-id)))))
         (catch Error e
           (let [{room-id "room"} (json/decode message)] 
             (broadcast-err! @state room-id e))))))

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

(defn start-game-handler [request]
  {:status 200 :headers {} :body "hi"})

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
    [:post "/start-game"] (#(start-game-handler request))
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
  (jetty/run-jetty (-> #(app %)
                       user-id-middleware
                       wrap-cookies
                       wrap-params) {:port 3000 :join? false}))

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
