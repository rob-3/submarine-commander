(ns captain-sonar.captain-sonar
  (:gen-class)
  (:require
   [clojure.walk :refer [keywordize-keys]]
   [hiccup.page :as page]
   [hiccup2.core :as h]
   [ring.adapter.jetty :as jetty]
   [ring.util.codec :refer [form-decode]]))

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
              (h/html children)))

(ns-unmap *ns* 'index-page)
(defmulti index-page request-type)
(defmethod index-page :default [_request]
  (let [uuid (random-uuid)]
    (page-skeleton (index uuid))))

(defn room [uuid]
  (str (h/html [:div (str "Room " uuid)])))

(ns-unmap *ns* 'room-handler)
(defmulti room-handler request-type)
(defmethod room-handler :htmx [request]
  (def request request)
  (let [uuid (:id (query-params request))]
    (room uuid)))

(defmethod room-handler :default [request]
  (let [uuid (:id (query-params request))]
    (page-skeleton [:div.container
                    [:h1.title "Captain Sonar"]
                    (room uuid)])))

(defn app [request]
  (case [(:request-method request) (:uri request)]
    [:get "/"] {:status 200 :headers {} :body (index-page request)}
    [:get "/index.css"] (file-rsp "resources/index.css")
    [:post "/"] {:status 200 :headers {} :body "post"}
    [:get "/room"] {:status 200 :headers {} :body (str (room-handler request))}
    {:status 404 :headers {} :body (html [:h1 "404 Not Found"])}))

(defn -main
  "Start here!"
  [& _args]
  (jetty/run-jetty #(app %) {:port 3000 :join? false}))

(comment
  (-main)
  (html [:h1.hi "hi"] [:h1 "hi"])
  (page/html5 [:h1 "hi"]))
