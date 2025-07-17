(ns dev.rob-3.submarine-commander.lenses
  (:require
   [clojure.set :as set]
   [com.rpl.specter :refer [END LAST select-one setval transform]]))

(defn orders [gs team]
  (select-one [:teams team :orders] gs))

(defn red-orders [gs]
  (orders gs :team/red))

(defn reset-orders [gs team]
  (setval [:teams team :orders] {:captain nil :first-mate nil :engineer nil} gs))

(defn blue-torp-charge [gs]
  (select-one [:teams :team/blue :systems :torpedo] gs))

(defn health [gs team]
  (select-one [:teams team :health] gs))

(defn take-damage [gs team quantity]
  (transform [:teams team :health] #(- % quantity) gs))

(defn blue-mines [gs]
  (select-one [:teams :team/blue :mines] gs))

(defn blue-mine-charge [gs]
  (select-one [:teams :team/blue :systems :mine] gs))

(defn blue-location [gs]
  (select-one [:teams :team/blue :trail LAST] gs))

(defn trail-path [team] [:teams team :trail])

(defn trail [gs team]
  (select-one (trail-path team) gs))

(defn update-trail [gs team f & args]
  (transform (trail-path team) #(apply f % args) gs))

(defn move [gs team new-location]
  (setval [:teams team :trail END] [new-location] gs))

(defn location [gs team]
  (select-one [(trail-path team) LAST] gs))

(defn mines [gs team]
  (select-one [:teams team :mines] gs))

(defn systems [gs team]
  (select-one [:teams team :systems] gs))

(defn charge [gs team system]
  (select-one [:teams team :systems system] gs))

(defn charge-up [gs team system]
  (transform [:teams team :systems system] inc gs))

(defn breakdowns [gs team]
  (select-one [:teams team :breakdowns] gs))

(def all-reactors #{:reactor1 :reactor2 :reactor3 :reactor4 :reactor5 :reactor6})

(def valid-breakdowns
  {:west #{:red1 :yellow1 :green1 :green2 :reactor1 :reactor2}
   :north #{:yellow2 :yellow3 :red2 :green3 :red3 :reactor3}
   :south #{:green4 :yellow4 :red4 :red5 :reactor4 :yellow5}
   :east #{:green5 :yellow6 :red6 :reactor5 :green6 :reactor6}})

(defn valid-breakdown? [breakdown direction]
  (contains? (direction valid-breakdowns) breakdown))

(defn breakdowns-full? [gs team direction]
  (let [bds (select-one [:teams team :breakdowns] gs)]
    (or (set/subset? (direction valid-breakdowns) bds)
        (set/subset? all-reactors bds))))

(def breakdown->dir
  (into {} (for [[dir bds] valid-breakdowns
                 bd bds]
             [bd dir])))

(defn break-system [gs team breakdown]
  (let [gs (transform [:teams team :breakdowns] #(conj % breakdown) gs)
        direction (breakdown->dir breakdown)]
    (cond-> gs
      (breakdowns-full? gs team direction) (-> (take-damage team 1)
                                               (update-in [:teams team :breakdowns] empty)))))

(defn island-map [gs]
  (select-one [:map] gs))

(defn dir-of [[x1 y1] [x2 y2]]
  (cond
    (and (= x1 x2) (= y1 (dec y2))) :down
    (and (= x1 x2) (= y1 (inc y2))) :up
    (and (= x1 (dec x2)) (= y1 y2)) :right
    (and (= x1 (inc x2)) (= y1 y2)) :left
    :else nil))

(comment
  (dir-of [1 1] [1 2]))

(def dirs->box-drawing
  {[:left :left] \─
   [:left :down] \┌
   [:left :up] \└
   [:up :right] \┌
   [:up :up] \│
   [:up :left] \┐
   [:right :right] \─
   [:right :down] \┐
   [:right :up] \┘
   [:down :down] \│
   [:down :right] \└
   [:down :left] \┘
   [nil :left] \─
   [nil :right] \─
   [nil :up] \│
   [nil :down] \│
   [:left nil] \─
   [:right nil] \─
   [:up nil] \│
   [:down nil] \│})

(defn three-points->char [prev cur next]
  (dirs->box-drawing [(dir-of prev cur)
                      (dir-of cur next)]))

(defn board-of [gs team]
  (let [mines (mines gs team)
        t (trail gs team)
        location (location gs team)
        islands (select-one [:map :islands] gs)
        trailess-board
        (loop [x 1
               y 1
               board []]
          (if (<= y 15)
            (let [x' (if (= x 15) 1 (inc x))
                  y' (if (= x 15) (inc y) y)]
              (recur x' y' (update board
                                   (dec y)
                                   (fnil conj [])
                                   (cond
                                     (= location [x y]) :location
                                     (contains? mines [x y]) :mine
                                       ;;(contains? trail [x y]) :trail
                                     (contains? islands [x y]) :island
                                     :else :empty))))
            board))
        trail-chars (let [middle (->> (partition 3 1 t)
                                      (mapv #(apply three-points->char %)))
                          full-path (cons \X (conj middle \*))]
                      full-path)]
    (loop [board trailess-board
           [[t c] & trail] (map vector t trail-chars)]
      (if c
        (recur (assoc-in board (reverse (map dec t)) c) trail)
        board))))

(defn team-of [gs player-id]
  (select-one [:players player-id :color] gs))

(defn print-board [board]
  (doseq [row board]
    (doseq [cell row]
      (print (case cell
               :location \X
               :mine \M
               :trail \'
               :island \#
               :empty \·)))
    (println)))

(comment
  (require '[dev.rob-3.submarine-commander.game-engine :as game-engine])
  (def gs game-engine/state)
  (def team :team/blue)
  (def board (board-of gs team))
  (def board (assoc-in board [11 14] :trail))
  (def board (assoc-in board [12 14] :trail))
  (def board (assoc-in board [13 14] :trail)))
