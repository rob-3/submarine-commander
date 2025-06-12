(ns dev.rob-3.submarine-commander.lenses
  (:require
   [com.rpl.specter :refer [LAST END select-one setval transform]]))

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

(defn island-map [gs]
  (select-one [:map] gs))

(defn board-of [gs team]
  (let [mines (mines gs team)
        trail (trail gs team)
        location (location gs team)
        islands (select-one [:map :islands] gs)]
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
                                 (contains? trail [x y]) :trail
                                 (contains? islands [x y]) :island
                                 :else :empty))))
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
