(ns dev.rob-3.submarine-commander.lenses)

(defn blue-trail [gs]
  (get-in gs [:teams :team/blue :trail]))

(defn red-orders [gs]
  (get-in gs [:teams :team/red :orders]))

(defn blue-torp-charge [gs]
  (get-in gs [:teams :team/blue :systems :torpedo]))

(defn health [gs team]
  (get-in gs [:teams team :health]))

(defn blue-mines [gs]
  (get-in gs [:teams :team/blue :mines]))

(defn blue-mine-charge [gs]
  (get-in gs [:teams :team/blue :systems :mine]))

(defn blue-location [gs]
  (last (blue-trail gs)))

(defn trail [gs team]
  (get-in gs [:teams team :trail]))

(defn location [gs team]
  (last (trail gs team)))

(defn mines [gs team]
  (get-in gs [:teams team :mines]))

(defn systems [gs team]
  (get-in gs [:teams team :systems]))

(defn charge [gs team system]
  (get-in gs [:teams team :systems system]))

(defn breakdowns [gs team]
  (get-in gs [:teams team :breakdowns]))

(defn board-of [gs team]
  (let [mines (mines gs team)
        trail (trail gs team)
        location (location gs team)
        islands (get-in gs [:map :islands])]
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
