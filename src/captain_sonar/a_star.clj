(ns captain-sonar.a-star
  (:require [clojure.math :as math]
            [clojure.data.priority-map :refer [priority-map-keyfn]]
            [captain-sonar.maps :as maps]))

(defn euclidian-distance [v1 v2]
  (math/sqrt (reduce + (map #(math/pow (- %1 %2) 2) v1 v2)))) 

(comment
  (euclidian-distance [0 0] [3 4]))

;; An A* implementation for torpedo pathfinding
;; neighbors-fn :: node -> {:node node :cost +int}[]
;; heuristic-fn :: node, node -> float
;; start :: node
;; finish :: node
;; assumes no negative weights or heuristic-fn return values
(defn a* [& {:keys [heuristic-fn start finish]
             neighbors-of :neighbors-fn}]
  (if (= start finish) 0
      (loop [work-queue (priority-map-keyfn :heuristic
                                            start {:heuristic ##-Inf :cost 0})]
        (let [current-data (first work-queue)
              [node {:keys [cost from heuristic]}] current-data]
          (cond
            (= node finish) {:cost cost
                             :path (loop [from from
                                          path (list finish)]
                                     (if from
                                       (recur (get-in work-queue [from :from]) (conj path from))
                                       path))}
            (= ##Inf heuristic) :no-path
            :else (let [neighbors (neighbors-of node)
                        neighbors-data (for [{neighbor :node neighbor-cost :cost} neighbors
                                             :let [total-cost (+ cost neighbor-cost)
                                                   current-cost (:cost (get work-queue neighbor))]
                                             :when (< total-cost (or current-cost ##Inf))]
                                         [neighbor
                                          {:heuristic (+ total-cost (heuristic-fn finish neighbor))
                                           :cost total-cost
                                           :from node}])
                        work-queue (-> work-queue
                                       (into neighbors-data)
                                       (update node assoc :heuristic ##Inf))]
                    (recur work-queue)))))))

(comment
  ;; basic test in empty grid
  (a* :neighbors-fn (fn [[x y]] [{:node [(dec x) y] :cost 1}
                                 {:node [x (dec y)] :cost 1}
                                 {:node [(inc x) y] :cost 1}
                                 {:node [x (inc y)] :cost 1}])
      :heuristic-fn euclidian-distance
      :start [0 0]
      :finish [10 10])
  ;; closest to a real usage
  (a* :neighbors-fn (fn [[x y]]
                      (->> [[(dec x) y]
                            [x (dec y)]
                            [(inc x) y]
                            [x (inc y)]]
                           (remove maps/alpha)
                           (filter (fn [[x y]] (and (<= 1 x 15) (<= 1 y 15))))
                           (mapv (fn [[x y]] {:node [x y] :cost 1}))))
      :heuristic-fn euclidian-distance
      :start [1 1]
      :finish [2 4])
  ;; no neighbors should give :no-path
  (a* :neighbors-fn (fn [_] [])
      :heuristic-fn euclidian-distance
      :start [1 1]
      :finish [1 2]))
