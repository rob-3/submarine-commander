(ns captain-sonar.maps)

(def alpha
  {:dimensions [[1 1] [15 15]]
   :islands #{[2 2] [3 2] [7 2] [12 2] [2 3] [9 3] [12 3] [9 4] [5 6] [15 6]
              [3 7] [12 7] [2 8] [7 8] [8 8] [9 8] [12 8] [4 9] [13 9] [5 10]
              [2 11] [8 11] [11 11] [2 12] [4 12] [14 12] [4 13] [7 13] [4 14]
              [8 14] [14 14]}})

(defn- within-dimensions? [dimensions [x y]]
  (let [[[x-min y-min] [x-max y-max]] dimensions]
    (boolean (and (<= x-min x x-max)
                  (<= y-min y y-max)))))

(defn- valid-space? [map coord]
  (and (not (contains? (:islands map) coord))
       (within-dimensions? (:dimensions map) coord)))

(comment
  (assert (valid-space? alpha [1 1]))
  (assert (not (valid-space? alpha [2 2])))
  (assert (not (valid-space? alpha [16 1])))
  (assert (not (valid-space? alpha [1 16])))
  (assert (not (valid-space? alpha [0 5])))
  (assert (not (valid-space? alpha [-1 5])))
  (assert (not (valid-space? alpha [3 -5]))))

(defn adj-spaces [[x y]]
  [[(dec x) y]
   [x (dec y)]
   [(inc x) y]
   [x (inc y)]])

(defn neighbors [map coord]
  (vec (for [space (adj-spaces coord)
             :when (valid-space? map space)]
         space)))

(comment
  (neighbors alpha [2 3]))

(comment
  ;; prints the map to stdout
  (mapv (fn [y]
          (mapv (fn [x]
                  (if (contains? (:islands alpha) [x y])
                    (print \X)
                    (print \·)))
                (range 1 16))
          (println ""))
        (range 1 16)))
