(ns captain-sonar.actions)

;; TODO add islands
(def islands #{})

(defn make-move [move [x y]]
  (let [[x' y'] (case move
                  :north [x (dec y)]
                  :south [x (inc y)]
                  :east [(inc x) y]
                  :west [(dec x) y])]
    (if (contains? islands [x' y'])
      :illegal-island-move
      (if (and (>= 15 x' 1)
               (>= 15 y' 1))
        [x' y']
        :illegal-offmap-move))))

(def valid-breakdowns
  {:west #{:red :yellow :green1 :green2 :reactor1 :reactor2}
   :north #{:yellow1 :yellow2 :red1 :green1 :red2 :reactor}
   :south #{:green :yellow1 :red1 :red2 :reactor :yellow2}
   :east #{:green1 :yellow :red :reactor :green2 :reactor2}})

(defn breakdown-system [breakdown direction all-breakdowns]
  (let [bds (direction all-breakdowns)
        bds' (conj bds breakdown)
        valid-bds (direction valid-breakdowns)]
    (cond
      (not (contains? valid-bds breakdown)) :illegal-breakdown-value
      (= bds' bds) :illegal-duplicate-selection
      :else (assoc all-breakdowns direction bds'))))

(def max-system-charges
  {:torpedo 3
   :mine 3
   :drone 4
   :sonar 3
   :silence 6})

;; TODO What do the rules say about charging systems?
;; It's not clear if the first mate MUST charge a system with each move.
;; My ruling is that they MUST if an empty system exists (not including "scenario"
;; if it is not used), otherwise they should confirm charging none.
;; A move should never happen without any UI confirmation from the first mate.
(defn charge-system [system-to-charge current-charges]
  (let [current-charge (system-to-charge current-charges)
        max-charge (system-to-charge max-system-charges)
        fully-charged (= current-charges max-system-charges)]
    (cond
      (= system-to-charge :none) (if fully-charged current-charges :illegal-noncharge)
      (< current-charge max-charge) (update current-charges system-to-charge inc)
      :else :illegal-system-charge)))

(defn attempt-move
  [{:keys [location systems breakdowns] :as state} direction system-to-charge breakdown]
  (let [location' (make-move direction location)
        systems' (charge-system system-to-charge systems)
        breakdowns' (breakdown-system breakdown direction breakdowns)]
    (cond
      (= location' :illegal-island-move) :illegal-island-move
      (= location' :illegal-offmap-move) :illegal-offmap-move
      (= systems' :illegal-system-charge) :illegal-system-charge
      (= systems' :illegal-noncharge) :illegal-noncharge
      (= breakdowns' :illegal-breakdown-value) :illegal-breakdown-value
      (= breakdowns' :illegal-duplicate-selection) :illegal-duplicate-selection
      :else (-> state
                (assoc :location location')
                (assoc :systems systems')
                (assoc :breakdowns breakdowns')))))

(defn surface [{:keys [surfaced] :as state}]
  (if surfaced
    :illegal-redundant-surface
    (assoc state :surfaced true)))

(comment
  ;; FIXME these are terrible inline (comment) tests; we'll make these proper soon
  (def x {:location [1 2]
          :health 4
          :systems {:torpedo 0
                    :mine 0
                    :drone 0
                    :sonar 0
                    :silence 0}
          ;; the presence of a keyword means the box is marked
          :breakdowns {:west #{:red :yellow :green1 :green2 :reactor1 :reactor2}
                       :north #{:yellow1 :yellow2 :red1 :red2 :reactor}
                       :south #{:green :yellow1 :red1 :red2 :reactor :yellow2}
                       :east #{:green1 :yellow :red :reactor :green2 :reactor2}}
          :surfaced false})
  (attempt-move x :north :torpedo :green1)
  (surface x))
