(ns captain-sonar.actions
  (:require
   [captain-sonar.maps :as maps]
   [captain-sonar.systems :refer [mine-charged? red-broken?]]))

(defn make-move [move trail island-map]
  (let [[x y] (last trail)
        [x' y'] (case move
                  :north [x (dec y)]
                  :south [x (inc y)]
                  :east [(inc x) y]
                  :west [(dec x) y])]
    (cond
      (contains? island-map [x' y']) :illegal-island-move
      (not (and (>= 15 x' 1) (>= 15 y' 1))) :illegal-offmap-move
      (some #{[x' y']} trail) :illegal-trail-cross
      :else (conj trail [x' y']))))

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
  [{:keys [trail systems breakdowns] :as state} direction system-to-charge breakdown]
  ;; FIXME pass map as parameter
  (let [trail' (make-move direction trail maps/alpha)
        systems' (charge-system system-to-charge systems)
        breakdowns' (breakdown-system breakdown direction breakdowns)]
    (cond
      (= trail' :illegal-island-move) :illegal-island-move
      (= trail' :illegal-offmap-move) :illegal-offmap-move
      (= trail' :illegal-trail-cross) :illegal-trail-cross
      (= systems' :illegal-system-charge) :illegal-system-charge
      (= systems' :illegal-noncharge) :illegal-noncharge
      (= breakdowns' :illegal-breakdown-value) :illegal-breakdown-value
      (= breakdowns' :illegal-duplicate-selection) :illegal-duplicate-selection
      :else (-> state
                (assoc :trail trail')
                (assoc :systems systems')
                (assoc :breakdowns breakdowns')))))

(defn surface [{:keys [surfaced] :as state}]
  (if surfaced
    :illegal-redundant-surface
    (assoc state :surfaced true)))

(defn adjacent? [[x y] [x' y']]
  (<= (+ (abs (- x x')) (abs (- y y'))) 1))

;; Rulings:
;; * You _can_ lay a mine diagonally.
;; * You cannot lay a mine on your path or on top another mine.
;; * You cannot move through your own mine with or without silence.
(defn lay-mine [{:keys [mines trail breakdowns systems] :as state} mine-location]
  (let [charged? (mine-charged? systems)
        weapons-down? (red-broken? breakdowns)
        sub-location (last trail)
        in-trail? (some #{mine-location} trail)
        adj? (adjacent? sub-location mine-location)
        already-laid? (contains? mines mine-location)]
    (cond
      (not charged?) :illegal-mine-uncharged
      weapons-down? :illegal-weapons-are-broken
      already-laid? :illegal-mine-already-laid
      in-trail? :illegal-mine-in-trail
      (not adj?) :illegal-mine-not-adj
      :else (-> state
                (update :mines conj mine-location)
                (assoc-in [:systems :mine] 0)))))

(defn explosion-wrt
  "Explodes \"with regard to\" a submarine."
  [sub-state explosion-location]
  (let [target-location (last (:trail sub-state))
        direct-hit? (= explosion-location target-location)
        hit? (adjacent? explosion-location target-location)]
    (cond-> sub-state
      direct-hit? (update :health dec)
      hit? (update :health dec))))

(defn explosion-at
  "Explode at a location, damaging all subs and triggering any other state 
   changes due to the explosion."
  [game-state location]
  (reduce (fn [game-state team-color]
            (update game-state team-color explosion-wrt location))
          game-state
          [:red :blue]))

(defn detonate-mine [game-state team-detonating mine-location]
  {:pre [(#{:red :blue} team-detonating)]}
  (let [breakdowns (get-in game-state [team-detonating :breakdowns])
        weapons-down? (red-broken? breakdowns)
        mines (get-in game-state [team-detonating :mines])
        mine-exists? (contains? mines mine-location)]
    (cond
      weapons-down? :illegal-weapons-are-broken
      (not mine-exists?) :illegal-no-such-mine
      :else (-> game-state
                (update-in [team-detonating :mines] disj mine-location)
                (explosion-at mine-location)))))

(comment
  (require '[captain-sonar.game-engine :as game-engine])
  ;; FIXME these are terrible inline (comment) tests; we'll make these proper soon
  (def x {:trail [[2 5]]
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
          :surfaced false
          :mines #{}})
  (attempt-move x :north :torpedo :green1)
  (surface x)
  (lay-mine x [2 6])
  (explosion-wrt x [2 6])
  (explosion-at game-engine/state [1 2])
  (detonate-mine game-engine/state :red [1 2]))
