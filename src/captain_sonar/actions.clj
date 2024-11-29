(ns captain-sonar.actions
  (:require
   [captain-sonar.maps :as maps]
   [captain-sonar.systems :refer [broken?]]))

(def teams [:team/red :team/blue])
(defn team? [t] (boolean (#{:team/red :team/blue} t)))

(defn location? [l] (boolean (and (vector? l) (= 2 (count l)))))

(defn make-move [trail direction island-map distance]
  {:pre [(#{:north :south :east :west} direction)]}
  (loop [trail trail
         distance distance]
    (let [[x y] (last trail)
          [x' y'] (case direction
                    :north [x (dec y)]
                    :south [x (inc y)]
                    :east [(inc x) y]
                    :west [(dec x) y])]
      (cond
        (contains? (:islands island-map) [x' y']) :illegal-island-move
        (not (and (>= 15 x' 1) (>= 15 y' 1))) :illegal-offmap-move
        (some #{[x' y']} trail) :illegal-trail-cross
        (= distance 0) trail
        :else (recur (conj trail [x' y']) (dec distance))))))

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

(def system->color
  {:torpedo :red
   :mine :red
   :drone :green
   :sonar :green
   :silence :yellow})

(def system? (set (keys max-system-charges)))

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
  (let [trail' (make-move trail direction maps/alpha 1)
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

(defn within-n? [[x y] [x' y'] n]
  (<= (+ (abs (- x x')) (abs (- y y'))) n))

(defn adjacent? [location1 location2]
  (within-n? location1 location2 1))

;; Rulings:
;; * You _can_ lay a mine diagonally.
;; * You cannot lay a mine on your path or on top another mine.
;; * You cannot move through your own mine with or without silence.
(defn lay-mine [game-state team-laying mine-location]
  (let [{:keys [mines trail]} (get-in game-state [:teams team-laying])
        sub-location (last trail)
        in-trail? (some #{mine-location} trail)
        adj? (adjacent? sub-location mine-location)
        already-laid? (contains? mines mine-location)]
    (cond
      already-laid? :illegal-mine-already-laid
      in-trail? :illegal-mine-in-trail
      (not adj?) :illegal-mine-not-adj
      :else (update-in game-state [:teams team-laying :mines] conj mine-location))))

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
  ;; FIXME this function should also trigger chain reactions
  ;; Ruling: this doesn't cause the mines to explode, just be removed + notify players
  ;; This should probably be a configurable option.
  (reduce (fn [game-state team-color]
            (update-in game-state [:teams team-color] explosion-wrt location))
          game-state
          teams))

(defn detonate-mine [game-state team-detonating mine-location]
  {:pre [(team? team-detonating)]}
  (let [breakdowns (get-in game-state [:teams team-detonating :breakdowns])
        weapons-down? (broken? breakdowns :red)
        mines (get-in game-state [:teams team-detonating :mines])
        mine-exists? (contains? mines mine-location)]
    (cond
      weapons-down? :illegal-weapons-are-broken
      (not mine-exists?) :illegal-no-such-mine
      :else (-> game-state
                (update-in [:teams team-detonating :mines] disj mine-location)
                (explosion-at mine-location)))))

;; Rulings:
;; * Torpedos cannot move through islands, see official designer ruling at
;;   https://boardgamegeek.com/thread/1913121/rule-clarification-torpedomissilesmines
(defn fire-torpedo [game-state team-firing firing-to]
  {:pre [(team? team-firing)]}
  (let [firing-team-location (last (get-in game-state [:teams team-firing :trail]))
        ;; FIXME handle edge cases with firing around islands
        in-range? (within-n? firing-team-location team-firing 4)]
    (if (not in-range?)
      :illegal-out-of-range
      (explosion-at game-state firing-to))))

(defn use-sonar [game-state team-using team-targeted]
  {:pre [(team? team-using) (team? team-targeted)]}
  (update game-state :events conj {:type :sonar
                                   :from team-using
                                   :to team-targeted}))

(defn in-sector? [guessed-sector location]
  ;; FIXME we're assuming the 15x15 board here
  (let [sector-width 5
        sector-height 5
        sectors-per-row 3
        [x y] location
        sector (+ (inc (quot (dec x) sector-width))
                  (* sectors-per-row (quot (dec y) sector-height)))]
    (= sector guessed-sector)))

(defn use-drone [game-state team-using team-targeted guessed-sector]
  {:pre [(team? team-using) (team? team-targeted)]}
  (let [team-location (last (get-in game-state [:teams team-targeted :trail]))
        are-they-there? (in-sector? guessed-sector team-location)]
    (update game-state :events conj {:type :drone-inform
                                     :team team-using
                                     :answer are-they-there?})))

(defn use-silence [game-state team-moving direction island-map]
  {:pre [(team? team-moving)
         (#{:north :south :east :west} direction)]}
  (let [trail (get-in game-state [:teams team-moving :trail])
        trail' (make-move trail direction island-map 4)]
    (if (keyword? trail')
      trail'
      (assoc-in game-state [:teams team-moving :trail] trail'))))

(defn activate-system [game-state {:keys [system team-activating params]}]
  {:pre [(team? team-activating)
         (system? system)]}
  (let [{:keys [systems breakdowns]} (get-in game-state [:teams team-activating])
        charged? (< (system systems) (system max-system-charges))
        disabled? (broken? breakdowns (system->color system))]
    (cond
      (not charged?) :system-uncharged
      disabled? :system-down
      :else (as-> game-state game-state
              (assoc-in game-state [:teams team-activating :systems system] 0)
              (case system
                :torpedo (do
                           (assert (location? (:location params)))
                           (fire-torpedo game-state team-activating (:location params)))
                :mine (do
                        (assert (location? (:location params)))
                        (lay-mine game-state team-activating (:location params)))
                :drone (do
                         (assert (team? (:target-team params)))
                         (use-drone game-state team-activating (:target-team params) (:guessed-sector params)))
                :sonar (do
                         (assert (team? (:target-team params)))
                         (use-sonar game-state team-activating (:target-team params)))
                ;; FIXME don't hardcode the map
                ;; it should probably live in the game state somewhere
                :silence (do
                           (assert (#{:north :east :south :west} (:direction params)))
                           (use-silence game-state team-activating (:direction params) maps/alpha)))))))

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
  (lay-mine game-engine/state :team/red [1 2])
  (explosion-wrt x [2 6])
  (explosion-at game-engine/state [1 2])
  (detonate-mine game-engine/state :team/red [1 2])
  (use-drone game-engine/state :team/red :team/blue 9)
  (use-silence game-engine/state :team/red :north maps/alpha))
