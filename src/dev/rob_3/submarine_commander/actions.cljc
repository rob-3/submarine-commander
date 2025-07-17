(ns dev.rob-3.submarine-commander.actions
  (:require
   [dev.rob-3.submarine-commander.a-star :refer [a* maze-distance] :as a-star]
   [dev.rob-3.submarine-commander.error :refer [err-> err?] :as err]
   [dev.rob-3.submarine-commander.lenses :refer [break-system breakdown->dir
                                                 breakdowns charge-up
                                                 island-map location move
                                                 orders reset-orders systems
                                                 trail valid-breakdown?]]
   [dev.rob-3.submarine-commander.maps :as maps]
   [dev.rob-3.submarine-commander.systems :refer [broken?]]))

(def teams [:team/red :team/blue])
(defn team? [t] (boolean (#{:team/red :team/blue} t)))

(defn location? [l] (boolean (and (vector? l) (= 2 (count l)))))

(defn make-move [gs team direction distance]
  {:pre [(#{:north :south :east :west} direction)]}
  (loop [gs gs
         distance distance]
    (let [[x y] (location gs team)
          [x' y'] (case direction
                    :north [x (dec y)]
                    :south [x (inc y)]
                    :east [(inc x) y]
                    :west [(dec x) y])
          island-map (island-map gs)
          trail (trail gs team)]
      (cond
        (= distance 0) gs
        (contains? (:islands island-map) [x' y']) (assoc gs :error ::err/illegal-island-move)
        (not (and (>= 15 x' 1) (>= 15 y' 1))) (assoc gs :error ::err/illegal-offmap-move)
        (some #{[x' y']} trail) (assoc gs :error ::err/illegal-trail-cross)
        :else (recur (move gs team [x' y']) (dec distance))))))

(defn breakdown-system [gs team breakdown direction]
  (let [valid-bd (valid-breakdown? breakdown direction)
        already-chosen (contains? (breakdowns gs team) breakdown)]
    (cond
      (not valid-bd) (assoc gs :error ::err/illegal-breakdown-value)
      already-chosen (assoc gs :error ::err/illegal-duplicate-selection)
      :else (break-system gs team breakdown))))

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

(defn fully-charged? [gs team system]
  (let [current-charge (system (systems gs team))
        max-charge (system max-system-charges)]
    (= current-charge max-charge)))

;; TODO What do the rules say about charging systems?
;; It's not clear if the first mate MUST charge a system with each move.
;; My ruling is that they MUST if an empty system exists (not including "scenario"
;; if it is not used), otherwise they should confirm charging none.
;; A move should never happen without any UI confirmation from the first mate.
(defn charge-system [gs team system-to-charge]
  (let [fully-charged (fully-charged? gs team system-to-charge)]
    (cond
      (and (= system-to-charge :none) fully-charged) gs
      (= system-to-charge :none) (assoc gs :error ::err/illegal-noncharge)
      (not fully-charged) (charge-up gs team system-to-charge)
      :else (assoc gs :error ::err/illegal-system-charge))))

(defn attempt-move [gs team]
  ;; FIXME pass map via game state
  (let [orders (orders gs team)
        direction (:captain orders)
        system-to-charge (:first-mate orders)
        breakdown (:engineer orders)]
    (if (or (nil? direction)
            (nil? system-to-charge)
            (nil? breakdown)
            (not= direction (breakdown->dir breakdown)))
      gs
      (err-> gs
             (make-move team direction 1)
             (charge-system team system-to-charge)
             (breakdown-system team breakdown direction)
             (reset-orders team)))))

(defn surface [{:keys [surfaced] :as state}]
  (if surfaced
    ::err/illegal-redundant-surface
    (assoc state :surfaced true)))

(defn adjacent? [location1 location2]
  (let [[x1 y1] location1
        possible #{[(inc x1) y1]
                   [x1 (inc y1)]
                   [(inc x1) (inc y1)]
                   [(dec x1) y1]
                   [x1 (dec y1)]
                   [(dec x1) (dec y1)]
                   [(inc x1) (dec y1)]
                   [(dec x1) (inc y1)]}]
    (contains? possible location2)))

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
      already-laid? ::err/illegal-mine-already-laid
      in-trail? ::err/illegal-mine-in-trail
      (not adj?) ::err/illegal-mine-not-adj
      :else (update-in game-state [:teams team-laying :mines] conj mine-location))))

(defn explosion-wrt
  "Explodes \"with regard to\" a submarine."
  [sub-state explosion-location]
  (let [target-location (last (:trail sub-state))
        direct-hit? (= explosion-location target-location)
        hit? (adjacent? explosion-location target-location)]
    (cond
      direct-hit? (update sub-state :health #(- % 2))
      hit? (update sub-state :health dec)
      :else sub-state)))

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
      weapons-down? ::err/illegal-weapons-are-broken
      (not mine-exists?) ::err/illegal-no-such-mine
      :else (-> game-state
                (update-in [:teams team-detonating :mines] disj mine-location)
                (explosion-at mine-location)))))

;; Rulings:
;; * Torpedos cannot move through islands, see official designer ruling at
;;   https://boardgamegeek.com/thread/1913121/rule-clarification-torpedomissilesmines
(defn fire-torpedo [game-state team-firing firing-to]
  {:pre [(team? team-firing) (location? firing-to)]}
  (let [firing-team-location (location game-state team-firing)
        in-range? (-> (a* :heuristic-fn maze-distance
                          :neighbors-fn (partial maps/neighbors (:map game-state))
                          :start firing-team-location
                          :finish firing-to)
                      :cost
                      (<= 4))]
    (if (not in-range?)
      ::err/illegal-out-of-range
      (explosion-at game-state firing-to))))

(defn use-sonar [game-state team-using team-targeted]
  {:pre [(team? team-using) (team? team-targeted)]}
  (update game-state :events conj {:type :sonar
                                   :from team-using
                                   :to team-targeted}))

(def sector->n
  {:sector/one 1
   :sector/two 2
   :sector/three 3
   :sector/four 4
   :sector/five 5
   :sector/six 6
   :sector/seven 7
   :sector/eight 8
   :sector/nine 9})

(defn sector? [x]
  (contains? sector->n x))

(defn in-sector? [guessed-sector location]
  ;; FIXME we're assuming the 15x15 board here
  (let [sector-width 5
        sector-height 5
        sectors-per-row 3
        [x y] location
        sector (+ (inc (quot (dec x) sector-width))
                  (* sectors-per-row (quot (dec y) sector-height)))]
    (= sector (sector->n guessed-sector))))

(defn use-drone [game-state team-using team-targeted guessed-sector]
  {:pre [(team? team-using) (team? team-targeted) (sector? guessed-sector)]}
  (let [team-location (last (get-in game-state [:teams team-targeted :trail]))
        are-they-there? (in-sector? guessed-sector team-location)]
    (update game-state :events conj {:type :drone-inform
                                     :team team-using
                                     :answer are-they-there?})))

(defn use-silence [gs team direction distance charge breakdown]
  {:pre [(team? team)
         (#{:north :south :east :west} direction)
         (<= 0 distance 4)]}
  (err-> gs
         (make-move team direction distance)
         (charge-system team charge)
         (breakdown-system team breakdown direction)))

(defn activate-system [game-state {:keys [system team-activating params]}]
  {:pre [(team? team-activating)
         (system? system)]}
  (let [{:keys [systems breakdowns]} (get-in game-state [:teams team-activating])
        charged? (= (system systems) (system max-system-charges))
        disabled? (broken? breakdowns (system->color system))]
    (cond
      (not charged?) ::err/system-uncharged
      disabled? ::err/system-down
      :else (as-> game-state game-state
              (assoc-in game-state [:teams team-activating :systems system] 0)
              (case system
                :torpedo (do
                           (assert (location? (:target params)))
                           (fire-torpedo game-state team-activating (:target params)))
                :mine (do
                        (assert (location? (:location params)))
                        (lay-mine game-state team-activating (:location params)))
                :drone (do
                         (assert (team? (:target-team params)))
                         (if (sector? (:guessed-sector params))
                           (use-drone game-state team-activating (:target-team params) (:guessed-sector params))
                           ::err/not-a-sector))
                :sonar (do
                         (assert (team? (:target-team params)))
                         (use-sonar game-state team-activating (:target-team params)))
                ;; FIXME don't hardcode the map
                ;; it should probably live in the game state somewhere
                :silence (do
                           (assert (#{:north :east :south :west :nomove} (:direction params)))
                           (if (= :nomove (:direction params))
                             game-state
                             (if (<= (:distance params) 4)
                               (use-silence game-state team-activating (:direction params) (:distance params) (:charge params) (:breakdown params))
                               ::err/silence-too-far))))))))

(defn update-captains-orders [orders direction]
  {:pre [(#{:north :south :east :west} direction)]}
  (-> orders
      (assoc :captain direction)
      (assoc :first-mate nil)
      (assoc :engineer nil)))

(defn update-firstmate-orders [orders system]
  {:pre [(system? system)]}
  (assoc orders :first-mate system))

(defn update-engineer-orders [orders breakdown]
  (assoc orders :engineer breakdown))

(defn tick [game-state & {:keys [team action direction system breakdown mine guess target target-team move distance charge]}]
  (if (err? (:error game-state))
    game-state
    (let [team-state (get-in game-state [:teams team])
          orders (:orders team-state)
          gs' (case action
                :order/captain (let [orders' (update-captains-orders orders direction)]
                                 (assoc-in game-state [:teams team :orders] orders'))
                :order/first-mate (let [orders' (update-firstmate-orders orders system)]
                                    (assoc-in game-state [:teams team :orders] orders'))
                :order/engineer (let [orders' (update-engineer-orders orders breakdown)]
                                  (assoc-in game-state [:teams team :orders] orders'))
                :order/mine (activate-system game-state {:system :mine
                                                         :team-activating team
                                                         :params {:location target}})
                :order/detonate (detonate-mine game-state team mine)
                :order/torpedo (activate-system game-state {:system :torpedo
                                                            :team-activating team
                                                            :params {:target target}})
                :order/drone (activate-system game-state {:system :drone
                                                          :team-activating team
                                                          :params {:target-team target-team :guessed-sector guess}})
                :order/sonar (activate-system game-state {:system :sonar
                                                          :team-activating team
                                                          :params {:target-team target-team}})
                :order/silence (activate-system game-state {:system :silence
                                                            :team-activating team
                                                            :params {:direction move
                                                                     :distance distance
                                                                     :charge charge
                                                                     :breakdown breakdown}}))
          gs'' (cond
                 (err? gs') (assoc game-state :error gs')
                 (err? (:error gs')) (assoc game-state :error (:error gs'))
                 :else gs')
          gs''' (attempt-move gs'' team)]
      gs''')))

(comment
  (require '[dev.rob-3.submarine-commander.game-engine :as game-engine])
  ;; FIXME these are terrible inline (comment) tests; we'll make these proper soon
  (def x {:trail [[2 5]]
          :health 4
          :systems {:torpedo 0
                    :mine 0
                    :drone 0
                    :sonar 0
                    :silence 0}
          ;; the presence of a keyword means the box is marked
          :breakdowns #{}
          :orders {:captain :north
                   :first-mate nil
                   :engineer nil}
          :surfaced false
          :mines #{}})
  ;(attempt-move maps/alpha x)
  (surface x)
  (lay-mine game-engine/state :team/red [1 2])
  (explosion-wrt x [2 6])
  (explosion-at game-engine/state [1 2])
  (detonate-mine game-engine/state :team/red [1 2])
  (use-drone game-engine/state :team/red :team/blue 9)
  (use-silence game-engine/state :team/red :north)
  (tick game-engine/state
        :action :order/captain
        :direction :north
        :team :team/blue))
