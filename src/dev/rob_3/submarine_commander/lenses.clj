(ns dev.rob-3.submarine-commander.lenses 
  (:require
   [dev.rob-3.submarine-commander.actions :refer [team?]]))

(defn blue-trail [gs]
  (get-in gs [:teams :team/blue :trail]))

(defn red-orders [gs]
  (get-in gs [:teams :team/red :orders]))

(defn blue-torp-charge [gs]
  (get-in gs [:teams :team/blue :systems :torpedo]))

(defn blue-health [gs]
  (get-in gs [:teams :team/blue :health]))

(defn blue-mines [gs]
  (get-in gs [:teams :team/blue :mines]))

(defn blue-mine-charge [gs]
  (get-in gs [:teams :team/blue :systems :mine]))

(defn blue-location [gs]
  (last (blue-trail gs)))

(defn trail [gs team]
  {:pre [(team? team)]}
  (get-in gs [:teams team :trail]))

(defn location [gs team]
  {:pre [(team? team)]}
  (last (trail gs team)))
