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
