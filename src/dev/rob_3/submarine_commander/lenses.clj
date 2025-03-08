(ns dev.rob-3.submarine-commander.lenses)

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
