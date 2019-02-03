(ns dombot.cards.base-cards)

(def curse {:name :curse :type #{:curse} :cost 0 :victory-points -1})
(def estate {:name :estate :type #{:victory} :cost 2 :victory-points 1})
(def duchy {:name :duchy :type #{:victory} :cost 5 :victory-points 3})
(def province {:name :province :type #{:victory} :cost 8 :victory-points 6})
(def copper {:name :copper :type #{:treasure} :cost 0 :coin-value 1})
(def silver {:name :silver :type #{:treasure} :cost 3 :coin-value 2})
(def gold {:name :gold :type #{:treasure} :cost 6 :coin-value 3})

(defn supply [number-of-players victory-pile-size]
  [{:card curse :pile-size (* 10 (dec number-of-players))}
   {:card estate :pile-size victory-pile-size}
   {:card duchy :pile-size victory-pile-size}
   {:card province :pile-size victory-pile-size}
   {:card copper :pile-size (- 60 (* 7 number-of-players))}
   {:card silver :pile-size 40}
   {:card gold :pile-size 30}])
