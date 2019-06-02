(ns dombot.cards.base-cards
  (:require [dombot.cards.prosperity :as prosperity]))

(def curse {:name           :curse
            :types          #{:curse}
            :cost           0
            :victory-points -1})

(def estate {:name :estate :types #{:victory} :cost 2 :victory-points 1})
(def duchy {:name :duchy :types #{:victory} :cost 5 :victory-points 3})
(def province {:name :province :types #{:victory} :cost 8 :victory-points 6})
(def copper {:name :copper :types #{:treasure} :cost 0 :coin-value 1})
(def silver {:name :silver :types #{:treasure} :cost 3 :coin-value 2})
(def gold {:name :gold :types #{:treasure} :cost 6 :coin-value 3})

(defn make-pile [card pile-size]
  {:card card :pile-size pile-size})

(defn supply [number-of-players victory-pile-size & [{:keys [prosperity?]}]]
  (->> [(make-pile curse (* 10 (dec number-of-players)) #_#_:category :base-cards)
        (make-pile estate (+ victory-pile-size (* 3 number-of-players)))
        (make-pile duchy victory-pile-size)
        (make-pile province victory-pile-size)
        (when prosperity?
          (make-pile prosperity/colony victory-pile-size))
        (make-pile copper 60)
        (make-pile silver 40)
        (make-pile gold 30)
        (when prosperity?
          (make-pile prosperity/platinum 12))]
       (remove nil?)))
