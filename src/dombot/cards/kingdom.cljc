(ns dombot.cards.kingdom
  (:require [dombot.operations :as op]
            [dombot.cards.base-cards :as base :refer [copper estate]]
            [dombot.cards.dominion :as dominion]
            [dombot.cards.intrigue :as intrigue]
            [dombot.utils :as ut]))

(def kingdom-cards (concat
                     dominion/kingdom-cards
                     intrigue/kingdom-cards))

(defn create-kingdom [sets victory-pile-size]
  (->> kingdom-cards
       (filter (comp sets :set))
       shuffle
       (take 10)
       (sort-by (juxt :cost :name))
       (map (fn [{:keys [:types] :as card}]
              (let [pile-size (if (:victory types) victory-pile-size 10)]
                {:card card :pile-size pile-size})))))

(defn create-player [name]
  {:name                name
   :hand                []
   :deck                []
   :discard             []
   :play-area           []
   :approx-discard-size 0
   :actions             0
   :coins               0
   :buys                0
   :number-of-turns     0})

(defn prepare-cards [game player-no]
  (-> game
      (ut/redupeat 7 op/gain player-no :copper)
      (ut/redupeat 3 op/gain player-no :estate)
      (op/draw player-no 5)))

(defn create-game [player-names mode sets]
  (let [number-of-players (count player-names)
        victory-pile-size (case number-of-players
                            2 8
                            3 12
                            4 12)
        starting-player (rand-int number-of-players)]
    (ut/reset-ids!)
    (as-> {:mode            mode
           :supply          (vec (concat (base/supply number-of-players victory-pile-size)
                                         (create-kingdom sets victory-pile-size)))
           :players         (vec (map create-player player-names))
           :current-player  starting-player
           :starting-player starting-player} game
          (reduce prepare-cards game (range number-of-players)))))
