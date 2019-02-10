(ns dombot.cards.kingdom
  (:require [dombot.cards.base-cards :as base :refer [copper estate]]
            [dombot.cards.dominion :as dominion]
            [dombot.cards.intrigue :as intrigue]))

(def kingdom-cards (concat
                     dominion/kingdom-cards
                     intrigue/kingdom-cards))

(defn create-kingdom [sets victory-pile-size]
  (->> kingdom-cards
       (filter (comp sets :set))
       shuffle
       (take 10)
       (sort-by (juxt :cost :name))
       (map (fn [{:keys [:type] :as card}]
              {:card card :pile-size (if (:victory type) victory-pile-size 10)}))))

(defn create-player [name]
  (let [deck (->> (concat (repeat 7 copper) (repeat 3 estate))
                  shuffle)]
    {:name                name
     :hand                (take 5 deck)
     :deck                (drop 5 deck)
     :discard             []
     :play-area           []
     :approx-discard-size 0
     :actions             0
     :coins               0
     :buys                0
     :number-of-turns     0}))

(defn create-game [player-names mode sets]
  (let [number-of-players (count player-names)
        victory-pile-size (case number-of-players
                            2 8
                            3 12
                            4 12)
        starting-player (rand-int number-of-players)]
    {:mode            mode
     :supply          (vec (concat (base/supply number-of-players victory-pile-size)
                                   (create-kingdom sets victory-pile-size)))
     :players         (vec (map create-player player-names))
     :current-player  starting-player
     :starting-player starting-player}))
