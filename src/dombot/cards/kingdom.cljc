(ns dombot.cards.kingdom
  (:require [dombot.cards.base-cards :as base :refer [copper estate]]
            [dombot.cards.dominion :as dominion]))

(def kingdom-cards (concat
                     dominion/kingdom-cards))

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
    {:name name
     :hand (take 5 deck)
     :deck (drop 5 deck)
     :action 0
     :coins 0
     :buys 0}))

(defn create-game [player-names mode]
  (let [number-of-players (count player-names)
        victory-pile-size (case number-of-players
                            2 8
                            3 12
                            4 12)]
    {:mode           mode
     :supply         (vec (concat (base/supply number-of-players victory-pile-size)
                                  (create-kingdom #{:dominion} victory-pile-size)))
     :players        (vec (map create-player player-names))
     :current-player (rand-int number-of-players)}))
