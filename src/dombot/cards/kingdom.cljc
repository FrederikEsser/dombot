(ns dombot.cards.kingdom
  (:require [dombot.operations :as op]
            [dombot.cards.base-cards :as base :refer [copper estate]]
            [dombot.cards.dominion :as dominion]
            [dombot.cards.intrigue :as intrigue]
            [dombot.cards.seaside :as seaside]
            [dombot.cards.renaissance :as renaissance]
            [dombot.cards.promos :as promos]
            [dombot.utils :as ut]))

(def kingdom-cards (concat
                     dominion/kingdom-cards
                     intrigue/kingdom-cards
                     seaside/kingdom-cards
                     renaissance/kingdom-cards
                     promos/kingdom-cards))

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
   :actions-played      0
   :number-of-turns     0})

(defn prepare-cards [game player-no]
  (-> game
      (ut/redupeat 7 op/gain {:player-no player-no
                              :card-name :copper})
      (ut/redupeat 3 op/gain {:player-no player-no
                              :card-name :estate})
      (op/draw {:player-no player-no :arg 5})))

(defn setup-game [{:keys [supply] :as game}]
  (let [setup-effects (->> supply
                           (mapcat (comp :setup :card)))]
    (-> game
        (op/push-effect-stack {:effects setup-effects})
        op/check-stack)))

(defn create-game [player-names mode sets]
  (let [number-of-players (count player-names)
        victory-pile-size (case number-of-players
                            2 8
                            3 12
                            4 12)
        starting-player (rand-int number-of-players)]
    (ut/reset-ids!)
    (as-> {:mode                mode
           :supply              (vec (concat (base/supply number-of-players victory-pile-size)
                                             (create-kingdom sets victory-pile-size)))
           :players             (vec (map create-player player-names))
           :track-gained-cards? true
           :current-player      starting-player
           :starting-player     starting-player} game
          (-> (reduce prepare-cards game (range number-of-players))
              setup-game))))
