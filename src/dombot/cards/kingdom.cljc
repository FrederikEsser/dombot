(ns dombot.cards.kingdom
  (:require [dombot.operations :as op]
            [dombot.cards.base-cards :as base :refer [copper estate]]
            [dombot.cards.dominion :as dominion]
            [dombot.cards.intrigue :as intrigue]
            [dombot.cards.seaside :as seaside]
            [dombot.cards.prosperity :as prosperity]
            [dombot.cards.cornucopia :as cornucopia]
            [dombot.cards.guilds :as guilds]
            [dombot.cards.renaissance :as renaissance]
            [dombot.cards.promos :as promos]
            [dombot.utils :as ut]))

(def kingdom-cards (concat
                     dominion/kingdom-cards
                     intrigue/kingdom-cards
                     seaside/kingdom-cards
                     prosperity/kingdom-cards
                     cornucopia/kingdom-cards
                     guilds/kingdom-cards
                     renaissance/kingdom-cards
                     promos/kingdom-cards))

(defn- random-kingdom [sets]
  (let [[kingdom randomizers] (->> kingdom-cards
                                   (filter (comp sets :set))
                                   shuffle
                                   (split-at 10))]
    (if (some (comp #{:young-witch} :name) kingdom)
      (let [kingdom (vec (concat kingdom
                                 [(or (->> randomizers (filter (comp #{2 3} :cost)) first)
                                      (->> randomizers first))]))
            bane-idx (->> kingdom
                          (keep-indexed (fn [idx {:keys [cost]}]
                                          (when (#{2 3} cost) idx)))
                          last)]
        (update-in kingdom [bane-idx] assoc :bane? true))
      kingdom)))

(defn create-kingdom-supply [kingdom victory-pile-size]
  (->> kingdom
       (sort-by (juxt :cost :name))
       (map (fn [{:keys [:types] :as card}]
              (let [pile-size (if (:victory types) victory-pile-size 10)]
                {:card card :pile-size pile-size})))))

(def event-landmark-project-list (concat
                                   renaissance/projects))

(defn random-elps [[set number]]
  (->> event-landmark-project-list
       (filter (comp #{set} :set))
       shuffle
       (take number)))

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
   :number-of-turns     0
   :phase               :out-of-turn})

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
        starting-player (rand-int number-of-players)
        kingdom (random-kingdom sets)
        elps (->> kingdom
                  (take 10)
                  (drop 2)
                  (split-at 4)
                  (keep (fn [cards]
                          (->> cards
                               (keep (comp #{:renaissance} :set))
                               first)))
                  frequencies
                  (mapcat random-elps))
        projects (->> elps
                      (filter (comp #{:project} :type))
                      vec)]
    (ut/reset-ids!)
    (as-> (merge
            {:mode                  mode
             :supply                (vec (concat (base/supply number-of-players victory-pile-size
                                                              {:prosperity? (-> kingdom first :set #{:prosperity})})
                                                 (create-kingdom-supply kingdom victory-pile-size)))
             :players               (vec (map create-player player-names))
             :track-gained-cards?   true
             :track-played-actions? true
             :current-player        starting-player
             :starting-player       starting-player}
            (when (not-empty projects)
              {:projects projects})) game
          (-> (reduce prepare-cards game (range number-of-players))
              setup-game))))
