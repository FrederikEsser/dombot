(ns dombot.cards.kingdom
  (:require [dombot.operations :as op]
            [dombot.cards.base-cards :as base :refer [copper estate]]
            [dombot.cards.dominion :as dominion]
            [dombot.cards.intrigue :as intrigue]
            [dombot.cards.seaside :as seaside]
            [dombot.cards.prosperity :as prosperity]
            [dombot.cards.cornucopia :as cornucopia]
            [dombot.cards.guilds :as guilds]
            [dombot.cards.adventures :as adventures]
            [dombot.cards.nocturne :as nocturne]
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
                     adventures/kingdom-cards
                     nocturne/kingdom-cards
                     renaissance/kingdom-cards
                     promos/kingdom-cards))

(defn- random-kingdom [sets included-card-names]
  (let [included-cards (filter (comp included-card-names :name) kingdom-cards)
        [kingdom randomizers] (->> kingdom-cards
                                   (remove (comp included-card-names :name))
                                   (filter (comp sets :set))
                                   shuffle
                                   (concat included-cards)
                                   (split-at 10))]
    (if (some (comp #{:young-witch} :name) kingdom)
      (let [kingdom  (vec (concat kingdom
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
       (map (fn [{:keys [types] :as card}]
              (let [pile-size (if (:victory types) victory-pile-size 10)]
                {:card card :pile-size pile-size})))))

(def landscapes (concat
                  adventures/events
                  renaissance/projects))

(defn random-landscape [[set number]]
  (->> landscapes
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

(defn prepare-cards [heirlooms game player-no]
  (-> game
      (ut/redupeat (- 7 (count heirlooms)) op/gain {:player-no player-no
                                                    :card-name :copper})
      (ut/redupeat 3 op/gain {:player-no player-no
                              :card-name :estate})
      (update-in [:players player-no :discard] concat (map ut/give-id! heirlooms))
      (op/draw {:player-no player-no :arg 5})))

(defn setup-game [{:keys [supply events] :as game}]
  (let [setup-effects (concat (mapcat (comp :setup :card) supply)
                              (mapcat (comp :setup second) events))]
    (-> game
        (op/push-effect-stack {:effects setup-effects})
        op/check-stack)))

(defn create-game [player-names mode sets]
  (let [number-of-players (count player-names)
        victory-pile-size (case number-of-players
                            2 8
                            3 12
                            4 12)
        starting-player   (rand-int number-of-players)
        kingdom           (random-kingdom sets #{})
        landscape         (->> kingdom
                               (take 10)
                               (drop 2)
                               (split-at 4)
                               (keep (fn [cards]
                                       (->> cards
                                            (keep (comp #{:adventures :renaissance} :set))
                                            first)))
                               frequencies
                               (mapcat random-landscape)
                               (sort-by (juxt :cost :name)))
        events            (->> landscape
                               (filter (comp #{:event} :type))
                               (map (fn [{:keys [name] :as event}]
                                      [name event]))
                               (into {}))
        projects          (->> landscape
                               (filter (comp #{:project} :type))
                               (map (fn [{:keys [name] :as project}]
                                      [name project]))
                               (into {}))
        heirlooms         (->> kingdom
                               (keep :heirloom))]
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
            (when (not-empty events)
              {:events events})
            (when (not-empty projects)
              {:projects projects})) game
          (-> (reduce (partial prepare-cards heirlooms) game (range number-of-players))
              setup-game))))
