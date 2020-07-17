(ns dombot.cards.kingdom
  (:require [dombot.operations :as op]
            [dombot.cards.base-cards :as base :refer [copper estate]]
            [dombot.cards.dominion :as dominion]
            [dombot.cards.intrigue :as intrigue]
            [dombot.cards.seaside :as seaside]
            [dombot.cards.prosperity :as prosperity]
            [dombot.cards.cornucopia :as cornucopia]
            [dombot.cards.dark-ages :as dark-ages]
            [dombot.cards.guilds :as guilds]
            [dombot.cards.adventures :as adventures]
            [dombot.cards.empires :as empires]
            [dombot.cards.nocturne :as nocturne]
            [dombot.cards.renaissance :as renaissance]
            [dombot.cards.menagerie :as menagerie]
            [dombot.cards.promos :as promos]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def kingdom-cards (concat
                     dominion/kingdom-cards
                     intrigue/kingdom-cards
                     seaside/kingdom-cards
                     prosperity/kingdom-cards
                     cornucopia/kingdom-cards
                     dark-ages/kingdom-cards
                     guilds/kingdom-cards
                     adventures/kingdom-cards
                     empires/kingdom-cards
                     nocturne/kingdom-cards
                     renaissance/kingdom-cards
                     menagerie/kingdom-cards
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
                                  [(or (->> randomizers
                                            (filter (comp (partial ut/costs-between 2 3) ut/normalize-cost :cost))
                                            first)
                                       (->> randomizers first))]))
            bane-idx (->> kingdom
                          (keep-indexed (fn [idx {:keys [cost]}]
                                          (when (ut/costs-between 2 3 (ut/normalize-cost cost)) idx)))
                          last)]
        (update-in kingdom [bane-idx] assoc :bane? true))
      kingdom)))

(defn- get-victory-pile-size [number-of-players]
  (case number-of-players
    2 8
    3 12
    4 12))

(defn create-kingdom-supply [kingdom number-of-players]
  (let [victory-pile-size (case number-of-players
                            2 8
                            3 12
                            4 12)]
    (->> kingdom
         (sort-by (juxt (comp (juxt :coin-cost :debt-cost) ut/normalize-cost :cost) :name))
         (map (fn [{:keys [types split-pile bane?] :as card}]
                (if split-pile
                  (let [pile-fn (effects/get-effect split-pile)]
                    (cond-> (pile-fn number-of-players)
                            bane? (update :split-pile (partial mapv (fn [pile]
                                                                      (update pile :card assoc :bane? true))))))
                  (let [pile-size (if (:victory types) victory-pile-size 10)]
                    {:card card :pile-size pile-size})))))))

(def landscapes (concat
                  adventures/events
                  empires/events
                  empires/landmarks
                  renaissance/projects
                  menagerie/events))

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

(defn prepare-cards [shelters? heirlooms game player-no]
  (-> game
      (ut/redupeat (- 7 (count heirlooms)) op/gain {:player-no player-no
                                                    :card-name :copper})
      (cond-> (not shelters?) (ut/redupeat 3 op/gain {:player-no player-no
                              :card-name :estate})
              shelters? (update-in [:players player-no :discard] concat (map ut/give-id! dark-ages/shelters)))
      (update-in [:players player-no :discard] concat (map ut/give-id! heirlooms))
      (op/draw {:player-no player-no :arg 5})))

(defn setup-game [{:keys [supply events landmarks] :as game}]
  (let [setup-effects (concat (mapcat (comp :setup :card) supply)
                              (mapcat (comp :setup second) events)
                              (mapcat (comp :setup second) landmarks))]
    (-> game
        (op/push-effect-stack {:effects setup-effects})
        op/check-stack)))

(defn create-game [player-names mode sets]
  (let [number-of-players (count player-names)
        starting-player   (rand-int number-of-players)
        kingdom           (random-kingdom sets #{})
        landscape         (->> kingdom
                               (take 10)
                               (drop 2)
                               (split-at 4)
                               (keep (fn [cards]
                                       (->> cards
                                            (keep (comp #{:adventures :empires :renaissance :menagerie} :set))
                                            first)))
                               frequencies
                               (mapcat random-landscape)
                               (sort-by (juxt (comp (juxt :coin-cost :debt-cost) ut/normalize-cost :cost) :name)))
        events            (->> landscape
                               (filter (comp #{:event} :type))
                               (map (fn [{:keys [name] :as event}]
                                      [name event]))
                               (into {}))
        landmarks         (->> landscape
                               (filter (comp #{:landmark} :type))
                               (map (fn [{:keys [name] :as landmark}]
                                      [name landmark]))
                               (into {}))
        projects          (->> landscape
                               (filter (comp #{:project} :type))
                               (map (fn [{:keys [name] :as project}]
                                      [name project]))
                               (into {}))
        heirlooms         (->> kingdom
                               (keep :heirloom))
        shelters?         (-> kingdom second :set #{:dark-ages})]
    (ut/reset-ids!)
    (as-> (merge
            {:mode                  mode
             :supply                (vec (concat (base/supply number-of-players (get-victory-pile-size number-of-players)
                                                              {:prosperity? (-> kingdom first :set #{:prosperity})})
                                                 (create-kingdom-supply kingdom number-of-players)))
             :players               (vec (map create-player player-names))
             :track-gained-cards?   true
             :track-played-actions? true
             :current-player        starting-player
             :starting-player       starting-player}
            (when (not-empty events)
              {:events events})
            (when (not-empty landmarks)
              {:landmarks landmarks})
            (when (not-empty projects)
              {:projects projects})) game
          (-> (reduce (partial prepare-cards shelters? heirlooms) game (range number-of-players))
              setup-game))))

(defn random-sets [required & [num-sets]]
  (->> kingdom-cards
       (map :set)
       distinct
       (remove (set (concat required #{:promos})))
       shuffle
       (take (- (or num-sets 3) (count required)))
       (concat required)
       sort))

(comment
  (random-sets #{:empires :menagerie} 5))
