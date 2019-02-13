(ns dombot.front-end-view
  (:require [dombot.utils :as ut]
            [dombot.specs :as specs]
            [clojure.spec.alpha :as s]))

(defn- choice-interaction [name area {:keys [source options min max]}]
  (when (and (= area source) ((set options) name))
    (if (= 1 min (or max (count options)))
      {:interaction :quick-choosable}
      {:interaction :choosable})))

(defn view-supply [{supply               :supply
                    {:keys [coins buys]} :player
                    choice               :choice
                    :as                  game}]
  (->> supply
       (map (fn [{{:keys [name types] :as card} :card
                  number-of-cards               :pile-size}]
              (let [cost (ut/get-cost game card)]
                (merge {:name            name
                        :name.ui         (ut/format-name name)
                        :types           types
                        :cost            cost
                        :number-of-cards number-of-cards}
                       (when (and (not choice)              ; todo: check phase
                                  (< 0 number-of-cards)
                                  buys (< 0 buys)
                                  coins (<= cost coins))
                         {:interaction :buyable})
                       (choice-interaction name :supply choice)))))))

(defn view-area [area {{:keys [phase actions] :as player} :player
                       choice                             :choice}
                 & [number-of-cards]]
  (let [cards (cond->> (get player area)
                       number-of-cards (take-last number-of-cards))]
    (->> cards
         (map (fn [{:keys [name types]}]
                (merge {:name    name
                        :name.ui (ut/format-name name)
                        :types   types}
                       (when (and (= :hand area)
                                  (not choice)
                                  (or (and (:action types)
                                           (#{:action} phase)
                                           (< 0 actions))
                                      (and (:treasure types)
                                           (#{:action :pay} phase))))
                         {:interaction :playable})
                       (choice-interaction name area choice))))
         frequencies
         (map (fn [[card number-of-cards]]
                (cond-> card
                        (< 1 number-of-cards) (assoc :number-of-cards number-of-cards)))))))

(defn view-hand [active-player? {{:keys [hand revealed-cards phase]} :player
                                 {:keys [source]}                    :choice
                                 :as                                 data}]
  (if (or active-player?
          (= (:hand revealed-cards) (count hand))
          (= :hand source)
          (= :end-of-game phase))
    (view-area :hand data)
    [{:name.ui         "Hand"
      :number-of-cards (count hand)}]))

(defn view-deck [{{:keys [deck look-at revealed]} :player
                  :as                             data}]
  (let [full-deck (concat look-at revealed deck)]
    (if (empty? full-deck)
      {}
      (merge {:number-of-cards (count full-deck)}
             (when (not-empty (concat look-at revealed))
               {:visible-cards (concat (view-area :look-at data)
                                       (view-area :revealed data))})))))

(defn view-discard [{{:keys [discard
                             approx-discard-size
                             revealed-cards]} :player
                     {:keys [reveal-source]}  :choice
                     :as                      data}]
  (if (empty? discard)
    {}
    (let [number-of-cards (or (:discard revealed-cards)
                              (and reveal-source (count discard))
                              1)]
      {:visible-cards   (view-area :discard data number-of-cards)
       :number-of-cards approx-discard-size})))

(defn view-options [options]
  (->> options
       (map (fn [option] (select-keys option [:option :text])))))

(defn view-choice [{:keys [source options min max] :as choice}]
  (merge (select-keys choice [:text :min :max])
         (when (= :special source)
           {:options (view-options options)})
         (when (= 1 min (or max (count options)))
           {:quick-choice? true})))

(defn view-player [active-player? {{:keys [name
                                           actions
                                           coins
                                           buys
                                           set-aside
                                           victory-points
                                           winner]} :player
                                   choice           :choice
                                   :as              data}]
  (merge {:name.ui   (ut/format-name name)
          :hand      (view-hand active-player? data)
          :play-area (view-area :play-area data)
          :deck      (view-deck data)
          :discard   (view-discard data)
          :actions   actions
          :coins     coins
          :buys      buys}
         (when active-player?
           {:active? true})
         (when (not-empty set-aside)
           {:set-aside (view-area :set-aside data)})
         (when choice
           {:choice (view-choice choice)})
         (when victory-points
           {:victory-points victory-points})
         (when (not (nil? winner))
           {:winner? winner})))

(defn view-trash [{:keys [trash choice]} mode]
  (if (empty? trash)
    []
    (case mode
      :compact (let [{:keys [name types]} (last trash)]
                 [{:name            name
                   :name.ui         (ut/format-name name)
                   :types           types
                   :number-of-cards (count trash)}])
      :full (->> trash
                 (map (fn [{:keys [name types]}]
                        (merge {:name    name
                                :name.ui (ut/format-name name)
                                :types   types}
                               (choice-interaction name :trash choice))))
                 frequencies
                 (map (fn [[card number-of-cards]]
                        (cond-> card
                                (< 1 number-of-cards) (assoc :number-of-cards number-of-cards))))))))

(defn view-commands [{:keys [players effect-stack current-player can-undo?]}]
  (let [{:keys [hand phase]} (get players current-player)
        [choice] effect-stack]
    {:can-undo?           (boolean can-undo?)
     :can-play-treasures? (boolean (and (not choice)
                                        (#{:action :pay} phase)
                                        (some (comp :treasure :types) hand)))
     :can-end-turn?       (and (not choice)
                               (not= phase :end-of-game))}))

(defn view-game [{:keys [supply cost-reductions players trash effect-stack current-player] :as game}]
  (let [[{:keys [player-no] :as choice}] effect-stack
        {:keys [phase] :as player} (get players current-player)]
    (->> {:supply   (view-supply {:supply          supply
                                  :cost-reductions cost-reductions
                                  :player          player
                                  :choice          choice})
          :players  (->> players
                         (map-indexed (fn [idx player]
                                        (let [active-player? (and (= idx current-player)
                                                                  (or (nil? choice)
                                                                      (= idx player-no))
                                                                  (not= phase :end-of-game))]
                                          (view-player active-player?
                                                       (merge {:player player}
                                                              (when (= idx player-no)
                                                                {:choice choice})))))))
          :trash    {:compact (view-trash {:trash trash :choice choice} :compact)
                     :full    (view-trash {:trash trash :choice choice} :full)}
          :commands (view-commands game)}
         (s/assert* ::specs/game))))