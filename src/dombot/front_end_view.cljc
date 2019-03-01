(ns dombot.front-end-view
  (:require [dombot.utils :as ut]
            [dombot.operations :refer [stay-in-play]]
            [dombot.specs :as specs]
            [clojure.spec.alpha :as s]))

(defn- choice-interaction [name area {:keys [source options min max]}]
  (when (and (= area source) ((set options) name))
    (if (= 1 (or max (count options)))
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
                        :name-ui         (ut/format-name name)
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
                       choice                             :choice
                       active?                            :active-player?}
                 & [position number-of-cards]]
  (let [take-fn (if (= :bottom position) take-last take)
        cards (cond->> (get player area)
                       number-of-cards (take-fn number-of-cards))]
    (->> cards
         (map (fn [{:keys [name types set-aside] :as card}]
                (merge {:name    name
                        :name-ui (ut/format-name name)
                        :types   types}
                       (when (stay-in-play card)
                         {:stay-in-play true})
                       (when (and (= :hand area)
                                  (not choice)
                                  (or (and (:action types)
                                           (#{:action} phase)
                                           (< 0 actions))
                                      (and (:treasure types)
                                           (#{:action :pay} phase))))
                         {:interaction :playable})
                       (choice-interaction name area choice)
                       (when (not-empty set-aside)
                         {:set-aside (map (if active? (comp ut/format-name :name) (constantly "Card")) set-aside)}))))
         frequencies
         (map (fn [[card number-of-cards]]
                (cond-> card
                        (< 1 number-of-cards) (assoc :number-of-cards number-of-cards)))))))

(defn view-hand [{active-player?                      :active-player?
                  {:keys [hand revealed-cards phase]} :player
                  choice                              :choice
                  :as                                 data}]
  (let [revealed-cards-in-hand (:hand revealed-cards)]
    (if (or active-player?
            (= revealed-cards-in-hand (count hand))
            choice
            (= :end-of-game phase))
      (view-area :hand data)
      (if (empty? hand)
        {}
        (merge {:number-of-cards (count hand)}
               (when revealed-cards-in-hand
                 {:visible-cards (view-area :hand data revealed-cards-in-hand)}))))))

(defn view-deck [{{:keys [deck
                          look-at
                          revealed
                          revealed-cards]} :player
                  :as                      data}]
  (let [full-deck (concat look-at revealed deck)
        revealed-cards-in-deck (:deck revealed-cards)]
    (if (empty? full-deck)
      {}
      (merge {:number-of-cards (count full-deck)}
             (when (or (not-empty look-at) (not-empty revealed) revealed-cards-in-deck)
               {:visible-cards (concat (view-area :look-at data)
                                       (view-area :revealed data)
                                       (when revealed-cards-in-deck (view-area :deck data :top revealed-cards-in-deck)))})))))

(defn view-discard [{{:keys [discard
                             approx-discard-size
                             revealed-cards]} :player
                     {:keys [reveal-source]}  :choice
                     :as                      data}]
  (if (empty? discard)
    {}
    (let [number-of-cards (or (and reveal-source (count discard))
                              (:discard revealed-cards)
                              1)]
      {:visible-cards   (view-area :discard data :bottom number-of-cards)
       :number-of-cards (max approx-discard-size number-of-cards)})))

(defn view-options [options]
  (->> options
       (map (fn [option] (select-keys option [:option :text])))))

(defn view-choice [{:keys [text source options min max optional?] :as choice}]
  (->> (merge {:text          text
               :min           (or min 0)
               :max           (or max (count options))
               :quick-choice? (and (= 1 min (or max (count options)))
                                   (not optional?))}
              (case source
                :special {:options (view-options options)}
                :deck-position {:interval {:from (first options)
                                           :to   (last options)}}
                {})
              (when-not (nil? optional?)
                {:optional? optional?}))
       (s/assert* ::specs/choice)))

(defn view-player [{{:keys [name
                            actions
                            coins
                            buys
                            set-aside
                            island-mat
                            native-village-mat
                            pirate-ship-coins
                            victory-points
                            winner]} :player
                    choice           :choice
                    active-player?   :active-player?
                    :as              data}]
  (merge {:name-ui   (ut/format-name name)
          :hand      (view-hand data)
          :play-area (concat (view-area :play-area-duration data)
                             (view-area :play-area data))
          :deck      (view-deck data)
          :discard   (view-discard data)
          :actions   actions
          :coins     coins
          :buys      buys
          :active?   active-player?}
         (when (not-empty set-aside)
           {:set-aside (view-area :set-aside data)})
         (when (not-empty island-mat)
           {:island-mat (view-area :island-mat data)})
         (when (not-empty native-village-mat)
           {:native-village-mat (if active-player?
                                  (view-area :native-village-mat data)
                                  {:number-of-cards (count native-village-mat)})})
         (when pirate-ship-coins
           {:pirate-ship-coins pirate-ship-coins})
         (when choice
           {:choice (view-choice choice)})
         (when victory-points
           {:victory-points victory-points})
         (when-not (nil? winner)
           {:winner? winner})))

(defn view-trash [{:keys [trash choice]} mode]
  (case mode
    :compact (if (empty? trash)
               {}
               (let [{:keys [name types]} (last trash)]
                 {:visible-cards   [{:name    name
                                     :name-ui (ut/format-name name)
                                     :types   types}]
                  :number-of-cards (count trash)}))
    :full (if (empty? trash)
            []
            (->> trash
                 (map (fn [{:keys [name types]}]
                        (merge {:name    name
                                :name-ui (ut/format-name name)
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
                                          (view-player (merge {:active-player? active-player?
                                                               :player         player}
                                                              (when (= idx player-no)
                                                                {:choice choice})))))))
          :trash    {:compact (view-trash {:trash trash :choice choice} :compact)
                     :full    (view-trash {:trash trash :choice choice} :full)}
          :commands (view-commands game)}
         (s/assert* ::specs/game))))