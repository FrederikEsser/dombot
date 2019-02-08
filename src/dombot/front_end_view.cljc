(ns dombot.front-end-view
  (:require [dombot.utils :as ut]))

(defn- choice-interaction [name area {:keys [source options min max]}]
  (when (and (= area source) ((set options) name))
    (if (= 1 min (or max (count options)))
      {:interaction :quick-choosable}
      {:interaction :choosable})))

(defn view-supply [{supply               :supply
                    {:keys [coins buys]} :player
                    choice               :choice}]
  (->> supply
       (map (fn [{{:keys [name type cost]} :card
                  number-of-cards          :pile-size}]
              (merge {:name            name
                      :name.ui         (ut/format-name name)
                      :type            type
                      :cost            cost
                      :number-of-cards number-of-cards}
                     (when (and (not choice)                ; todo: check phase
                                (< 0 number-of-cards)
                                buys (< 0 buys)
                                coins (<= cost coins))
                       {:interaction :buyable})
                     (choice-interaction name :supply choice))))))

(defn view-area [area {{:keys [phase actions] :as player} :player
                       choice                             :choice}]
  (let [cards (get player area)]
    (->> cards
         (map (fn [{:keys [name type]}]
                (merge {:name    name
                        :name.ui (ut/format-name name)
                        :type    type}
                       (when (and (= :hand area)
                                  (not choice)
                                  (or (and (:action type)
                                           (#{:action} phase)
                                           (< 0 actions))
                                      (and (:treasure type)
                                           (#{:action :pay} phase))))
                         {:interaction :playable})
                       (choice-interaction name area choice))))
         frequencies
         (map (fn [[card number-of-cards]]
                (assoc card :number-of-cards number-of-cards))))))

(defn view-hand [active-player? {{:keys [hand hand-revealed?]} :player
                                 {:keys [source]}              :choice
                                 :as                           data}]
  (if (or active-player? hand-revealed? (= :hand source))
    (view-area :hand data)
    [{:name.ui         "Hand"
      :number-of-cards (count hand)}]))

(defn view-deck [{{:keys [deck]} :player
                  :as            data}]
  (concat
    (view-area :look-at data)
    (view-area :revealed data)
    (when (< 0 (count deck))
      [{:name.ui         "Deck"
        :number-of-cards (count deck)}])))

(defn view-discard [{{:keys [discard approx-discard-size]} :player
                     {:keys [reveal-source] :as choice}    :choice
                     :as                                   data}]
  (if (empty? discard)
    []
    (if reveal-source
      (view-area :discard data)
      (let [{:keys [name type]} (last discard)]
        [(merge {:name    name
                 :name.ui (ut/format-name name)
                 :type    type}
                (when approx-discard-size
                  {:number-of-cards approx-discard-size})
                (choice-interaction name :discard choice))]))))

(defn view-choice [{:keys [options min max] :as choice}]
  (merge (select-keys choice [:text :min :max])
         (when (= 1 min (or max (count options)))
           {:quick-choice true})))

(defn view-player [active-player? {{:keys [name
                                           actions
                                           coins
                                           buys
                                           set-aside]} :player
                                   choice              :choice
                                   :as                 data}]
  (merge {:name      (ut/format-name name)
          :hand      (view-hand active-player? data)
          :play-area (view-area :play-area data)
          :deck      (view-deck data)
          :discard   (view-discard data)
          :actions   actions
          :money     coins
          :buys      buys}
         (when (not-empty set-aside)
           {:set-aside (view-area :set-aside data)})
         (when choice
           {:choice (view-choice choice)})))

(defn view-trash [trash mode]
  (if (empty? trash)
    []
    (case mode
      :short (let [{:keys [name type]} (last trash)]
               [(merge {:name            name
                        :name.ui         (ut/format-name name)
                        :type            type
                        :number-of-cards (count trash)})])
      :full (->> trash
                 (map (fn [{:keys [name type]}]
                        (merge {:name    name
                                :name.ui (ut/format-name name)
                                :type    type})))
                 frequencies
                 (map (fn [[card number-of-cards]]
                        (assoc card :number-of-cards number-of-cards)))))))

(defn view-commands [{:keys [players effect-stack current-player can-undo?]}]
  (let [{:keys [hand phase]} (get players current-player)
        [choice] effect-stack]
    {:can-undo?           can-undo?
     :can-play-treasures? (and (some (comp :treasure :type) hand)
                               (#{:action :pay} phase)
                               (not choice))
     :can-end-turn?       (not choice)}))

(defn view-game [{:keys [supply players trash effect-stack current-player] :as game}]
  (let [[{:keys [player-no] :as choice}] effect-stack]
    (cond-> {:supply      (view-supply {:supply supply
                                        :player (get players current-player)
                                        :choice choice})
             :players     (->> players
                               (map-indexed (fn [idx player]
                                              (let [active-player? (and (= idx current-player)
                                                                        (or (nil? choice)
                                                                            (= idx player-no)))]
                                                (view-player active-player?
                                                             (merge {:player player}
                                                                    (when (= idx player-no)
                                                                      {:choice choice})))))))
             :trash-short (view-trash trash :short)
             :trash-full  (view-trash trash :full)
             :commands    (view-commands game)})))