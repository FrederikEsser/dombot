(ns dombot.front-end-model
  (:require [dombot.utils :as ut]))

(defn- choice-interaction [name area {:keys [source options min max]}]
  (when (and (= area source) ((set options) name))
    (if (= 1 min (or max (count options)))
      {:interaction :quick-choosable}
      {:interaction :choosable})))

(defn model-supply [{supply               :supply
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

(defn model-area [area {{:keys [phase actions] :as player} :player
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

(defn model-hand [active-player? {{:keys [hand hand-revealed?]} :player
                                  :as                           data}]
  (if (or active-player? hand-revealed?)
    (model-area :hand data)
    [{:name.ui         "Hand"
      :number-of-cards (count hand)}]))

(defn model-deck [{{:keys [deck]} :player
                   :as            data}]
  (concat
    (model-area :look-at data)
    (model-area :revealed data)
    (when (< 0 (count deck))
      [{:name.ui         "Deck"
        :number-of-cards (count deck)}])))

(defn model-discard [{{:keys [discard approx-discard-size]} :player
                      {:keys [reveal-source] :as choice}    :choice
                      :as                                   data}]
  (if (empty? discard)
    []
    (if reveal-source
      (model-area :discard data)
      (let [{:keys [name type]} (last discard)]
        [(merge {:name    name
                 :name.ui (ut/format-name name)
                 :type    type}
                (when approx-discard-size
                  {:number-of-cards approx-discard-size})
                (choice-interaction name :discard choice))]))))

(defn model-player [active-player? {{:keys [name actions coins buys]} :player
                                    :as                               data}]
  {:name      (ut/format-name name)
   :hand      (model-hand active-player? data)
   :play-area (model-area :play-area data)
   :deck      (model-deck data)
   :discard   (model-discard data)
   :actions   actions
   :money     coins
   :buys      buys})

(defn model-game [{:keys [supply players trash effect-stack current-player]}]
  (let [[{:keys [player-no text] :as choice}] effect-stack]
    (cond-> {:supply  (model-supply {:supply supply
                                     :player (get players current-player)
                                     :choice choice})
             :players (->> players
                           (map-indexed (fn [idx player]
                                          (model-player (= idx current-player)
                                                        (merge {:player player}
                                                               (when (= idx player-no)
                                                                 {:choice choice}))))))
             :trash   (ut/frequencies-of trash :name)}
            choice (assoc :choice {:text   text
                                   :player (ut/format-name (get-in players [player-no :name]))}))))