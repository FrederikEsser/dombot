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

(defn model-hand [data]
  (model-area :hand data))

(defn model-discard [{{:keys [discard]}                  :player
                      {:keys [reveal-source] :as choice} :choice
                      :as                                data}]
  (if reveal-source
    (model-area :discard data)
    (let [{:keys [name type]} (last discard)
          size (count discard)
          variance (Math/round (/ size 4.0))]
      [(merge {:number-of-cards (- (+ size
                                      (rand-int (inc variance)))
                                   (rand-int (inc variance)))}
              (when (not-empty discard)
                {:name    name
                 :name.ui (ut/format-name name)
                 :type    type})
              (choice-interaction name :discard choice))])))

(defn model-active-player [{{:keys [name deck actions coins buys]} :player
                            :as                                    data}]
  {:name      (ut/format-name name)
   :hand      (model-hand data)
   :play-area (model-area :play-area data)
   :deck      {:number-of-cards (count deck)}
   :discard   (model-discard data)
   :actions   actions
   :money     coins
   :buys      buys})

(defn model-game [{:keys [supply players trash effect-stack current-player] :as game}]
  (let [[{:keys [player-no text options] :as choice}] effect-stack
        current-player-choice (when (= player-no current-player) choice)]
    (cond-> {:supply (model-supply {:supply supply
                                    :player (get players current-player)
                                    :choice choice})
             :player (model-active-player {:player (get players current-player)
                                           :choice current-player-choice})
             :trash  (ut/frequencies-of trash :name)}
            (or text (not-empty options)) (assoc :choice {:text   text
                                                          :player (ut/format-name (get-in players [player-no :name]))}))))