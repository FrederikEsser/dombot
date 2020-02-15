(ns dombot.cards.empires
  (:require [dombot.operations :refer [push-effect-stack give-choice]]
            [dombot.cards.common :refer [reveal-from-deck topdeck-from-revealed]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]
            [clojure.string :as string]))

(defn- chariot-race-compare [{:keys [players] :as game} {:keys [player-no]}]
  (let [next-player (mod (inc player-no) (count players))
        own-card    (first (get-in game [:players player-no :revealed]))
        other-card  (first (get-in game [:players next-player :revealed]))
        race-won?   (and own-card
                         other-card
                         (> (ut/get-cost game own-card) (ut/get-cost game other-card)))]
    (cond-> game
            other-card (push-effect-stack {:player-no next-player
                                           :effects   [[:topdeck-from-revealed {:card-name (:name other-card)}]]})

            own-card (push-effect-stack {:player-no player-no
                                         :effects   (concat [[:put-revealed-into-hand {:card-name (:name own-card)}]]
                                                            (when race-won?
                                                              [[:give-coins 1]
                                                               [:give-victory-points 1]]))}))))

(defn- chariot-race-reveal [{:keys [players] :as game} {:keys [player-no]}]
  (let [next-player (mod (inc player-no) (count players))]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:reveal-from-deck 1]
                                        [::chariot-race-compare]]})
        (push-effect-stack {:player-no next-player
                            :effects   [[:reveal-from-deck 1]]}))))

(effects/register {::chariot-race-compare chariot-race-compare
                   ::chariot-race-reveal  chariot-race-reveal})

(def chariot-race {:name    :chariot-race
                   :set     :empires
                   :types   #{:action}
                   :cost    3
                   :effects [[:give-actions 1]
                             [::chariot-race-reveal]]})

(def kingdom-cards [chariot-race])

(def events [])