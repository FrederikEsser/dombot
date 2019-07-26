(ns dombot.cards.nocturne
  (:require [dombot.operations :refer [push-effect-stack give-choice]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- tragic-hero-demise [game {:keys [player-no card-id] :as args}]
  (let [hand-size (count (get-in game [:players player-no :hand]))]
    (cond-> game
            (<= 8 hand-size) (push-effect-stack {:player-no player-no
                                                 :effects   [[:trash-from-play-area {:trash-card-id card-id}]
                                                             [:give-choice {:text    "Gain a Treasure."
                                                                            :choice  :gain
                                                                            :options [:supply {:type :treasure}]
                                                                            :min     1
                                                                            :max     1}]]}))))

(effects/register {::tragic-hero-demise tragic-hero-demise})

(def tragic-hero {:name    :tragic-hero
                  :set     :nocturne
                  :types   #{:action}
                  :cost    5
                  :effects [[:draw 3]
                            [:give-buys 1]
                            [::tragic-hero-demise]]})

(def kingdom-cards [tragic-hero])
