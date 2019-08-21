(ns dombot.cards.adventures
  (:require [dombot.operations :refer [push-effect-stack give-choice]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- raze-trash-from-area [game {:keys [player-no choice]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no (:area choice)] {:name (:card-name choice)})
        cost (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-area {:choice choice}]
                                         [:look-at cost]
                                         [:give-choice {:text    (str "Look at " cost " cards from the top of your deck. Put one of them into your hand and discard the rest.")
                                                        :choice  :take-from-look-at
                                                        :options [:player :look-at]
                                                        :min     1
                                                        :max     1}]
                                         [:discard-all-look-at]]})))

(effects/register {::raze-trash-from-area raze-trash-from-area})

(def raze {:name    :raze
           :set     :adventures
           :types   #{:action}
           :cost    2
           :effects [[:give-actions 1]
                     [:give-choice {:text    "Trash Raze or a card from your hand."
                                    :choice  ::raze-trash-from-area
                                    :options [:multi
                                              [:player :play-area {:this true}]
                                              [:player :hand]]
                                    :min     1
                                    :max     1}]]})

(def kingdom-cards [raze])