(ns dombot.cards.intrigue
  (:require [dombot.operations :refer [move-card push-effect-stack give-choice]]
            [dombot.cards.common :refer :all]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def courtyard {:name    :courtyard
                :set     :intrigue
                :type    #{:action}
                :cost    2
                :effects [[:draw 3]
                          [:give-choice {:text    "Put a card from your hand onto your deck."
                                         :choice  :topdeck-from-hand
                                         :options [:player :hand]
                                         :min     1
                                         :max     1}]]})

(defn mining-village-trash [game player-no card-name]
  (cond-> game
          (= :mining-village card-name) (push-effect-stack player-no [[:trash-last-from-play-area card-name]
                                                                      [:give-money 2]])))

(effects/register {::mining-village-trash mining-village-trash})

(def mining-village {:name    :mining-village
                     :set     :intrigue
                     :type    #{:action}
                     :cost    4
                     :effects [[:draw 1]
                               [:give-actions 2]
                               [:give-choice {:text    "You may trash this for +$2."
                                              :choice  ::mining-village-trash
                                              :options [:player :play-area {:this true}]
                                              :max     1}]]})

(defn upgrade-trash [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {{:keys [cost]} :card} (ut/get-card-idx player :hand card-name)
        cost (inc cost)]
    (-> game
        (push-effect-stack player-no [[:trash-from-hand card-name]
                                      [:give-choice {:text    (str "Gain a card costing exactly $" cost ".")
                                                     :choice  :gain
                                                     :options [:supply {:cost cost}]
                                                     :min     1
                                                     :max     1}]]))))

(effects/register {::upgrade-trash upgrade-trash})

(def upgrade {:name    :upgrade
              :set     :intrigue
              :type    #{:action}
              :cost    5
              :effects [[:draw 1]
                        [:give-actions 1]
                        [:give-choice {:text    "Trash a card from your hand."
                                       :choice  ::upgrade-trash
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]]})

(def kingdom-cards [courtyard
                    mining-village
                    upgrade])