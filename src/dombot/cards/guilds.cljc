(ns dombot.cards.guilds
  (:require [dombot.operations :refer [push-effect-stack give-choice]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def advisor {:name    :advisor
              :set     :guilds
              :types   #{:action}
              :cost    4
              :effects [[:give-actions 1]
                        [:reveal-from-deck 3]
                        [:give-choice {:text       "Choose which of the revealed cards will be discarded."
                                       :choice     :discard-from-revealed
                                       :options    [:player :revealed]
                                       :min        1
                                       :max        1
                                       :hide-hand? true}]
                        [:put-all-revealed-into-hand]]})

(def baker {:name    :baker
            :set     :guilds
            :types   #{:action}
            :cost    5
            :effects [[:draw 1]
                      [:give-actions 1]
                      [:give-coffers 1]]
            :setup   [[:all-players {:effects [[:give-coffers 1]]}]]})

(def candlestick-maker {:name    :candlestick-maker
                        :set     :guilds
                        :types   #{:action}
                        :cost    2
                        :effects [[:give-actions 1]
                                  [:give-buys 1]
                                  [:give-coffers 1]]})

(def merchant-guild {:name          :merchant-guild
                     :set           :guilds
                     :types         #{:action}
                     :cost          5
                     :effects       [[:give-buys 1]
                                     [:give-coins 1]]
                     :while-in-play {:on-buy [[:give-coffers 1]]}})

(defn- plaza-discard-treasure [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-coffers 1]]})))

(effects/register {::plaza-discard-treasure plaza-discard-treasure})

(def plaza {:name    :plaza
            :set     :guilds
            :types   #{:action}
            :cost    4
            :effects [[:draw 1]
                      [:give-actions 2]
                      [:give-choice {:text    "You may discard a Treasure for +1 Coffers."
                                     :choice  ::plaza-discard-treasure
                                     :options [:player :hand {:type :treasure}]
                                     :max     1}]]})

(def kingdom-cards [advisor
                    baker
                    candlestick-maker
                    merchant-guild
                    plaza])
