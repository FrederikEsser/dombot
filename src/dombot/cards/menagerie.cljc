(ns dombot.cards.menagerie
  (:require [dombot.operations :refer [push-effect-stack gain]]
            [dombot.cards.common :refer []]
            [dombot.cards.dominion :as dominion]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def horse {:name    :horse
            :set     :menagerie
            :types   #{:action}
            :cost    3
            :effects [[:draw 2]
                      [:give-actions 1]
                      [:return-this-to-supply {:area :extra-cards}]]})

(defn- barge-choice [game {:keys [player-no card-id choice]}]
  (let [effects [[:draw 3]
                 [:give-buys 1]]]
    (push-effect-stack game {:player-no player-no
                             :effects   (case choice
                                          :now effects
                                          :next-turn [[:add-trigger {:trigger {:duration :once
                                                                               :event    :at-start-turn
                                                                               :mode     :semi
                                                                               :effects  effects}
                                                                     :card-id card-id}]])})))

(effects/register {::barge-choice barge-choice})

(def barge {:name    :barge
            :set     :menagerie
            :types   #{:action :duration}
            :cost    5
            :effects [[:give-choice {:text    "Either now or at the start of your next turn, +3 Cards and +1 Buy."
                                     :choice  ::barge-choice
                                     :options [:special
                                               {:option :now :text "Now"}
                                               {:option :next-turn :text "Next turn"}]
                                     :min     1
                                     :max     1}]]})

(defn- livery-on-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        cost (ut/get-cost game card)]
    (cond-> game
            (>= cost 4) (gain {:player-no player-no :card-name :horse :from :extra-cards}))))

(effects/register {::livery-on-gain livery-on-gain})

(def livery {:name    :livery
             :set     :menagerie
             :types   #{:action}
             :cost    5
             :effects [[:give-coins 3]]
             :trigger {:event    :on-gain
                       :duration :turn
                       :mode     :auto
                       :effects  [[::livery-on-gain]]}})


(def supplies {:name       :supplies
               :set        :menagerie
               :types      #{:treasure}
               :cost       2
               :coin-value 1
               :effects    [[:gain {:card-name   :horse
                                    :from        :extra-cards
                                    :to          :deck
                                    :to-position :top}]]
               :setup      [[:setup-extra-cards {:extra-cards [{:card horse :pile-size 30}]}]]})

(defn- village-green-choice [game {:keys [player-no card-id choice]}]
  (let [effects [[:draw 1]
                 [:give-actions 2]]]
    (push-effect-stack game {:player-no player-no
                             :effects   (case choice
                                          :now effects
                                          :next-turn [[:add-trigger {:trigger {:duration :once
                                                                               :event    :at-start-turn
                                                                               :mode     :semi
                                                                               :effects  effects}
                                                                     :card-id card-id}]])})))

(effects/register {::village-green-choice village-green-choice})

(def village-green {:name       :village-green
                    :set        :menagerie
                    :types      #{:action :duration :reaction}
                    :cost       3
                    :effects    [[:give-choice {:text    "Either now or at the start of your next turn, +3 Cards and +1 Buy."
                                                :choice  ::village-green-choice
                                                :options [:special
                                                          {:option :now :text "Now"}
                                                          {:option :next-turn :text "Next turn"}]
                                                :min     1
                                                :max     1}]]
                    :on-discard [[:give-choice {:text    "You may play Village Green."
                                                :choice  ::dominion/vassal-play-action
                                                :options [:player :discard {:last true
                                                                            :name :village-green}]
                                                :max     1}]]})

(def kingdom-cards [barge
                    livery
                    supplies
                    village-green])
