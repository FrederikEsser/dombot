(ns dombot.cards.promos
  (:require [dombot.operations :refer [move-card push-effect-stack]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- dismantle-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)
        max-cost (dec cost)]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat [[:trash-from-hand {:card-name card-name}]]
                                                (when (pos? cost)
                                                  [[:give-choice {:text    (str "Gain a card costing up to $" max-cost ".")
                                                                  :choice  :gain
                                                                  :options [:supply {:max-cost max-cost}]
                                                                  :min     1
                                                                  :max     1}]
                                                   [:gain {:card-name :gold}]]))})))

(effects/register {::dismantle-trash dismantle-trash})

(def dismantle {:name    :dismantle
                :set     :promos
                :types   #{:action}
                :cost    4
                :effects [[:give-choice {:text    "Trash a card from your hand."
                                         :choice  ::dismantle-trash
                                         :options [:player :hand]
                                         :min     1
                                         :max     1}]]})

(def envoy {:name    :envoy
            :set     :promos
            :types   #{:action}
            :cost    4
            :effects [[:reveal-from-deck 5]
                      [:give-choice {:text       "Choose which of the revealed cards will be discarded."
                                     :choice     :discard-from-revealed
                                     :options    [:player :revealed]
                                     :min        1
                                     :max        1
                                     :hide-hand? true}]
                      [:put-all-revealed-into-hand]]})

(defn stash-put [game {:keys [player-no card-id position]}]
  (move-card game {:player-no   player-no
                   :card-name   :stash
                   :from        :stash
                   :to          :deck
                   :to-position position}))

(effects/register {::stash-put stash-put})

(def stash {:name            :stash
            :set             :promos
            :types           #{:treasure}
            :cost            5
            :coin-value      2
            :before-triggers {:shuffle [[:move-card {:card-name :stash
                                                     :from      :discard
                                                     :to        :stash}]]}
            :after-triggers  {:shuffle [[:give-choice {:text    "Put the Stash anywhere in your deck."
                                                       :choice  ::stash-put
                                                       :options [:deck-position]
                                                       :min     1
                                                       :max     1}]]}})

(def kingdom-cards [dismantle
                    envoy
                    stash])
