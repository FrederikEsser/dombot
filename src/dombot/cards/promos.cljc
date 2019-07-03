(ns dombot.cards.promos
  (:require [dombot.operations :refer [move-card]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

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

(def kingdom-cards [envoy
                    stash])
