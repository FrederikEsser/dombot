(ns dombot.cards.prosperity
  (:require [dombot.operations :refer [#_#_#_#_move-cards push-effect-stack give-choice is-unaffected?]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def kings-court {:name    :king's-court
                  :set     :prosperity
                  :types   #{:action}
                  :cost    7
                  :effects [[:give-choice {:text    "You may play an Action card from your hand three times."
                                           :choice  [:repeat-action {:times 3}]
                                           :options [:player :hand {:type :action}]
                                           :max     1}]]})

(def workers-village {:name    :worker's-village
                      :set     :prosperity
                      :types   #{:action}
                      :cost    4
                      :effects [[:draw 1]
                                [:give-actions 2]
                                [:give-buys 1]]})

(def kingdom-cards [kings-court
                    workers-village])