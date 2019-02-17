(ns dombot.cards.promos
  (:require [dombot.operations :refer [move-card]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn stash-put [game player-no position]
  (move-card game player-no {:from        :stash
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

(def kingdom-cards [stash])
