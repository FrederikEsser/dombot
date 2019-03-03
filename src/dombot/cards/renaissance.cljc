(ns dombot.cards.renaissance
  (:require [dombot.operations :refer [push-effect-stack]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn recruiter-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-hand {:card-name card-name}]
                                         [:give-villagers cost]]})))

(effects/register {::recruiter-trash recruiter-trash})

(def recruiter {:name    :recruiter
                :set     :renaissance
                :types   #{:action}
                :cost    5
                :effects [[:draw 2]
                          [:give-choice {:text    "Trash a cards from your hand."
                                         :choice  ::recruiter-trash
                                         :options [:player :hand]
                                         :min     1
                                         :max     1}]]})

(def scholar {:name    :scholar
              :set     :renaissance
              :types   #{:action}
              :cost    5
              :effects [[:discard-all-hand]
                        [:draw 7]]})

(def kingdom-cards [recruiter
                    scholar])
