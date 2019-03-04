(ns dombot.cards.renaissance
  (:require [dombot.operations :refer [push-effect-stack give-choice]]
            [dombot.cards.common :refer [reveal-hand]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def acting-troupe {:name    :acting-troupe
                    :set     :renaissance
                    :types   #{:action}
                    :cost    3
                    :effects [[:give-villagers 4]
                              [:trash-this]]})

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

(defn villain-attack [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])
        has-eligible-card? (some (comp (partial <= 2) (partial ut/get-cost game)) hand)]
    (cond (< (count hand) 5) game
          has-eligible-card? (give-choice game {:player-no player-no
                                                :text      "Discard a card costing $2 or more."
                                                :choice    :discard-from-hand
                                                :options   [:player :hand {:min-cost 2}]
                                                :min       1
                                                :max       1})
          :else (reveal-hand game {:player-no player-no}))))

(effects/register {::villain-attack villain-attack})

(def villain {:name    :villain
              :set     :renaissance
              :types   #{:action :attack}
              :cost    5
              :effects [[:give-coffers 2]
                        [:attack {:effects [[::villain-attack]]}]]})

(def kingdom-cards [acting-troupe
                    recruiter
                    scholar
                    villain])
