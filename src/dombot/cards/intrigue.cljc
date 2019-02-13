(ns dombot.cards.intrigue
  (:require [dombot.operations :refer [move-card push-effect-stack give-choice draw ensure-deck-has-cards]]
            [dombot.cards.common :refer :all]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def bridge {:name    :bridge
             :set     :intrigue
             :types   #{:action}
             :cost    4
             :effects [[:give-buys 1]
                       [:give-coins 1]
                       [:add-cost-reduction 1]]})

(def courtyard {:name    :courtyard
                :set     :intrigue
                :types   #{:action}
                :cost    2
                :effects [[:draw 3]
                          [:give-choice {:text    "Put a card from your hand onto your deck."
                                         :choice  :topdeck-from-hand
                                         :options [:player :hand]
                                         :min     1
                                         :max     1}]]})

(def harem {:name           :harem
            :set            :intrigue
            :types          #{:treasure :victory}
            :cost           6
            :coin-value     2
            :victory-points 2})

(defn lurker-choice [game player-no choice]
  (case choice
    :trash (give-choice game player-no {:text    "Trash an Action card from the Supply."
                                        :choice  :trash-from-supply
                                        :options [:supply {:types :action}]
                                        :min     1
                                        :max     1})
    :gain (give-choice game player-no {:text    "Gain an Action card from the trash."
                                       :choice  :gain-from-trash
                                       :options [:trash {:types :action}]
                                       :min     1
                                       :max     1})))

(effects/register {::lurker-choice lurker-choice})

(def lurker {:name    :lurker
             :set     :intrigue
             :types   #{:action}
             :cost    2
             :effects [[:give-actions 1]
                       [:give-choice {:text    "Choose one:"
                                      :choice  ::lurker-choice
                                      :options [:special
                                                {:option :trash :text "Trash an Action card from the Supply."}
                                                {:option :gain :text "Gain an Action card from the trash."}]
                                      :min     1
                                      :max     1}]]})

(defn mining-village-trash [game player-no card-name]
  (cond-> game
          (= :mining-village card-name) (push-effect-stack player-no [[:trash-last-from-play-area card-name]
                                                                      [:give-coins 2]])))

(effects/register {::mining-village-trash mining-village-trash})

(def mining-village {:name    :mining-village
                     :set     :intrigue
                     :types   #{:action}
                     :cost    4
                     :effects [[:draw 1]
                               [:give-actions 2]
                               [:give-choice {:text    "You may trash this for +$2."
                                              :choice  ::mining-village-trash
                                              :options [:player :play-area {:this true}]
                                              :max     1}]]})

(defn pawn-choices [game player-no choices]
  (let [choices (set choices)]
    (assert (= 2 (count choices)) "The choices must be different.")
    (cond-> game
            (:card choices) (draw player-no 1)
            (:action choices) (give-actions player-no 1)
            (:buy choices) (give-buys player-no 1)
            (:coin choices) (give-coins player-no 1))))

(effects/register {::pawn-choices pawn-choices})

(def pawn {:name    :pawn
           :set     :intrigue
           :types   #{:action}
           :cost    2
           :effects [[:give-choice {:text    "Choose two:"
                                    :choice  ::pawn-choices
                                    :options [:special
                                              {:option :card :text "+1 Card"}
                                              {:option :action :text "+1 Action"}
                                              {:option :buy :text "+1 Buy"}
                                              {:option :coin :text "+$1"}]
                                    :min     2
                                    :max     2}]]})

(defn shanty-town-draw [game player-no]
  (let [hand (get-in game [:players player-no :hand])
        action-cards-in-hand? (some (comp :action :types) hand)]
    (push-effect-stack game player-no [[:reveal-hand]
                                       (when-not action-cards-in-hand?
                                         [:draw 2])])))

(effects/register {::shanty-town-draw shanty-town-draw})

(def shanty-town {:name    :shanty-town
                  :set     :intrigue
                  :types   #{:action}
                  :cost    3
                  :effects [[:give-actions 2]
                            [::shanty-town-draw]]})

(defn steward-choices [game player-no choice]
  (cond-> game
          (= :cards choice) (draw player-no 2)
          (= :coins choice) (give-coins player-no 2)
          (= :trash choice) (give-choice player-no {:text    "Trash 2 cards from your hand."
                                                    :choice  :trash-from-hand
                                                    :options [:player :hand]
                                                    :min     2
                                                    :max     2})))

(effects/register {::steward-choices steward-choices})

(def steward {:name    :steward
              :set     :intrigue
              :types   #{:action}
              :cost    3
              :effects [[:give-choice {:text    "Choose one:"
                                       :choice  ::steward-choices
                                       :options [:special
                                                 {:option :cards :text "+2 Cards"}
                                                 {:option :coins :text "+$2"}
                                                 {:option :trash :text "Trash 2 cards from your hand."}]
                                       :min     1
                                       :max     1}]]})

(defn swindler-attack [game player-no]
  (-> game
      (ensure-deck-has-cards player-no 1)
      (as-> game
            (let [[top-card] (get-in game [:players player-no :deck])
                  cost (ut/get-cost game top-card)]
              (cond-> game
                      top-card (push-effect-stack player-no [[:trash-from-topdeck]
                                                             [:give-choice {:text    (str "Gain a card costing $" cost " (attacker chooses).")
                                                                            :choice  :gain
                                                                            :options [:supply {:cost cost}]
                                                                            :min     1
                                                                            :max     1}]]))))))

(effects/register {::swindler-attack swindler-attack})

(def swindler {:name    :swindler
               :set     :intrigue
               :types   #{:action :attack}
               :cost    3
               :effects [[:give-coins 2]
                         [:attack {:effects [[::swindler-attack]]}]]})

(defn trading-post-trash [game player-no card-names]
  (-> game
      (push-effect-stack player-no [[:trash-from-hand card-names]
                                    (when (and (coll? card-names) (= 2 (count card-names)))
                                      [:gain-to-hand :silver])])))

(effects/register {::trading-post-trash trading-post-trash})

(def trading-post {:name    :trading-post
                   :set     :intrigue
                   :types   #{:action}
                   :cost    5
                   :effects [[:give-choice {:text    "Trash 2 cards from your hand."
                                            :choice  ::trading-post-trash
                                            :options [:player :hand]
                                            :min     2
                                            :max     2}]]})

(defn upgrade-trash [game player-no card-name]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] card-name)
        cost (inc (ut/get-cost game card))]
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
              :types   #{:action}
              :cost    5
              :effects [[:draw 1]
                        [:give-actions 1]
                        [:give-choice {:text    "Trash a card from your hand."
                                       :choice  ::upgrade-trash
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]]})

(def kingdom-cards [bridge
                    courtyard
                    harem
                    lurker
                    mining-village
                    pawn
                    shanty-town
                    steward
                    swindler
                    trading-post
                    upgrade])