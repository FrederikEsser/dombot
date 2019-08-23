(ns dombot.cards.adventures
  (:require [dombot.operations :refer [push-effect-stack give-choice]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- amulet-choices [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [(case choice
                                         :coin [:give-coins 1]
                                         :trash [:give-choice {:text    "Trash a card from your hand."
                                                               :choice  :trash-from-hand
                                                               :options [:player :hand]
                                                               :min     1
                                                               :max     1}]
                                         :silver [:gain {:card-name :silver}])]}))

(effects/register {::amulet-choices amulet-choices})

(def amulet-choice {:text    "Choose one:"
                    :choice  ::amulet-choices
                    :options [:special
                              {:option :coin :text "+$1"}
                              {:option :trash :text "Trash a card from your hand."}
                              {:option :silver :text "Gain a Silver."}]
                    :min     1
                    :max     1})

(def amulet {:name    :amulet
             :set     :adventures
             :types   #{:action :duration}
             :cost    3
             :effects [[:give-choice amulet-choice]]
             :trigger {:trigger           :at-start-turn
                       :duration          :once
                       :simultaneous-mode :auto
                       :effects           [[:give-choice amulet-choice]]}})

(defn- caravan-guard-play [game {:keys [player-no card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:id card-id})]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:play-from-hand {:card-name :caravan-guard}]
                                         [:card-effect {:card card}]]})))

(effects/register {::caravan-guard-play caravan-guard-play})

(def caravan-guard {:name      :caravan-guard
                    :set       :adventures
                    :types     #{:action :duration :reaction}
                    :cost      3
                    :effects   [[:draw 1]
                                [:give-actions 1]]
                    :trigger   {:trigger           :at-start-turn
                                :duration          :once
                                :simultaneous-mode :auto
                                :effects           [[:give-coins 1]]}
                    :reacts-to :attack
                    :reaction  [[::caravan-guard-play]]})

(def hireling {:name    :hireling
               :set     :adventures
               :types   #{:action :duration}
               :cost    6
               :trigger {:trigger           :at-start-turn
                         :duration          :game
                         :simultaneous-mode :auto
                         :effects           [[:draw 1]]}})

(def lost-city {:name    :lost-city
                :set     :adventures
                :types   #{:action}
                :cost    5
                :effects [[:draw 2]
                          [:give-actions 2]]
                :on-gain [[:other-players {:effects [[:draw 1]]}]]})

(defn- magpie-check-revealed [game {:keys [player-no]}]
  (let [{:keys [name] :as card} (last (get-in game [:players player-no :revealed]))
        types (ut/get-types game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [(when (:treasure types)
                                           [:take-from-revealed {:card-name name}])
                                         (when (or (:action types) (:victory types))
                                           [:gain {:card-name :magpie}])]})))

(effects/register {::magpie-check-revealed magpie-check-revealed})

(def magpie {:name    :magpie
             :set     :adventures
             :types   #{:action}
             :cost    4
             :effects [[:draw 1]
                       [:give-actions 1]
                       [:reveal-from-deck 1]
                       [::magpie-check-revealed]
                       [:topdeck-all-revealed]]})

(defn- raze-trash-from-area [game {:keys [player-no choice]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no (:area choice)] {:name (:card-name choice)})
        cost (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-area {:choice choice}]
                                         [:look-at cost]
                                         [:give-choice {:text    (str "Look at " cost " cards from the top of your deck. Put one of them into your hand and discard the rest.")
                                                        :choice  :take-from-look-at
                                                        :options [:player :look-at]
                                                        :min     1
                                                        :max     1}]
                                         [:discard-all-look-at]]})))

(effects/register {::raze-trash-from-area raze-trash-from-area})

(def raze {:name    :raze
           :set     :adventures
           :types   #{:action}
           :cost    2
           :effects [[:give-actions 1]
                     [:give-choice {:text    "Trash Raze or a card from your hand."
                                    :choice  ::raze-trash-from-area
                                    :options [:multi
                                              [:player :play-area {:this true}]
                                              [:player :hand]]
                                    :min     1
                                    :max     1}]]})

(def kingdom-cards [amulet
                    caravan-guard
                    hireling
                    lost-city
                    magpie
                    raze])