(ns dombot.cards.dark-ages
  (:require [dombot.operations :refer [push-effect-stack give-choice]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def beggar {:name      :beggar
             :set       :dark-ages
             :types     #{:action :reaction}
             :cost      2
             :effects   [[:gain-to-hand {:card-name :copper}]
                         [:gain-to-hand {:card-name :copper}]
                         [:gain-to-hand {:card-name :copper}]]
             :reacts-to :attack
             :reaction  [[:discard-from-hand {:card-name :beggar}]
                         [:gain-to-topdeck {:card-name :silver}]
                         [:gain {:card-name :silver}]]})

(defn- forager-give-coins [game {:keys [player-no]}]
  (let [different-treasures (->> game
                                 :trash
                                 (filter (comp :treasure (partial ut/get-types game)))
                                 (map :name)
                                 distinct
                                 count)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:give-coins different-treasures]]})))

(effects/register {::forager-give-coins forager-give-coins})

(def forager {:name    :forager
              :set     :dark-ages
              :types   #{:action}
              :cost    3
              :effects [[:give-actions 1]
                        [:give-buys 1]
                        [:give-choice {:text    "Trash a card from your hand."
                                       :choice  :trash-from-hand
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]
                        [::forager-give-coins]]})

(defn- poor-house-lose-coins [game {:keys [player-no]}]
  (let [{:keys [coins hand]} (get-in game [:players player-no])
        treasures-in-hand (->> hand
                               (filter (comp :treasure (partial ut/get-types game)))
                               count)]
    (assoc-in game [:players player-no :coins] (max 0 (- coins treasures-in-hand)))))

(effects/register {::poor-house-lose-coins poor-house-lose-coins})

(def poor-house {:name    :poor-house
                 :set     :dark-ages
                 :types   #{:action}
                 :cost    1
                 :effects [[:give-coins 4]
                           [:reveal-hand]
                           [::poor-house-lose-coins]]})

(defn squire-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [(case choice
                                         :actions [:give-actions 2]
                                         :buys [:give-buys 2]
                                         :silver [:gain {:card-name :silver}])]}))

(effects/register {::squire-choice squire-choice})

(def squire {:name     :squire
             :set      :dark-ages
             :types    #{:action}
             :cost     2
             :effects  [[:give-coins 1]
                        [:give-choice {:text    "Choose one:"
                                       :choice  ::squire-choice
                                       :options [:special
                                                 {:option :actions :text "+2 Actions"}
                                                 {:option :buys :text "+2 Buys"}
                                                 {:option :silver :text "Gain a Silver"}]
                                       :min     1
                                       :max     1}]]
             :on-trash [[:give-choice {:text    "Gain an Attack card."
                                       :choice  :gain
                                       :options [:supply {:type :attack}]
                                       :min     1
                                       :max     1}]]})

(def kingdom-cards [beggar
                    forager
                    poor-house
                    squire])
