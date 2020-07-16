(ns dombot.cards.dark-ages
  (:require [dombot.operations :refer [push-effect-stack give-choice]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]
            [clojure.set :refer [intersection]]))

(def spoils {:name       :spoils
             :set        :dark-ages
             :types      #{:treasure}
             :cost       0
             :coin-value 3
             :effects    [[:return-this-to-supply {:area :extra-cards}]]})

(def armory {:name    :armory
             :set     :dark-ages
             :types   #{:action}
             :cost    4
             :effects [[:give-choice {:text    "Gain a card onto your deck costing up to $4."
                                      :choice  :gain-to-topdeck
                                      :options [:supply {:max-cost 4}]
                                      :min     1
                                      :max     1}]]})

(def bandit-camp {:name    :bandit-camp
                  :set     :dark-ages
                  :types   #{:action}
                  :cost    5
                  :effects [[:draw 1]
                            [:give-actions 2]
                            [:gain {:card-name :spoils
                                    :from      :extra-cards}]]
                  :setup   [[:setup-extra-cards {:extra-cards [{:card spoils :pile-size 15}]}]]})

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

(defn- vagrant-check-revealed [game {:keys [player-no]}]
  (let [{:keys [name] :as card} (last (get-in game [:players player-no :revealed]))
        take-card? (->> (ut/get-types game card)
                        (intersection #{:curse :ruins :shelter :victory})
                        not-empty)]
    (cond-> game
            take-card? (push-effect-stack {:player-no player-no
                                           :effects   [[:take-from-revealed {:card-name name}]]}))))

(effects/register {::vagrant-check-revealed vagrant-check-revealed})

(def vagrant {:name    :vagrant
              :set     :dark-ages
              :types   #{:action}
              :cost    2
              :effects [[:draw 1]
                        [:give-actions 1]
                        [:reveal-from-deck 1]
                        [::vagrant-check-revealed]
                        [:topdeck-all-revealed]]})

(def kingdom-cards [armory
                    bandit-camp
                    beggar
                    forager
                    poor-house
                    squire
                    vagrant])
