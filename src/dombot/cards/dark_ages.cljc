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

(def kingdom-cards [beggar
                    poor-house])
