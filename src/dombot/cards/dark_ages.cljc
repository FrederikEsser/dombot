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

(def altar {:name    :altar
            :set     :dark-ages
            :types   #{:action}
            :cost    6
            :effects [[:give-choice {:text    "Trash a card from your hand."
                                     :choice  :trash-from-hand
                                     :options [:player :hand]
                                     :min     1
                                     :max     1}]
                      [:give-choice {:text    "Gain a card costing up to $5."
                                     :choice  :gain
                                     :options [:supply {:max-cost 5}]
                                     :min     1
                                     :max     1}]]})

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

(defn- counterfeit-treasure [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:repeat-action {:card-name card-name
                                                                     :times     2}]
                                                    [:trash-from-play-area {:card-name card-name}]]})))

(effects/register {::counterfeit-treasure counterfeit-treasure})

(def counterfeit {:name            :counterfeit
                  :set             :dark-ages
                  :types           #{:treasure}
                  :cost            5
                  :coin-value      1
                  :effects         [[:give-buys 1]
                                    [:give-choice {:text    "You may play a Treasure from your hand twice."
                                                   :choice  ::counterfeit-treasure
                                                   :options [:player :hand {:type :treasure}]
                                                   :max     1}]]
                  :auto-play-index -1})

(defn feodum-victory-points [cards]
  (quot (->> cards
             (filter (comp #{:silver} :name))
             count)
        3))

(effects/register {::feodum-victory-points feodum-victory-points})

(def feodum {:name           :feodum
             :set            :dark-ages
             :types          #{:victory}
             :cost           4
             :victory-points ::feodum-victory-points
             :on-trash       [[:gain {:card-name :silver}]
                              [:gain {:card-name :silver}]
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

(defn- fortress-trashed [game {:keys [player-no card-id]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:move-card {:move-card-id card-id
                                                    :from         :trash
                                                    :to           :hand}]]}))

(effects/register {::fortress-trashed fortress-trashed})

(def fortress {:name     :fortress
               :set      :dark-ages
               :types    #{:action}
               :cost     4
               :effects  [[:draw 1]
                          [:give-actions 2]]
               :on-trash [[::fortress-trashed]]})

(defn- hunting-grounds-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :duchy [[:gain {:card-name :duchy}]]
                                        :estates [[:gain {:card-name :estate}]
                                                  [:gain {:card-name :estate}]
                                                  [:gain {:card-name :estate}]])}))

(effects/register {::hunting-grounds-choice hunting-grounds-choice})

(def hunting-grounds {:name     :hunting-grounds
                      :set      :dark-ages
                      :types    #{:action}
                      :cost     6
                      :effects  [[:draw 4]]
                      :on-trash [[:give-choice {:text    "Gain"
                                                :choice  ::hunting-grounds-choice
                                                :options [:special
                                                          {:option :duchy :text "a Duchy"}
                                                          {:option :estates :text "3 Estates"}]
                                                :min     1
                                                :max     1}]]})

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

(def wandering-minstrel {:name    :wandering-minstrel
                         :set     :dark-ages
                         :types   #{:action}
                         :cost    4
                         :effects [[:draw 1]
                                   [:give-actions 2]
                                   [:reveal-from-deck 3]
                                   [:give-choice {:text    "Put the Action cards back in any order."
                                                  :choice  :topdeck-from-revealed
                                                  :options [:player :revealed {:type :action}]
                                                  :min     3}]
                                   [:discard-all-revealed]]})

(def kingdom-cards [altar
                    armory
                    bandit-camp
                    beggar
                    counterfeit
                    feodum
                    forager
                    fortress
                    hunting-grounds
                    poor-house
                    squire
                    vagrant
                    wandering-minstrel])

(defn- hovel-on-buy [game {:keys [player-no card]}]
  (let [types (ut/get-types game card)]
    (cond-> game
            (:victory types) (give-choice {:player-no player-no
                                           :text      "You may trash a Hovel from your hand."
                                           :choice    :trash-from-hand
                                           :options   [:player :hand {:name :hovel}]
                                           :max       1}))))

(effects/register {::hovel-on-buy hovel-on-buy})

(def hovel {:name     :hovel
            :set      :dark-ages
            :types    #{:reaction :shelter}
            :cost     1
            :reaction {:on-buy [[::hovel-on-buy]]}})

(def necropolis {:name    :necropolis
                 :set     :dark-ages
                 :types   #{:action :shelter}
                 :cost    1
                 :effects [[:give-actions 2]]})

(def overgrown-estate {:name     :overgrown-estate
                       :set      :dark-ages
                       :types    #{:victory :shelter}
                       :cost     1
                       :on-trash [[:draw 1]]})


(def shelters [hovel
               necropolis
               overgrown-estate])
