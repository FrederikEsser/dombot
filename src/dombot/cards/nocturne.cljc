(ns dombot.cards.nocturne
  (:require [dombot.operations :refer [push-effect-stack give-choice move-card attack-other-players]]
            [dombot.cards.common :refer [reveal-hand]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- gain-to-hand [game {:keys [player-no gained-card-id] :as args}]
  (move-card game {:player-no    player-no
                   :move-card-id gained-card-id
                   :from         :gaining
                   :to           :hand}))

(effects/register {::gain-to-hand gain-to-hand})

(def cobbler {:name    :cobbler
              :set     :nocturne
              :types   #{:night :duration}
              :cost    5
              :trigger {:trigger           :at-start-turn
                        :duration          :once
                        :simultaneous-mode :auto
                        :effects           [[:give-choice {:text    "Gain a card to your hand costing up to $4."
                                                           :choice  :gain-to-hand
                                                           :options [:supply {:max-cost 4}]
                                                           :min     1
                                                           :max     1}]]}})

(defn- conclave-play-action [game {:keys [player-no card-name]}]
  (let [{card :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:play-from-hand {:card-name card-name}]
                                                 [:card-effect {:card card}]
                                                 [:give-actions 1]]}))))

(defn- conclave-give-choice [game {:keys [player-no]}]
  (let [actions-in-play (->> (get-in game [:players player-no :play-area])
                             (filter (comp :action :types))
                             (map :name)
                             set)]
    (give-choice game {:player-no player-no
                       :text      "You may play an Action card from your hand that you don't have a copy of in play."
                       :choice    ::conclave-play-action
                       :options   [:player :hand {:type     :action
                                                  :not-name actions-in-play}]
                       :max       1})))

(effects/register {::conclave-play-action conclave-play-action
                   ::conclave-give-choice conclave-give-choice})

(def conclave {:name    :conclave
               :set     :nocturne
               :types   #{:action}
               :cost    4
               :effects [[:give-coins 2]
                         [::conclave-give-choice]]})

(def den-of-sin {:name    :den-of-sin
                 :set     :nocturne
                 :types   #{:night :duration}
                 :cost    5
                 :trigger {:trigger           :at-start-turn
                           :duration          :once
                           :simultaneous-mode :auto
                           :effects           [[:draw 2]]}
                 :on-gain [[::gain-to-hand]]})

(def ghost-town {:name    :ghost-town
                 :set     :nocturne
                 :types   #{:night :duration}
                 :cost    3
                 :trigger {:trigger           :at-start-turn
                           :duration          :once
                           :simultaneous-mode :auto
                           :effects           [[:draw 1]
                                               [:give-actions 1]]}
                 :on-gain [[::gain-to-hand]]})

(defn- monastery-trash [game {:keys [player-no]}]
  (let [gained-cards (count (get-in game [:players player-no :gained-cards]))]
    (cond-> game
            (pos? gained-cards) (give-choice {:player-no player-no
                                              :text      (str "Trash up to " gained-cards " card" (when (< 1 gained-cards) "s")
                                                              " from your hand or Coppers you have in play.")
                                              :choice    :trash-from-area
                                              :options   [:multi
                                                          [:player :hand]
                                                          [:player :play-area {:name :copper}]]
                                              :max       gained-cards}))))

(effects/register {::monastery-trash monastery-trash})

(def monastery {:name    :monastery
                :set     :nocturne
                :types   #{:night}
                :cost    2
                :effects [[::monastery-trash]]})

(def night-watchman {:name    :night-watchman
                     :set     :nocturne
                     :types   #{:night}
                     :cost    3
                     :effects [[:look-at 5]
                               [:give-choice {:text    "Discard any number of the top 5 cards of your deck."
                                              :choice  :discard-from-look-at
                                              :options [:player :look-at]}]
                               [:topdeck-all-look-at]
                               #_[:give-choice {:text    "Put the rest back on top in any order."
                                                :choice  :topdeck-from-look-at
                                                :options [:player :look-at]
                                                :min     5}]]
                     :on-gain [[::gain-to-hand]]})

(defn- raider-attack [game {:keys [player-no card-names]}]
  (let [hand               (get-in game [:players player-no :hand])
        has-eligible-card? (some (comp card-names :name) hand)]
    (cond (< (count hand) 5) game
          has-eligible-card? (give-choice game {:player-no player-no
                                                :text      "Discard a copy of a card the attacker has in play."
                                                :choice    :discard-from-hand
                                                :options   [:player :hand {:names card-names}]
                                                :min       1
                                                :max       1})
          :else (reveal-hand game {:player-no player-no}))))

(defn- make-raider-attack [game {:keys [player-no]}]
  (let [card-names (->> (get-in game [:players player-no :play-area])
                        (map :name)
                        set)]
    (attack-other-players game {:player-no player-no
                                :effects   [[::raider-attack {:card-names card-names}]]})))

(effects/register {::raider-attack      raider-attack
                   ::make-raider-attack make-raider-attack})

(def raider {:name    :raider
             :set     :nocturne
             :types   #{:night :duration :attack}
             :cost    6
             :effects [[::make-raider-attack]]
             :trigger {:trigger           :at-start-turn
                       :duration          :once
                       :simultaneous-mode :auto
                       :effects           [[:give-coins 3]]}})

(defn- tragic-hero-demise [game {:keys [player-no card-id]}]
  (let [hand-size (count (get-in game [:players player-no :hand]))]
    (cond-> game
            (<= 8 hand-size) (push-effect-stack {:player-no player-no
                                                 :effects   [[:trash-from-play-area {:trash-card-id card-id}]
                                                             [:give-choice {:text    "Gain a Treasure."
                                                                            :choice  :gain
                                                                            :options [:supply {:type :treasure}]
                                                                            :min     1
                                                                            :max     1}]]}))))

(effects/register {::tragic-hero-demise tragic-hero-demise})

(def tragic-hero {:name    :tragic-hero
                  :set     :nocturne
                  :types   #{:action}
                  :cost    5
                  :effects [[:draw 3]
                            [:give-buys 1]
                            [::tragic-hero-demise]]})

(def kingdom-cards [cobbler
                    conclave
                    den-of-sin
                    ghost-town
                    monastery
                    night-watchman
                    raider
                    tragic-hero])
