(ns dombot.cards.nocturne
  (:require [dombot.operations :refer [push-effect-stack give-choice move-card]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- gain-to-hand [game {:keys [player-no gained-card-id] :as args}]
  (move-card game {:player-no    player-no
                   :move-card-id gained-card-id
                   :from         :gaining
                   :to           :hand}))

(effects/register {::gain-to-hand gain-to-hand})

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

(defn- tragic-hero-demise [game {:keys [player-no card-id] :as args}]
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

(def kingdom-cards [conclave
                    ghost-town
                    monastery
                    tragic-hero])
