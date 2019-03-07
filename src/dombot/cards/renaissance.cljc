(ns dombot.cards.renaissance
  (:require [dombot.operations :refer [push-effect-stack give-choice draw move-cards]]
            [dombot.cards.common :refer [reveal-hand]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def acting-troupe {:name    :acting-troupe
                    :set     :renaissance
                    :types   #{:action}
                    :cost    3
                    :effects [[:give-villagers 4]
                              [:trash-this]]})

(def ducat {:name       :ducat
            :set        :renaissance
            :types      #{:treasure}
            :cost       2
            :coin-value 0
            :effects    [[:give-coffers 1]
                         [:give-buys 1]]
            :on-gain    [[:give-choice {:text    "You may trash a Copper from your hand."
                                        :choice  :trash-from-hand
                                        :options [:player :hand {:name :copper}]
                                        :max     1}]]})

(def experiment {:name    :experiment
                 :set     :renaissance
                 :types   #{:action}
                 :cost    3
                 :effects [[:draw 2]
                           [:give-actions 1]
                           [:return-this-to-supply]]
                 :on-gain [[:do-gain {:card-name :experiment}]]}) ; todo: Handle on-gain effects

(defn hideout-trash [game {:keys [player-no card-name] :as args}]
  (let [{{:keys [types]} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (push-effect-stack game (merge args {:effects (concat [[:trash-from-hand {:card-name card-name}]]
                                                          (when (:victory types)
                                                            [[:gain {:card-name :curse}]]))}))))

(effects/register {::hideout-trash hideout-trash})

(def hideout {:name    :hideout
              :set     :renaissance
              :types   #{:action}
              :cost    4
              :effects [[:draw 1]
                        [:give-actions 2]
                        [:give-choice {:text    "Trash a card from your hand."
                                       :choice  ::hideout-trash
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]]})

(def inventor {:name    :inventor
               :set     :renaissance
               :types   #{:action}
               :cost    4
               :effects [[:give-choice {:text    "Gain a card costing up to $4."
                                        :choice  :gain
                                        :options [:supply {:max-cost 4}]
                                        :min     1
                                        :max     1}]
                         [:add-cost-reduction 1]]})

(def lackeys {:name    :lackeys
              :set     :renaissance
              :types   #{:action}
              :cost    2
              :effects [[:draw 2]]
              :on-gain [[:give-villagers 2]]})

(defn mountain-village-draw [game {:keys [player-no]}]
  (let [discard (get-in game [:players player-no :discard])]
    (if (empty? discard)
      (draw game {:player-no player-no
                  :arg       1})
      (give-choice game {:player-no player-no
                         :text      "Look through your discard pile and put a card from it into your hand."
                         :choice    :take-from-discard
                         :options   [:player :discard]
                         :min       1
                         :max       1}))))

(effects/register {::mountain-village-draw mountain-village-draw})

(def mountain-village {:name    :mountain-village
                       :set     :renaissance
                       :types   #{:action}
                       :cost    4
                       :effects [[:give-actions 2]
                                 [::mountain-village-draw]]})

(def old-witch {:name    :old-witch
                :set     :renaissance
                :types   #{:action :attack}
                :cost    5
                :effects [[:draw 3]
                          [:attack {:effects [[:gain {:card-name :curse}]
                                              [:give-choice {:text    "You may trash a Curse from your hand."
                                                             :choice  :trash-from-hand
                                                             :options [:player :hand {:name :curse}]
                                                             :max     1}]]}]]})

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

(defn researcher-set-aside [game {:keys [player-no card-id]}]
  (let [set-aside (get-in game [:players player-no :set-aside])]
    (-> game
        (cond-> (not-empty set-aside) (ut/update-in-vec [:players player-no :play-area] {:id card-id}
                                                        (fn [researcher]
                                                          (-> researcher
                                                              (update :set-aside concat set-aside)
                                                              (update :at-start-turn concat [(for [card-name (map :name set-aside)]
                                                                                               [:put-set-aside-into-hand {:card-name card-name}])])))))
        (update-in [:players player-no] dissoc :set-aside))))

(defn researcher-trash [game {:keys [player-no card-name] :as args}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (push-effect-stack game (merge args {:effects [[:trash-from-hand {:card-name card-name}]
                                                   [:set-aside {:number-of-cards cost}]
                                                   [::researcher-set-aside]]}))))

(effects/register {::researcher-set-aside researcher-set-aside
                   ::researcher-trash     researcher-trash})

(def researcher {:name    :researcher
                 :set     :renaissance
                 :types   #{:action :duration}
                 :cost    4
                 :effects [[:give-actions 1]
                           [:give-choice {:text    "Trash a card from your hand."
                                          :choice  ::researcher-trash
                                          :options [:player :hand]
                                          :min     1
                                          :max     1}]]})

(def scholar {:name    :scholar
              :set     :renaissance
              :types   #{:action}
              :cost    5
              :effects [[:discard-all-hand]
                        [:draw 7]]})

(def silk-merchant {:name     :silk-merchant
                    :set      :renaissance
                    :types    #{:action}
                    :cost     4
                    :effects  [[:draw 2]
                               [:give-buys 1]]
                    :on-gain  [[:give-coffers 1]
                               [:give-villagers 1]]
                    :on-trash [[:give-coffers 1]
                               [:give-villagers 1]]})

(def spices {:name       :spices
             :set        :renaissance
             :types      #{:treasure}
             :cost       5
             :coin-value 2
             :effects    [[:give-buys 1]]
             :on-gain    [[:give-coffers 2]]})

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
                    ducat
                    experiment
                    hideout
                    inventor
                    lackeys
                    mountain-village
                    old-witch
                    recruiter
                    researcher
                    scholar
                    silk-merchant
                    spices
                    villain])
