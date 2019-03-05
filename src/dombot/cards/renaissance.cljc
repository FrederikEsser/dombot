(ns dombot.cards.renaissance
  (:require [dombot.operations :refer [push-effect-stack give-choice draw]]
            [dombot.cards.common :refer [reveal-hand]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def acting-troupe {:name    :acting-troupe
                    :set     :renaissance
                    :types   #{:action}
                    :cost    3
                    :effects [[:give-villagers 4]
                              [:trash-this]]})

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
                                              [:give-choice {:text      "You may trash a Curse from your hand."
                                                             :choice    :trash-from-hand
                                                             :options   [:player :hand {:name :curse}]
                                                             :max       1}]]}]]})

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
                    experiment
                    hideout
                    lackeys
                    mountain-village
                    old-witch
                    recruiter
                    scholar
                    villain])
