(ns dombot.cards.cornucopia
  (:require [dombot.operations :refer [push-effect-stack]]
            [dombot.cards.common :refer [reveal-hand]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- farming-village-reveal [game {:keys [player-no]}]
  (let [{:keys [revealed deck discard]} (get-in game [:players player-no])
        {:keys [types name] :as card} (last revealed)]
    (cond (or (:treasure types)
              (:action types)) (push-effect-stack game {:player-no player-no
                                                        :effects   [[:move-card {:card-name name
                                                                                 :from      :revealed
                                                                                 :to        :hand}]]})
          (not-empty (concat deck discard)) (push-effect-stack game {:player-no player-no
                                                                     :effects   [[:reveal-from-deck 1]
                                                                                 [::farming-village-reveal]]})
          :else game)))

(effects/register {::farming-village-reveal farming-village-reveal})

(def farming-village {:name    :farming-village
                      :set     :cornucopia
                      :types   #{:action}
                      :cost    4
                      :effects [[:give-actions 2]
                                [::farming-village-reveal]
                                [:discard-all-revealed]]})

(defn- hamlet-give-action [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-actions 1]]})))

(defn- hamlet-give-buy [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-buys 1]]})))

(effects/register {::hamlet-give-action hamlet-give-action
                   ::hamlet-give-buy    hamlet-give-buy})

(def hamlet {:name    :hamlet
             :set     :cornucopia
             :types   #{:action}
             :cost    2
             :effects [[:draw 1]
                       [:give-actions 1]
                       [:give-choice {:text    "You may discard a card for +1 Action."
                                      :choice  ::hamlet-give-action
                                      :options [:player :hand]
                                      :max     1}]
                       [:give-choice {:text    "You may discard a card for +1 Buy."
                                      :choice  ::hamlet-give-buy
                                      :options [:player :hand]
                                      :max     1}]]})

(defn- menagerie-draw [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])
        different-names? (->> hand
                              (map :name)
                              (apply distinct?))]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:reveal-hand]
                                         [:draw (if different-names? 3 1)]]})))

(effects/register {::menagerie-draw menagerie-draw})

(def menagerie {:name    :menagerie
                :set     :cornucopia
                :types   #{:action}
                :cost    3
                :effects [[:give-actions 1]
                          [::menagerie-draw]]})

(def remake {:name    :remake
             :set     :cornucopia
             :types   #{:action}
             :cost    4
             :effects [[:upgrade-give-choice]
                       [:upgrade-give-choice]]})

(def kingdom-cards [farming-village
                    hamlet
                    menagerie
                    remake])