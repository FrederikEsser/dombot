(ns dombot.cards.cornucopia
  (:require [dombot.operations :refer [push-effect-stack give-choice gain]]
            [dombot.cards.common :refer [reveal-hand give-coins]]
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

(defn- fortune-teller-reveal [game {:keys [player-no]}]
  (let [{:keys [revealed deck discard]} (get-in game [:players player-no])
        {:keys [types name] :as card} (last revealed)]
    (cond (or (:victory types)
              (:curse types)) (push-effect-stack game {:player-no player-no
                                                       :effects   [[:move-card {:card-name   name
                                                                                :from        :revealed
                                                                                :to          :deck
                                                                                :to-position :top}]]})
          (not-empty (concat deck discard)) (push-effect-stack game {:player-no player-no
                                                                     :effects   [[:reveal-from-deck 1]
                                                                                 [::fortune-teller-reveal]]})
          :else game)))

(effects/register {::fortune-teller-reveal fortune-teller-reveal})

(def fortune-teller {:name    :fortune-teller
                     :set     :cornucopia
                     :types   #{:action :attack}
                     :cost    3
                     :effects [[:give-coins 2]
                               [:attack {:effects [[::fortune-teller-reveal]
                                                   [:discard-all-revealed]]}]]})

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

(defn- harvest-give-coins [game {:keys [player-no]}]
  (let [revealed (get-in game [:players player-no :revealed])]
    (give-coins game {:player-no player-no :arg (->> revealed (map :name) set count)})))

(effects/register {::harvest-give-coins harvest-give-coins})

(def harvest {:name    :harvest
              :set     :cornucopia
              :types   #{:action}
              :cost    5
              :effects [[:reveal-from-deck 4]
                        [::harvest-give-coins]
                        [:discard-all-revealed]]})

(defn- horn-of-plenty-gain [game {:keys [player-no card-id card-name]}]
  (let [{{:keys [types]} :card} (ut/get-pile-idx game card-name)]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat [[:gain {:card-name card-name}]]
                                                (when (:victory types)
                                                  [[:trash-this {:card-id card-id}]]))})))

(defn- horn-of-plenty-give-choice [game {:keys [player-no card-id]}]
  (let [different-cards-in-play (->> (get-in game [:players player-no :play-area])
                                     (map :name)
                                     set
                                     count)]
    (give-choice game {:player-no player-no
                       :card-id   card-id
                       :text      (str "Gain a card costing up to $" different-cards-in-play ".")
                       :choice    ::horn-of-plenty-gain
                       :options   [:supply {:max-cost different-cards-in-play}]
                       :min       1
                       :max       1})))

(effects/register {::horn-of-plenty-gain        horn-of-plenty-gain
                   ::horn-of-plenty-give-choice horn-of-plenty-give-choice})

(def horn-of-plenty {:name       :horn-of-plenty
                     :set        :cornucopia
                     :types      #{:treasure}
                     :cost       5
                     :coin-value 0
                     :effects    [[::horn-of-plenty-give-choice]]
                     :auto-play-index 2})

(defn- hunting-party-reveal [game {:keys [player-no]}]
  (let [{:keys [hand revealed deck discard]} (get-in game [:players player-no])
        hand-card-names (->> hand (map :name) set)
        {:keys [name] :as card} (last revealed)]
    (cond (and card
               (not (hand-card-names name))) (push-effect-stack game {:player-no player-no
                                                                      :effects   [[:move-card {:card-name name
                                                                                               :from      :revealed
                                                                                               :to        :hand}]]})
          (not-empty (concat deck discard)) (push-effect-stack game {:player-no player-no
                                                                     :effects   [[:reveal-from-deck 1]
                                                                                 [::hunting-party-reveal]]})
          :else game)))

(effects/register {::hunting-party-reveal hunting-party-reveal})

(def hunting-party {:name    :hunting-party
                    :set     :cornucopia
                    :types   #{:action}
                    :cost    5
                    :effects [[:draw 1]
                              [:give-actions 1]
                              [:reveal-hand]
                              [::hunting-party-reveal]
                              [:discard-all-revealed]]})

(defn- jester-gain-copy [game {:keys [card-name choice]}]
  (gain game {:player-no choice
              :card-name card-name}))

(defn- jester-give-choice [game {:keys [player-no attacking-player-no]}]
  (let [{:keys [name types] :as card} (last (get-in game [:players player-no :discard]))]
    (if (:victory types)
      (gain game {:player-no player-no
                  :card-name :curse})
      (cond-> game
              card (give-choice {:player-no attacking-player-no
                                 :text      (str "Who gains a " (ut/format-name name) "?")
                                 :choice    [::jester-gain-copy {:card-name name}]
                                 :options   [:special
                                             {:option player-no :text (-> (get-in game [:players player-no :name]) ut/format-name)}
                                             {:option attacking-player-no :text (-> (get-in game [:players attacking-player-no :name]) ut/format-name)}]
                                 :min       1
                                 :max       1})))))

(effects/register {::jester-gain-copy   jester-gain-copy
                   ::jester-give-choice jester-give-choice})

(def jester {:name    :jester
             :set     :cornucopia
             :types   #{:action :attack}
             :cost    5
             :effects [[:give-coins 2]
                       [:attack {:effects [[:discard-from-topdeck 1]
                                           [::jester-give-choice]]}]]})

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
                    fortune-teller
                    hamlet
                    harvest
                    horn-of-plenty
                    hunting-party
                    jester
                    menagerie
                    remake])