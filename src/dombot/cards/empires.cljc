(ns dombot.cards.empires
  (:require [dombot.operations :refer [push-effect-stack attack-other-players]]
            [dombot.cards.common :refer [give-coins]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- place-vp-token [game {:keys [card-name]}]
  (let [{:keys [idx]} (ut/get-pile-idx game card-name)]
    (-> game
        (update-in [:supply idx :tokens] concat [{:token-type :victory-point}]))))

(defn- take-vp-tokens [game {:keys [player-no card-name]}]
  (let [{:keys [tokens idx]} (ut/get-pile-idx game card-name)
        vp-tokens (->> tokens
                       (filter (comp #{:victory-point} :token-type))
                       count)]
    (-> game
        (update-in [:supply idx :tokens] (partial remove (comp #{:victory-point} :token-type)))
        (update-in [:supply idx] ut/dissoc-if-empty :tokens)
        (push-effect-stack {:player-no player-no
                            :effects   [[:give-victory-points vp-tokens]]}))))

(effects/register {::place-vp-token place-vp-token
                   ::take-vp-tokens take-vp-tokens})

(defn- chariot-race-compare [{:keys [players] :as game} {:keys [player-no]}]
  (let [next-player (mod (inc player-no) (count players))
        own-card    (first (get-in game [:players player-no :revealed]))
        other-card  (first (get-in game [:players next-player :revealed]))
        race-won?   (and own-card
                         other-card
                         (> (ut/get-cost game own-card) (ut/get-cost game other-card)))]
    (cond-> game
            other-card (push-effect-stack {:player-no next-player
                                           :effects   [[:topdeck-from-revealed {:card-name (:name other-card)}]]})

            own-card (push-effect-stack {:player-no player-no
                                         :effects   (concat [[:put-revealed-into-hand {:card-name (:name own-card)}]]
                                                            (when race-won?
                                                              [[:give-coins 1]
                                                               [:give-victory-points 1]]))}))))

(defn- chariot-race-reveal [{:keys [players] :as game} {:keys [player-no]}]
  (let [next-player (mod (inc player-no) (count players))]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:reveal-from-deck 1]
                                        [::chariot-race-compare]]})
        (push-effect-stack {:player-no next-player
                            :effects   [[:reveal-from-deck 1]]}))))

(effects/register {::chariot-race-compare chariot-race-compare
                   ::chariot-race-reveal  chariot-race-reveal})

(def chariot-race {:name    :chariot-race
                   :set     :empires
                   :types   #{:action}
                   :cost    3
                   :effects [[:give-actions 1]
                             [::chariot-race-reveal]]})

(defn- charm-on-buy [game {:keys [player-no card-name trigger-id]}]
  (let [{card :card} (ut/get-pile-idx game card-name)
        cost (ut/get-cost game card)]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:remove-trigger {:trigger-id trigger-id}]
                                        [:give-choice {:text    (str "You may gain a card other than " (ut/format-name card-name) " costing exactly $" cost ".")
                                                       :choice  :gain
                                                       :options [:supply {:not-names #{card-name}
                                                                          :cost      cost}]
                                                       :max     1}]]}))))


(defn- charm-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :coins [[:give-buys 1]
                                                [:give-coins 2]]
                                        :gain [[:add-trigger {:trigger {:event    :on-buy
                                                                        :duration :once-turn
                                                                        :effects  [[::charm-on-buy]]}}]])}))

(effects/register {::charm-on-buy charm-on-buy
                   ::charm-choice charm-choice})


(def charm {:name    :charm
            :set     :empires
            :types   #{:treasure}
            :cost    5
            :effects [[:give-choice {:text    "Choose one:"
                                     :choice  ::charm-choice
                                     :options [:special
                                               {:option :coins :text "+1 Buy and +$2"}
                                               {:option :gain :text "The next time you buy a card this turn, you may also gain a differently named card with the same cost."}]
                                     :min     1
                                     :max     1}]]})

(defn- farmers-market-reap [game {:keys [player-no card-id]}]
  (let [{:keys [tokens]} (ut/get-pile-idx game :farmers'-market)
        vp-tokens (->> tokens
                       (filter (comp #{:victory-point} :token-type))
                       count)]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   (if (>= vp-tokens 4)
                                         [[:trash-from-play-area {:trash-card-id card-id}]
                                          [::take-vp-tokens {:card-name :farmers'-market}]]
                                         [[::place-vp-token {:card-name :farmers'-market}]
                                          [:give-coins (inc vp-tokens)]])}))))

(effects/register {::farmers-market-reap farmers-market-reap})

(def farmers-market {:name    :farmers'-market
                     :set     :empires
                     :types   #{:action :gathering}
                     :cost    3
                     :effects [[:give-buys 1]
                               [::farmers-market-reap]]})

(def forum {:name    :forum
            :set     :empires
            :types   #{:action}
            :cost    5
            :effects [[:draw 3]
                      [:give-actions 1]
                      [:give-choice {:text    "Discard 2 cards."
                                     :choice  :discard-from-hand
                                     :options [:player :hand]
                                     :min     2
                                     :max     2}]]
            :on-buy  [[:give-buys 1]]})

(defn- legionary-attack [game {:keys [player-no card-name]}]
  (cond-> game
          (= :gold card-name) (attack-other-players {:player-no player-no
                                                     :effects   [[:discard-down-to 2]
                                                                 [:draw 1]]})))

(effects/register {::legionary-attack legionary-attack})

(def legionary {:name    :legionary
                :set     :empires
                :types   #{:action :attack}
                :cost    5
                :effects [[:give-coins 3]
                          [:give-choice {:text    "You may reveal a Gold from your hand."
                                         :choice  ::legionary-attack
                                         :options [:player :hand {:name :gold}]
                                         :max     1}]]})

(defn- sacrifice-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        types (ut/get-types game card)]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   (concat
                                                  [[:trash-from-hand {:card-name card-name}]]
                                                  (when (types :action)
                                                    [[:draw 2]
                                                     [:give-actions 2]])
                                                  (when (types :treasure)
                                                    [[:give-coins 2]])
                                                  (when (types :victory)
                                                    [[:give-victory-points 2]]))}))))

(effects/register {::sacrifice-trash sacrifice-trash})

(def sacrifice {:name    :sacrifice
                :set     :empires
                :types   #{:action}
                :cost    4
                :effects [[:give-choice {:text    "Trash a card from your hand."
                                         :choice  ::sacrifice-trash
                                         :options [:player :hand]
                                         :min     1
                                         :max     1}]]})

(def temple {:name    :temple
             :set     :empires
             :types   #{:action :gathering}
             :cost    4
             :effects [[:give-victory-points 1]
                       [:give-choice {:text    "Trash from 1 to 3 differently named cards from your hand."
                                      :choice  :trash-from-hand
                                      :options [:player :hand]
                                      :unique? true
                                      :min     1
                                      :max     3}]
                       [::place-vp-token {:card-name :temple}]]
             :on-gain [[::take-vp-tokens {:card-name :temple}]]})

(def kingdom-cards [chariot-race
                    charm
                    farmers-market
                    forum
                    legionary
                    sacrifice
                    temple])


(defn- advance-trash [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:trash-from-hand {:card-name card-name}]
                                                    [:give-choice {:text    "Gain a Action card costing up to $6."
                                                                   :choice  :gain
                                                                   :options [:supply {:type     :action
                                                                                      :max-cost 6}]
                                                                   :min     1
                                                                   :max     1}]]})))

(effects/register {::advance-trash advance-trash})

(def advance {:name   :advance
              :set    :empires
              :type   :event
              :cost   0
              :on-buy [[:give-choice {:text    "You may trash an Action card from your hand."
                                      :choice  ::advance-trash
                                      :options [:player :hand {:type :action}]
                                      :max     1}]]})

(def banquet {:name   :banquet
              :set    :empires
              :type   :event
              :cost   3
              :on-buy [[:gain {:card-name :copper}]
                       [:gain {:card-name :copper}]
                       [:give-choice {:text    "Gain a non-Victory card costing up to $5."
                                      :choice  :gain
                                      :options [:supply {:not-type :victory
                                                         :max-cost 5}]
                                      :min     1
                                      :max     1}]]})

(defn- conquest-victory-points [game {:keys [player-no]}]
  (let [gained-silvers (->> (get-in game [:players player-no :gained-cards])
                            (filter (comp #{:silver} :name))
                            count)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:give-victory-points gained-silvers]]})))

(effects/register {::conquest-victory-points conquest-victory-points})

(def conquest {:name   :conquest
               :set    :empires
               :type   :event
               :cost   6
               :on-buy [[:gain {:card-name :silver}]
                        [:gain {:card-name :silver}]
                        [::conquest-victory-points]]})

(def delve {:name   :delve
            :set    :empires
            :type   :event
            :cost   2
            :on-buy [[:give-buys 1]
                     [:gain {:card-name :silver}]]})

(defn- dominate-gain-province [game {:keys [player-no]}]
  (let [{:keys [pile-size]} (ut/get-pile-idx game :province)]
    (cond-> game
            (pos? pile-size) (push-effect-stack {:player-no player-no
                                                 :effects   [[:gain {:card-name :province}]
                                                             [:give-victory-points 9]]}))))

(effects/register {::dominate-gain-province dominate-gain-province})

(def dominate {:name   :dominate
               :set    :empires
               :type   :event
               :cost   14
               :on-buy [[::dominate-gain-province]]})

(defn- ritual-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:trash-from-hand {:card-name card-name}]
                                                 [:give-victory-points cost]]}))))

(defn- ritual-gain-curse [game {:keys [player-no]}]
  (let [{:keys [pile-size]} (ut/get-pile-idx game :curse)]
    (cond-> game
            (pos? pile-size) (push-effect-stack {:player-no player-no
                                                 :effects   [[:gain {:card-name :curse}]
                                                             [:give-choice {:text    "Trash a card from your hand."
                                                                            :choice  ::ritual-trash
                                                                            :options [:player :hand]
                                                                            :min     1
                                                                            :max     1}]]}))))

(effects/register {::ritual-trash      ritual-trash
                   ::ritual-gain-curse ritual-gain-curse})

(def ritual {:name   :ritual
             :set    :empires
             :type   :event
             :cost   4
             :on-buy [[::ritual-gain-curse]]})

(def salt-the-earth {:name   :salt-the-earth
                     :set    :empires
                     :type   :event
                     :cost   4
                     :on-buy [[:give-victory-points 1]
                              [:give-choice {:text    "Trash a Victory card from the Supply."
                                             :choice  :trash-from-supply
                                             :options [:supply {:type :victory}]
                                             :min     1
                                             :max     1}]]})

(defn- windfall-gain-gold [game {:keys [player-no]}]
  (let [deck    (get-in game [:players player-no :deck])
        discard (get-in game [:players player-no :discard])]
    (cond-> game
            (and (empty? deck)
                 (empty? discard)) (push-effect-stack {:player-no player-no
                                                       :effects   (repeat 3 [:gain {:card-name :gold}])}))))

(effects/register {::windfall-gain-gold windfall-gain-gold})

(def windfall {:name   :windfall
               :set    :empires
               :type   :event
               :cost   5
               :on-buy [[::windfall-gain-gold]]})

(def events [advance
             banquet
             conquest
             delve
             dominate
             ritual
             salt-the-earth
             windfall])
