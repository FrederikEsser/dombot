(ns dombot.cards.guilds
  (:require [dombot.operations :refer [push-effect-stack give-choice draw move-card move-cards]]
            [dombot.cards.common :refer [reveal-hand trash-from-look-at]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def advisor {:name    :advisor
              :set     :guilds
              :types   #{:action}
              :cost    4
              :effects [[:give-actions 1]
                        [:reveal-from-deck 3]
                        [:give-choice {:text       "Choose which of the revealed cards will be discarded."
                                       :choice     :discard-from-revealed
                                       :options    [:player :revealed]
                                       :min        1
                                       :max        1
                                       :hide-hand? true}]
                        [:put-all-revealed-into-hand]]})

(def baker {:name    :baker
            :set     :guilds
            :types   #{:action}
            :cost    5
            :effects [[:draw 1]
                      [:give-actions 1]
                      [:give-coffers 1]]
            :setup   [[:all-players {:effects [[:give-coffers 1]]}]]})

(defn- butcher-gain [game {:keys [player-no card-name trashed-card-cost]}]
  (let [{:keys [card]} (ut/get-pile-idx game card-name)
        {:keys [coin-cost]} (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:remove-coffers (- coin-cost (:coin-cost trashed-card-cost))]
                                         [:gain {:card-name card-name}]]})))

(defn- butcher-trash [game {:keys [player-no card-name]}]
  (if card-name
    (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
          cost    (ut/get-cost game card)
          coffers (or (get-in game [:players player-no :coffers]) 0)]
      (push-effect-stack game {:player-no player-no
                               :effects   [[:trash-from-hand {:card-name card-name}]
                                           [:give-choice {:text    (str "Gain a card costing up to " (ut/format-cost cost) " plus any number of Coffers you spend.")
                                                          :choice  [::butcher-gain {:trashed-card-cost cost}]
                                                          :options [:supply {:max-cost (ut/add-to-cost cost coffers)}]
                                                          :min     1
                                                          :max     1}]]}))
    game))

(effects/register {::butcher-gain  butcher-gain
                   ::butcher-trash butcher-trash})

(def butcher {:name    :butcher
              :set     :guilds
              :types   #{:action}
              :cost    5
              :effects [[:give-coffers 2]
                        [:give-choice {:text    "You may trash a card from your hand."
                                       :choice  ::butcher-trash
                                       :options [:player :hand]
                                       :max     1}]]})

(def candlestick-maker {:name    :candlestick-maker
                        :set     :guilds
                        :types   #{:action}
                        :cost    2
                        :effects [[:give-actions 1]
                                  [:give-buys 1]
                                  [:give-coffers 1]]})

(defn- doctor-trash [game {:keys [player-no card-name]}]
  (let [card-names (->> (get-in game [:players player-no :revealed])
                        (map :name)
                        (filter #{card-name}))]
    (move-cards game {:player-no  player-no
                      :card-names card-names
                      :from       :revealed
                      :to         :trash})))

(defn- doctor-reveal [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:reveal-from-deck 3]
                                       [::doctor-trash {:card-name card-name}]
                                       [:give-choice {:text    "Put the revealed cards back onto your deck in any order."
                                                      :choice  :topdeck-from-revealed
                                                      :options [:player :revealed]
                                                      :min     3
                                                      :max     3}]]}))

(defn- doctor-choice [game {:keys [player-no choice]}]
  (let [[{:keys [name]}] (get-in game [:players player-no :look-at])]
    (move-card game (merge {:player-no player-no
                            :card-name name
                            :from      :look-at
                            :to        choice}
                           (when (= choice :deck)
                             {:to-position :top})))))

(defn- doctor-look-at [game {:keys [player-no]}]
  (let [{:keys [deck discard]} (get-in game [:players player-no])]
    (if (not-empty (concat deck discard))
      (push-effect-stack game {:player-no player-no
                               :effects   [[:look-at 1]
                                           [:give-choice {:text    "Look at the top card of your deck;"
                                                          :choice  ::doctor-choice
                                                          :options [:special
                                                                    {:option :trash :text "Trash it."}
                                                                    {:option :discard :text "Discard it."}
                                                                    {:option :deck :text "Put it back."}]
                                                          :min     1
                                                          :max     1}]]})
      game)))

(defn- doctor-overpay [game {:keys [player-no amount]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (repeat amount [::doctor-look-at])}))

(effects/register {::doctor-trash   doctor-trash
                   ::doctor-reveal  doctor-reveal
                   ::doctor-choice  doctor-choice
                   ::doctor-look-at doctor-look-at
                   ::doctor-overpay doctor-overpay})

(def doctor {:name    :doctor
             :set     :guilds
             :types   #{:action}
             :cost    3
             :effects [[:give-choice {:text    "Name a card."
                                      :choice  ::doctor-reveal
                                      :options [:supply {:all true}]
                                      :min     1
                                      :max     1}]]
             :overpay ::doctor-overpay})

(defn- herald-play-action [game {:keys [player-no]}]
  (let [revealed (get-in game [:players player-no :revealed])
        {:keys [name] :as card} (first revealed)
        types    (ut/get-types game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (and card (:action types))
                                          [[:play-from-revealed {:card-name name}]
                                           [:card-effect {:card card}]]
                                          [[:topdeck-all-revealed]])})))

(defn- herald-overpay [game {:keys [player-no amount]}]
  (give-choice game {:player-no player-no
                     :text      (str "Put " amount " cards from your discard onto your deck.")
                     :choice    :topdeck-from-discard
                     :options   [:player :discard]
                     :min       amount
                     :max       amount}))

(effects/register {::herald-play-action herald-play-action
                   ::herald-overpay     herald-overpay})

(def herald {:name    :herald
             :set     :guilds
             :types   #{:action}
             :cost    4
             :effects [[:draw 1]
                       [:give-actions 1]
                       [:reveal-from-deck 1]
                       [::herald-play-action]]
             :overpay ::herald-overpay})

(defn journeyman-reveal [game {:keys [player-no card-name]}]
  (let [{:keys [revealed deck discard]} (get-in game [:players player-no])
        card-names (->> revealed
                        (map :name)
                        (remove #{card-name}))]
    (if (and (< (count card-names) 3)
             (not-empty (concat deck discard)))
      (push-effect-stack game {:player-no player-no
                               :effects   [[:reveal-from-deck 1]
                                           [::journeyman-reveal {:card-name card-name}]]})
      (push-effect-stack game {:player-no player-no
                               :effects   [[:take-from-revealed {:card-names card-names}]
                                           [:discard-all-revealed]]}))))

(effects/register {::journeyman-reveal journeyman-reveal})

(def journeyman {:name    :journeyman
                 :set     :guilds
                 :types   #{:action}
                 :cost    5
                 :effects [[:give-choice {:text    "Name a card."
                                          :choice  ::journeyman-reveal
                                          :options [:supply {:all true}]
                                          :min     1
                                          :max     1}]]})

(defn- masterpiece-overpay [game {:keys [player-no amount]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (repeat amount [:gain {:card-name :silver}])}))

(effects/register {::masterpiece-overpay masterpiece-overpay})

(def masterpiece {:name       :masterpiece
                  :set        :guilds
                  :types      #{:treasure}
                  :cost       3
                  :coin-value 1
                  :overpay    ::masterpiece-overpay})

(def merchant-guild {:name          :merchant-guild
                     :set           :guilds
                     :types         #{:action}
                     :cost          5
                     :effects       [[:give-buys 1]
                                     [:give-coins 1]]
                     :while-in-play {:on-buy [[:give-coffers 1]]}})

(defn- plaza-discard-treasure [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-coffers 1]]})))

(effects/register {::plaza-discard-treasure plaza-discard-treasure})

(def plaza {:name    :plaza
            :set     :guilds
            :types   #{:action}
            :cost    4
            :effects [[:draw 1]
                      [:give-actions 2]
                      [:give-choice {:text    "You may discard a Treasure for +1 Coffers."
                                     :choice  ::plaza-discard-treasure
                                     :options [:player :hand {:type :treasure}]
                                     :max     1}]]})

(defn- soothsayer-attack [game {:keys [player-no]}]
  (let [{:keys [pile-size]} (ut/get-pile-idx game :curse)]
    (cond-> game
            (pos? pile-size) (push-effect-stack {:player-no player-no
                                                 :effects   [[:gain {:card-name :curse}]
                                                             [:draw 1]]}))))

(effects/register {::soothsayer-attack soothsayer-attack})

(def soothsayer {:name    :soothsayer
                 :set     :guilds
                 :types   #{:action :attack}
                 :cost    5
                 :effects [[:gain {:card-name :gold}]
                           [:attack {:effects [[::soothsayer-attack]]}]]})

(defn- stonemason-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost      (ut/get-cost game card)
        gain-card [:give-choice {:text    (str "Gain 2 cards each costing less than " (ut/format-cost cost) ".")
                                 :choice  :gain
                                 :options [:supply {:costs-less-than cost}]
                                 :min     1
                                 :max     1}]]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-hand {:card-name card-name}]
                                         gain-card
                                         gain-card]})))

(defn- stonemason-overpay [game {:keys [player-no amount]}]
  (let [gain-action [:give-choice {:text    (str "Gain 2 Action card each costing exactly $" amount ".")
                                   :choice  :gain
                                   :options [:supply {:type :action
                                                      :cost amount}]
                                   :min     1
                                   :max     1}]]
    (push-effect-stack game {:player-no player-no
                             :effects   [gain-action
                                         gain-action]})))

(effects/register {::stonemason-trash   stonemason-trash
                   ::stonemason-overpay stonemason-overpay})

(def stonemason {:name    :stonemason
                 :set     :guilds
                 :types   #{:action}
                 :cost    2
                 :effects [[:give-choice {:text    "Trash a card from your hand."
                                          :choice  ::stonemason-trash
                                          :options [:player :hand]
                                          :min     1
                                          :max     1}]]
                 :overpay ::stonemason-overpay})

(defn taxman-attack [game {:keys [player-no card-name]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond
      (< (count hand) 5) game
      (some (comp #{card-name} :name) hand) (give-choice game {:player-no player-no
                                                               :text      (str "Discard a " (ut/format-name card-name) ".")
                                                               :choice    :discard-from-hand
                                                               :options   [:player :hand {:name card-name}]
                                                               :min       1
                                                               :max       1})
      :else (-> game
                (reveal-hand {:player-no player-no})))))

(defn- taxman-trash [game {:keys [player-no card-name]}]
  (if card-name
    (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
          max-cost (-> (ut/get-cost game card)
                       (ut/add-to-cost 3))]
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   [[:trash-from-hand {:card-name card-name}]
                                          [:attack {:effects [[::taxman-attack {:card-name card-name}]]}]
                                          [:give-choice {:text    (str "Gain a Treasure onto your deck costing up to " (ut/format-cost max-cost) ".")
                                                         :choice  :gain-to-topdeck
                                                         :options [:supply {:max-cost max-cost :type :treasure}]
                                                         :min     1
                                                         :max     1}]]})))
    game))

(effects/register {::taxman-attack taxman-attack
                   ::taxman-trash  taxman-trash})

(def taxman {:name    :taxman
             :set     :guilds
             :types   #{:action :attack}
             :cost    4
             :effects [[:give-choice {:text    "You may trash a treasure from your hand."
                                      :choice  ::taxman-trash
                                      :options [:player :hand {:type :treasure}]
                                      :max     1}]]})

(def kingdom-cards [advisor
                    baker
                    butcher
                    candlestick-maker
                    doctor
                    herald
                    journeyman
                    masterpiece
                    merchant-guild
                    plaza
                    soothsayer
                    stonemason
                    taxman])
