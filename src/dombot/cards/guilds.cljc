(ns dombot.cards.guilds
  (:require [dombot.operations :refer [push-effect-stack give-choice draw]]
            [dombot.cards.common :refer [reveal-hand]]
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

(def candlestick-maker {:name    :candlestick-maker
                        :set     :guilds
                        :types   #{:action}
                        :cost    2
                        :effects [[:give-actions 1]
                                  [:give-buys 1]
                                  [:give-coffers 1]]})

(defn- herald-play-action [game {:keys [player-no]}]
  (let [revealed (get-in game [:players player-no :revealed])
        {:keys [name types] :as card} (first revealed)]
    (assert (= 1 (count revealed)) "Herald error: Number of revealed cards is not 1.")
    (push-effect-stack game {:player-no player-no
                             :effects   (if (:action types)
                                          [[:play-from-revealed {:card-name name}]
                                           [:card-effect {:card card}]]
                                          [[:topdeck-from-revealed {:card-name name}]])})))

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

(defn- soothsayer-draw [game {:keys [player-no previous-curse-count]}]
  (let [{curse-count :pile-size} (ut/get-pile-idx game :curse)]
    (cond-> game
            (not= curse-count previous-curse-count) (draw {:player-no player-no
                                                           :arg       1}))))

(defn- soothsayer-attack [game {:keys [player-no]}]
  (let [{curse-count :pile-size} (ut/get-pile-idx game :curse)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:gain {:card-name :curse}]
                                         [::soothsayer-draw {:previous-curse-count curse-count}]]})))

(effects/register {::soothsayer-draw   soothsayer-draw
                   ::soothsayer-attack soothsayer-attack})

(def soothsayer {:name    :soothsayer
                 :set     :guilds
                 :types   #{:action :attack}
                 :cost    5
                 :effects [[:gain {:card-name :gold}]
                           [:attack {:effects [[::soothsayer-attack]]}]]})

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
          max-cost (+ 3 (ut/get-cost game player-no card))]
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   [[:trash-from-hand {:card-name card-name}]
                                          [:attack {:effects [[::taxman-attack {:card-name card-name}]]}]
                                          [:give-choice {:text    (str "Gain a Treasure onto your deck costing up to $" max-cost ".")
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
                    candlestick-maker
                    herald
                    masterpiece
                    merchant-guild
                    plaza
                    soothsayer
                    taxman])
