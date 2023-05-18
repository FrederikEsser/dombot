(ns dombot.cards.prosperity
  (:require [dombot.operations :refer [push-effect-stack give-choice move-card move-cards]]
            [dombot.cards.common :refer [give-coins discard-from-hand give-victory-points]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def colony {:name           :colony
             :types          #{:victory}
             :cost           11
             :victory-points 10})

(def platinum {:name       :platinum
               :types      #{:treasure}
               :cost       9
               :coin-value 5})

(defn- anvil-discard [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-choice {:text    "Gain a card costing up to $4."
                                                                   :choice  :gain
                                                                   :options [:supply {:max-cost 4}]
                                                                   :min     1
                                                                   :max     1}]]})))

(effects/register {::anvil-discard anvil-discard})

(def anvil {:name            :anvil
            :set             :prosperity
            :types           #{:treasure}
            :cost            3
            :coin-value      1
            :effects         [[:give-choice {:text    "You may discard a Treasure to gain a card costing up to $4."
                                             :choice  ::anvil-discard
                                             :options [:player :hand {:type :treasure}]
                                             :max     1}]]
            :auto-play-index -1})

(defn- bank-give-coins [game {:keys [player-no]}]
  (let [number-of-treasures-in-play (->> (get-in game [:players player-no :play-area])
                                         (filter (comp :treasure (partial ut/get-types game)))
                                         count)]
    (give-coins game {:player-no player-no :arg number-of-treasures-in-play})))

(effects/register {::bank-give-coins bank-give-coins})

(def bank {:name            :bank
           :set             :prosperity
           :types           #{:treasure}
           :cost            7
           :effects         [[:give-buys 1]
                             [::bank-give-coins]]
           :auto-play-index 3})

(defn- bishop-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        {:keys [coin-cost]} (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-hand {:card-name card-name}]
                                         [:give-victory-points (quot coin-cost 2)]]})))

(effects/register {::bishop-trash bishop-trash})

(def bishop {:name    :bishop
             :set     :prosperity
             :types   #{:action}
             :cost    4
             :effects [[:give-coins 1]
                       [:give-victory-points 1]
                       [:give-choice {:text    "Trash a card from your hand."
                                      :choice  ::bishop-trash
                                      :options [:player :hand]
                                      :min     1
                                      :max     1}]
                       [:other-players {:effects [[:give-choice {:text    "You may trash a card from your hand."
                                                                 :choice  :trash-from-hand
                                                                 :options [:player :hand]
                                                                 :max     1}]]}]]})

(defn curse->treasure [game _]
  (let [{:keys [idx]} (ut/get-pile-idx game :curse)]
    (update-in game [:supply idx :card] assoc
               :types #{:curse :treasure}
               :coin-value 1)))

(effects/register {::curse->treasure curse->treasure})

(def charlatan {:name    :charlatan
                :set     :prosperity
                :types   #{:action :attack}
                :cost    5
                :effects [[:give-coins 3]
                          [:attack {:effects [[:gain {:card-name :curse}]]}]]
                :setup   [[::curse->treasure]]})

(defn- city-effects [game {:keys [player-no]}]
  (let [empty-piles (ut/empty-supply-piles game)]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat [[:draw 1]
                                                 [:give-actions 2]]
                                                (when (<= 1 empty-piles)
                                                  [[:draw 1]])
                                                (when (<= 2 empty-piles)
                                                  [[:give-buys 1]
                                                   [:give-coins 1]]))})))

(effects/register {::city-effects city-effects})

(def city {:name    :city
           :set     :prosperity
           :types   #{:action}
           :cost    5
           :effects [[::city-effects]]})

(defn- collection-on-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        types (ut/get-types game card)]
    (cond-> game
            (:action types) (give-victory-points {:player-no player-no :arg 1}))))

(effects/register {::collection-on-gain collection-on-gain})

(def collection {:name       :collection
                 :set        :prosperity
                 :types      #{:treasure}
                 :cost       5
                 :coin-value 2
                 :effects    [[:give-buys 1]]
                 :trigger    {:event    :on-gain
                              :duration :turn
                              :mode     :auto
                              :effects  [[::collection-on-gain]]}})

(defn contraband-give-choice [{:keys [players] :as game} {:keys [player-no]}]
  (let [next-player (mod (inc player-no) (count players))]
    (give-choice game {:player-no next-player
                       :text      "Name a card that can't be bought this turn."
                       :choice    :mark-unbuyable
                       :options   [:supply {:all true}]
                       :min       1
                       :max       1})))

(effects/register {::contraband-give-choice contraband-give-choice})

(def contraband {:name            :contraband
                 :set             :prosperity
                 :types           #{:treasure}
                 :cost            5
                 :coin-value      3
                 :effects         [[:give-buys 1]
                                   [::contraband-give-choice]]
                 :auto-play-index -2})

(def counting-house {:name    :counting-house
                     :set     :prosperity
                     :types   #{:action}
                     :cost    5
                     :effects [[:give-choice {:text    "Put any number of Coppers from your discard pile into your hand."
                                              :choice  :take-from-discard
                                              :options [:player :discard {:name :copper}]}]]})

(def expand {:name    :expand
             :set     :prosperity
             :types   #{:action}
             :cost    7
             :effects [[:give-choice {:text    "Trash a card from your hand."
                                      :choice  [:trash-and-gain {:extra-cost 3}]
                                      :options [:player :hand]
                                      :min     1
                                      :max     1}]]})

(defn- forge-trash [game {:keys [player-no card-names]}]
  (let [total-cost (->> card-names
                        (map (fn [card-name]
                               (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
                                 (ut/get-cost game card))))
                        (keep :coin-cost)
                        (apply +))]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-hand {:card-names card-names}]
                                         [:give-choice {:text    (str "Gain a card costing exactly $" total-cost ".")
                                                        :choice  :gain
                                                        :options [:supply {:cost total-cost}]
                                                        :min     1
                                                        :max     1}]]})))

(effects/register {::forge-trash forge-trash})

(def forge {:name    :forge
            :set     :prosperity
            :types   #{:action}
            :cost    7
            :effects [[:give-choice {:text    "Trash any number of cards from your hand."
                                     :choice  ::forge-trash
                                     :options [:player :hand]}]]})

(def goons {:name          :goons
            :set           :prosperity
            :types         #{:action :attack}
            :cost          6
            :effects       [[:give-buys 1]
                            [:give-coins 2]
                            [:attack {:effects [[:discard-down-to 3]]}]]
            :while-in-play {:on-buy [[:give-victory-points 1]]}})

(defn grand-market-buyable? [game {:keys [player-no]}]
  (->> (get-in game [:players player-no :play-area])
       (some (comp #{:copper} :name))
       not))

(effects/register {::grand-market-buyable? grand-market-buyable?})

(def grand-market {:name     :grand-market
                   :set      :prosperity
                   :types    #{:action}
                   :cost     6
                   :effects  [[:draw 1]
                              [:give-actions 1]
                              [:give-buys 1]
                              [:give-coins 2]]
                   :buyable? ::grand-market-buyable?})

(defn- hoard-on-buy [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game card-name)
        types (ut/get-types game card)]
    (cond-> game
            (:victory types) (push-effect-stack {:player-no player-no
                                                 :effects   [[:gain {:card-name :gold}]]}))))

(effects/register {::hoard-on-buy hoard-on-buy})

(def hoard {:name          :hoard
            :set           :prosperity
            :types         #{:treasure}
            :cost          6
            :coin-value    2
            :while-in-play {:on-buy [[::hoard-on-buy]]}})

(def kings-court {:name    :king's-court
                  :set     :prosperity
                  :types   #{:action}
                  :cost    7
                  :effects [[:give-choice {:text    "You may play an Action card from your hand three times."
                                           :choice  [:repeat-action {:times 3}]
                                           :options [:player :hand {:type :action}]
                                           :max     1}]]})

(defn- loan-reveal [game {:keys [player-no]}]
  (let [{:keys [revealed deck discard]} (get-in game [:players player-no])
        {:keys [name] :as card} (last revealed)
        types (ut/get-types game card)]
    (cond (:treasure types) (give-choice game {:player-no player-no
                                               :text      (str "You may trash the revealed " (ut/format-name name) ".")
                                               :choice    :trash-from-revealed
                                               :options   [:player :revealed {:type :treasure}]
                                               :max       1})
          (not-empty (concat deck discard)) (push-effect-stack game {:player-no player-no
                                                                     :effects   [[:reveal-from-deck 1]
                                                                                 [::loan-reveal]]})
          :else game)))

(effects/register {::loan-reveal loan-reveal})

(def loan {:name       :loan
           :set        :prosperity
           :types      #{:treasure}
           :cost       3
           :coin-value 1
           :effects    [[:reveal-from-deck 1]
                        [::loan-reveal]
                        [:discard-all-revealed]]})

(defn- magnate-draw [game {:keys [player-no]}]
  (let [treasures-in-hand (->> (get-in game [:players player-no :hand])
                               (filter (comp :treasure (partial ut/get-types game)))
                               count)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:draw treasures-in-hand]]})))

(effects/register {::magnate-draw magnate-draw})

(def magnate {:name    :magnate
              :set     :prosperity
              :types   #{:action}
              :cost    5
              :effects [[:reveal-hand]
                        [::magnate-draw]]})

(def mint {:name    :mint
           :set     :prosperity
           :types   #{:action}
           :cost    5
           :effects [[:give-choice {:text    "You may reveal a Treasure card from your hand to gain a copy of it."
                                    :choice  :gain
                                    :options [:player :hand {:type :treasure}]
                                    :max     1}]]
           :on-buy  [[:trash-from-play-area {:type :treasure}]]})

(def monument {:name    :monument
               :set     :prosperity
               :types   #{:action}
               :cost    4
               :effects [[:give-coins 2]
                         [:give-victory-points 1]]})

(defn- mountebank-gain [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:gain {:card-name :curse}]
                                       [:gain {:card-name :copper}]]}))

(defn- mountebank-discard-curse [game {:keys [card-name] :as args}]
  (if (= card-name :curse)
    (discard-from-hand game args)
    (mountebank-gain game args)))

(defn- mountebank-attack [game {:keys [player-no] :as args}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name :curse})]
    (if card
      (give-choice game {:player-no player-no
                         :text      "You may discard a Curse."
                         :choice    ::mountebank-discard-curse
                         :options   [:player :hand {:name :curse}]
                         :max       1})
      (mountebank-gain game args))))

(effects/register {::mountebank-attack        mountebank-attack
                   ::mountebank-discard-curse mountebank-discard-curse})

(def mountebank {:name    :mountebank
                 :set     :prosperity
                 :types   #{:action :attack}
                 :cost    5
                 :effects [[:give-coins 2]
                           [:attack {:effects [[::mountebank-attack]]}]]})

(defn- peddler-buy-cost [game {:keys [player-no]}]
  (let [actions-in-play (->> (get-in game [:players player-no :play-area])
                             (filter (comp :action (partial ut/get-types game)))
                             count)]
    (max 0 (- 8 (* 2 actions-in-play)))))

(effects/register {::peddler-buy-cost peddler-buy-cost})

(def peddler {:name     :peddler
              :set      :prosperity
              :types    #{:action}
              :cost     8
              :buy-cost ::peddler-buy-cost
              :effects  [[:draw 1]
                         [:give-actions 1]
                         [:give-coins 1]]})

(def quarry {:name          :quarry
             :set           :prosperity
             :types         #{:treasure}
             :cost          4
             :coin-value    1
             :while-in-play {:cost-reductions [{:type :action :reduction 2}]}})

(defn rabble-discard [game {:keys [player-no]}]
  (let [card-names (->> (get-in game [:players player-no :revealed])
                        (filter (fn [card]
                                  (let [types (ut/get-types game card)]
                                    (or (:action types) (:treasure types)))))
                        (map :name))]
    (move-cards game {:player-no  player-no
                      :card-names card-names
                      :from       :revealed
                      :to         :discard})))

(effects/register {::rabble-discard rabble-discard})

(def rabble {:name    :rabble
             :set     :prosperity
             :types   #{:action :attack}
             :cost    5
             :effects [[:draw 3]
                       [:attack {:effects [[:reveal-from-deck 3]
                                           [::rabble-discard]
                                           [:give-choice {:text    "Put the revealed cards back onto your deck in any order."
                                                          :choice  :topdeck-from-revealed
                                                          :options [:player :revealed]
                                                          :min     3
                                                          :max     3}]]}]]})

(def royal-seal {:name          :royal-seal
                 :set           :prosperity
                 :types         #{:treasure}
                 :cost          5
                 :coin-value    2
                 :while-in-play {:on-gain [[:topdeck-gained-choice]]}})

(defn- talisman-on-buy [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game card-name)
        types (ut/get-types game card)
        cost  (ut/get-cost game card)]
    (cond-> game
            (and (not (:victory types))
                 (ut/costs-up-to 4 cost)) (push-effect-stack {:player-no player-no
                                                              :effects   [[:gain {:card-name card-name}]]}))))

(effects/register {::talisman-on-buy talisman-on-buy})

(def talisman {:name          :talisman
               :set           :prosperity
               :types         #{:treasure}
               :cost          4
               :coin-value    1
               :while-in-play {:on-buy [[::talisman-on-buy]]}})

(def tiara {:name            :tiara
            :set             :prosperity
            :types           #{:treasure}
            :cost            4
            :effects         [[:give-buys 1]
                              [:add-trigger {:trigger {:event    :on-gain
                                                       :duration :turn
                                                       :effects  [[:topdeck-gained-choice]]}}]
                              [:give-choice {:text    "You may play a Treasure from your hand twice."
                                             :choice  [:repeat-action {:times 2}]
                                             :options [:player :hand {:type :treasure}]
                                             :max     1}]]
            :auto-play-index -1})

(defn- trade-route-give-coins [{:keys [trade-route-mat] :as game} {:keys [player-no]}]
  (cond-> game
          trade-route-mat (give-coins {:player-no player-no :arg trade-route-mat})))

(defn trade-route-move-token [game {:keys [card-name]}]
  (let [{:keys [idx]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})]
    (-> game
        (update :trade-route-mat ut/plus 1)
        (update-in [:supply idx :tokens] dissoc :trade-route)
        (update-in [:supply idx] ut/dissoc-if-empty :tokens))))

(defn- trade-route-setup [game _]
  (update game :supply (partial mapv (fn [pile]
                                       (let [{{:keys [types]} :card} (ut/access-top-card pile)]
                                         (cond-> pile
                                                 (:victory types) (assoc-in [:tokens :trade-route] {:number-of-tokens 1
                                                                                                    :on-gain          [[::trade-route-move-token]]})))))))

(effects/register {::trade-route-give-coins trade-route-give-coins
                   ::trade-route-move-token trade-route-move-token
                   ::trade-route-setup      trade-route-setup})

(def trade-route {:name    :trade-route
                  :set     :prosperity
                  :types   #{:action}
                  :cost    3
                  :effects [[:give-buys 1]
                            [::trade-route-give-coins]
                            [:give-choice {:text    "Trash a card from your hand."
                                           :choice  :trash-from-hand
                                           :options [:player :hand]
                                           :min     1
                                           :max     1}]]
                  :setup   [[::trade-route-setup]]})

(defn- vault-discard [game {:keys [player-no card-names] :as args}]
  (-> game
      (push-effect-stack {:player-no player-no
                          :effects   (concat [[:discard-from-hand args]]
                                             (when (= 2 (count card-names))
                                               [[:draw 1]]))})))

(effects/register {::vault-discard vault-discard})

(def vault {:name    :vault
            :set     :prosperity
            :types   #{:action}
            :cost    5
            :effects [[:draw 2]
                      [:give-choice {:text    "Discard any number of cards for +$1 each."
                                     :choice  :discard-for-coins
                                     :options [:player :hand]}]
                      [:other-players {:effects [[:give-choice {:text      "You may discard 2 cards, to draw a card."
                                                                :choice    ::vault-discard
                                                                :options   [:player :hand]
                                                                :min       2
                                                                :max       2
                                                                :optional? true}]]}]]})

(defn- venture-reveal [game {:keys [player-no]}]
  (let [{:keys [revealed deck discard]} (get-in game [:players player-no])
        {:keys [name] :as card} (last revealed)
        types (ut/get-types game card)]
    (cond (:treasure types) (push-effect-stack game {:player-no player-no
                                                     :effects   [[:move-card {:card-name name
                                                                              :from      :revealed
                                                                              :to        :play-area}]
                                                                 [:card-effect {:card card}]]})
          (not-empty (concat deck discard)) (push-effect-stack game {:player-no player-no
                                                                     :effects   [[:reveal-from-deck 1]
                                                                                 [::venture-reveal]]})
          :else game)))

(effects/register {::venture-reveal venture-reveal})

(def venture {:name            :venture
              :set             :prosperity
              :types           #{:treasure}
              :cost            5
              :coin-value      1
              :effects         [[:reveal-from-deck 1]
                                [::venture-reveal]
                                [:discard-all-revealed]]
              :auto-play-index 1})

(defn watchtower-choice [game {:keys [player-no choice gained-card-id]}]
  (case choice
    :trash (move-card game {:player-no    player-no
                            :move-card-id gained-card-id
                            :from         :gaining
                            :to           :trash})
    :topdeck (move-card game {:player-no    player-no
                              :move-card-id gained-card-id
                              :from         :gaining
                              :to           :deck
                              :to-position  :top})
    :nothing game))

(defn watchtower-give-choice [game {:keys [player-no gained-card-id]}]
  (let [{{:keys [name] :as card} :card} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})]
    (cond-> game
            card (give-choice {:player-no player-no
                               :text      (str "You may reveal a Watchtower from your hand, to either trash the gained " (ut/format-name name) " or put it onto your deck.")
                               :choice    [::watchtower-choice {:gained-card-id gained-card-id}]
                               :options   [:special
                                           {:option :trash :text (str "Trash " (ut/format-name name) ".")}
                                           {:option :topdeck :text (str "Put " (ut/format-name name) " onto your deck.")}
                                           {:option :nothing :text "Don't reveal Watchtower."}]
                               :min       1
                               :max       1}))))

(effects/register {::watchtower-choice      watchtower-choice
                   ::watchtower-give-choice watchtower-give-choice})

(def watchtower {:name     :watchtower
                 :set      :prosperity
                 :types    #{:action :reaction}
                 :cost     3
                 :effects  [[:draw-up-to 6]]
                 :reaction {:on-gain [[::watchtower-give-choice]]}})

(def workers-village {:name    :worker's-village
                      :set     :prosperity
                      :types   #{:action}
                      :cost    4
                      :effects [[:draw 1]
                                [:give-actions 2]
                                [:give-buys 1]]})

(def kingdom-cards [anvil
                    bank
                    bishop
                    charlatan
                    city
                    collection
                    #_contraband
                    #_counting-house
                    expand
                    forge
                    #_goons
                    grand-market
                    hoard
                    kings-court
                    #_loan
                    magnate
                    mint
                    monument
                    #_mountebank
                    peddler
                    quarry
                    rabble
                    #_royal-seal
                    #_talisman
                    tiara
                    #_trade-route
                    vault
                    #_venture
                    watchtower
                    workers-village])