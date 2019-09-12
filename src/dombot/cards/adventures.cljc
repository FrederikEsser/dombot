(ns dombot.cards.adventures
  (:require [dombot.operations :refer [push-effect-stack give-choice move-card]]
            [dombot.cards.common :refer [add-trigger set-aside=>hand-trigger]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- amulet-choices [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [(case choice
                                         :coin [:give-coins 1]
                                         :trash [:give-choice {:text    "Trash a card from your hand."
                                                               :choice  :trash-from-hand
                                                               :options [:player :hand]
                                                               :min     1
                                                               :max     1}]
                                         :silver [:gain {:card-name :silver}])]}))

(effects/register {::amulet-choices amulet-choices})

(def amulet-choice {:text    "Choose one:"
                    :choice  ::amulet-choices
                    :options [:special
                              {:option :coin :text "+$1"}
                              {:option :trash :text "Trash a card from your hand."}
                              {:option :silver :text "Gain a Silver."}]
                    :min     1
                    :max     1})

(def amulet {:name    :amulet
             :set     :adventures
             :types   #{:action :duration}
             :cost    3
             :effects [[:give-choice amulet-choice]]
             :trigger {:event    :at-start-turn
                       :duration :once
                       :mode     :manual
                       :effects  [[:give-choice amulet-choice]]}})

(defn- artificer-discard [game {:keys [player-no card-names]}]
  (let [cost (count card-names)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:discard-from-hand {:card-names card-names}]
                                         [:give-choice {:text    (str "You may gain a card onto your deck costing exactly $" cost ".")
                                                        :choice  :gain-to-topdeck
                                                        :options [:supply {:cost cost}]
                                                        :max     1}]]})))

(effects/register {::artificer-discard artificer-discard})

(def artificer {:name    :artificer
                :set     :adventures
                :types   #{:action}
                :cost    5
                :effects [[:draw 1]
                          [:give-actions 1]
                          [:give-coins 1]
                          [:give-choice {:text    "Discard any number of cards."
                                         :choice  ::artificer-discard
                                         :options [:player :hand]}]]})

(defn- caravan-guard-play [game {:keys [player-no card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:id card-id})]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:play-from-hand {:card-name :caravan-guard}]
                                         [:card-effect {:card card}]]})))

(effects/register {::caravan-guard-play caravan-guard-play})

(def caravan-guard {:name      :caravan-guard
                    :set       :adventures
                    :types     #{:action :duration :reaction}
                    :cost      3
                    :effects   [[:draw 1]
                                [:give-actions 1]]
                    :trigger   {:event    :at-start-turn
                                :duration :once
                                :mode     :auto
                                :effects  [[:give-coins 1]]}
                    :reacts-to :attack
                    :reaction  [[::caravan-guard-play]]})

(def dungeon {:name    :dungeon
              :set     :adventures
              :types   #{:action :duration}
              :cost    3
              :effects [[:give-actions 1]
                        [:draw 2]
                        [:give-choice {:text    "Discard 2 cards."
                                       :choice  :discard-from-hand
                                       :options [:player :hand]
                                       :min     2
                                       :max     2}]]
              :trigger {:event    :at-start-turn
                        :duration :once
                        :mode     :manual
                        :effects  [[:draw 2]
                                   [:give-choice {:text    "Discard 2 cards."
                                                  :choice  :discard-from-hand
                                                  :options [:player :hand]
                                                  :min     2
                                                  :max     2}]]}})

(defn- gear-set-aside [game {:keys [player-no card-id]}]
  (let [set-aside (get-in game [:players player-no :gear-set-aside])]
    (-> game
        (update-in [:players player-no] dissoc :gear-set-aside)
        (add-trigger {:player-no player-no
                      :card-id   card-id
                      :trigger   (merge set-aside=>hand-trigger {:set-aside set-aside})}))))

(defn gear-choose-cards [game {:keys [player-no card-id card-names]}]
  (cond-> game
          (not-empty card-names) (push-effect-stack {:player-no player-no
                                                     :card-id   card-id
                                                     :effects   [[:move-cards {:card-names card-names
                                                                               :from       :hand
                                                                               :to         :gear-set-aside}]
                                                                 [::gear-set-aside]]})))

(effects/register {::gear-set-aside    gear-set-aside
                   ::gear-choose-cards gear-choose-cards})


(def gear {:name    :gear
           :set     :adventures
           :types   #{:action :duration}
           :cost    3
           :effects [[:draw 2]
                     [:give-choice {:text    "Set aside up to 2 cards from your hand."
                                    :choice  ::gear-choose-cards
                                    :options [:player :hand]
                                    :max     2}]]})

(def hireling {:name    :hireling
               :set     :adventures
               :types   #{:action :duration}
               :cost    6
               :trigger {:event    :at-start-turn
                         :duration :game
                         :mode     :semi
                         :effects  [[:draw 1]]}})

(def lost-city {:name    :lost-city
                :set     :adventures
                :types   #{:action}
                :cost    5
                :effects [[:draw 2]
                          [:give-actions 2]]
                :on-gain [[:other-players {:effects [[:draw 1]]}]]})

(defn- magpie-check-revealed [game {:keys [player-no]}]
  (let [{:keys [name] :as card} (last (get-in game [:players player-no :revealed]))
        types (ut/get-types game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [(when (:treasure types)
                                           [:take-from-revealed {:card-name name}])
                                         (when (or (:action types) (:victory types))
                                           [:gain {:card-name :magpie}])]})))

(effects/register {::magpie-check-revealed magpie-check-revealed})

(def magpie {:name    :magpie
             :set     :adventures
             :types   #{:action}
             :cost    4
             :effects [[:draw 1]
                       [:give-actions 1]
                       [:reveal-from-deck 1]
                       [::magpie-check-revealed]
                       [:topdeck-all-revealed]]})

(defn- traveller-exchange [{:keys [supply extra-cards] :as game} {:keys [player-no from-card to-card]}]
  (let [pile-location (cond (some (comp #{from-card} :name :card) supply) :supply
                            (some (comp #{from-card} :name :card) extra-cards) :extra-cards)
        {:keys [pile-size]} (ut/get-pile-idx game :extra-cards to-card)]
    (cond-> game
            (and pile-size
                 (pos? pile-size)) (push-effect-stack {:player-no player-no
                                                       :effects   [[:move-card {:card-name from-card
                                                                                :from      :play-area
                                                                                :to        pile-location}]
                                                                   [:move-card {:card-name to-card
                                                                                :from      :extra-cards
                                                                                :to        :discard}]]}))))

(effects/register {::traveller-exchange traveller-exchange})

(def champion {:name    :champion
               :set     :adventures
               :types   #{:action :duration}
               :cost    6
               :effects [[:give-actions 1]
                         [:mark-unaffected]]
               :trigger {:event    :play-action
                         :duration :game
                         :mode     :auto
                         :effects  [[:give-actions 1]]}})

(def hero {:name        :hero
           :set         :adventures
           :types       #{:action :traveller}
           :cost        5
           :effects     [[:give-coins 2]
                         [:give-choice {:text    "Gain a Treasure."
                                        :choice  :gain
                                        :options [:supply {:type :treasure}]
                                        :min     1
                                        :max     1}]]
           :at-clean-up [[::traveller-exchange {:from-card :hero :to-card :champion}]]})

(defn- warrior-trash [game {:keys [player-no]}]
  (let [{:keys [name] :as card} (last (get-in game [:players player-no :discard]))
        cost (ut/get-cost game card)]
    (cond-> game
            (and card (<= 3 cost 4)) (move-card {:player-no     player-no
                                                 :card-name     name
                                                 :from          :discard
                                                 :from-position :bottom
                                                 :to            :trash}))))

(defn- warrior-attack [game {:keys [player-no attacking-player-no]}]
  (let [travellers-in-play (->> (get-in game [:players attacking-player-no :play-area])
                                (filter (comp :traveller (partial ut/get-types game)))
                                count)]
    (push-effect-stack game {:player-no player-no
                             :effects   (->> (repeat travellers-in-play [[:discard-from-topdeck 1]
                                                                         [::warrior-trash]])
                                             (apply concat))})))

(effects/register {::warrior-trash  warrior-trash
                   ::warrior-attack warrior-attack})

(def warrior {:name        :warrior
              :set         :adventures
              :types       #{:action :attack :traveller}
              :cost        4
              :effects     [[:draw 2]
                            [:attack {:effects [[::warrior-attack]]}]]
              :at-clean-up [[::traveller-exchange {:from-card :warrior :to-card :hero}]]})

(defn- treasure-hunter-gain-silver [{:keys [players] :as game} {:keys [player-no]}]
  (let [prev-player  (mod (dec player-no) (count players))
        gained-cards (->> (get-in game [:players prev-player :gained-cards])
                          count)]
    (push-effect-stack game {:player-no player-no
                             :effects   (repeat gained-cards [:gain {:card-name :silver}])})))

(effects/register {::treasure-hunter-gain-silver treasure-hunter-gain-silver})

(def treasure-hunter {:name        :treasure-hunter
                      :set         :adventures
                      :types       #{:action :traveller}
                      :cost        3
                      :effects     [[:give-actions 1]
                                    [:give-coins 1]
                                    [::treasure-hunter-gain-silver]]
                      :at-clean-up [[::traveller-exchange {:from-card :treasure-hunter :to-card :warrior}]]})

(def page {:name        :page
           :set         :adventures
           :types       #{:action :traveller}
           :cost        2
           :effects     [[:draw 1]
                         [:give-actions 1]]
           :at-clean-up [[::traveller-exchange {:from-card :page :to-card :treasure-hunter}]]
           :setup       [[:setup-extra-cards {:extra-cards [{:card treasure-hunter :pile-size 5}
                                                            {:card warrior :pile-size 5}
                                                            {:card hero :pile-size 5}
                                                            {:card champion :pile-size 5}]}]]})

(defn port-12 [game _]
  (let [{:keys [idx]} (ut/get-pile-idx game :port)]
    (assoc-in game [:supply idx :pile-size] 12)))

(effects/register {::port-12 port-12})

(def port {:name    :port
           :set     :adventures
           :types   #{:action}
           :cost    4
           :effects [[:draw 1]
                     [:give-actions 2]]
           :on-buy  [[:gain {:card-name :port}]]
           :setup   [[::port-12]]})

(defn- raze-trash-from-area [game {:keys [player-no choice]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no (:area choice)] {:name (:card-name choice)})
        cost (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-area {:choice choice}]
                                         [:look-at cost]
                                         [:give-choice {:text    (str "Look at " cost " cards from the top of your deck. Put one of them into your hand and discard the rest.")
                                                        :choice  :take-from-look-at
                                                        :options [:player :look-at]
                                                        :min     1
                                                        :max     1}]
                                         [:discard-all-look-at]]})))

(effects/register {::raze-trash-from-area raze-trash-from-area})

(def raze {:name    :raze
           :set     :adventures
           :types   #{:action}
           :cost    2
           :effects [[:give-actions 1]
                     [:give-choice {:text    "Trash Raze or a card from your hand."
                                    :choice  ::raze-trash-from-area
                                    :options [:mixed
                                              [:player :play-area {:this true}]
                                              [:player :hand]]
                                    :min     1
                                    :max     1}]]})

(def treasure-trove {:name       :treasure-trove
                     :set        :adventures
                     :types      #{:treasure}
                     :cost       5
                     :coin-value 2
                     :effects    [[:gain {:card-name :gold}]
                                  [:gain {:card-name :copper}]]})

(def kingdom-cards [amulet
                    artificer
                    caravan-guard
                    dungeon
                    gear
                    hireling
                    lost-city
                    magpie
                    page
                    port
                    raze
                    treasure-trove])

(def bonfire {:name   :bonfire
              :set    :adventures
              :type   :event
              :cost   3
              :on-buy [[:give-choice {:text    "Trash up to 2 cards you have in play."
                                      :choice  :trash-from-play-area
                                      :options [:player :play-area]
                                      :max     2}]]})

(def travelling-fair-trigger {:event    :on-gain
                              :duration :turn
                              :effects  [[:topdeck-gained-choice]]})

(def travelling-fair {:name   :travelling-fair
                      :set    :adventures
                      :type   :event
                      :cost   2
                      :on-buy [[:give-buys 2]
                               [:add-trigger {:trigger travelling-fair-trigger}]]})

(def events [bonfire
             travelling-fair])
