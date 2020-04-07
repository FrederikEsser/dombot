(ns dombot.cards.menagerie
  (:require [dombot.operations :refer [push-effect-stack gain]]
            [dombot.cards.common :refer []]
            [dombot.cards.dominion :as dominion]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def horse {:name    :horse
            :set     :menagerie
            :types   #{:action}
            :cost    3
            :effects [[:draw 2]
                      [:give-actions 1]
                      [:return-this-to-supply {:area :extra-cards}]]})

(defn- exile-this [game {:keys [player-no card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:move-card {:move-card-id card-id
                                                              :from         :play-area
                                                              :to           :exile}]]}))))

(defn- exile-from-supply [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:move-card {:card-name card-name
                                                    :from      :supply
                                                    :to        :exile}]]}))

(defn- exile-from-hand [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:move-card {:card-name card-name
                                                                 :from      :hand
                                                                 :to        :exile}]]})))

(defn- exile-from-revealed [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:move-card {:card-name card-name
                                                    :from      :revealed
                                                    :to        :exile}]]}))

(defn- discard-from-exile [game {:keys [player-no card-name]}]
  (if card-name
    (let [card-names (->> (get-in game [:players player-no :exile])
                          (map :name)
                          (filter #{card-name}))]
      (push-effect-stack game {:player-no player-no
                               :effects   [[:move-cards {:card-names card-names
                                                         :from       :exile
                                                         :to         :discard}]]}))
    game))

(defn- exile-on-gain [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:text    (str "You may discard all copies of " (ut/format-name card-name) " from Exile.")
                                                      :choice  ::discard-from-exile
                                                      :options [:player :exile {:names #{card-name}}]
                                                      :max     1}]]}))

(def exile-trigger {:name     :exile
                    :event    :on-gain
                    :duration :game
                    :effects  [[::exile-on-gain]]})

(defn- add-exile-trigger [game {:keys [player-no]}]
  (let [trigger-exists? (->> (get-in game [:players player-no :triggers])
                             (some (comp #{:exile} :name)))]
    (cond-> game
            (not trigger-exists?) (push-effect-stack {:player-no player-no
                                                      :effects   [[:add-trigger {:trigger exile-trigger}]]}))))

(effects/register {::exile-this          exile-this
                   ::exile-from-supply   exile-from-supply
                   ::exile-from-hand     exile-from-hand
                   ::exile-from-revealed exile-from-revealed
                   ::discard-from-exile  discard-from-exile
                   ::exile-on-gain       exile-on-gain
                   ::add-exile-trigger   add-exile-trigger})

(defn- barge-choice [game {:keys [player-no card-id choice]}]
  (let [effects [[:draw 3]
                 [:give-buys 1]]]
    (push-effect-stack game {:player-no player-no
                             :effects   (case choice
                                          :now effects
                                          :next-turn [[:add-trigger {:trigger {:duration :once
                                                                               :event    :at-start-turn
                                                                               :mode     :semi
                                                                               :effects  effects}
                                                                     :card-id card-id}]])})))

(effects/register {::barge-choice barge-choice})

(def barge {:name    :barge
            :set     :menagerie
            :types   #{:action :duration}
            :cost    5
            :effects [[:give-choice {:text    "Either now or at the start of your next turn, +3 Cards and +1 Buy."
                                     :choice  ::barge-choice
                                     :options [:special
                                               {:option :now :text "Now"}
                                               {:option :next-turn :text "Next turn"}]
                                     :min     1
                                     :max     1}]]})

(defn- bounty-hunter-exile [game {:keys [player-no card-name]}]
  (let [already-exiled? (->> (get-in game [:players player-no :exile])
                             (some (comp #{card-name} :name)))]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat [[::exile-from-hand {:card-name card-name}]]
                                                (when-not already-exiled?
                                                  [[:give-coins 3]]))})))

(effects/register {::bounty-hunter-exile bounty-hunter-exile})

(def bounty-hunter {:name    :bounty-hunter
                    :set     :menagerie
                    :types   #{:action}
                    :cost    4
                    :effects [[:give-actions 1]
                              [:give-choice {:text    "Exile a card from your hand."
                                             :choice  ::bounty-hunter-exile
                                             :options [:player :hand]
                                             :min     1
                                             :max     1}]]
                    :setup   [[:all-players {:effects [[::add-exile-trigger]]}]]})

(def camel-train {:name    :camel-train
                  :set     :menagerie
                  :types   #{:action}
                  :cost    3
                  :effects [[:give-choice {:text    "Exile a non-Victory card from the Supply."
                                           :choice  ::exile-from-supply
                                           :options [:supply {:not-type :victory}]
                                           :min     1
                                           :max     1}]]
                  :on-gain [[::exile-from-supply {:card-name :gold}]]
                  :setup   [[:all-players {:effects [[::add-exile-trigger]]}]]})

(def cardinal {:name    :cardinal
               :set     :menagerie
               :types   #{:action :attack}
               :cost    4
               :effects [[:give-coins 2]
                         [:attack {:effects [[:reveal-from-deck 2]
                                             [:give-choice {:text    "Exile a revealed card costing from $3 to $6."
                                                            :choice  ::exile-from-revealed
                                                            :options [:player :revealed {:min-cost 3 :max-cost 6}]
                                                            :min     1
                                                            :max     1}]
                                             [:discard-all-revealed]]}]]
               :setup   [[:all-players {:effects [[::add-exile-trigger]]}]]})

(def cavalry {:name    :cavalry
              :set     :menagerie
              :types   #{:action}
              :cost    4
              :effects [[:gain {:card-name :horse :from :extra-cards}]
                        [:gain {:card-name :horse :from :extra-cards}]]
              :on-gain [[:draw 2]
                        [:give-buys 1]
                        [:return-to-action-phase]]
              :setup   [[:setup-extra-cards {:extra-cards [{:card horse :pile-size 30}]}]]})

(defn- coven-curse [game {:keys [player-no]}]
  (let [{:keys [pile-size]} (ut/get-pile-idx game :curse)]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (pos? pile-size)
                                          [[::exile-from-supply {:card-name :curse}]]
                                          [[::discard-from-exile {:card-name :curse}]])})))

(effects/register {::coven-curse coven-curse})

(def coven {:name    :coven
            :set     :menagerie
            :types   #{:action :attack}
            :cost    5
            :effects [[:give-actions 1]
                      [:give-coins 2]
                      [:attack {:effects [[::coven-curse]]}]]
            :setup   [[:all-players {:effects [[::add-exile-trigger]]}]]})

(defn- livery-on-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        cost (ut/get-cost game card)]
    (cond-> game
            (ut/costs-at-least 4 cost) (gain {:player-no player-no :card-name :horse :from :extra-cards}))))

(effects/register {::livery-on-gain livery-on-gain})

(def livery {:name    :livery
             :set     :menagerie
             :types   #{:action}
             :cost    5
             :effects [[:give-coins 3]]
             :trigger {:event    :on-gain
                       :duration :turn
                       :mode     :auto
                       :effects  [[::livery-on-gain]]}
             :setup   [[:setup-extra-cards {:extra-cards [{:card horse :pile-size 30}]}]]})

(defn- kiln-on-play [game {:keys [player-no card] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:text    (str "You may gain a copy of " (ut/format-name (:name card)) ".")
                                                      :choice  :gain
                                                      :options [:supply {:names #{(:name card)}}]
                                                      :max     1}]]}))

(effects/register {::kiln-on-play kiln-on-play})

(def kiln {:name    :kiln
           :set     :menagerie
           :types   #{:action}
           :cost    5
           :effects [[:give-coins 2]]
           :trigger {:event    :play-card
                     :duration :once-turn
                     :effects  [[::kiln-on-play]]}})

(def mastermind {:name    :mastermind
                 :set     :menagerie
                 :types   #{:action :duration}
                 :cost    5
                 :trigger {:event    :at-start-turn
                           :duration :once
                           :mode     :manual
                           :effects  [[:give-choice {:text    "You may play an Action card from your hand three times."
                                                     :choice  [:repeat-action {:times 3}]
                                                     :options [:player :hand {:type :action}]
                                                     :max     1}]]}})

(def sanctuary {:name    :sanctuary
                :set     :menagerie
                :types   #{:action}
                :cost    5
                :effects [[:draw 1]
                          [:give-actions 1]
                          [:give-buys 1]
                          [:give-choice {:text    "You may Exile a card from your hand."
                                         :choice  ::exile-from-hand
                                         :options [:player :hand]
                                         :max     1}]]
                :setup   [[:all-players {:effects [[::add-exile-trigger]]}]]})

(defn scrap-choices [game {:keys [player-no choice choices]}]
  (assert (or choices choice) "No choices specified for scrap.")
  (let [choices (or choices [choice])]
    (assert (apply distinct? choices) "The choices must be different.")
    (push-effect-stack game {:player-no player-no
                             :effects   [(when (:card (set choices)) [:draw 1])
                                         (when (:action (set choices)) [:give-actions 1])
                                         (when (:buy (set choices)) [:give-buys 1])
                                         (when (:coin (set choices)) [:give-coins 1])
                                         (when (:silver (set choices)) [:gain {:card-name :silver}])
                                         (when (:horse (set choices)) [:gain {:card-name :horse
                                                                              :from      :extra-cards}])]})))

(defn scrap-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        {:keys [coin-cost]} (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat
                                          [[:trash-from-hand {:card-name card-name}]]
                                          (when (pos? coin-cost)
                                            [[:give-choice {:text    (str "Choose " (ut/number->text coin-cost) ":")
                                                            :choice  ::scrap-choices
                                                            :options [:special
                                                                      {:option :card :text "+1 Card"}
                                                                      {:option :action :text "+1 Action"}
                                                                      {:option :buy :text "+1 Buy"}
                                                                      {:option :coin :text "+$1"}
                                                                      {:option :silver :text "Gain a Silver"}
                                                                      {:option :horse :text "Gain a Horse"}]
                                                            :min     coin-cost
                                                            :max     coin-cost}]]))})))

(effects/register {::scrap-choices scrap-choices
                   ::scrap-trash   scrap-trash})

(def scrap {:name    :scrap
            :set     :menagerie
            :types   #{:action}
            :cost    3
            :effects [[:give-choice {:text    "Trash a card from your hand."
                                     :choice  ::scrap-trash
                                     :options [:player :hand]
                                     :min     1
                                     :max     1}]]
            :setup   [[:setup-extra-cards {:extra-cards [{:card horse :pile-size 30}]}]]})

(defn- ignore-actions [game {:keys [player-no]}]
  (assoc-in game [:players player-no :ignore-actions?] true))

(effects/register {::ignore-actions ignore-actions})

(def snowy-village {:name    :snowy-village
                    :set     :menagerie
                    :types   #{:action}
                    :cost    3
                    :effects [[:draw 1]
                              [:give-actions 4]
                              [:give-buys 1]
                              [::ignore-actions]]})

(def stockpile {:name       :stockpile
                :set        :menagerie
                :types      #{:treasure}
                :cost       3
                :coin-value 3
                :effects    [[:give-buys 1]
                             [::exile-this]]
                :setup      [[:all-players {:effects [[::add-exile-trigger]]}]]})

(def supplies {:name       :supplies
               :set        :menagerie
               :types      #{:treasure}
               :cost       2
               :coin-value 1
               :effects    [[:gain {:card-name   :horse
                                    :from        :extra-cards
                                    :to          :deck
                                    :to-position :top}]]
               :setup      [[:setup-extra-cards {:extra-cards [{:card horse :pile-size 30}]}]]})

(defn- village-green-choice [game {:keys [player-no card-id choice]}]
  (let [effects [[:draw 1]
                 [:give-actions 2]]]
    (push-effect-stack game {:player-no player-no
                             :effects   (case choice
                                          :now effects
                                          :next-turn [[:add-trigger {:trigger {:duration :once
                                                                               :event    :at-start-turn
                                                                               :mode     :semi
                                                                               :effects  effects}
                                                                     :card-id card-id}]])})))

(effects/register {::village-green-choice village-green-choice})

(def village-green {:name       :village-green
                    :set        :menagerie
                    :types      #{:action :duration :reaction}
                    :cost       3
                    :effects    [[:give-choice {:text    "Either now or at the start of your next turn, +1 Card and +2 Actions."
                                                :choice  ::village-green-choice
                                                :options [:special
                                                          {:option :now :text "Now"}
                                                          {:option :next-turn :text "Next turn"}]
                                                :min     1
                                                :max     1}]]
                    :on-discard [[:give-choice {:text    "You may play Village Green."
                                                :choice  ::dominion/vassal-play-action
                                                :options [:player :discard {:last true
                                                                            :name :village-green}]
                                                :max     1}]]})

(def kingdom-cards [barge
                    bounty-hunter
                    camel-train
                    cardinal
                    cavalry
                    coven
                    kiln
                    livery
                    mastermind
                    sanctuary
                    scrap
                    snowy-village
                    stockpile
                    supplies
                    village-green])

(defn- alliance-gain-base-cards [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:gain {:card-name :province}]
                                       [:gain {:card-name :duchy}]
                                       [:gain {:card-name :estate}]
                                       [:gain {:card-name :gold}]
                                       [:gain {:card-name :silver}]
                                       [:gain {:card-name :copper}]]}))

(effects/register {::alliance-gain-base-cards alliance-gain-base-cards})

(def alliance {:name   :alliance
               :set    :menagerie
               :type   :event
               :cost   10
               :on-buy [[::alliance-gain-base-cards]]})

(def bargain {:name   :bargain
              :set    :menagerie
              :type   :event
              :cost   4
              :on-buy [[:give-choice {:text    "Gain a non-Victory card costing up to $5."
                                      :choice  :gain
                                      :options [:supply {:not-type :victory
                                                         :max-cost 5}]
                                      :min     1
                                      :max     1}]
                       [:other-players {:effects [[:gain {:card-name :horse
                                                          :from      :extra-cards}]]}]]
              :setup  [[:setup-extra-cards {:extra-cards [{:card horse :pile-size 30}]}]]})

(defn- commerce-gain-gold [game {:keys [player-no]}]
  (let [gained-cards (->> (get-in game [:players player-no :gained-cards])
                          (map :name)
                          set
                          count)]
    (cond-> game
            (pos? gained-cards) (push-effect-stack {:player-no player-no
                                                    :effects   (repeat gained-cards [:gain {:card-name :gold}])}))))

(effects/register {::commerce-gain-gold commerce-gain-gold})

(def commerce {:name   :commerce
               :set    :menagerie
               :type   :event
               :cost   5
               :on-buy [[::commerce-gain-gold]]})

(def demand {:name   :demand
             :set    :menagerie
             :type   :event
             :cost   5
             :on-buy [[:gain {:card-name   :horse
                              :from        :extra-cards
                              :to          :deck
                              :to-position :top}]
                      [:give-choice {:text    "Gain a card costing up to $4 onto your deck."
                                     :choice  [:gain {:to          :deck
                                                      :to-position :top}]
                                     :options [:supply {:not-type :victory
                                                        :max-cost 4}]
                                     :min     1
                                     :max     1}]]
             :setup  [[:setup-extra-cards {:extra-cards [{:card horse :pile-size 30}]}]]})

(defn- desperation-gain-curse [game {:keys [player-no]}]
  (let [{:keys [pile-size]} (ut/get-pile-idx game :curse)]
    (cond-> game
            (pos? pile-size) (push-effect-stack {:player-no player-no
                                                 :effects   [[:gain {:card-name :curse}]
                                                             [:give-buys 1]
                                                             [:give-coins 2]]}))))

(effects/register {::desperation-gain-curse desperation-gain-curse})

(def desperation {:name          :desperation
                  :set           :menagerie
                  :type          :event
                  :cost          0
                  :once-per-turn true
                  :on-buy        [[::desperation-gain-curse]]})

(defn- gamble-handle-revealed [game {:keys [player-no]}]
  (let [{:keys [name] :as card} (get-in game [:players player-no :revealed 0])
        types (ut/get-types game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (or (:treasure types)
                                                (:action types))
                                          [[:give-choice {:text    (str "You may play the revealed " (ut/format-name name) ".")
                                                          :choice  :play-from-revealed
                                                          :options [:player :revealed {:name name}]
                                                          :max     1}]]
                                          [[:discard-from-revealed {:card-name name}]])})))

(effects/register {::gamble-handle-revealed gamble-handle-revealed})

(def gamble {:name   :gamble
             :set    :menagerie
             :type   :event
             :cost   2
             :on-buy [[:give-buys 1]
                      [:reveal-from-deck 1]
                      [::gamble-handle-revealed]
                      [:topdeck-all-revealed]]})

(defn- populate-gain-actions [{:keys [supply] :as game} {:keys [player-no]}]
  (let [action-cards (->> supply
                          (map (comp :card ut/access-top-card))
                          (filter (comp :action (partial ut/get-types game)))
                          (map :name))]
    (cond-> game
            (not-empty action-cards) (push-effect-stack {:player-no player-no
                                                         :effects   (for [card-name action-cards]
                                                                      [:gain {:card-name card-name}])}))))

(effects/register {::populate-gain-actions populate-gain-actions})

(def populate {:name   :populate
               :set    :menagerie
               :type   :event
               :cost   10
               :on-buy [[::populate-gain-actions]]})

(def ride {:name   :ride
           :set    :menagerie
           :type   :event
           :cost   2
           :on-buy [[:gain {:card-name :horse
                            :from      :extra-cards}]]
           :setup  [[:setup-extra-cards {:extra-cards [{:card horse :pile-size 30}]}]]})

(defn- stampeding-horses [game {:keys [player-no]}]
  (let [cards-in-play (->> (get-in game [:players player-no :play-area])
                           count)]
    (cond-> game
            (<= cards-in-play 5) (push-effect-stack {:player-no player-no
                                                     :effects   (repeat 5 [:gain-to-topdeck {:card-name :horse
                                                                                             :from      :extra-cards}])}))))

(effects/register {::stampeding-horses stampeding-horses})

(def stampede {:name   :stampede
               :set    :menagerie
               :type   :event
               :cost   5
               :on-buy [[::stampeding-horses]]
               :setup  [[:setup-extra-cards {:extra-cards [{:card horse :pile-size 30}]}]]})

(defn- toil-play-action [game {:keys [player-no card-name]}]
  (let [{card :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:play-from-hand {:card-name card-name}]
                                                 [:card-effect {:card card}]]}))))

(effects/register {::toil-play-action toil-play-action})

(def toil {:name   :toil
           :set    :menagerie
           :type   :event
           :cost   2
           :on-buy [[:give-buys 1]
                    [:give-choice {:text    "You may play an Action card from your hand."
                                   :choice  ::toil-play-action
                                   :options [:player :hand {:type :action}]
                                   :max     1}]]})

(def events [alliance
             bargain
             commerce
             demand
             desperation
             gamble
             populate
             ride
             stampede
             toil])
