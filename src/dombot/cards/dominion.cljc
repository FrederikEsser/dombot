(ns dombot.cards.dominion
  (:require [dombot.operations :refer [move-cards push-effect-stack give-choice is-unaffected?]]
            [dombot.cards.common :refer [reveal-hand]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def artisan {:name    :artisan
              :set     :dominion
              :types   #{:action}
              :cost    6
              :effects [[:give-choice {:text    "Gain a card to your hand costing up to $5."
                                       :choice  :gain-to-hand
                                       :options [:supply {:max-cost 5}]
                                       :min     1
                                       :max     1}]
                        [:give-choice {:text    "Put a card from your hand onto your deck."
                                       :choice  :topdeck-from-hand
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]]})

(def bandit {:name    :bandit
             :set     :dominion
             :types   #{:action :attack}
             :cost    5
             :effects [[:gain {:card-name :gold}]
                       [:attack {:effects [[:reveal-from-deck 2]
                                           [:give-choice {:text    "Trash a revealed Treasure other than Copper, and discards the rest."
                                                          :choice  :trash-from-revealed
                                                          :options [:player :revealed {:type     :treasure
                                                                                       :not-name :copper}]
                                                          :min     1
                                                          :max     1}]
                                           [:discard-all-revealed]]}]]})

(defn bureaucrat-topdeck-victory [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:reveal-from-hand {:card-name card-name}]
                                       [:topdeck-from-revealed {:card-name card-name}]]}))

(defn bureaucrat-attack [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (if (some (comp :victory :types) hand)
      (give-choice game {:player-no player-no
                         :text      "Reveal a Victory card from your hand and put it onto your deck."
                         :choice    ::bureaucrat-topdeck-victory
                         :options   [:player :hand {:type :victory}]
                         :min       1
                         :max       1})
      (-> game
          (reveal-hand {:player-no player-no})))))

(effects/register {::bureaucrat-topdeck-victory bureaucrat-topdeck-victory
                   ::bureaucrat-attack          bureaucrat-attack})

(def bureaucrat {:name    :bureaucrat
                 :set     :dominion
                 :types   #{:action :attack}
                 :cost    4
                 :effects [[:gain-to-topdeck {:card-name :silver}]
                           [:attack {:effects [[::bureaucrat-attack]]}]]})

(defn cellar-sift [game {:keys [player-no card-names]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:discard-from-hand {:card-names card-names}]
                                       [:draw (count card-names)]]}))

(effects/register {::cellar-sift cellar-sift})

(def cellar {:name    :cellar
             :set     :dominion
             :types   #{:action}
             :cost    2
             :effects [[:give-actions 1]
                       [:give-choice {:text    "Discard any number of cards, then draw that many."
                                      :choice  ::cellar-sift
                                      :options [:player :hand]}]]})

(def chapel {:name    :chapel
             :set     :dominion
             :types   #{:action}
             :cost    2
             :effects [[:give-choice {:text    "Trash up to 4 cards from your hand."
                                      :choice  :trash-from-hand
                                      :options [:player :hand]
                                      :max     4}]]})

(def council-room {:name    :council-room
                   :set     :dominion
                   :types   #{:action}
                   :cost    5
                   :effects [[:draw 4]
                             [:give-buys 1]
                             [:other-players {:effects [[:draw 1]]}]]})

(def festival {:name    :festival
               :set     :dominion
               :types   #{:action}
               :cost    5
               :effects [[:give-actions 2]
                         [:give-coins 2]
                         [:give-buys 1]]})

(defn gardens-victory-points [cards]
  (quot (count cards) 10))

(effects/register {::gardens-victory-points gardens-victory-points})

(def gardens {:name           :gardens
              :set            :dominion
              :types          #{:victory}
              :cost           4
              :victory-points ::gardens-victory-points})

(def harbinger {:name    :harbinger
                :set     :dominion
                :types   #{:action}
                :cost    3
                :effects [[:draw 1]
                          [:give-actions 1]
                          [:give-choice {:text    "You may put a card from your discard pile onto your deck."
                                         :choice  :topdeck-from-discard
                                         :options [:player :discard]
                                         :max     1}]]})

(def laboratory {:name    :laboratory
                 :set     :dominion
                 :types   #{:action}
                 :cost    5
                 :effects [[:draw 2]
                           [:give-actions 1]]})

(defn library-set-aside [game {:keys [card-name] :as args}]
  (cond-> game
          card-name (move-cards (merge args {:from          :hand
                                             :from-position :bottom
                                             :to            :set-aside}))))

(defn library-check-for-action [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])
        {:keys [types name]} (last hand)]
    (cond-> game
            (:action types) (give-choice {:player-no player-no
                                          :text      (str "You may skip the " (ut/format-name name) "; set it aside, discarding it afterwards.")
                                          :choice    ::library-set-aside
                                          :options   [:player :hand {:last true}]
                                          :max       1}))))

(defn library-draw [game {:keys [player-no]}]
  (let [{:keys [hand deck discard]} (get-in game [:players player-no])]
    (cond-> game
            (and (< (count hand) 7)
                 (not-empty (concat deck discard))) (push-effect-stack {:player-no player-no
                                                                        :effects   [[:draw 1]
                                                                                    [::library-check-for-action]
                                                                                    [::library-draw]]}))))

(effects/register {::library-draw             library-draw
                   ::library-check-for-action library-check-for-action
                   ::library-set-aside        library-set-aside})

(def library {:name    :library
              :set     :dominion
              :types   #{:action}
              :cost    5
              :effects [[::library-draw]
                        [:discard-all-set-aside]]})

(def market {:name    :market
             :set     :dominion
             :types   #{:action}
             :cost    5
             :effects [[:draw 1]
                       [:give-actions 1]
                       [:give-coins 1]
                       [:give-buys 1]]})

(def merchant-trigger {:trigger  [:play :silver]
                       :duration :once
                       :effects  [[:give-coins 1]]})

(def merchant {:name    :merchant
               :set     :dominion
               :types   #{:action}
               :cost    3
               :effects [[:draw 1]
                         [:give-actions 1]
                         [:add-trigger {:trigger merchant-trigger}]]})

(def militia {:name    :militia
              :set     :dominion
              :types   #{:action :attack}
              :cost    4
              :effects [[:give-coins 2]
                        [:attack {:effects [[:discard-down-to 3]]}]]})

(defn mine-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        max-cost (+ 3 (ut/get-cost game card))]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:trash-from-hand {:card-name card-name}]
                                        [:give-choice {:text    (str "Gain a Treasure to your hand costing up to $" max-cost ".")
                                                       :choice  :gain-to-hand
                                                       :options [:supply {:max-cost max-cost :type :treasure}]
                                                       :min     1
                                                       :max     1}]]}))))

(effects/register {::mine-trash mine-trash})

(def mine {:name    :mine
           :set     :dominion
           :types   #{:action}
           :cost    5
           :effects [[:give-choice {:text    "You may trash a Treasure from your hand."
                                    :choice  ::mine-trash
                                    :options [:player :hand {:type :treasure}]
                                    :max     1}]]})

(defn moat-can-react? [game player-no]
  (not (is-unaffected? game player-no)))

(effects/register {::moat-can-react? moat-can-react?})

(def moat {:name       :moat
           :set        :dominion
           :types      #{:action :reaction}
           :cost       2
           :effects    [[:draw 2]]
           :reacts-to  :attack
           :react-pred ::moat-can-react?
           :reaction   [[:mark-unaffected {:works :once}]]})

(defn moneylender-trash [game {:keys [player-no card-name]}]
  (cond-> game
          (= :copper card-name) (push-effect-stack {:player-no player-no
                                                    :effects   [[:trash-from-hand {:card-name :copper}]
                                                                [:give-coins 3]]})))

(effects/register {::moneylender-trash moneylender-trash})

(def moneylender {:name    :moneylender
                  :set     :dominion
                  :types   #{:action}
                  :cost    4
                  :effects [[:give-choice {:text    "You may trash a Copper from your hand for +$3"
                                           :choice  ::moneylender-trash
                                           :options [:player :hand {:name :copper}]
                                           :max     1}]]})

(defn poacher-discard [game {:keys [player-no]}]
  (let [empty-piles (ut/empty-supply-piles game)]
    (cond-> game
            (< 0 empty-piles) (give-choice {:player-no player-no
                                            :text      (str "Discard a card per empty supply pile [" empty-piles "].")
                                            :choice    :discard-from-hand
                                            :options   [:player :hand]
                                            :min       empty-piles
                                            :max       empty-piles}))))

(effects/register {::poacher-discard poacher-discard})

(def poacher {:name    :poacher
              :set     :dominion
              :types   #{:action}
              :cost    4
              :effects [[:draw 1]
                        [:give-actions 1]
                        [:give-coins 1]
                        [::poacher-discard]]})

(defn remodel-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        max-cost (+ 2 (ut/get-cost game card))]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:trash-from-hand {:card-name card-name}]
                                        [:give-choice {:text    (str "Gain a card costing up to $" max-cost ".")
                                                       :choice  :gain
                                                       :options [:supply {:max-cost max-cost}]
                                                       :min     1
                                                       :max     1}]]}))))

(effects/register {::remodel-trash remodel-trash})

(def remodel {:name    :remodel
              :set     :dominion
              :types   #{:action}
              :cost    4
              :effects [[:give-choice {:text    "Trash a card from your hand."
                                       :choice  ::remodel-trash
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]]})

(def sentry {:name    :sentry
             :set     :dominion
             :types   #{:action}
             :cost    5
             :effects [[:draw 1]
                       [:give-actions 1]
                       [:look-at 2]
                       [:give-choice {:text    "Trash any number of the top 2 cards of your deck."
                                      :choice  :trash-from-look-at
                                      :options [:player :look-at]}]
                       [:give-choice {:text    "Discard any number of the top 2 cards of your deck."
                                      :choice  :discard-from-look-at
                                      :options [:player :look-at]}]
                       [:give-choice {:text    "Put the rest back on top in any order."
                                      :choice  :topdeck-from-look-at
                                      :options [:player :look-at]
                                      :min     2}]]})

(def smithy {:name    :smithy
             :set     :dominion
             :types   #{:action}
             :cost    4
             :effects [[:draw 3]]})

(def throne-room {:name    :throne-room
                  :set     :dominion
                  :types   #{:action}
                  :cost    4
                  :effects [[:give-choice {:text    "You may play an Action card from your hand twice."
                                           :choice  :play-action-twice
                                           :options [:player :hand {:type :action}]
                                           :max     1}]]})

(defn vassal-play-action [game {:keys [player-no card-name]}]
  (let [{:keys [discard]} (get-in game [:players player-no])
        {:keys [name] :as card} (last discard)]
    (cond-> game
            (= name card-name) (-> (push-effect-stack {:player-no player-no
                                                       :effects   [[:play-from-discard {:card-name card-name}]
                                                                   [:card-effect {:card card}]]})))))

(effects/register {::vassal-play-action vassal-play-action})

(def vassal {:name    :vassal
             :set     :dominion
             :types   #{:action}
             :cost    3
             :effects [[:give-coins 2]
                       [:discard-from-topdeck 1]
                       [:give-choice {:text    "You may play the discarded Action."
                                      :choice  ::vassal-play-action
                                      :options [:player :discard {:type :action :last true}]
                                      :max     1}]]})

(def village {:name    :village
              :set     :dominion
              :types   #{:action}
              :cost    3
              :effects [[:draw 1]
                        [:give-actions 2]]})

(def witch {:name    :witch
            :set     :dominion
            :types   #{:action :attack}
            :cost    5
            :effects [[:draw 2]
                      [:attack {:effects [[:gain {:card-name :curse}]]}]]})

(def woodcutter {:name    :woodcutter
                 :set     :dominion
                 :types   #{:action}
                 :cost    3
                 :effects [[:give-coins 2]
                           [:give-buys 1]]})

(def workshop {:name    :workshop
               :set     :dominion
               :types   #{:action}
               :cost    3
               :effects [[:give-choice {:text    "Gain a card costing up to $4."
                                        :choice  :gain
                                        :options [:supply {:max-cost 4}]
                                        :min     1
                                        :max     1}]]})

(def kingdom-cards [artisan
                    bandit
                    bureaucrat
                    cellar
                    chapel
                    council-room
                    festival
                    gardens
                    harbinger
                    laboratory
                    library
                    market
                    merchant
                    militia
                    mine
                    moat
                    moneylender
                    poacher
                    remodel
                    sentry
                    smithy
                    throne-room
                    vassal
                    village
                    witch
                    workshop])
