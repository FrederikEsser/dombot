(ns dombot.cards.dominion
  (:require [dombot.operations :refer [move-card push-effect-stack give-choice]]
            [dombot.cards.common :refer :all]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def artisan {:name    :artisan
              :set     :dominion
              :type    #{:action}
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
             :type    #{:action :attack}
             :cost    5
             :effects [[:gain :gold]
                       [:attack {:effects [[:reveal-from-deck 2]
                                           [:give-choice {:text    "Trash a revealed Treasure other than Copper, and discards the rest."
                                                          :choice  :trash-from-revealed
                                                          :options [:player :reveal {:type     :treasure
                                                                                     :not-name :copper}]
                                                          :min     1
                                                          :max     1}]
                                           [:discard-all-revealed]]}]]})

(defn bureaucrat-attack [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (if (some (comp :victory :type) hand)
      (give-choice game player-no {:text    "Reveal a Victory card from your hand and put it onto your deck."
                                   :choice  :topdeck-from-hand
                                   :options [:player :hand {:type :victory}]
                                   :min     1
                                   :max     1})
      (-> game
          (assoc-in [:reveal player-no] hand)))))

(effects/register {::bureaucrat-attack bureaucrat-attack})

(def bureaucrat {:name    :bureaucrat
                 :set     :dominion
                 :type    #{:action :attack}
                 :cost    4
                 :effects [[:gain-to-topdeck :silver]
                           [:attack {:effects [[::bureaucrat-attack]]}]]})

(defn cellar-sift [game player-no card-names]
  (push-effect-stack game player-no [[:discard-from-hand card-names]
                                     [:draw (count card-names)]]))

(effects/register {::cellar-sift cellar-sift})

(def cellar {:name    :cellar
             :set     :dominion
             :type    #{:action}
             :cost    2
             :effects [[:give-actions 1]
                       [:give-choice {:text    "Discard any number of cards, then draw that many."
                                      :choice  ::cellar-sift
                                      :options [:player :hand]}]]})

(def chapel {:name    :chapel
             :set     :dominion
             :type    #{:action}
             :cost    2
             :effects [[:give-choice {:text    "Trash up to 4 cards from your hand."
                                      :choice  :trash-from-hand
                                      :options [:player :hand]
                                      :max     4}]]})

(def council-room {:name    :council-room
                   :set     :dominion
                   :type    #{:action}
                   :cost    5
                   :effects [[:draw 4]
                             [:give-buys 1]
                             [:other-players {:effects [[:draw 1]]}]]})

(def festival {:name    :festival
               :set     :dominion
               :type    #{:action}
               :cost    5
               :effects [[:give-actions 2]
                         [:give-money 2]
                         [:give-buys 1]]})

(def gardens {:name           :gardens
              :set            :dominion
              :type           #{:victory}
              :cost           4
              :victory-points (fn [cards]
                                (Math/floorDiv (int (count cards)) (int 10)))}) ; TODO: Convert to data

(def harbinger {:name    :harbinger
                :set     :dominion
                :type    #{:action}
                :cost    3
                :effects [[:draw 1]
                          [:give-actions 1]
                          [:give-choice {:text    "You may put a card from your discard pile onto your deck."
                                         :choice  :topdeck-from-discard
                                         :options [:player :discard]
                                         :max     1}]]})

(def laboratory {:name    :laboratory
                 :set     :dominion
                 :type    #{:action}
                 :cost    5
                 :effects [[:draw 2]
                           [:give-actions 1]]})

(defn library-set-aside [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name     card-name
                                          :from          :hand
                                          :from-position :bottom
                                          :to            :set-aside})))

(defn library-check-for-action [game player-no]
  (let [hand (get-in game [:players player-no :hand])
        {:keys [type name]} (last hand)]
    (cond-> game
            (:action type) (give-choice player-no {:text    (str "You may skip the " (ut/format-name name) "; set it aside, discarding it afterwards.")
                                                   :choice  ::library-set-aside
                                                   :options [:player :hand {:last true}]
                                                   :max     1}))))

(defn library-draw [game player-no]
  (let [{:keys [hand deck discard]} (get-in game [:players player-no])]
    (cond-> game
            (and (< (count hand) 7)
                 (not-empty (concat deck discard))) (push-effect-stack player-no [[:draw 1]
                                                                                  [::library-check-for-action]
                                                                                  [::library-draw]]))))

(effects/register {::library-draw             library-draw
                   ::library-check-for-action library-check-for-action
                   ::library-set-aside        library-set-aside})

(def library {:name    :library
              :set     :dominion
              :type    #{:action}
              :cost    5
              :effects [[::library-draw]
                        [:discard-all-set-aside]]})

(def market {:name    :market
             :set     :dominion
             :type    #{:action}
             :cost    5
             :effects [[:draw 1]
                       [:give-actions 1]
                       [:give-money 1]
                       [:give-buys 1]]})

(def merchant-trigger {:trigger [:play :silver]
                       :effects [[:give-money 1]]})

(def merchant {:name    :merchant
               :set     :dominion
               :type    #{:action}
               :cost    3
               :effects [[:draw 1]
                         [:give-actions 1]
                         [:add-trigger merchant-trigger]]})

(def militia {:name    :militia
              :set     :dominion
              :type    #{:action :attack}
              :cost    4
              :effects [[:give-money 2]
                        [:attack {:effects [[:discard-down-to 3]]}]]})

(defn mine-trash [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {{:keys [cost]} :card} (ut/get-card-idx player :hand card-name)
        max-cost (+ 3 cost)]
    (-> game
        (push-effect-stack player-no [[:trash-from-hand card-name]
                                      [:give-choice {:text    (str "Gain a Treasure to your hand costing up to $" max-cost ".")
                                                     :choice  :gain-to-hand
                                                     :options [:supply {:max-cost max-cost :type :treasure}]
                                                     :min     1
                                                     :max     1}]]))))

(effects/register {::mine-trash mine-trash})

(def mine {:name    :mine
           :set     :dominion
           :type    #{:action}
           :cost    5
           :effects [[:give-choice {:text    "You may trash a Treasure from your hand."
                                    :choice  ::mine-trash
                                    :options [:player :hand {:type :treasure}]
                                    :max     1}]]})

(defn moat-reaction [{:keys [effect-stack] :as game} player-no]
  ; TODO: make sure it's the correct attack effect
  (let [{:keys [idx]} (ut/get-effect-idx effect-stack :attack)]
    (assert idx "There hasn't been played any Attack to Moat.")
    (-> game
        (assoc :effect-stack (-> effect-stack
                                 vec
                                 (update-in [idx :effect 1 :unaffected] conj player-no))))))

(effects/register {::moat-reaction moat-reaction})

(def moat {:name      :moat
           :set       :dominion
           :type      #{:action :reaction}
           :cost      2
           :effects   [[:draw 2]]
           :reacts-to :attack
           :reaction  [[::moat-reaction]]})

(defn moneylender-trash [game player-no card-name]
  (cond-> game
          (= :copper card-name) (push-effect-stack player-no [[:trash-from-hand :copper]
                                                              [:give-money 3]])))

(effects/register {::moneylender-trash moneylender-trash})

(def moneylender {:name    :moneylender
                  :set     :dominion
                  :type    #{:action}
                  :cost    4
                  :effects [[:give-choice {:text    "You may trash a Copper from your hand for +$3"
                                           :choice  ::moneylender-trash
                                           :options [:player :hand {:name :copper}]
                                           :max     1}]]})

(defn poacher-discard [game player-no]
  (let [empty-piles (ut/empty-supply-piles game)]
    (cond-> game
            (< 0 empty-piles) (give-choice player-no {:text    (str "Discard a card per empty supply pile [" empty-piles "].")
                                                      :choice  :discard-from-hand
                                                      :options [:player :hand]
                                                      :min     empty-piles
                                                      :max     empty-piles}))))

(effects/register {::poacher-discard poacher-discard})

(def poacher {:name    :poacher
              :set     :dominion
              :type    #{:action}
              :cost    4
              :effects [[:draw 1]
                        [:give-actions 1]
                        [:give-money 1]
                        [::poacher-discard]]})

(defn remodel-trash [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {{:keys [cost]} :card} (ut/get-card-idx player :hand card-name)
        max-cost (+ 2 cost)]
    (-> game
        (push-effect-stack player-no [[:trash-from-hand card-name]
                                      [:give-choice {:text    (str "Gain a card costing up to $" max-cost ".")
                                                     :choice  :gain
                                                     :options [:supply {:max-cost max-cost}]
                                                     :min     1
                                                     :max     1}]]))))

(effects/register {::remodel-trash remodel-trash})

(def remodel {:name    :remodel
              :set     :dominion
              :type    #{:action}
              :cost    4
              :effects [[:give-choice {:text    "Trash a card from your hand."
                                       :choice  ::remodel-trash
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]]})

(def sentry {:name    :sentry
             :set     :dominion
             :type    #{:action}
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
             :type    #{:action}
             :cost    4
             :effects [[:draw 3]]})

(def throne-room {:name    :throne-room
                  :set     :dominion
                  :type    #{:action}
                  :cost    4
                  :effects [[:give-choice {:text    "You may play an Action card from your hand twice."
                                           :choice  :play-action-twice
                                           :options [:player :hand {:type :action}]
                                           :max     1}]]})

(defn vassal-play-action [game player-no card-name]
  (let [{:keys [discard]} (get-in game [:players player-no])
        {:keys [name] :as card} (last discard)]
    (cond-> game
            (= name card-name) (-> (push-effect-stack player-no [[:play-from-discard card-name]
                                                                 [:card-effect card]])))))

(effects/register {::vassal-play-action vassal-play-action})

(def vassal {:name    :vassal
             :set     :dominion
             :type    #{:action}
             :cost    3
             :effects [[:give-money 2]
                       [:discard-from-topdeck 1]
                       [:give-choice {:text    "You may play the discarded Action."
                                      :choice  ::vassal-play-action
                                      :options [:player :discard {:type :action :last true}]
                                      :max     1}]]})

(def village {:name    :village
              :set     :dominion
              :type    #{:action}
              :cost    3
              :effects [[:draw 1]
                        [:give-actions 2]]})

(def witch {:name    :witch
            :set     :dominion
            :type    #{:action :attack}
            :cost    5
            :effects [[:draw 2]
                      [:attack {:effects [[:gain :curse]]}]]})

(def woodcutter {:name    :woodcutter
                 :set     :dominion
                 :type    #{:action}
                 :cost    3
                 :effects [[:give-money 2]
                           [:give-buys 1]]})

(def workshop {:name    :workshop :set :dominion :type #{:action} :cost 3
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