(ns dombot.cards.intrigue
  (:require [dombot.operations :refer [gain move-card push-effect-stack give-choice draw peek-deck]]
            [dombot.cards.common :refer [give-actions give-coins give-buys]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn baron-choice [game player-no card-name]
  (if (= :estate card-name)
    (push-effect-stack game player-no [[:discard-from-hand :estate]
                                       [:give-coins 4]])
    (gain game player-no :estate)))

(effects/register {::baron-choice baron-choice})

(defn baron-discard-estate [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (if (some (comp #{:estate} :name) hand)
      (give-choice game player-no {:text      "You may discard an Estate for +$4."
                                   :player-no 0
                                   :choice    ::baron-choice
                                   :options   [:player :hand {:name :estate}]
                                   :max       1})
      (gain game player-no :estate))))

(effects/register {::baron-discard-estate baron-discard-estate})

(def baron {:name    :baron
            :set     :intrigue
            :types   #{:action}
            :cost    4
            :effects [[:give-buys 1]
                      [::baron-discard-estate]]})

(def bridge {:name    :bridge
             :set     :intrigue
             :types   #{:action}
             :cost    4
             :effects [[:give-buys 1]
                       [:give-coins 1]
                       [:add-cost-reduction 1]]})

(def courtyard {:name    :courtyard
                :set     :intrigue
                :types   #{:action}
                :cost    2
                :effects [[:draw 3]
                          [:give-choice {:text    "Put a card from your hand onto your deck."
                                         :choice  :topdeck-from-hand
                                         :options [:player :hand]
                                         :min     1
                                         :max     1}]]})

(defn diplomat-give-actions [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (<= (count hand) 5) (give-actions player-no 2))))

(effects/register {::diplomat-give-actions diplomat-give-actions})

(defn diplomat-can-react? [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (<= 5 (count hand))))

(effects/register-options {::diplomat-can-react? diplomat-can-react?}) ; todo: should be registered somewhere else

(def diplomat {:name       :diplomat
               :set        :intrigue
               :types      #{:action :reaction}
               :cost       4
               :effects    [[:draw 2]
                            [::diplomat-give-actions]]
               :reacts-to  :attack
               :react-pred ::diplomat-can-react?
               :reaction   [[:draw 2]
                            [:give-choice {:text    "Discard 3 cards."
                                           :choice  :discard-from-hand
                                           :options [:player :hand]
                                           :min     3
                                           :max     3}]]})

(def harem {:name           :harem
            :set            :intrigue
            :types          #{:treasure :victory}
            :cost           6
            :coin-value     2
            :victory-points 2})

(defn ironworks-gain [game player-no card-name]
  (let [{{:keys [types]} :card} (ut/get-pile-idx game card-name)]
    (push-effect-stack game player-no [[:gain card-name]
                                       (when (:action types) [:give-actions 1])
                                       (when (:treasure types) [:give-coins 1])
                                       (when (:victory types) [:draw 1])])))

(effects/register {::ironworks-gain ironworks-gain})

(def ironworks {:name    :ironworks
                :set     :intrigue
                :types   #{:action}
                :cost    4
                :effects [[:give-choice {:text    "Gain a card costing up to $4."
                                         :choice  ::ironworks-gain
                                         :options [:supply {:max-cost 4}]
                                         :min     1
                                         :max     1}]]})

(defn lurker-choice [game player-no choice]
  (case choice
    :trash (give-choice game player-no {:text    "Trash an Action card from the Supply."
                                        :choice  :trash-from-supply
                                        :options [:supply {:types :action}]
                                        :min     1
                                        :max     1})
    :gain (give-choice game player-no {:text    "Gain an Action card from the trash."
                                       :choice  :gain-from-trash
                                       :options [:trash {:types :action}]
                                       :min     1
                                       :max     1})))

(effects/register {::lurker-choice lurker-choice})

(def lurker {:name    :lurker
             :set     :intrigue
             :types   #{:action}
             :cost    2
             :effects [[:give-actions 1]
                       [:give-choice {:text    "Choose one:"
                                      :choice  ::lurker-choice
                                      :options [:special
                                                {:option :trash :text "Trash an Action card from the Supply."}
                                                {:option :gain :text "Gain an Action card from the trash."}]
                                      :min     1
                                      :max     1}]]})

(def mill {:name    :mill
           :set     :intrigue
           :types   #{:action :victory}
           :cost    4
           :effects []})

(defn mining-village-trash [game player-no card-name]
  (cond-> game
          (= :mining-village card-name) (push-effect-stack player-no [[:trash-last-from-play-area card-name]
                                                                      [:give-coins 2]])))

(effects/register {::mining-village-trash mining-village-trash})

(def mining-village {:name    :mining-village
                     :set     :intrigue
                     :types   #{:action}
                     :cost    4
                     :effects [[:draw 1]
                               [:give-actions 2]
                               [:give-choice {:text    "You may trash this for +$2."
                                              :choice  ::mining-village-trash
                                              :options [:player :play-area {:this true}]
                                              :max     1}]]})

(defn minion-attack [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (<= 5 (count hand)) (push-effect-stack player-no [[:discard-all-hand]
                                                              [:draw 4]]))))

(effects/register {::minion-attack minion-attack})

(defn minion-choice [game player-no choice]
  (cond-> game
          (= :coins choice) (give-coins player-no 2)
          (= :discard choice) (push-effect-stack player-no [[:discard-all-hand]
                                                            [:draw 4]
                                                            [:attack {:effects [[::minion-attack]]}]])))

(effects/register {::minion-choice minion-choice})

(def minion {:name    :minion
             :set     :intrigue
             :types   #{:action :attack}
             :cost    5
             :effects [[:give-actions 1]
                       [:give-choice {:text    "Choose one:"
                                      :choice  ::minion-choice
                                      :options [:special
                                                {:option :coins :text "+$2"}
                                                {:option :discard :text "Discard your hand, +4 Cards."}]
                                      :min     1
                                      :max     1}]]})

(defn nobles-choices [game player-no choice]
  (cond-> game
          (= :cards choice) (draw player-no 3)
          (= :actions choice) (give-actions player-no 2)))

(effects/register {::nobles-choice nobles-choices})

(def nobles {:name           :nobles
             :set            :intrigue
             :types          #{:action :victory}
             :cost           6
             :effects        [[:give-choice {:text    "Choose one:"
                                             :choice  ::nobles-choice
                                             :options [:special
                                                       {:option :cards :text "+3 Cards"}
                                                       {:option :actions :text "+2 Actions"}]
                                             :min     1
                                             :max     1}]]
             :victory-points 2})

(defn pawn-choices [game player-no choices]
  (let [choices (set choices)]
    (assert (= 2 (count choices)) "The choices must be different.")
    (cond-> game
            (:card choices) (draw player-no 1)
            (:action choices) (give-actions player-no 1)
            (:buy choices) (give-buys player-no 1)
            (:coin choices) (give-coins player-no 1))))

(effects/register {::pawn-choices pawn-choices})

(def pawn {:name    :pawn
           :set     :intrigue
           :types   #{:action}
           :cost    2
           :effects [[:give-choice {:text    "Choose two:"
                                    :choice  ::pawn-choices
                                    :options [:special
                                              {:option :card :text "+1 Card"}
                                              {:option :action :text "+1 Action"}
                                              {:option :buy :text "+1 Buy"}
                                              {:option :coin :text "+$1"}]
                                    :min     2
                                    :max     2}]]})

(defn shanty-town-draw [game player-no]
  (let [hand (get-in game [:players player-no :hand])
        action-cards-in-hand? (some (comp :action :types) hand)]
    (push-effect-stack game player-no [[:reveal-hand]
                                       (when-not action-cards-in-hand?
                                         [:draw 2])])))

(effects/register {::shanty-town-draw shanty-town-draw})

(def shanty-town {:name    :shanty-town
                  :set     :intrigue
                  :types   #{:action}
                  :cost    3
                  :effects [[:give-actions 2]
                            [::shanty-town-draw]]})

(defn steward-choices [game player-no choice]
  (cond-> game
          (= :cards choice) (draw player-no 2)
          (= :coins choice) (give-coins player-no 2)
          (= :trash choice) (give-choice player-no {:text    "Trash 2 cards from your hand."
                                                    :choice  :trash-from-hand
                                                    :options [:player :hand]
                                                    :min     2
                                                    :max     2})))

(effects/register {::steward-choices steward-choices})

(def steward {:name    :steward
              :set     :intrigue
              :types   #{:action}
              :cost    3
              :effects [[:give-choice {:text    "Choose one:"
                                       :choice  ::steward-choices
                                       :options [:special
                                                 {:option :cards :text "+2 Cards"}
                                                 {:option :coins :text "+$2"}
                                                 {:option :trash :text "Trash 2 cards from your hand."}]
                                       :min     1
                                       :max     1}]]})

(defn swindler-attack [game player-no]
  (-> game
      (peek-deck player-no 1)
      (as-> game
            (let [[top-card] (get-in game [:players player-no :deck])
                  cost (ut/get-cost game top-card)]
              (cond-> game
                      top-card (push-effect-stack player-no [[:trash-from-topdeck]
                                                             [:give-choice {:text    (str "Gain a card costing $" cost " (attacker chooses).")
                                                                            :choice  :gain
                                                                            :options [:supply {:cost cost}]
                                                                            :min     1
                                                                            :max     1}]]))))))

(effects/register {::swindler-attack swindler-attack})

(def swindler {:name    :swindler
               :set     :intrigue
               :types   #{:action :attack}
               :cost    3
               :effects [[:give-coins 2]
                         [:attack {:effects [[::swindler-attack]]}]]})

(defn trading-post-trash [game player-no card-names]
  (-> game
      (push-effect-stack player-no [[:trash-from-hand card-names]
                                    (when (and (coll? card-names) (= 2 (count card-names)))
                                      [:gain-to-hand :silver])])))

(effects/register {::trading-post-trash trading-post-trash})

(def trading-post {:name    :trading-post
                   :set     :intrigue
                   :types   #{:action}
                   :cost    5
                   :effects [[:give-choice {:text    "Trash 2 cards from your hand."
                                            :choice  ::trading-post-trash
                                            :options [:player :hand]
                                            :min     2
                                            :max     2}]]})

(defn upgrade-trash [game player-no card-name]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] card-name)
        cost (inc (ut/get-cost game card))]
    (-> game
        (push-effect-stack player-no [[:trash-from-hand card-name]
                                      [:give-choice {:text    (str "Gain a card costing exactly $" cost ".")
                                                     :choice  :gain
                                                     :options [:supply {:cost cost}]
                                                     :min     1
                                                     :max     1}]]))))

(effects/register {::upgrade-trash upgrade-trash})

(def upgrade {:name    :upgrade
              :set     :intrigue
              :types   #{:action}
              :cost    5
              :effects [[:draw 1]
                        [:give-actions 1]
                        [:give-choice {:text    "Trash a card from your hand."
                                       :choice  ::upgrade-trash
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]]})

(defn wishing-well-guess [game player-no guess]
  (-> game
      (peek-deck player-no 1)
      (as-> game
            (let [[{:keys [name]}] (get-in game [:players player-no :deck])]
              (push-effect-stack game player-no [[:reveal-from-deck 1]
                                                 (if (= guess name)
                                                   [:put-revealed-into-hand guess]
                                                   [:topdeck-from-revealed name])])))))

(effects/register {::wishing-well-guess wishing-well-guess})

(def wishing-well {:name    :wishing-well
                   :set     :intrigue
                   :types   #{:action}
                   :cost    3
                   :effects [[:draw 1]
                             [:give-actions 1]
                             [:give-choice {:text    "Name a card."
                                            :choice  ::wishing-well-guess
                                            :options [:supply]
                                            :min     1
                                            :max     1}]]})

(def kingdom-cards [baron
                    bridge
                    courtyard
                    diplomat
                    harem
                    ironworks
                    lurker
                    mining-village
                    minion
                    nobles
                    pawn
                    shanty-town
                    steward
                    swindler
                    trading-post
                    upgrade
                    wishing-well])