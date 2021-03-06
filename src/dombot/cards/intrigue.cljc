(ns dombot.cards.intrigue
  (:require [dombot.operations :refer [gain move-card move-cards push-effect-stack give-choice draw peek-deck]]
            [dombot.cards.common :refer [give-actions give-coins discard-from-hand]]
            [dombot.utils :as ut]
            [dombot.effects :as effects])
  (:refer-clojure :exclude [replace]))

(defn baron-choice [game {:keys [player-no card-name]}]
  (if (= :estate card-name)
    (push-effect-stack game {:player-no player-no
                             :effects   [[:discard-from-hand {:card-name :estate}]
                                         [:give-coins 4]]})
    (gain game {:player-no player-no
                :card-name :estate})))

(defn baron-discard-estate [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (if (some (comp #{:estate} :name) hand)
      (give-choice game {:player-no player-no
                         :text      "You may discard an Estate for +$4."
                         :choice    ::baron-choice
                         :options   [:player :hand {:name :estate}]
                         :max       1})
      (gain game {:player-no player-no
                  :card-name :estate}))))

(effects/register {::baron-choice         baron-choice
                   ::baron-discard-estate baron-discard-estate})

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

(defn conspirator-cantrip [game {:keys [player-no]}]
  (let [{:keys [actions-played]} (get-in game [:players player-no])]
    (cond-> game
            (<= 3 (count actions-played)) (push-effect-stack {:player-no player-no
                                                              :effects   [[:draw 1]
                                                                          [:give-actions 1]]}))))

(effects/register {::conspirator-cantrip conspirator-cantrip})

(def conspirator {:name    :conspirator
                  :set     :intrigue
                  :types   #{:action}
                  :cost    4
                  :effects [[:give-coins 2]
                            [::conspirator-cantrip]]})

(defn courtier-choices [game {:keys [player-no choice choices]}]
  (assert (or choices choice) "No choices specified for Courtier.")
  (let [choices (or choices [choice])]
    (assert (apply distinct? choices) "The choices must be different.")
    (push-effect-stack game {:player-no player-no
                             :effects   [(when (:action (set choices)) [:give-actions 1])
                                         (when (:buy (set choices)) [:give-buys 1])
                                         (when (:coins (set choices)) [:give-coins 3])
                                         (when (:gold (set choices)) [:gain {:card-name :gold}])]})))

(defn courtier-reveal [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        num-types (count (ut/get-types game card))]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:reveal {:card-name card-name}]
                                         [:give-choice {:text    (str "Choose " (ut/number->text num-types) ":")
                                                        :choice  ::courtier-choices
                                                        :options [:special
                                                                  {:option :action :text "+1 Action"}
                                                                  {:option :buy :text "+1 Buy"}
                                                                  {:option :coins :text "+$3"}
                                                                  {:option :gold :text "Gain a Gold."}]
                                                        :min     num-types
                                                        :max     num-types}]]})))

(effects/register {::courtier-choices courtier-choices
                   ::courtier-reveal  courtier-reveal})

(def courtier {:name    :courtier
               :set     :intrigue
               :types   #{:action}
               :cost    5
               :effects [[:give-choice {:text    "Reveal a card from your hand."
                                        :choice  ::courtier-reveal
                                        :options [:player :hand]
                                        :min     1
                                        :max     1}]]})

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

(defn diplomat-give-actions [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (<= (count hand) 5) (give-actions {:player-no player-no :arg 2}))))

(defn diplomat-can-react? [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (<= 5 (count hand))))

(effects/register {::diplomat-give-actions diplomat-give-actions
                   ::diplomat-can-react?   diplomat-can-react?})

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

(defn duke-victory-points [cards]
  (let [duchy-count (->> cards
                         (filter (comp #{:duchy} :name))
                         count)]
    {:victory-points duchy-count
     :notes          (str duchy-count " " (if (= 1 duchy-count)
                                            "Duchy"
                                            "Duchies"))}))

(effects/register {::duke-victory-points duke-victory-points})

(def duke {:name           :duke
           :set            :intrigue
           :types          #{:victory}
           :cost           5
           :victory-points ::duke-victory-points})

(def harem {:name           :harem
            :set            :intrigue
            :types          #{:treasure :victory}
            :cost           6
            :coin-value     2
            :victory-points 2})

(defn ironworks-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game card-name)
        types (ut/get-types game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:gain {:card-name card-name}]
                                         (when (:action types) [:give-actions 1])
                                         (when (:treasure types) [:give-coins 1])
                                         (when (:victory types) [:draw 1])]})))

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

(defn lurker-choice [game {:keys [player-no choice]}]
  (assert choice "No choice specified for Lurker.")
  (case choice
    :trash (give-choice game {:player-no player-no
                              :text      "Trash an Action card from the Supply."
                              :choice    :trash-from-supply
                              :options   [:supply {:type :action}]
                              :min       1
                              :max       1})
    :gain (give-choice game {:player-no player-no
                             :text      "Gain an Action card from the trash."
                             :choice    :gain-from-trash
                             :options   [:trash {:type :action}]
                             :min       1
                             :max       1})))

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

(defn masquerade-pass [{:keys [players] :as game} {:keys [player-no card-name]}]
  (let [next-player (mod (inc player-no) (count players))]
    (move-cards game {:player-no player-no
                      :card-name card-name
                      :from      :hand
                      :to-player next-player
                      :to        :masquerade-passed})))

(defn masquerade-take [game {:keys [player-no]}]
  (let [passed (get-in game [:players player-no :masquerade-passed])]
    (move-cards game {:player-no  player-no
                      :card-names (map :name passed)
                      :from       :masquerade-passed
                      :to         :hand})))

(effects/register {::masquerade-pass masquerade-pass
                   ::masquerade-take masquerade-take})

(def masquerade {:name    :masquerade
                 :set     :intrigue
                 :types   #{:action}
                 :cost    3
                 :effects [[:draw 2]
                           [:all-players {:effects [[:give-choice {:text    "Pass a card to the next player."
                                                                   :choice  ::masquerade-pass
                                                                   :options [:player :hand]
                                                                   :min     1
                                                                   :max     1}]]
                                          :at-once true}]
                           [:all-players {:effects [[::masquerade-take]]}]
                           [:give-choice {:text    "You may trash a card from your hand."
                                          :choice  :trash-from-hand
                                          :options [:player :hand]
                                          :max     1}]]})

(defn mill-discard [game {:keys [player-no card-name card-names]}]
  (cond-> game
          card-name (discard-from-hand {:player-no player-no
                                        :card-name card-name})
          card-names (push-effect-stack {:player-no player-no
                                         :effects   [[:discard-from-hand {:card-names card-names}]
                                                     (when (= 2 (ut/count-as-coll card-names))
                                                       [:give-coins 2])]})))

(effects/register {::mill-discard mill-discard})

(def mill {:name           :mill
           :set            :intrigue
           :types          #{:action :victory}
           :cost           4
           :effects        [[:draw 1]
                            [:give-actions 1]
                            [:give-choice {:text      "You may discard 2 cards, for +$2."
                                           :choice    ::mill-discard
                                           :options   [:player :hand]
                                           :min       2
                                           :max       2
                                           :optional? true}]]
           :victory-points 1})

(defn mining-village-trash [game {:keys [card-name] :as args}]
  (cond-> game
          (= :mining-village card-name) (push-effect-stack (merge args {:effects [[:trash-this]
                                                                                  [:give-coins 2]]}))))

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

(defn minion-attack [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (<= 5 (count hand)) (push-effect-stack {:player-no player-no
                                                    :effects   [[:discard-all-hand]
                                                                [:draw 4]]}))))

(defn minion-choice [game {:keys [player-no choice]}]
  (assert choice "No choice specified for Minion.")
  (cond-> game
          (= :coins choice) (give-coins {:player-no player-no :arg 2})
          (= :discard choice) (push-effect-stack {:player-no player-no
                                                  :effects   [[:discard-all-hand]
                                                              [:draw 4]
                                                              [:attack {:effects [[::minion-attack]]}]]})))

(effects/register {::minion-attack minion-attack
                   ::minion-choice minion-choice})

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

(defn nobles-choices [game {:keys [player-no choice]}]
  (assert choice "No choice specified for Nobles.")
  (case choice
    :cards (draw game {:player-no player-no :arg 3})
    :actions (give-actions game {:player-no player-no :arg 2})))

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

(def patrol {:name    :patrol
             :set     :intrigue
             :types   #{:action}
             :cost    5
             :effects [[:draw 3]
                       [:reveal-from-deck 4]
                       [:put-revealed-types-into-hand {:types #{:victory :curse}}]
                       [:give-choice {:text    "Put the revealed cards back on your deck."
                                      :choice  :topdeck-from-revealed
                                      :options [:player :revealed]
                                      :min     4
                                      :max     4}]]})

(defn pawn-choices [game {:keys [player-no choices]}]
  (assert choices "No choices specified for Pawn.")
  (assert (apply distinct? choices) "The choices must be different.")
  (push-effect-stack game {:player-no player-no
                           :effects   [(when (:card (set choices)) [:draw 1])
                                       (when (:action (set choices)) [:give-actions 1])
                                       (when (:buy (set choices)) [:give-buys 1])
                                       (when (:coin (set choices)) [:give-coins 1])]}))

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

(defn replace-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game card-name)
        types (ut/get-types game card)]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [(if (some #{:action :treasure} types)
                                          [:gain-to-topdeck {:card-name card-name}]
                                          [:gain {:card-name card-name}])
                                        (when (:victory types)
                                          [:attack {:effects [[:gain {:card-name :curse}]]}])]}))))

(defn replace-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        max-cost (-> (ut/get-cost game card)
                     (ut/add-to-cost 2))]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:trash-from-hand {:card-name card-name}]
                                        [:give-choice {:text    (str "Gain a card costing up to " (ut/format-cost max-cost) ".")
                                                       :choice  ::replace-gain
                                                       :options [:supply {:max-cost max-cost}]
                                                       :min     1
                                                       :max     1}]]}))))

(effects/register {::replace-gain  replace-gain
                   ::replace-trash replace-trash})

(def replace {:name    :replace
              :set     :intrigue
              :types   #{:action :attack}
              :cost    5
              :effects [[:give-choice {:text    "Trash a card from your hand."
                                       :choice  ::replace-trash
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]]})

(defn secret-passage-put [game {:keys [player-no position]}]
  (move-card game {:player-no     player-no
                   :from          :secret-passage
                   :from-position :top
                   :to            :deck
                   :to-position   position}))

(defn secret-passage-take [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:move-card {:card-name card-name
                                                    :from      :hand
                                                    :to        :secret-passage}]
                                       [:give-choice {:text    (str "Put the " (ut/format-name card-name) " anywhere in your deck.")
                                                      :choice  ::secret-passage-put
                                                      :options [:deck-position]
                                                      :min     1
                                                      :max     1}]]}))

(effects/register {::secret-passage-put  secret-passage-put
                   ::secret-passage-take secret-passage-take})

(def secret-passage {:name    :secret-passage
                     :set     :intrigue
                     :types   #{:action}
                     :cost    4
                     :effects [[:draw 2]
                               [:give-actions 1]
                               [:give-choice {:text    "Put a card from your hand anywhere in your deck."
                                              :choice  ::secret-passage-take
                                              :options [:player :hand]
                                              :min     1
                                              :max     1}]]})

(defn shanty-town-draw [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:reveal-hand]
                                         (when-not (some (comp :action (partial ut/get-types game)) hand)
                                           [:draw 2])]})))

(effects/register {::shanty-town-draw shanty-town-draw})

(def shanty-town {:name    :shanty-town
                  :set     :intrigue
                  :types   #{:action}
                  :cost    3
                  :effects [[:give-actions 2]
                            [::shanty-town-draw]]})

(defn steward-choices [game {:keys [player-no choice]}]
  (assert choice "No choice specified for Steward.")
  (case choice
    :cards (draw game {:player-no player-no :arg 2})
    :coins (give-coins game {:player-no player-no :arg 2})
    :trash (give-choice game {:player-no player-no
                              :text      "Trash 2 cards from your hand."
                              :choice    :trash-from-hand
                              :options   [:player :hand]
                              :min       2
                              :max       2})))

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

(defn swindler-attack [game {:keys [player-no]}]
  (let [{[top-card] :deck
         discard    :discard} (get-in game [:players player-no])
        cost (ut/get-cost game top-card)]
    (assert (or top-card (empty? discard)) "Discard was not properly shuffled for Swindler Attack.")
    (cond-> game
            top-card (push-effect-stack {:player-no player-no
                                         :effects   [[:trash-from-topdeck]
                                                     [:give-choice {:text    (str "Gain a card costing " (ut/format-cost cost) " (attacker chooses).")
                                                                    :choice  :gain
                                                                    :options [:supply {:cost cost}]
                                                                    :min     1
                                                                    :max     1}]]}))))

(effects/register {::swindler-attack swindler-attack})

(def swindler {:name    :swindler
               :set     :intrigue
               :types   #{:action :attack}
               :cost    3
               :effects [[:give-coins 2]
                         [:attack {:effects [[:peek-deck 1]
                                             [::swindler-attack]]}]]})

(defn torturer-choice [game {:keys [player-no choice]}]
  (case choice
    :discard (give-choice game {:player-no player-no
                                :text      "Discard 2 cards."
                                :choice    :discard-from-hand
                                :options   [:player :hand]
                                :min       2
                                :max       2})
    :curse (push-effect-stack game {:player-no player-no
                                    :effects   [[:gain-to-hand {:card-name :curse}]]})))

(effects/register {::torturer-choice torturer-choice})

(def torturer {:name    :torturer
               :set     :intrigue
               :types   #{:action :attack}
               :cost    5
               :effects [[:draw 3]
                         [:attack {:effects [[:give-choice {:text    "Choose one:"
                                                            :choice  ::torturer-choice
                                                            :options [:special
                                                                      {:option :discard :text "Discard 2 cards."}
                                                                      {:option :curse :text "Gain a Curse to your hand."}]
                                                            :min     1
                                                            :max     1}]]}]]})

(defn trading-post-trash [game {:keys [player-no card-name card-names]}]
  (assert (or card-name card-names) "No card-names specified for Trading Post.")
  (let [card-names (or card-names [card-name])]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:trash-from-hand {:card-names card-names}]
                                        (when (= 2 (ut/count-as-coll card-names))
                                          [:gain-to-hand {:card-name :silver}])]}))))

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

(def upgrade {:name    :upgrade
              :set     :intrigue
              :types   #{:action}
              :cost    5
              :effects [[:draw 1]
                        [:give-actions 1]
                        [:upgrade-give-choice]]})

(def wishing-well {:name    :wishing-well
                   :set     :intrigue
                   :types   #{:action}
                   :cost    3
                   :effects [[:draw 1]
                             [:give-actions 1]
                             [:peek-deck 1]
                             [:name-a-card {:effect :draw-named-card}]]})

(def kingdom-cards [baron
                    bridge
                    conspirator
                    courtier
                    courtyard
                    diplomat
                    duke
                    harem
                    ironworks
                    lurker
                    masquerade
                    mill
                    mining-village
                    minion
                    nobles
                    patrol
                    pawn
                    replace
                    secret-passage
                    shanty-town
                    steward
                    swindler
                    torturer
                    trading-post
                    upgrade
                    wishing-well])
