(ns dombot.cards.adventures
  (:require [dombot.operations :refer [push-effect-stack give-choice move-card draw affect-all-players]]
            [dombot.cards.common :refer [add-trigger set-aside=>hand-trigger give-coins]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]
            [clojure.string :as string]))

(defn- setup-journey-token [game {:keys [player-no]}]
  (assoc-in game [:players player-no :journey-token] :face-up))

(defn- setup-journey-tokens [game args]
  (affect-all-players game {:effects [[::setup-journey-token]]}))

(defn- turn-journey-token [game {:keys [player-no]}]
  (update-in game [:players player-no :journey-token] #(if (= :face-up %) :face-down :face-up)))

(effects/register {::setup-journey-token  setup-journey-token
                   ::setup-journey-tokens setup-journey-tokens
                   ::turn-journey-token   turn-journey-token})

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

(defn- put-this-on-tavern-mat [game {:keys [player-no card-id]}]
  (move-card game {:player-no    player-no
                   :move-card-id card-id
                   :from         :play-area
                   :to           :tavern-mat}))

(defn- call-reserve [game {:keys [player-no card-id]}]
  (move-card game {:player-no    player-no
                   :move-card-id card-id
                   :from         :tavern-mat
                   :to           :play-area}))

(effects/register {:put-this-on-tavern-mat put-this-on-tavern-mat
                   :call-reserve           call-reserve})

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

(defn- distant-lands-victory-points [game {:keys [player-no card-id]}]
  (ut/update-in-vec game [:players player-no :tavern-mat] {:id card-id}
                    assoc :victory-points 4))

(effects/register {::distant-lands-victory-points distant-lands-victory-points})

(def distant-lands {:name    :distant-lands
                    :set     :adventures
                    :types   #{:action :reserve :victory}
                    :cost    5
                    :effects [[:put-this-on-tavern-mat]
                              [::distant-lands-victory-points]]})

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

(defn- giant-trample [game {:keys [player-no]}]
  (let [{:keys [name] :as card} (last (get-in game [:players player-no :revealed]))
        cost (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (and card (<= 3 cost 6))
                                          [[:trash-from-revealed {:card-name name}]]
                                          [[:discard-all-revealed]
                                           [:gain {:card-name :curse}]])})))

(defn- giant-journey [game {:keys [player-no]}]
  (let [journey-token (get-in game [:players player-no :journey-token])]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (= :face-down journey-token)
                                          [[:give-coins 1]]
                                          [[:give-coins 5]
                                           [:attack {:effects [[:reveal-from-deck 1]
                                                               [::giant-trample]]}]])})))

(effects/register {::giant-trample giant-trample
                   ::giant-journey giant-journey})

(def giant {:name    :giant
            :set     :adventures
            :types   #{:action :attack}
            :cost    5
            :effects [[::turn-journey-token]
                      [::giant-journey]]
            :setup   [[::setup-journey-tokens]]})

(defn- haunted-woods-topdeck-hand [game {:keys [player-no]}]
  (let [cards-in-hand (count (get-in game [:players player-no :hand]))]
    (give-choice game {:player-no player-no
                       :text      "Put your hand onto your deck in any order."
                       :choice    :topdeck-from-hand
                       :options   [:player :hand]
                       :min       cards-in-hand
                       :max       cards-in-hand})))

(effects/register {::haunted-woods-topdeck-hand haunted-woods-topdeck-hand})

(def haunted-woods-trigger {:event    :on-buy
                            :duration :attack
                            :effects  [[::haunted-woods-topdeck-hand]]})

(def haunted-woods {:name    :haunted-woods
                    :set     :adventures
                    :types   #{:action :attack :duration}
                    :cost    5
                    :effects [[:attack {:effects [[:add-trigger {:trigger haunted-woods-trigger}]]}]]
                    :trigger {:event    :at-start-turn
                              :duration :once
                              :mode     :semi
                              :effects  [[:draw 3]
                                         [:remove-enemy-triggers]]}})

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

(defn- miser-choice [game {:keys [player-no choice]}]
  (case choice
    :copper (move-card game {:player-no player-no
                             :card-name :copper
                             :from      :hand
                             :to        :tavern-mat})
    :coins (let [coppers-on-tavern (->> (get-in game [:players player-no :tavern-mat])
                                        (filter (comp #{:copper} :name))
                                        count)]
             (give-coins game {:player-no player-no
                               :arg       coppers-on-tavern}))))

(defn- miser-give-choice [game {:keys [player-no]}]
  (let [coppers-on-tavern (->> (get-in game [:players player-no :tavern-mat])
                               (filter (comp #{:copper} :name))
                               count)]
    (give-choice game {:player-no player-no
                       :text      "Choose one: "
                       :choice    ::miser-choice
                       :options   [:special
                                   {:option :copper :text "Put a Copper from your hand onto your Tavern mat."}
                                   {:option :coins :text (str "+$" coppers-on-tavern)}]
                       :min       1
                       :max       1})))

(effects/register {::miser-choice      miser-choice
                   ::miser-give-choice miser-give-choice})

(def miser {:name    :miser
            :set     :adventures
            :types   #{:action}
            :cost    4
            :effects [[::miser-give-choice]]})

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

(defn- ranger-journey [game {:keys [player-no]}]
  (let [journey-token (get-in game [:players player-no :journey-token])]
    (cond-> game
            (= :face-up journey-token) (draw {:player-no player-no
                                              :arg       5}))))

(effects/register {::ranger-journey ranger-journey})

(def ranger {:name    :ranger
             :set     :adventures
             :types   #{:action}
             :cost    4
             :effects [[:give-buys 1]
                       [::turn-journey-token]
                       [::ranger-journey]]
             :setup   [[::setup-journey-tokens]]})

(def ratcatcher {:name    :ratcatcher
                 :set     :adventures
                 :types   #{:action :reserve}
                 :cost    2
                 :effects [[:draw 1]
                           [:give-actions 1]
                           [:put-this-on-tavern-mat]]
                 :call    {:event   :at-start-turn
                           :effects [[:give-choice {:text    "Trash a card from your hand."
                                                    :choice  :trash-from-hand
                                                    :options [:player :hand]
                                                    :min     1
                                                    :max     1}]]}})

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

(def swamp-hag-trigger {:event    :on-buy
                        :duration :attack
                        :effects  [[:gain {:card-name :curse}]]})

(def swamp-hag {:name    :swamp-hag
                :set     :adventures
                :types   #{:action :attack :duration}
                :cost    5
                :effects [[:attack {:effects [[:add-trigger {:trigger swamp-hag-trigger}]]}]]
                :trigger {:event    :at-start-turn
                          :duration :once
                          :mode     :auto
                          :effects  [[:give-coins 3]
                                     [:remove-enemy-triggers]]}})

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
                    distant-lands
                    dungeon
                    gear
                    giant
                    haunted-woods
                    hireling
                    lost-city
                    magpie
                    miser
                    page
                    port
                    ranger
                    ratcatcher
                    raze
                    swamp-hag
                    treasure-trove])

(defn- alms-petition [game {:keys [player-no]}]
  (let [no-treasures-in-play? (->> (get-in game [:players player-no :play-area])
                                   (not-any? (comp :treasure (partial ut/get-types game))))]
    (cond-> game
            no-treasures-in-play? (give-choice {:player-no player-no
                                                :text      "Gain a card costing up to $4."
                                                :choice    :gain
                                                :options   [:supply {:max-cost 4}]
                                                :min       1
                                                :max       1}))))

(effects/register {::alms-petition alms-petition})

(def alms {:name          :alms
           :set           :adventures
           :type          :event
           :cost          0
           :once-per-turn true
           :on-buy        [[::alms-petition]]})

(def bonfire {:name   :bonfire
              :set    :adventures
              :type   :event
              :cost   3
              :on-buy [[:give-choice {:text    "Trash up to 2 cards you have in play."
                                      :choice  :trash-from-play-area
                                      :options [:player :play-area]
                                      :max     2}]]})

(def expedition-trigger {:name     :expedition
                         :event    :at-draw-hand
                         :duration :once
                         :effects  [[:draw 2]]})

(def expedition {:name   :expedition
                 :set    :adventures
                 :type   :event
                 :cost   3
                 :on-buy [[:add-trigger {:trigger expedition-trigger}]]})

(defn- pilgrimage-gain [game {:keys [player-no card-name card-names]}]
  (let [card-names (if card-name [card-name] card-names)]
    (assert (or (< (count card-names) 2)
                (apply distinct? card-names)) (str "Pilgrimage error: All choices must be different: " (->> card-names
                                                                                                            (map ut/format-name)
                                                                                                            (string/join ", "))))
    (cond-> game
            (not-empty card-names) (push-effect-stack {:player-no player-no
                                                       :effects   (->> card-names
                                                                       (map (fn [card-name]
                                                                              [:gain {:card-name card-name}])))}))))

(defn- pilgrimage-journey [game {:keys [player-no]}]
  (let [journey-token (get-in game [:players player-no :journey-token])]
    (cond-> game
            (= :face-up journey-token) (give-choice {:player-no player-no
                                                     :text      "Choose up to 3 differently named cards you have in play and gain a copy of each."
                                                     :choice    ::pilgrimage-gain
                                                     :options   [:player :play-area]
                                                     :unique?   true
                                                     :max       3}))))

(effects/register {::pilgrimage-gain    pilgrimage-gain
                   ::pilgrimage-journey pilgrimage-journey})

(def pilgrimage {:name          :pilgrimage
                 :set           :adventures
                 :type          :event
                 :cost          4
                 :once-per-turn true
                 :on-buy        [[::turn-journey-token]
                                 [::pilgrimage-journey]]
                 :setup         [[::setup-journey-tokens]]})

(defn- quest-discard [game {:keys [player-no card-name card-names required-cards]}]
  (let [card-names      (if card-name
                          [card-name]
                          card-names)
        number-of-cards (count card-names)]
    (cond-> game
            (pos? number-of-cards) (push-effect-stack {:player-no player-no
                                                       :effects   (concat [[:discard-from-hand {:card-names card-names}]]
                                                                          (when (= number-of-cards required-cards)
                                                                            [[:gain {:card-name :gold}]]))}))))

(defn- quest-choice [game {:keys [player-no choice]}]
  (case choice
    :attack (give-choice game {:player-no player-no
                               :text      "Discard an Attack."
                               :choice    [::quest-discard {:required-cards 1}]
                               :options   [:player :hand {:type :attack}]
                               :min       1
                               :max       1})
    :curses (give-choice game {:player-no player-no
                               :text      "Discard two Curses."
                               :choice    [::quest-discard {:required-cards 2}]
                               :options   [:player :hand {:name :curse}]
                               :min       2
                               :max       2})
    :six-cards (give-choice game {:player-no player-no
                                  :text      "Discard six cards."
                                  :choice    [::quest-discard {:required-cards 6}]
                                  :options   [:player :hand]
                                  :min       6
                                  :max       6})))

(effects/register {::quest-discard quest-discard
                   ::quest-choice  quest-choice})

(def quest {:name   :quest
            :set    :adventures
            :type   :event
            :cost   0
            :on-buy [[:give-choice {:text    "You may discard to gain a Gold."
                                    :choice  ::quest-choice
                                    :options [:special
                                              {:option :attack :text "Discard an Attack"}
                                              {:option :curses :text "Discard two Curses"}
                                              {:option :six-cards :text "Discard six cards"}]
                                    :min     1
                                    :max     1}]]})

(def save-trigger {:name     :save
                   :event    :at-draw-hand
                   :duration :once
                   :effects  [[:put-set-aside-into-hand]]})

(defn- save-set-aside [game {:keys [player-no card-name]}]
  (let [{:keys [card idx]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (-> game
        (update-in [:players player-no :hand] ut/vec-remove idx)
        (add-trigger {:player-no player-no
                      :trigger   (assoc save-trigger :set-aside [card])}))))

(effects/register {::save-set-aside save-set-aside})

(def save {:name          :save
           :set           :adventures
           :type          :event
           :cost          1
           :once-per-turn true
           :on-buy        [[:give-buys 1]
                           [:give-choice {:text    "Set aside a card from your hand, and put it into your hand at end of turn."
                                          :choice  ::save-set-aside
                                          :options [:player :hand]
                                          :min     1
                                          :max     1}]]})

(def scouting-party {:name   :scouting-party
                     :set    :adventures
                     :type   :event
                     :cost   2
                     :on-buy [[:give-buys 1]
                              [:look-at 5]
                              [:give-choice {:text    "Discard 3 of the top 5 cards of your deck."
                                             :choice  :discard-from-look-at
                                             :options [:player :look-at]
                                             :min     3
                                             :max     3}]
                              [:topdeck-all-look-at]]})

(defn- trade-trash [game {:keys [player-no card-name card-names]}]
  (let [card-names      (if card-name
                          [card-name]
                          card-names)
        number-of-cards (count card-names)]
    (cond-> game
            (pos? number-of-cards) (push-effect-stack {:player-no player-no
                                                       :effects   (concat [[:trash-from-hand {:card-names card-names}]]
                                                                          (repeat number-of-cards [:gain {:card-name :silver}]))}))))

(effects/register {::trade-trash trade-trash})

(def trade {:name   :trade
            :set    :adventures
            :type   :event
            :cost   5
            :on-buy [[:give-choice {:text    "Trash up to 2 cards from your hand."
                                    :choice  ::trade-trash
                                    :options [:player :hand]
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

(def events [alms
             bonfire
             expedition
             pilgrimage
             quest
             save
             scouting-party
             trade
             travelling-fair])
