(ns dombot.cards.empires
  (:require [dombot.operations :refer [push-effect-stack give-choice attack-other-players gain]]
            [dombot.cards.common :refer [give-victory-points add-trigger]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- place-vp-token [game {:keys [card-name]}]
  (let [{:keys [idx]} (ut/get-pile-idx game card-name)]
    (-> game
        (update-in [:supply idx :tokens :victory-point :number-of-tokens] ut/plus 1))))

(defn- take-vp-tokens [game {:keys [player-no card-name]}]
  (let [{:keys [tokens idx]} (ut/get-pile-idx game card-name)
        vp-tokens (or (get-in tokens [:victory-point :number-of-tokens]) 0)]
    (-> game
        (update-in [:supply idx :tokens] dissoc :victory-point)
        (update-in [:supply idx] ut/dissoc-if-empty :tokens)
        (push-effect-stack {:player-no player-no
                            :effects   [[:give-victory-points vp-tokens]]}))))

(effects/register {::place-vp-token place-vp-token
                   ::take-vp-tokens take-vp-tokens})

(defn archive-put-card-into-hand [game {:keys [player-no trigger-id choice]}]
  (let [{:keys [trigger]} (ut/get-trigger-idx game [:players player-no :triggers] {:id trigger-id})
        {:keys [idx card]} (ut/get-card-idx trigger [:set-aside] {:name choice})]
    (-> game
        (ut/update-in-vec [:players player-no :triggers] {:id trigger-id}
                          update :set-aside ut/vec-remove idx)
        (update-in [:players player-no :hand] concat [card]))))

(def archive-trigger {:event    :at-start-turn
                      :duration :until-empty
                      :mode     :manual
                      :effects  [[::archive-pick-card]]})

(defn- archive-pick-card [game {:keys [player-no trigger-id card-id set-aside]}]
  (give-choice game {:player-no player-no
                     :card-id   card-id
                     :text      "Put a Card from your Archive into your hand."
                     :choice    [::archive-put-card-into-hand {:trigger-id trigger-id}]
                     :options   (concat [:special]
                                        (->> set-aside
                                             (map (fn [{:keys [name]}]
                                                    {:option name :text (ut/format-name name)}))
                                             set))
                     :min       1
                     :max       1}))

(defn- archive-clear-set-aside [game {:keys [player-no]}]
  (update-in game [:players player-no] dissoc :archive-set-aside))

(defn- archive-set-aside [game {:keys [player-no card-id]}]
  (let [set-aside  (get-in game [:players player-no :archive-set-aside])
        trigger-id (ut/next-id!)]
    (-> game
        (push-effect-stack {:player-no player-no
                            :card-id   card-id
                            :effects   (cond
                                         (< 1 (count set-aside)) [[::archive-clear-set-aside]
                                                                  [:add-trigger {:trigger (merge archive-trigger {:id        trigger-id
                                                                                                                  :set-aside set-aside})}]
                                                                  [::archive-pick-card {:trigger-id trigger-id
                                                                                        :set-aside  set-aside}]]
                                         (= 1 (count set-aside)) [[:move-cards {:number-of-cards 1
                                                                                :from            :archive-set-aside
                                                                                :from-position   :top
                                                                                :to              :hand}]])}))))

(effects/register {::archive-put-card-into-hand archive-put-card-into-hand
                   ::archive-pick-card          archive-pick-card
                   ::archive-clear-set-aside    archive-clear-set-aside
                   ::archive-set-aside          archive-set-aside})

(def archive {:name    :archive
              :set     :empires
              :types   #{:action :duration}
              :cost    5
              :effects [[:give-actions 1]
                        [:move-cards {:number-of-cards 3
                                      :from            :deck
                                      :from-position   :top
                                      :to              :archive-set-aside}]
                        [::archive-set-aside]]})

(def castles {:name       :castles
              :set        :empires
              :cost       3
              :split-pile ::castles-pile})

(defn humble-castle-victory-points [cards]
  (->> cards
       (filter (comp :castle :types))
       count))

(def humble-castle {:name           :humble-castle
                    :set            :empires
                    :types          #{:treasure :victory :castle}
                    :cost           3
                    :coin-value     1
                    :victory-points ::humble-castle-victory-points})

(def crumbling-castle {:name           :crumbling-castle
                       :set            :empires
                       :types          #{:victory :castle}
                       :cost           4
                       :victory-points 1
                       :on-gain        [[:give-victory-points 1]
                                        [:gain {:card-name :silver}]]
                       :on-trash       [[:give-victory-points 1]
                                        [:gain {:card-name :silver}]]})

(defn- small-castle-trash-from-area [game {:keys [player-no choice]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no (:area choice)] {:name (:card-name choice)})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:trash-from-area {:choice choice}]
                                                 [:give-choice {:text    "Gain a Castle."
                                                                :choice  :gain
                                                                :options [:supply {:type :castle}]
                                                                :min     1
                                                                :max     1}]]}))))

(def small-castle {:name           :small-castle
                   :set            :empires
                   :types          #{:action :victory :castle}
                   :cost           5
                   :effects        [[:give-choice {:text    "Trash this or a Castle from your hand."
                                                   :choice  ::small-castle-trash-from-area
                                                   :options [:mixed
                                                             [:player :play-area {:this true}]
                                                             [:player :hand {:type :castle}]]
                                                   :min     1
                                                   :max     1}]]
                   :victory-points 2})

(defn- haunted-castle-spook [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (>= (count hand) 5) (give-choice {:player-no player-no
                                              :text      "Put 2 cards from your hand onto your deck."
                                              :choice    :topdeck-from-hand
                                              :options   [:player :hand]
                                              :min       2
                                              :max       2}))))

(defn- haunted-castle-on-gain [{:keys [current-player] :as game} {:keys [player-no]}]
  (cond-> game
          (= current-player player-no) (push-effect-stack {:player-no player-no
                                                           :effects   [[:gain {:card-name :gold}]
                                                                       [:other-players {:effects [[::haunted-castle-spook]]}]]})))

(def haunted-castle {:name           :haunted-castle
                     :set            :empires
                     :types          #{:victory :castle}
                     :cost           6
                     :victory-points 2
                     :on-gain        [[::haunted-castle-on-gain]]})

(defn- opulent-castle-discard-victory [game {:keys [player-no card-names]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:discard-from-hand {:card-names card-names}]
                                       [:give-coins (* 2 (count card-names))]]}))

(def opulent-castle {:name           :opulent-castle
                     :set            :empires
                     :types          #{:action :victory :castle}
                     :cost           7
                     :effects        [[:give-choice {:text    "Discard any number of Victory cards for $2 per card."
                                                     :choice  ::opulent-castle-discard-victory
                                                     :options [:player :hand {:type :victory}]}]]
                     :victory-points 3})

(defn- sprawling-castle-gain [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case card-name
                                        :duchy [[:gain {:card-name :duchy}]]
                                        :estate (repeat 3 [:gain {:card-name :estate}]))}))

(def sprawling-castle {:name           :sprawling-castle
                       :set            :empires
                       :types          #{:victory :castle}
                       :cost           8
                       :victory-points 4
                       :on-gain        [[:give-choice {:text    "Gain a Duchy or 3 Estates."
                                                       :choice  ::sprawling-castle-gain
                                                       :options [:supply {:names #{:duchy :estate}
                                                                          :all   true}]
                                                       :min     1
                                                       :max     1}]]})

(defn- grand-castle-on-gain [game {:keys [player-no]}]
  (let [hand          (get-in game [:players player-no :hand])
        play-area     (get-in game [:players player-no :play-area])
        victory-cards (->> (concat hand play-area)
                           (filter (comp :victory (partial ut/get-types game)))
                           count)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:reveal-hand]
                                         [:give-victory-points victory-cards]]})))

(def grand-castle {:name           :grand-castle
                   :set            :empires
                   :types          #{:victory :castle}
                   :cost           9
                   :victory-points 5
                   :on-gain        [[::grand-castle-on-gain]]})

(defn kings-castle-victory-points [cards]
  (->> cards
       (filter (comp :castle :types))
       count
       (* 2)))

(def kings-castle {:name           :king's-castle
                   :set            :empires
                   :types          #{:victory :castle}
                   :cost           10
                   :victory-points ::kings-castle-victory-points})

(defn castles-pile [player-count]
  (let [one-or-two (if (> player-count 2) 2 1)]
    {:split-pile [{:card humble-castle :pile-size one-or-two}
                  {:card crumbling-castle :pile-size 1}
                  {:card small-castle :pile-size one-or-two}
                  {:card haunted-castle :pile-size 1}
                  {:card opulent-castle :pile-size one-or-two}
                  {:card sprawling-castle :pile-size 1}
                  {:card grand-castle :pile-size 1}
                  {:card kings-castle :pile-size one-or-two}]}))

(effects/register {::humble-castle-victory-points   humble-castle-victory-points
                   ::small-castle-trash-from-area   small-castle-trash-from-area
                   ::haunted-castle-spook           haunted-castle-spook
                   ::haunted-castle-on-gain         haunted-castle-on-gain
                   ::opulent-castle-discard-victory opulent-castle-discard-victory
                   ::sprawling-castle-gain          sprawling-castle-gain
                   ::grand-castle-on-gain           grand-castle-on-gain
                   ::kings-castle-victory-points    kings-castle-victory-points
                   ::castles-pile                   castles-pile})

(defn- catapult-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost  (ut/get-cost game card)
        types (ut/get-types game card)]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   (concat [[:trash-from-hand {:card-name card-name}]]
                                                        (when (>= cost 3)
                                                          [[:attack {:effects [[:gain {:card-name :curse}]]}]])
                                                        (when (:treasure types)
                                                          [[:attack {:effects [[:discard-down-to 3]]}]]))}))))

(def catapult {:name       :catapult
               :set        :empires
               :types      #{:action :attack}
               :cost       3
               :effects    [[:give-coins 1]
                            [:give-choice {:text    "Trash a card from your hand."
                                           :choice  ::catapult-trash
                                           :options [:player :hand]
                                           :min     1
                                           :max     1}]]
               :split-pile ::catapult-rocks-pile})

(defn- rocks-gain-silver [game {:keys [player-no]}]
  (let [phase (get-in game [:players player-no :phase])]
    (gain game (merge {:player-no player-no
                       :card-name :silver}
                      (if (= :buy phase)
                        {:to          :deck
                         :to-position :top}
                        {:to :hand})))))

(def rocks {:name       :rocks
            :set        :empires
            :types      #{:treasure}
            :cost       4
            :coin-value 1
            :on-gain    [[::rocks-gain-silver]]
            :on-trash   [[::rocks-gain-silver]]})

(defn catapult-rocks-pile [player-count]
  {:split-pile [{:card catapult :pile-size 5}
                {:card rocks :pile-size 5}]})

(effects/register {::catapult-trash      catapult-trash
                   ::rocks-gain-silver   rocks-gain-silver
                   ::catapult-rocks-pile catapult-rocks-pile})

(defn- chariot-race-compare [{:keys [players] :as game} {:keys [player-no]}]
  (let [next-player (mod (inc player-no) (count players))
        own-card    (first (get-in game [:players player-no :revealed]))
        other-card  (first (get-in game [:players next-player :revealed]))
        race-won?   (and own-card
                         other-card
                         (> (ut/get-cost game own-card) (ut/get-cost game other-card)))]
    (cond-> game
            other-card (push-effect-stack {:player-no next-player
                                           :effects   [[:topdeck-from-revealed {:card-name (:name other-card)}]]})

            own-card (push-effect-stack {:player-no player-no
                                         :effects   (concat [[:put-revealed-into-hand {:card-name (:name own-card)}]]
                                                            (when race-won?
                                                              [[:give-coins 1]
                                                               [:give-victory-points 1]]))}))))

(defn- chariot-race-reveal [{:keys [players] :as game} {:keys [player-no]}]
  (let [next-player (mod (inc player-no) (count players))]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:reveal-from-deck 1]
                                        [::chariot-race-compare]]})
        (push-effect-stack {:player-no next-player
                            :effects   [[:reveal-from-deck 1]]}))))

(effects/register {::chariot-race-compare chariot-race-compare
                   ::chariot-race-reveal  chariot-race-reveal})

(def chariot-race {:name    :chariot-race
                   :set     :empires
                   :types   #{:action}
                   :cost    3
                   :effects [[:give-actions 1]
                             [::chariot-race-reveal]]})

(defn- charm-on-buy [game {:keys [player-no card-name trigger-id]}]
  (let [{card :card} (ut/get-pile-idx game card-name)
        cost (ut/get-cost game card)]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:remove-trigger {:trigger-id trigger-id}]
                                        [:give-choice {:text    (str "You may gain a card other than " (ut/format-name card-name) " costing exactly $" cost ".")
                                                       :choice  :gain
                                                       :options [:supply {:not-names #{card-name}
                                                                          :cost      cost}]
                                                       :max     1}]]}))))


(defn- charm-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :coins [[:give-buys 1]
                                                [:give-coins 2]]
                                        :gain [[:add-trigger {:trigger {:event    :on-buy
                                                                        :duration :once-turn
                                                                        :effects  [[::charm-on-buy]]}}]])}))

(effects/register {::charm-on-buy charm-on-buy
                   ::charm-choice charm-choice})


(def charm {:name            :charm
            :set             :empires
            :types           #{:treasure}
            :cost            5
            :effects         [[:give-choice {:text    "Choose one:"
                                             :choice  ::charm-choice
                                             :options [:special
                                                       {:option :coins :text "+1 Buy and +$2"}
                                                       {:option :gain :text "The next time you buy a card this turn, you may also gain a differently named card with the same cost."}]
                                             :min     1
                                             :max     1}]]
            :auto-play-index 2})

(defn- crown-repeat-card [game {:keys [player-no card-id]}]
  (let [phase (get-in game [:players player-no :phase])]
    (cond-> game
            (#{:action :pay :buy} phase) (give-choice {:player-no player-no
                                                       :card-id   card-id
                                                       :text      (str "You may play "
                                                                       (case phase
                                                                         :action "an Action"
                                                                         :pay "a Treasure"
                                                                         :buy "a Treasure")
                                                                       " from your hand twice.")
                                                       :choice    [:repeat-action {:times 2}]
                                                       :options   [:player :hand {:type (case phase
                                                                                          :action :action
                                                                                          :pay :treasure
                                                                                          :buy :treasure)}]
                                                       :max       1}))))

(effects/register {::crown-repeat-card crown-repeat-card})

(def crown {:name            :crown
            :set             :empires
            :types           #{:action :treasure}
            :cost            5
            :effects         [[::crown-repeat-card]]
            :auto-play-index -1})

(defn encampment-return-to-supply [game {:keys [set-aside]}]
  (assert (= 1 (count set-aside)) (str "Encampment error: " (count set-aside) " cards were set aside: ["
                                       (->> set-aside
                                            (map (comp ut/format-name :name))
                                            (clojure.string/join ", ")) "]"))
  (let [{:keys [idx]} (ut/get-pile-idx game :supply :encampment #{:include-empty-split-piles})]
    (update-in game [:supply idx] ut/add-top-card (first set-aside))))

(def encampment-trigger {:name     :encampment
                         :event    :at-clean-up
                         :duration :once
                         :effects  [[::encampment-return-to-supply]]})

(defn- encampment-add-trigger [game {:keys [player-no]}]
  (let [set-aside (get-in game [:players player-no :encampment-set-aside])]
    (-> game
        (update-in [:players player-no] dissoc :encampment-set-aside)
        (add-trigger {:player-no player-no
                      :trigger   (merge encampment-trigger {:set-aside set-aside})}))))

(defn- encampment-set-aside [game {:keys [player-no card-id card-name]}]
  (cond-> game
          (nil? card-name) (push-effect-stack {:player-no player-no
                                               :effects   [[:move-card {:move-card-id card-id
                                                                        :from         :play-area
                                                                        :to           :encampment-set-aside}]
                                                           [::encampment-add-trigger]]})))

(defn- encampment-reveal-money [game {:keys [player-no] :as args}]
  (let [hand (get-in game [:players player-no :hand])]
    (push-effect-stack game (merge args
                                   {:effects (if (some (comp #{:gold :plunder} :name) hand)
                                               [[:give-choice {:text    "You may reveal a Gold or Plunder from your hand."
                                                               :choice  ::encampment-set-aside
                                                               :options [:player :hand {:names #{:gold :plunder}}]
                                                               :max     1}]]
                                               [[::encampment-set-aside]])}))))

(def encampment {:name       :encampment
                 :set        :empires
                 :types      #{:action}
                 :cost       2
                 :effects    [[:draw 2]
                              [:give-actions 2]
                              [::encampment-reveal-money]]
                 :split-pile ::encampment-plunder-pile})

(def plunder {:name       :plunder
              :set        :empires
              :types      #{:treasure}
              :cost       5
              :coin-value 2
              :effects    [[:give-victory-points 1]]})

(defn encampment-plunder-pile [player-count]
  {:split-pile [{:card encampment :pile-size 5}
                {:card plunder :pile-size 5}]})

(effects/register {::encampment-return-to-supply encampment-return-to-supply
                   ::encampment-add-trigger      encampment-add-trigger
                   ::encampment-set-aside        encampment-set-aside
                   ::encampment-reveal-money     encampment-reveal-money
                   ::encampment-plunder-pile     encampment-plunder-pile})

(def enchantress-trigger {:event    :instead-of-first-action
                          :duration :attack
                          :effects  [[:draw 1]
                                     [:give-actions 1]]})

(def enchantress {:name    :enchantress
                  :set     :empires
                  :types   #{:action :attack :duration}
                  :cost    3
                  :effects [[:attack {:effects [[:add-trigger {:trigger enchantress-trigger}]]}]]
                  :trigger {:event    :at-start-turn
                            :duration :once
                            :mode     :semi
                            :effects  [[:draw 2]
                                       [:remove-enemy-triggers]]}})

(defn- farmers-market-yield [game {:keys [player-no card-id]}]
  (let [{:keys [tokens]} (ut/get-pile-idx game :farmers'-market)
        vp-tokens (or (get-in tokens [:victory-point :number-of-tokens]) 0)]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   (if (>= vp-tokens 4)
                                         [[:trash-from-play-area {:trash-card-id card-id}]
                                          [::take-vp-tokens {:card-name :farmers'-market}]]
                                         [[::place-vp-token {:card-name :farmers'-market}]
                                          [:give-coins (inc vp-tokens)]])}))))

(effects/register {::farmers-market-yield farmers-market-yield})

(def farmers-market {:name    :farmers'-market
                     :set     :empires
                     :types   #{:action :gathering}
                     :cost    3
                     :effects [[:give-buys 1]
                               [::farmers-market-yield]]})

(def forum {:name    :forum
            :set     :empires
            :types   #{:action}
            :cost    5
            :effects [[:draw 3]
                      [:give-actions 1]
                      [:give-choice {:text    "Discard 2 cards."
                                     :choice  :discard-from-hand
                                     :options [:player :hand]
                                     :min     2
                                     :max     2}]]
            :on-buy  [[:give-buys 1]]})

(defn- groundskeeper-on-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        types (ut/get-types game card)]
    (cond-> game
            (:victory types) (give-victory-points {:player-no player-no :arg 1}))))

(def groundskeeper {:name          :groundskeeper
                    :set           :empires
                    :types         #{:action}
                    :cost          5
                    :effects       [[:draw 1]
                                    [:give-actions 1]]
                    :while-in-play {:on-gain [[::groundskeeper-on-gain]]}})

(effects/register {::groundskeeper-on-gain groundskeeper-on-gain})

(defn- legionary-attack [game {:keys [player-no card-name]}]
  (cond-> game
          (= :gold card-name) (attack-other-players {:player-no player-no
                                                     :effects   [[:discard-down-to 2]
                                                                 [:draw 1]]})))

(effects/register {::legionary-attack legionary-attack})

(def legionary {:name    :legionary
                :set     :empires
                :types   #{:action :attack}
                :cost    5
                :effects [[:give-coins 3]
                          [:give-choice {:text    "You may reveal a Gold from your hand."
                                         :choice  ::legionary-attack
                                         :options [:player :hand {:name :gold}]
                                         :max     1}]]})

(defn- patrician-take-card [game {:keys [player-no]}]
  (let [revealed (get-in game [:players player-no :revealed])
        {:keys [name] :as card} (first revealed)
        cost     (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (and card (>= cost 5))
                                          [[:take-from-revealed {:card-name name}]]
                                          [[:topdeck-all-revealed]])})))

(def patrician {:name       :patrician
                :set        :empires
                :types      #{:action}
                :cost       2
                :effects    [[:draw 1]
                             [:give-actions 1]
                             [:reveal-from-deck 1]
                             [::patrician-take-card]]
                :split-pile ::patrician-emporium-pile})

(defn- emporium-on-gain [game {:keys [player-no]}]
  (let [actions-in-play (->> (get-in game [:players player-no :play-area])
                             (filter (comp :action (partial ut/get-types game)))
                             count)]
    (cond-> game
            (>= actions-in-play 5) (give-victory-points {:player-no player-no :arg 2}))))

(def emporium {:name    :emporium
               :set     :empires
               :types   #{:action}
               :cost    5
               :effects [[:draw 1]
                         [:give-actions 1]
                         [:give-coins 1]]
               :on-gain [[::emporium-on-gain]]})

(defn patrician-emporium-pile [player-count]
  {:split-pile [{:card patrician :pile-size 5}
                {:card emporium :pile-size 5}]})

(effects/register {::patrician-take-card     patrician-take-card
                   ::emporium-on-gain        emporium-on-gain
                   ::patrician-emporium-pile patrician-emporium-pile})

(defn- sacrifice-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        types (ut/get-types game card)]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   (concat
                                                  [[:trash-from-hand {:card-name card-name}]]
                                                  (when (types :action)
                                                    [[:draw 2]
                                                     [:give-actions 2]])
                                                  (when (types :treasure)
                                                    [[:give-coins 2]])
                                                  (when (types :victory)
                                                    [[:give-victory-points 2]]))}))))

(effects/register {::sacrifice-trash sacrifice-trash})

(def sacrifice {:name    :sacrifice
                :set     :empires
                :types   #{:action}
                :cost    4
                :effects [[:give-choice {:text    "Trash a card from your hand."
                                         :choice  ::sacrifice-trash
                                         :options [:player :hand]
                                         :min     1
                                         :max     1}]]})

(def settlers {:name       :settlers
               :set        :empires
               :types      #{:action}
               :cost       2
               :effects    [[:draw 1]
                            [:give-actions 1]
                            [:give-choice {:text    "You may put a Copper from your discard pile into your hand."
                                           :choice  :take-from-discard
                                           :options [:player :discard {:name :copper}]
                                           :max     1}]]
               :split-pile ::settlers-bustling-village-pile})

(def bustling-village {:name    :bustling-village
                       :set     :empires
                       :types   #{:action}
                       :cost    5
                       :effects [[:draw 1]
                                 [:give-actions 3]
                                 [:give-choice {:text    "You may put a Settlers from your discard pile into your hand."
                                                :choice  :take-from-discard
                                                :options [:player :discard {:name :settlers}]
                                                :max     1}]]})

(defn settlers-bustling-village-pile [player-count]
  {:split-pile [{:card settlers :pile-size 5}
                {:card bustling-village :pile-size 5}]})

(effects/register {::settlers-bustling-village-pile settlers-bustling-village-pile})

(def temple {:name    :temple
             :set     :empires
             :types   #{:action :gathering}
             :cost    4
             :effects [[:give-victory-points 1]
                       [:give-choice {:text    "Trash from 1 to 3 differently named cards from your hand."
                                      :choice  :trash-from-hand
                                      :options [:player :hand]
                                      :unique? true
                                      :min     1
                                      :max     3}]
                       [::place-vp-token {:card-name :temple}]]
             :on-gain [[::take-vp-tokens {:card-name :temple}]]})

(defn- villa-return-to-action-phase [game {:keys [player-no]}]
  (let [phase (get-in game [:players player-no :phase])]
    (cond-> game
            (= :buy phase) (assoc-in [:players player-no :phase] :action))))

(effects/register {::villa-return-to-action-phase villa-return-to-action-phase})

(def villa {:name    :villa
            :set     :empires
            :types   #{:action}
            :cost    4
            :effects [[:give-actions 2]
                      [:give-buys 1]
                      [:give-coins 1]]
            :gain-to :hand
            :on-gain [[:give-actions 1]
                      [::villa-return-to-action-phase]]})

(defn- wild-hunt-choice [game {:keys [player-no choice]}]
  (case choice
    :cards (push-effect-stack game {:player-no player-no
                                    :effects   [[:draw 3]
                                                [::place-vp-token {:card-name :wild-hunt}]]})
    :estate (let [{:keys [pile-size]} (ut/get-pile-idx game :estate)]
              (cond-> game
                      (pos? pile-size) (push-effect-stack {:player-no player-no
                                                           :effects   [[:gain {:card-name :estate}]
                                                                       [::take-vp-tokens {:card-name :wild-hunt}]]})))))

(effects/register {::wild-hunt-choice wild-hunt-choice})

(def wild-hunt {:name    :wild-hunt
                :set     :empires
                :types   #{:action :gathering}
                :cost    5
                :effects [[:give-choice {:text    "Choose one:"
                                         :choice  ::wild-hunt-choice
                                         :options [:special
                                                   {:option :cards :text "+3 Cards and add 1VP to the Wild Hunt Supply pile"}
                                                   {:option :estate :text "Gain an Estate and the VP from the pile"}]
                                         :min     1
                                         :max     1}]]})

(def kingdom-cards [archive
                    castles
                    catapult
                    chariot-race
                    charm
                    crown
                    encampment
                    enchantress
                    farmers-market
                    forum
                    groundskeeper
                    legionary
                    patrician
                    sacrifice
                    settlers
                    temple
                    villa
                    wild-hunt])


(defn- advance-trash [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:trash-from-hand {:card-name card-name}]
                                                    [:give-choice {:text    "Gain a Action card costing up to $6."
                                                                   :choice  :gain
                                                                   :options [:supply {:type     :action
                                                                                      :max-cost 6}]
                                                                   :min     1
                                                                   :max     1}]]})))

(effects/register {::advance-trash advance-trash})

(def advance {:name   :advance
              :set    :empires
              :type   :event
              :cost   0
              :on-buy [[:give-choice {:text    "You may trash an Action card from your hand."
                                      :choice  ::advance-trash
                                      :options [:player :hand {:type :action}]
                                      :max     1}]]})

(def banquet {:name   :banquet
              :set    :empires
              :type   :event
              :cost   3
              :on-buy [[:gain {:card-name :copper}]
                       [:gain {:card-name :copper}]
                       [:give-choice {:text    "Gain a non-Victory card costing up to $5."
                                      :choice  :gain
                                      :options [:supply {:not-type :victory
                                                         :max-cost 5}]
                                      :min     1
                                      :max     1}]]})

(defn- conquest-victory-points [game {:keys [player-no]}]
  (let [gained-silvers (->> (get-in game [:players player-no :gained-cards])
                            (filter (comp #{:silver} :name))
                            count)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:give-victory-points gained-silvers]]})))

(effects/register {::conquest-victory-points conquest-victory-points})

(def conquest {:name   :conquest
               :set    :empires
               :type   :event
               :cost   6
               :on-buy [[:gain {:card-name :silver}]
                        [:gain {:card-name :silver}]
                        [::conquest-victory-points]]})

(def delve {:name   :delve
            :set    :empires
            :type   :event
            :cost   2
            :on-buy [[:give-buys 1]
                     [:gain {:card-name :silver}]]})

(defn- dominate-gain-province [game {:keys [player-no]}]
  (let [{:keys [pile-size]} (ut/get-pile-idx game :province)]
    (cond-> game
            (pos? pile-size) (push-effect-stack {:player-no player-no
                                                 :effects   [[:gain {:card-name :province}]
                                                             [:give-victory-points 9]]}))))

(effects/register {::dominate-gain-province dominate-gain-province})

(def dominate {:name   :dominate
               :set    :empires
               :type   :event
               :cost   14
               :on-buy [[::dominate-gain-province]]})

(defn- ritual-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:trash-from-hand {:card-name card-name}]
                                                 [:give-victory-points cost]]}))))

(defn- ritual-gain-curse [game {:keys [player-no]}]
  (let [{:keys [pile-size]} (ut/get-pile-idx game :curse)]
    (cond-> game
            (pos? pile-size) (push-effect-stack {:player-no player-no
                                                 :effects   [[:gain {:card-name :curse}]
                                                             [:give-choice {:text    "Trash a card from your hand."
                                                                            :choice  ::ritual-trash
                                                                            :options [:player :hand]
                                                                            :min     1
                                                                            :max     1}]]}))))

(effects/register {::ritual-trash      ritual-trash
                   ::ritual-gain-curse ritual-gain-curse})

(def ritual {:name   :ritual
             :set    :empires
             :type   :event
             :cost   4
             :on-buy [[::ritual-gain-curse]]})

(def salt-the-earth {:name   :salt-the-earth
                     :set    :empires
                     :type   :event
                     :cost   4
                     :on-buy [[:give-victory-points 1]
                              [:give-choice {:text    "Trash a Victory card from the Supply."
                                             :choice  :trash-from-supply
                                             :options [:supply {:type :victory}]
                                             :min     1
                                             :max     1}]]})

(defn- windfall-gain-gold [game {:keys [player-no]}]
  (let [deck    (get-in game [:players player-no :deck])
        discard (get-in game [:players player-no :discard])]
    (cond-> game
            (and (empty? deck)
                 (empty? discard)) (push-effect-stack {:player-no player-no
                                                       :effects   (repeat 3 [:gain {:card-name :gold}])}))))

(effects/register {::windfall-gain-gold windfall-gain-gold})

(def windfall {:name   :windfall
               :set    :empires
               :type   :event
               :cost   5
               :on-buy [[::windfall-gain-gold]]})

(def events [advance
             banquet
             conquest
             delve
             dominate
             ritual
             salt-the-earth
             windfall])

(defn- setup-landmark-vp [{:keys [players] :as game} {:keys [landmark-name]}]
  (assoc-in game [:landmarks landmark-name :vp-tokens] (* 6 (count players))))

(defn- take-landmark-vp [game {:keys [player-no landmark-name num-vp]}]
  (let [current-vp     (get-in game [:landmarks landmark-name :vp-tokens])
        vp-taken       (min num-vp current-vp)
        remaining-vp   (- current-vp vp-taken)
        remove-trigger (fn remove-trigger [player]
                         (-> player
                             (update :triggers (partial remove (every-pred (ut/match {:name landmark-name}))))
                             (ut/dissoc-if-empty :triggers)))]
    (cond-> game
            (pos? vp-taken) (-> (assoc-in [:landmarks landmark-name :vp-tokens] remaining-vp)
                                (give-victory-points {:player-no player-no
                                                      :arg       vp-taken}))
            (zero? remaining-vp) (-> (update-in [:landmarks landmark-name] dissoc :vp-tokens)
                                     (update :players (partial mapv remove-trigger))))))

(defn- take-all-landmark-vp [game {:keys [player-no landmark-name]}]
  (let [vp-tokens (get-in game [:landmarks landmark-name :vp-tokens])]
    (cond-> game
            vp-tokens (-> (update-in [:landmarks landmark-name] dissoc :vp-tokens)
                          (give-victory-points {:player-no player-no
                                                :arg       vp-tokens})))))

(defn- move-pile-vp-to-landmark [game {:keys [pile-idx landmark-name]}]
  (let [current-vp (get-in game [:supply pile-idx :tokens :victory-point :number-of-tokens])]
    (cond-> game
            (pos? current-vp) (-> (update-in [:supply pile-idx :tokens :victory-point :number-of-tokens] dec)
                                  (update-in [:landmarks landmark-name :vp-tokens] ut/plus 1))
            (zero? (dec current-vp)) (-> (update-in [:supply pile-idx :tokens] dissoc :victory-point)
                                         (update-in [:supply pile-idx] ut/dissoc-if-empty :tokens)))))

(effects/register {::setup-landmark-vp setup-landmark-vp
                   ::take-landmark-vp  take-landmark-vp})

(defn- aqueduct-on-gain-treasure [game {:keys [card-name]}]
  (let [{:keys [card tokens idx]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        types (ut/get-types game card)]
    (cond-> game
            (and (:treasure types)
                 (:victory-point tokens)) (move-pile-vp-to-landmark {:pile-idx      idx
                                                                     :landmark-name :aqueduct}))))

(defn- aqueduct-on-gain-victory [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        types (ut/get-types game card)]
    (cond-> game
            (:victory types) (take-all-landmark-vp {:player-no     player-no
                                                    :landmark-name :aqueduct}))))

(defn- aqueduct-setup-vp-tokens [game args]
  (let [{silver-idx :idx} (ut/get-pile-idx game :silver)
        {gold-idx :idx} (ut/get-pile-idx game :gold)]
    (-> game
        (assoc-in [:supply silver-idx :tokens :victory-point :number-of-tokens] 8)
        (assoc-in [:supply gold-idx :tokens :victory-point :number-of-tokens] 8))))

(effects/register {::aqueduct-on-gain-treasure aqueduct-on-gain-treasure
                   ::aqueduct-on-gain-victory  aqueduct-on-gain-victory
                   ::aqueduct-setup-vp-tokens  aqueduct-setup-vp-tokens})

(def aqueduct-treasure-trigger {:name     :aqueduct
                                :duration :game
                                :event    :on-gain
                                :effects  [[::aqueduct-on-gain-treasure]]})

(def aqueduct-victory-trigger {:name     :aqueduct
                               :duration :game
                               :event    :on-gain
                               :effects  [[::aqueduct-on-gain-victory]]})

(def aqueduct {:name  :aqueduct
               :set   :empires
               :type  :landmark
               :setup [[::aqueduct-setup-vp-tokens]
                       [:all-players {:effects [[:add-trigger {:trigger aqueduct-treasure-trigger}]
                                                [:add-trigger {:trigger aqueduct-victory-trigger}]]}]]})

(defn- arena-discard-action [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [::take-landmark-vp {:landmark-name :arena
                                                                         :num-vp        2}]]})))

(effects/register {::arena-discard-action arena-discard-action})

(def arena-trigger {:name     :arena
                    :duration :game
                    :event    :at-start-buy
                    :effects  [[:give-choice {:text    "You may discard an Action card for 2VP."
                                              :choice  ::arena-discard-action
                                              :options [:player :hand {:type :action}]
                                              :max     1}]]})

(def arena {:name  :arena
            :set   :empires
            :type  :landmark
            :setup [[::setup-landmark-vp {:landmark-name :arena}]
                    [:all-players {:effects [[:add-trigger {:trigger arena-trigger}]]}]]})

(defn- bandit-ford-scoring [cards _]
  (->> cards
       (filter (comp #{:silver :gold} :name))
       count
       (* -2)))

(def bandit-ford {:name         :bandit-ford
                  :set          :empires
                  :type         :landmark
                  :when-scoring ::bandit-ford-scoring})

(defn- basilica-on-buy [game {:keys [player-no card-name]}]
  (let [coins-left (get-in game [:players player-no :coins])]
    (cond-> game
            (>= coins-left 2) (take-landmark-vp {:player-no     player-no
                                                 :landmark-name :basilica
                                                 :num-vp        2}))))

(effects/register {::basilica-on-buy basilica-on-buy})

(def basilica-trigger {:name     :basilica
                       :duration :game
                       :event    :on-buy
                       :effects  [[::basilica-on-buy]]})

(def basilica {:name  :basilica
               :set   :empires
               :type  :landmark
               :setup [[::setup-landmark-vp {:landmark-name :basilica}]
                       [:all-players {:effects [[:add-trigger {:trigger basilica-trigger}]]}]]})

(defn- baths-at-end-turn [game {:keys [player-no]}]
  (let [gained-cards (get-in game [:players player-no :gained-cards])]
    (cond-> game
            (empty? gained-cards) (take-landmark-vp {:player-no     player-no
                                                     :landmark-name :baths
                                                     :num-vp        2}))))

(effects/register {::baths-at-end-turn baths-at-end-turn})

(def baths-trigger {:name     :baths
                    :duration :game
                    :event    :at-draw-hand
                    :effects  [[::baths-at-end-turn]]})

(def baths {:name  :baths
            :set   :empires
            :type  :landmark
            :setup [[::setup-landmark-vp {:landmark-name :baths}]
                    [:all-players {:effects [[:add-trigger {:trigger baths-trigger}]]}]]})

(defn- battlefield-on-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        types (ut/get-types game card)]
    (cond-> game
            (:victory types) (take-landmark-vp {:player-no     player-no
                                                :landmark-name :battlefield
                                                :num-vp        2}))))

(effects/register {::battlefield-on-gain battlefield-on-gain})

(def battlefield-trigger {:name     :battlefield
                          :duration :game
                          :event    :on-gain
                          :effects  [[::battlefield-on-gain]]})

(def battlefield {:name  :battlefield
                  :set   :empires
                  :type  :landmark
                  :setup [[::setup-landmark-vp {:landmark-name :battlefield}]
                          [:all-players {:effects [[:add-trigger {:trigger battlefield-trigger}]]}]]})

(defn- colonnade-on-buy [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        types                (ut/get-types game card)
        bought-card-in-play? (->> (get-in game [:players player-no :play-area])
                                  (some (comp #{card-name} :name)))]
    (cond-> game
            (and (:action types)
                 bought-card-in-play?) (take-landmark-vp {:player-no     player-no
                                                          :landmark-name :colonnade
                                                          :num-vp        2}))))

(effects/register {::colonnade-on-buy colonnade-on-buy})

(def colonnade-trigger {:name     :colonnade
                        :duration :game
                        :event    :on-buy
                        :effects  [[::colonnade-on-buy]]})

(def colonnade {:name  :colonnade
                :set   :empires
                :type  :landmark
                :setup [[::setup-landmark-vp {:landmark-name :colonnade}]
                        [:all-players {:effects [[:add-trigger {:trigger colonnade-trigger}]]}]]})

(defn- defiled-shrine-on-gain-action [game {:keys [card-name]}]
  (let [{:keys [card tokens idx]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        types (ut/get-types game card)]
    (cond-> game
            (and (:action types)
                 (:victory-point tokens)) (move-pile-vp-to-landmark {:pile-idx      idx
                                                                     :landmark-name :defiled-shrine}))))

(defn- defiled-shrine-on-buy-curse [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        types (ut/get-types game card)]
    (cond-> game
            (:curse types) (take-all-landmark-vp {:player-no     player-no
                                                  :landmark-name :defiled-shrine}))))

(defn- defiled-shrine-setup-vp-tokens [game args]
  (let [setup-vp-tokens (fn [pile]
                          (let [{{:keys [types]} :card} (ut/access-top-card pile)]
                            (cond-> pile
                                    (and (:action types)
                                         (not (:gathering types))) (assoc-in [:tokens :victory-point :number-of-tokens] 2))))]
    (-> game
        (update :supply (partial mapv setup-vp-tokens)))))

(effects/register {::defiled-shrine-on-gain-action  defiled-shrine-on-gain-action
                   ::defiled-shrine-on-buy-curse    defiled-shrine-on-buy-curse
                   ::defiled-shrine-setup-vp-tokens defiled-shrine-setup-vp-tokens})

(def defiled-shrine-action-trigger {:name     :defiled-shrine
                                    :duration :game
                                    :event    :on-gain
                                    :effects  [[::defiled-shrine-on-gain-action]]})

(def defiled-shrine-curse-trigger {:name     :defiled-shrine
                                   :duration :game
                                   :event    :on-buy
                                   :effects  [[::defiled-shrine-on-buy-curse]]})

(def defiled-shrine {:name  :defiled-shrine
                     :set   :empires
                     :type  :landmark
                     :setup [[::defiled-shrine-setup-vp-tokens]
                             [:all-players {:effects [[:add-trigger {:trigger defiled-shrine-action-trigger}]
                                                      [:add-trigger {:trigger defiled-shrine-curse-trigger}]]}]]})

(defn- fountain-scoring [cards _]
  (let [number-of-coppers (->> cards
                               (filter (comp #{:copper} :name))
                               count)]
    (if (<= 10 number-of-coppers)
      15
      0)))

(def fountain {:name         :fountain
               :set          :empires
               :type         :landmark
               :when-scoring ::fountain-scoring})

(defn- keep-scoring [cards {:keys [players]}]
  (let [count-treasures (fn [cards]
                          (->> cards
                               (filter (comp :treasure :types))
                               (map :name)
                               frequencies))
        most-treasures  (->> players
                             (map (comp count-treasures :hand))
                             (apply merge-with max))]
    (->> cards
         count-treasures
         (filter (fn [[card-name num]]
                   (= num (get most-treasures card-name))))
         count
         (* 5))))

(def keep-lm {:name         :keep
              :set          :empires
              :type         :landmark
              :when-scoring ::keep-scoring})

(defn- labyrinth-on-gain [game {:keys [player-no]}]
  (let [gained-cards (get-in game [:players player-no :gained-cards])]
    (cond-> game
            (= 2 (count gained-cards)) (take-landmark-vp {:player-no     player-no
                                                          :landmark-name :labyrinth
                                                          :num-vp        2}))))

(effects/register {::labyrinth-on-gain labyrinth-on-gain})

(def labyrinth-trigger {:name     :labyrinth
                        :duration :game
                        :event    :on-gain
                        :effects  [[::labyrinth-on-gain]]})

(def labyrinth {:name  :labyrinth
                :set   :empires
                :type  :landmark
                :setup [[::setup-landmark-vp {:landmark-name :labyrinth}]
                        [:all-players {:effects [[:add-trigger {:trigger labyrinth-trigger}]]}]]})

(defn- museum-scoring [cards _]
  (->> cards
       (map :name)
       set
       count
       (* 2)))

(def museum {:name         :museum
             :set          :empires
             :type         :landmark
             :when-scoring ::museum-scoring})

(defn- obelisk-scoring [cards {:keys [landmarks]}]
  (let [{:keys [chosen-cards]} (:obelisk landmarks)]
    (->> cards
         (filter (comp chosen-cards :name))
         count
         (* 2))))

(defn- obelisk-setup [{:keys [supply] :as game} _]
  (let [{:keys [card split-pile]} (->> supply
                                       (filter (comp :action :types :card ut/access-top-card))
                                       shuffle
                                       first)
        chosen-cards (if split-pile
                       (->> split-pile
                            (map (comp :name :card))
                            set)
                       #{(:name card)})]
    (assoc-in game [:landmarks :obelisk :chosen-cards] chosen-cards)))

(effects/register {::obelisk-scoring obelisk-scoring
                   ::obelisk-setup   obelisk-setup})

(def obelisk {:name         :obelisk
              :set          :empires
              :type         :landmark
              :when-scoring ::obelisk-scoring
              :setup        [[::obelisk-setup]]})

(defn- orchard-scoring [cards _]
  (->> cards
       (filter (comp :action :types))
       (map :name)
       frequencies
       vals
       (filter (partial <= 3))
       count
       (* 4)))

(def orchard {:name         :orchard
              :set          :empires
              :type         :landmark
              :when-scoring ::orchard-scoring})

(defn- palace-scoring [cards _]
  (let [coppers (->> cards
                     (filter (comp #{:copper} :name))
                     count)
        silvers (->> cards
                     (filter (comp #{:silver} :name))
                     count)
        golds   (->> cards
                     (filter (comp #{:gold} :name))
                     count)]
    (* 3 (min coppers silvers golds))))

(def palace {:name         :palace
             :set          :empires
             :type         :landmark
             :when-scoring ::palace-scoring})

(def tomb-trigger {:name     :tomb
                   :duration :game
                   :event    :on-trash
                   :effects  [[:give-victory-points 1]]})

(def tomb {:name  :tomb
           :set   :empires
           :type  :landmark
           :setup [[:all-players {:effects [[:add-trigger {:trigger tomb-trigger}]]}]]})

(defn- tower-scoring [cards {:keys [supply] :as game}]
  (->> cards
       (remove (comp :victory :types))
       (filter (fn [{:keys [name]}]
                 (when-let [{:keys [idx]} (ut/get-pile-idx game :supply name #{:include-empty-split-piles})]
                   (->> (get supply idx)
                        ut/access-top-card
                        :pile-size
                        zero?))))
       count))

(def tower {:name         :tower
            :set          :empires
            :type         :landmark
            :when-scoring ::tower-scoring})

(defn- triumphal-arch-scoring [cards _]
  (let [action-count (or (->> cards
                              (filter (comp :action :types))
                              (map :name)
                              frequencies
                              vals
                              (sort >)
                              second)
                         0)]
    (* 3 action-count)))

(def triumphal-arch {:name         :triumphal-arch
                     :set          :empires
                     :type         :landmark
                     :when-scoring ::triumphal-arch-scoring})

(defn- wall-scoring [cards _]
  (let [cards-after-15 (drop 15 cards)]
    (- (count cards-after-15))))

(def wall {:name         :wall
           :set          :empires
           :type         :landmark
           :when-scoring ::wall-scoring})

(defn- wolf-den-scoring [cards _]
  (->> cards
       (map :name)
       frequencies
       vals
       (filter #{1})
       count
       (* -3)))

(def wolf-den {:name         :wolf-den
               :set          :empires
               :type         :landmark
               :when-scoring ::wolf-den-scoring})

(effects/register {::bandit-ford-scoring    bandit-ford-scoring
                   ::fountain-scoring       fountain-scoring
                   ::keep-scoring           keep-scoring
                   ::museum-scoring         museum-scoring
                   ::orchard-scoring        orchard-scoring
                   ::palace-scoring         palace-scoring
                   ::tower-scoring          tower-scoring
                   ::triumphal-arch-scoring triumphal-arch-scoring
                   ::wall-scoring           wall-scoring
                   ::wolf-den-scoring       wolf-den-scoring})

(def landmarks [aqueduct
                arena
                bandit-ford
                basilica
                baths
                battlefield
                colonnade
                defiled-shrine
                fountain
                keep-lm
                labyrinth
                museum
                obelisk
                orchard
                palace
                tomb
                tower
                triumphal-arch
                wall
                wolf-den])
