(ns dombot.cards.nocturne
  (:require [dombot.operations :refer [push-effect-stack give-choice move-card move-cards attack-other-players gain
                                       state-maintenance check-stack get-on-buy-effects get-on-gain-effects]]
            [dombot.cards.common :refer [reveal-hand reveal-discard add-trigger setup-extra-cards]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn- ghost-repeat-action [game {:keys [player-no card-id set-aside]}]
  (let [{:keys [id] :as card} (first set-aside)]
    (-> game
        (update-in [:players player-no :play-area] concat set-aside)
        (push-effect-stack {:player-no player-no
                            :card-id   card-id
                            :effects   [[:card-effect {:card card}]
                                        [:card-effect {:card card}]
                                        [:register-repeated-play {:target-id id}]]}))))

(def ghost-trigger {:trigger           :at-start-turn
                    :duration          :once
                    :simultaneous-mode :auto
                    :effects           [[::ghost-repeat-action]]})

(defn- ghost-reveal [game {:keys [player-no card-id]}]
  (let [{:keys [revealed deck discard]} (get-in game [:players player-no])
        card  (last revealed)
        types (ut/get-types game card)]
    (cond (:action types) (-> game
                              (update-in [:players player-no :revealed] drop-last)
                              (push-effect-stack {:player-no player-no
                                                  :effects   [[:add-trigger {:trigger (merge ghost-trigger
                                                                                             {:set-aside [card]})
                                                                             :card-id card-id}]]})
                              (state-maintenance player-no :revealed :ghost))
          (not-empty (concat deck discard)) (push-effect-stack game {:player-no player-no
                                                                     :card-id   card-id
                                                                     :effects   [[:reveal-from-deck 1]
                                                                                 [::ghost-reveal]]})
          :else game)))

(effects/register {::ghost-repeat-action ghost-repeat-action
                   ::ghost-reveal        ghost-reveal})

(def ghost {:name    :ghost
            :set     :nocturne
            :types   #{:night :duration :spirit}
            :cost    4
            :effects [[:reveal-from-deck 1]
                      [::ghost-reveal]
                      [:discard-all-revealed]]})

(defn- imp-play-action [game {:keys [player-no card-name]}]
  (let [{card :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:play-from-hand {:card-name card-name}]
                                                 [:card-effect {:card card}]]}))))

(defn- imp-give-choice [game {:keys [player-no]}]
  (let [actions-in-play (->> (get-in game [:players player-no :play-area])
                             (filter (comp :action :types))
                             (map :name)
                             set)]
    (give-choice game {:player-no player-no
                       :text      "You may play an Action card from your hand that you don't have a copy of in play."
                       :choice    ::imp-play-action
                       :options   [:player :hand {:type     :action
                                                  :not-name actions-in-play}]
                       :max       1})))

(effects/register {::imp-play-action imp-play-action
                   ::imp-give-choice imp-give-choice})


(def imp {:name    :imp
          :set     :nocturne
          :types   #{:action :spirit}
          :cost    2
          :effects [[:draw 2]
                    [::imp-give-choice]]})

(defn- will-o-wisp-put-revealed-into-hand [game {:keys [player-no]}]
  (let [{:keys [id] :as card} (last (get-in game [:players player-no :revealed]))
        cost (ut/get-cost game card)]
    (cond-> game
            (and card (<= cost 2)) (move-card {:player-no    player-no
                                               :move-card-id id
                                               :from         :revealed
                                               :to           :hand}))))

(effects/register {::will-o-wisp-put-revealed-into-hand will-o-wisp-put-revealed-into-hand})

(def will-o-wisp {:name    :will-o'-wisp
                  :set     :nocturne
                  :types   #{:action :spirit}
                  :cost    0
                  :effects [[:draw 1]
                            [:give-actions 1]
                            [:reveal-from-deck 1]
                            [::will-o-wisp-put-revealed-into-hand]
                            [:topdeck-all-revealed]]})

(def spirit-piles {:ghost        {:card ghost :pile-size 6}
                   :imp          {:card imp :pile-size 13}
                   :will-o'-wisp {:card will-o-wisp :pile-size 12}})

(defn- grant-wish [game {:keys [player-no card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :card-id   card-id
                                     :effects   [[:return-this-to-supply {:area :extra-cards}]
                                                 [:give-choice {:text    "Gain a card to your hand costing up to $6."
                                                                :choice  :gain-to-hand
                                                                :options [:supply {:max-cost 6}]
                                                                :min     1
                                                                :max     1}]]}))))

(effects/register {::grant-wish grant-wish})

(def wish {:name    :wish
           :set     :nocturne
           :types   #{:action}
           :cost    0
           :effects [[:give-actions 1]
                     [::grant-wish]]})

(def wish-pile {:card wish :pile-size 12})

(defn- earth-gift-discard [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-choice {:text    "Gain a card costing up to $4."
                                                                   :choice  :gain
                                                                   :options [:supply {:max-cost 4}]
                                                                   :min     1
                                                                   :max     1}]]})))

(effects/register {::earth-gift-discard earth-gift-discard})

(def earth-gift {:name    :the-earth's-gift
                 :type    :boon
                 :effects [[:give-choice {:text    "You may discard a Treasure to gain a card costing up to $4."
                                          :choice  ::earth-gift-discard
                                          :options [:player :hand {:type :treasure}]
                                          :max     1}]]})

(def field-gift {:name                 :the-field's-gift
                 :type                 :boon
                 :effects              [[:give-actions 1]
                                        [:give-coins 1]]
                 :keep-until-clean-up? true})

(def flame-gift {:name    :the-flame's-gift
                 :type    :boon
                 :effects [[:give-choice {:text    "You may trash a card from your hand."
                                          :choice  :trash-from-hand
                                          :options [:player :hand]
                                          :max     1}]]})

(def forest-gift {:name                 :the-forest's-gift
                  :type                 :boon
                  :effects              [[:give-buys 1]
                                         [:give-coins 1]]
                  :keep-until-clean-up? true})

(def moon-gift {:name    :the-moon's-gift
                :type    :boon
                :effects [[:give-choice {:text    "You may put a card from your discard onto your deck."
                                         :choice  :topdeck-from-discard
                                         :options [:player :discard]
                                         :max     1}]]})

(def mountain-gift {:name    :the-mountain's-gift
                    :type    :boon
                    :effects [[:gain {:card-name :silver}]]})

(defn- receive-river-gift [{:keys [current-player] :as game} {:keys [player-no]}]
  (add-trigger game {:player-no (or current-player 0)
                     :trigger   {:trigger  :at-draw-hand
                                 :duration :once
                                 :effects  [[:draw {:arg       1
                                                    :player-no player-no}]]}}))

(effects/register {::receive-river-gift receive-river-gift})

(def river-gift {:name                 :the-river's-gift
                 :type                 :boon
                 :effects              [[::receive-river-gift]]
                 :keep-until-clean-up? true})

(def sea-gift {:name    :the-sea's-gift
               :type    :boon
               :effects [[:draw 1]]})

(defn- sky-gift-discard [game {:keys [player-no card-name card-names] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   (concat [[:discard-from-hand args]]
                                              (when (= 3 (count card-names))
                                                [[:gain {:card-name :gold}]]))}))

(effects/register {::sky-gift-discard sky-gift-discard})

(def sky-gift {:name    :the-sky's-gift
               :type    :boon
               :effects [[:give-choice {:text      "You may discard 3 cards to gain a Gold."
                                        :choice    ::sky-gift-discard
                                        :options   [:player :hand]
                                        :min       3
                                        :max       3
                                        :optional? true}]]})

(def sun-gift {:name    :the-sun's-gift
               :type    :boon
               :effects [[:look-at 4]
                         [:give-choice {:text    "Discard any number of the top 4 cards of your deck."
                                        :choice  :discard-from-look-at
                                        :options [:player :look-at]}]
                         [:give-choice {:text    "Put the rest back on top in any order."
                                        :choice  :topdeck-from-look-at
                                        :options [:player :look-at]
                                        :min     4}]]})

(def swamp-gift {:name    :the-swamp's-gift
                 :type    :boon
                 :effects [[:gain {:card-name :will-o'-wisp :from :extra-cards}]]})

(def wind-gift {:name    :the-wind's-gift
                :type    :boon
                :effects [[:draw 2]
                          [:give-choice {:text    "Discard 2 card."
                                         :choice  :discard-from-hand
                                         :options [:player :hand]
                                         :min     2
                                         :max     2}]]})

(defn- bad-omens-topdeck-coppers [game {:keys [player-no]}]
  (let [discard            (get-in game [:players player-no :discard])
        coppers-in-discard (->> discard
                                (filter (comp #{:copper} :name))
                                count
                                (min 2))]
    (cond-> game
            (pos? coppers-in-discard) (move-cards {:player-no  player-no
                                                   :card-names (repeat coppers-in-discard :copper)
                                                   :from       :discard
                                                   :to         :deck}))))

(defn- bad-omens-reveal [game {:keys [player-no]}]
  (let [cards-in-deck (count (get-in game [:players player-no :deck]))]
    (cond-> game
            (pos? cards-in-deck) (assoc-in [:players player-no :revealed-cards :deck] cards-in-deck)
            (< cards-in-deck 2) (reveal-discard {:player-no player-no}))))

(effects/register {::bad-omens-topdeck-coppers bad-omens-topdeck-coppers
                   ::bad-omens-reveal          bad-omens-reveal})

(def bad-omens {:name    :bad-omens
                :type    :hex
                :effects [[:put-deck-into-discard]
                          [::bad-omens-topdeck-coppers]
                          [::bad-omens-reveal]]})

(defn- famine-shuffle-deck [game {:keys [player-no]}]
  (-> game
      (update-in [:players player-no :deck] shuffle)
      (update-in [:players player-no :revealed-cards] dissoc :deck)
      (update-in [:players player-no] ut/dissoc-if-empty :revealed-cards)))

(defn- famine-discard-actions [game {:keys [player-no]}]
  (let [action-names (->> (get-in game [:players player-no :revealed])
                          (filter (comp :action (partial ut/get-types game)))
                          (map :name))]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:discard-from-revealed {:card-names action-names}]
                                         [:topdeck-all-revealed]
                                         [::famine-shuffle-deck]]})))

(effects/register {::famine-shuffle-deck    famine-shuffle-deck
                   ::famine-discard-actions famine-discard-actions})

(def famine {:name    :famine
             :type    :hex
             :effects [[:reveal-from-deck 3]
                       [::famine-discard-actions]]})

(defn fear-discard [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond
      (< (count hand) 5) game

      (some (partial ut/types-match game #{:action :treasure}) hand)
      (give-choice game {:player-no player-no
                         :text      "Discard an Action or Treasure."
                         :choice    :discard-from-hand
                         :options   [:player :hand {:types #{:action :treasure}}]
                         :min       1
                         :max       1})

      :else (-> game
                (reveal-hand {:player-no player-no})))))

(effects/register {::fear-discard fear-discard})

(def fear {:name    :fear
           :type    :hex
           :effects [[::fear-discard]]})

(def greed {:name    :greed
            :type    :hex
            :effects [[:gain-to-topdeck {:card-name :copper}]]})

(defn haunting-topdeck [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (<= 4 (count hand)) (give-choice {:player-no player-no
                                              :text      "Put a card from your hand onto your deck."
                                              :choice    :topdeck-from-hand
                                              :options   [:player :hand]
                                              :min       1
                                              :max       1}))))

(effects/register {::haunting-topdeck haunting-topdeck})

(def haunting {:name    :haunting
               :type    :hex
               :effects [[::haunting-topdeck]]})

(defn- locusts-trash [game {:keys [player-no]}]
  (let [[{:keys [name] :as top-card}] (get-in game [:players player-no :deck])
        cost  (ut/get-cost game top-card)
        types (ut/get-types game top-card)]
    (cond-> game
            top-card (push-effect-stack {:player-no player-no
                                         :effects   [[:trash-from-topdeck]
                                                     (if (#{:copper :estate} name)
                                                       [:gain {:card-name :curse}]
                                                       [:give-choice {:text    (str "Gain a " (ut/format-types types) " card costing up to $" (dec cost) ".")
                                                                      :choice  :gain
                                                                      :options [:supply {:types    types
                                                                                         :max-cost (dec cost)}]
                                                                      :min     1
                                                                      :max     1}])]}))))

(effects/register {::locusts-trash locusts-trash})

(def locusts {:name    :locusts
              :type    :hex
              :effects [[:peek-deck 1]
                        [::locusts-trash]]})

(def miserable {:name           :miserable
                :type           :state
                :victory-points -2})

(def twice-miserable {:name           :twice-miserable
                      :type           :state
                      :victory-points -4})

(defn- make-miserable [game {:keys [player-no]}]
  (let [states (get-in game [:players player-no :states])]
    (cond (some (comp #{:twice-miserable} :name) states) game
          (some (comp #{:miserable} :name) states) (update-in game [:players player-no :states] (fn [states]
                                                                                                  (->> states
                                                                                                       (remove #{miserable})
                                                                                                       (concat [twice-miserable]))))
          :else (update-in game [:players player-no :states] concat [miserable]))))

(effects/register {::make-miserable make-miserable})

(def misery {:name    :misery
             :type    :hex
             :effects [[::make-miserable]]})

(def plague {:name    :plague
             :type    :hex
             :effects [[:gain-to-hand {:card-name :curse}]]})

(def poverty {:name    :poverty
              :type    :hex
              :effects [[:discard-down-to 3]]})

(defn- war-reveal [game {:keys [player-no]}]
  (let [{:keys [revealed deck discard]} (get-in game [:players player-no])
        {:keys [name] :as card} (last revealed)
        cost (ut/get-cost game card)]
    (cond (#{3 4} cost) (push-effect-stack game {:player-no player-no
                                                 :effects   [[:trash-from-revealed {:card-name name}]]})
          (not-empty (concat deck discard)) (push-effect-stack game {:player-no player-no
                                                                     :effects   [[:reveal-from-deck 1]
                                                                                 [::war-reveal]]})
          :else game)))

(effects/register {::war-reveal war-reveal})

(def war {:name    :war
          :type    :hex
          :effects [[::war-reveal]
                    [:discard-all-revealed]]})

(def all-boons [earth-gift
                field-gift
                flame-gift
                forest-gift
                moon-gift
                mountain-gift
                river-gift
                sea-gift
                sky-gift
                sun-gift
                swamp-gift
                wind-gift])

(def all-hexes [bad-omens
                famine
                fear
                greed
                haunting
                locusts
                misery
                plague
                poverty
                war])

(defn- setup-boons [game args]
  (cond-> game
          (not (:boons game)) (-> (assoc :boons {:deck (shuffle all-boons)})
                                  (setup-extra-cards {:extra-cards [(:will-o'-wisp spirit-piles)]}))))

(defn- setup-hexes [game args]
  (cond-> game
          (not (:hexes game)) (assoc :hexes {:deck (shuffle all-hexes)})))

(effects/register {:setup-boons setup-boons
                   :setup-hexes setup-hexes})

(defn- maybe-shuffle [{:keys [deck discard] :as piles} & {:keys [number-of-cards]
                                                          :or   {number-of-cards 1}}]
  (if (< (count deck) number-of-cards)
    {:deck (concat deck (shuffle discard))}
    piles))

(defn- remove-boon-from-player [{:keys [boons] :as player} boon-name]
  (let [boons (remove (comp #{boon-name} :name) boons)]
    (if (empty? boons)
      (dissoc player :boons)
      (assoc player :boons boons))))

(defn- return-boon [game {:keys [player-no boon-name]}]
  (let [{boon :card} (ut/get-card-idx game [:players player-no :boons] {:name boon-name})]
    (cond-> game
            boon (-> (update-in [:players player-no] remove-boon-from-player boon-name)
                     (update-in [:boons :discard] concat [boon])))))

(defn receive-boon [game {:keys [player-no boon]}]
  (if (not boon)
    (let [{[{:keys [keep-until-clean-up?] :as boon} & deck] :deck
           discard                                          :discard} (maybe-shuffle (:boons game))
          discard (if keep-until-clean-up?
                    discard
                    (concat discard [boon]))]
      (-> game
          (assoc :boons (merge {}
                               (when deck {:deck deck})
                               (when discard {:discard discard})))
          (cond-> keep-until-clean-up? (update-in [:players player-no :boons] concat [boon]))
          (receive-boon {:player-no player-no
                         :boon      boon})))
    (let [{:keys [name effects keep-until-clean-up?]} boon
          has-boon? (some (comp #{name} :name) (get-in game [:players player-no :boons]))]
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   (concat effects
                                                 (when (and has-boon? keep-until-clean-up?)
                                                   [[:add-trigger {:trigger {:trigger  :at-clean-up
                                                                             :duration :once
                                                                             :effects  [[:return-boon {:boon-name name}]]}}]]))})
          check-stack))))

(defn receive-hex [game {:keys [player-no hex]}]
  (if (not hex)
    (let [{[hex & deck] :deck
           discard      :discard} (maybe-shuffle (:hexes game))
          discard (concat discard [hex])]
      (-> game
          (assoc :hexes (merge {}
                               (when deck {:deck deck})
                               (when discard {:discard discard})))
          (receive-hex {:player-no player-no
                        :hex       hex})))
    (let [{:keys [effects]} hex]
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   effects})
          check-stack))))

(defn others-receive-next-hex [game {:keys [player-no]}]
  (let [{[hex & deck] :deck
         discard      :discard} (maybe-shuffle (:hexes game))
        discard (concat discard [hex])]
    (-> game
        (assoc :hexes (merge {}
                             (when deck {:deck deck})
                             (when discard {:discard discard})))
        (attack-other-players {:player-no player-no
                               :effects   [[:receive-hex {:hex hex}]]}))))

(effects/register {:return-boon             return-boon
                   :receive-boon            receive-boon
                   :receive-hex             receive-hex
                   :others-receive-next-hex others-receive-next-hex})

(def bard {:name    :bard
           :set     :nocturne
           :types   #{:action :fate}
           :cost    4
           :effects [[:give-coins 2]
                     [:receive-boon]]
           :setup   [[:setup-boons]]})

(defn- blessed-village-choice [game {:keys [player-no boon-name choice]}]
  (let [{{:keys [keep-until-clean-up?] :as boon} :card} (ut/get-card-idx game [:players player-no :boons] {:name boon-name})
        boon-effects (concat (when-not keep-until-clean-up?
                               [[:return-boon {:boon-name boon-name}]])
                             [[:receive-boon {:boon boon}]])]
    (case choice
      :now (push-effect-stack game {:player-no player-no
                                    :effects   boon-effects})
      :at-start-turn (add-trigger game {:player-no player-no
                                        :trigger   {:trigger           :at-start-turn
                                                    :duration          :once
                                                    :simultaneous-mode :auto
                                                    :effects           boon-effects}}))))

(defn- blessed-village-take-boon [game {:keys [player-no]}]
  (let [{[{:keys [name] :as boon} & deck] :deck
         discard                          :discard} (maybe-shuffle (:boons game))]
    (-> game
        (assoc :boons (merge {}
                             (when deck {:deck deck})
                             (when discard {:discard discard})))
        (update-in [:players player-no :boons] concat [boon])
        (give-choice {:player-no player-no
                      :text      (str "Receive " (ut/format-name name) " now or at the start of your next turn.")
                      :choice    [::blessed-village-choice {:boon-name name}]
                      :options   [:special
                                  {:option :now :text "Now"}
                                  {:option :at-start-turn :text "Next turn"}]
                      :min       1
                      :max       1}))))

(effects/register {::blessed-village-choice    blessed-village-choice
                   ::blessed-village-take-boon blessed-village-take-boon})

(def blessed-village {:name    :blessed-village
                      :set     :nocturne
                      :types   #{:action :fate}
                      :cost    4
                      :effects [[:draw 1]
                                [:give-actions 2]]
                      :on-gain [[::blessed-village-take-boon]]
                      :setup   [[:setup-boons]]})

(defn- haunted-mirror-ghost [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:gain {:card-name :ghost
                                                            :from      :extra-cards}]]})))

(effects/register {::haunted-mirror-ghost haunted-mirror-ghost})

(def haunted-mirror {:name       :haunted-mirror
                     :set        :nocturne
                     :types      #{:treasure :heirloom}
                     :cost       0
                     :coin-value 1
                     :on-trash   [[:give-choice {:text    "You may discard an Action card, to gain a Ghost."
                                                 :choice  ::haunted-mirror-ghost
                                                 :options [:player :hand {:type :action}]
                                                 :max     1}]]})

(def cemetery {:name           :cemetery
               :set            :nocturne
               :types          #{:victory}
               :cost           4
               :victory-points 2
               :on-gain        [[:give-choice {:text    "Trash up to 4 cards from your hand."
                                               :choice  :trash-from-hand
                                               :options [:player :hand]
                                               :max     4}]]
               :heirloom       haunted-mirror
               :setup          [[:setup-extra-cards {:extra-cards [(:ghost spirit-piles)]}]]})

(defn- changeling-exchange [game {:keys [player-no card-name gained-card-id pile-location]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:move-card {:move-card-id gained-card-id
                                                                 :from         :gaining
                                                                 :to           pile-location}]
                                                    [:move-card {:card-name :changeling
                                                                 :from      :supply
                                                                 :to        :discard}]]})))

(defn- changeling-on-gain [{:keys [supply extra-cards] :as game} {:keys [player-no gained-card-id bought]}]
  (let [{{:keys [name] :as card} :card} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})
        pile-location   (cond (some (comp #{name} :name :card) supply) :supply
                              (some (comp #{name} :name :card) extra-cards) :extra-cards)
        cost            (ut/get-cost game card)
        on-buy-effects  (concat (get-on-buy-effects game player-no name)
                                (get-on-buy-effects game player-no :changeling))
        on-gain-effects (->> (get-on-gain-effects game player-no name)
                             (remove #{[::changeling-on-gain]}))
        ignore-gain?    (or (= :changeling name)
                            (and bought
                                 (empty? on-buy-effects)
                                 (empty? on-gain-effects)))]
    (cond-> game
            (and card
                 pile-location
                 (<= 3 cost)
                 (not ignore-gain?)) (give-choice {:player-no player-no
                                                   :text      (str "You may exchange the gained " (ut/format-name name) " for a Changeling.")
                                                   :choice    [::changeling-exchange {:gained-card-id gained-card-id
                                                                                      :pile-location  pile-location}]
                                                   :options   [:supply {:names #{:changeling}}]
                                                   :max       1}))))

(effects/register {::changeling-exchange changeling-exchange
                   ::changeling-on-gain  changeling-on-gain})

(def changeling-trigger {:trigger  :on-gain
                         :duration :game
                         :effects  [[::changeling-on-gain]]})

(def changeling {:name    :changeling
                 :set     :nocturne
                 :types   #{:night}
                 :cost    3
                 :effects [[:trash-this]
                           [:give-choice {:text    "Gain a copy of a card you have in play."
                                          :choice  :gain
                                          :options [:player :play-area]
                                          :min     1
                                          :max     1}]]
                 :setup   [[:all-players {:effects [[:add-trigger {:trigger changeling-trigger}]]}]]})

(def cobbler {:name    :cobbler
              :set     :nocturne
              :types   #{:night :duration}
              :cost    5
              :trigger {:trigger           :at-start-turn
                        :duration          :once
                        :simultaneous-mode :auto
                        :effects           [[:give-choice {:text    "Gain a card to your hand costing up to $4."
                                                           :choice  :gain-to-hand
                                                           :options [:supply {:max-cost 4}]
                                                           :min     1
                                                           :max     1}]]}})

(defn- conclave-play-action [game {:keys [player-no card-name]}]
  (let [{card :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:play-from-hand {:card-name card-name}]
                                                 [:card-effect {:card card}]
                                                 [:give-actions 1]]}))))

(defn- conclave-give-choice [game {:keys [player-no]}]
  (let [actions-in-play (->> (get-in game [:players player-no :play-area])
                             (filter (comp :action :types))
                             (map :name)
                             set)]
    (give-choice game {:player-no player-no
                       :text      "You may play an Action card from your hand that you don't have a copy of in play."
                       :choice    ::conclave-play-action
                       :options   [:player :hand {:type     :action
                                                  :not-name actions-in-play}]
                       :max       1})))

(effects/register {::conclave-play-action conclave-play-action
                   ::conclave-give-choice conclave-give-choice})

(def conclave {:name    :conclave
               :set     :nocturne
               :types   #{:action}
               :cost    4
               :effects [[:give-coins 2]
                         [::conclave-give-choice]]})

(defn crypt-put-treasure-into-hand [game {:keys [player-no card-id choice]}]
  (let [{:keys [trigger]} (ut/get-trigger-idx game [:players player-no :triggers] {:card-id card-id})
        {:keys [idx card]} (ut/get-card-idx trigger [:set-aside] {:name choice})]
    (-> game
        (ut/update-in-vec [:players player-no :triggers] {:card-id card-id}
                          update :set-aside ut/vec-remove idx)
        (update-in [:players player-no :hand] concat [card]))))

(defn- crypt-pick-treasure [game {:keys [player-no card-id set-aside]}]
  (give-choice game {:player-no player-no
                     :card-id   card-id
                     :text      "Put a Treasure from your Crypt into your hand."
                     :choice    ::crypt-put-treasure-into-hand
                     :options   (concat [:special]
                                        (->> set-aside
                                             (map (fn [{:keys [name]}]
                                                    {:option name :text (ut/format-name name)}))
                                             set))
                     :min       1
                     :max       1}))



(def crypt-trigger {:trigger           :at-start-turn
                    :duration          :until-empty
                    :simultaneous-mode :auto
                    :effects           [[::crypt-pick-treasure]]})

(defn- crypt-set-aside [game {:keys [player-no card-id]}]
  (let [set-aside (get-in game [:players player-no :crypt-set-aside])]
    (-> game
        (update-in [:players player-no] dissoc :crypt-set-aside)
        (add-trigger {:player-no player-no
                      :card-id   card-id
                      :trigger   (merge crypt-trigger {:set-aside set-aside})}))))

(defn crypt-choose-treasures [game {:keys [player-no card-id card-names]}]
  (cond-> game
          (not-empty card-names) (push-effect-stack {:player-no player-no
                                                     :card-id   card-id
                                                     :effects   [[:move-cards {:card-names card-names
                                                                               :from       :play-area
                                                                               :to         :crypt-set-aside}]
                                                                 [::crypt-set-aside]]})))

(effects/register {::crypt-put-treasure-into-hand crypt-put-treasure-into-hand
                   ::crypt-pick-treasure          crypt-pick-treasure
                   ::crypt-set-aside              crypt-set-aside
                   ::crypt-choose-treasures       crypt-choose-treasures})

(def crypt {:name    :crypt
            :set     :nocturne
            :types   #{:night :duration}
            :cost    5
            :effects [[:give-choice {:text    "Set aside any number of Treasures you have in play."
                                     :choice  ::crypt-choose-treasures
                                     :options [:player :play-area {:type :treasure}]}]]})

(def cursed-village {:name    :cursed-village
                     :set     :nocturne
                     :types   #{:action :doom}
                     :cost    5
                     :effects [[:give-actions 2]
                               [:draw-up-to 6]]
                     :on-gain [[:receive-hex]]
                     :setup   [[:setup-hexes]]})

(def den-of-sin {:name    :den-of-sin
                 :set     :nocturne
                 :types   #{:night :duration}
                 :cost    5
                 :trigger {:trigger           :at-start-turn
                           :duration          :once
                           :simultaneous-mode :auto
                           :effects           [[:draw 2]]}
                 :gain-to :hand})

(defn- devils-workshop-gain [game {:keys [player-no]}]
  (let [gained-cards (count (get-in game [:players player-no :gained-cards]))]
    (cond-> game
            (zero? gained-cards) (gain {:player-no player-no
                                        :card-name :gold})
            (= 1 gained-cards) (give-choice {:player-no player-no
                                             :text      "Gain a card costing up to $4."
                                             :choice    :gain
                                             :options   [:supply {:max-cost 4}]
                                             :min       1
                                             :max       1})
            (<= 2 gained-cards) (gain {:player-no player-no
                                       :card-name :imp
                                       :from      :extra-cards}))))

(effects/register {::devils-workshop-gain devils-workshop-gain})

(def devils-workshop {:name    :devil's-workshop
                      :set     :nocturne
                      :types   #{:night}
                      :cost    4
                      :effects [[::devils-workshop-gain]]
                      :setup   [[:setup-extra-cards {:extra-cards [(:imp spirit-piles)]}]]})

(defn- exorcist-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:trash-from-hand {:card-name card-name}]
                                                 [:give-choice {:text    (str "Gain a Spirit costing up to $" (dec cost) ".")
                                                                :choice  [:gain {:from :extra-cards}]
                                                                :options [:extra-cards {:type     :spirit
                                                                                        :max-cost (dec cost)}]
                                                                :min     1
                                                                :max     1}]]}))))

(effects/register {::exorcist-trash exorcist-trash})

(def exorcist {:name    :exorcist
               :set     :nocturne
               :types   #{:night}
               :cost    4
               :effects [[:give-choice {:text    "Trash a card from your hand to gain a cheaper Spirit."
                                        :choice  ::exorcist-trash
                                        :options [:player :hand]
                                        :min     1
                                        :max     1}]]
               :setup   [[:setup-extra-cards {:extra-cards (vals spirit-piles)}]]})

(defn- faithful-hound-set-aside [{:keys [current-player] :as game} {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:move-card {:card-name :faithful-hound
                                                                 :from      :discard
                                                                 :to        :set-aside}]
                                                    [:add-trigger {:player-no (or current-player 0)
                                                                   :trigger   {:trigger  :at-draw-hand
                                                                               :duration :once
                                                                               :effects  [[:move-card {:player-no player-no
                                                                                                       :card-name :faithful-hound
                                                                                                       :from      :set-aside
                                                                                                       :to        :hand}]]}}]]})))

(effects/register {::faithful-hound-set-aside faithful-hound-set-aside})

(def faithful-hound {:name       :faithful-hound
                     :set        :nocturne
                     :types      #{:action :reaction}
                     :cost       2
                     :effects    [[:draw 2]]
                     :on-discard [[:give-choice {:text    "You may set Faithful Hound aside, and put it into your hand at end of this turn."
                                                 :choice  ::faithful-hound-set-aside
                                                 :options [:player :discard {:last true
                                                                             :name :faithful-hound}]
                                                 :max     1}]]})

(def lucky-coin {:name       :lucky-coin
                 :set        :nocturne
                 :types      #{:treasure :heirloom}
                 :cost       4
                 :coin-value 1
                 :effects    [[:gain {:card-name :silver}]]})

(defn- discard-for-boon [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:receive-boon]]})))

(effects/register {::discard-for-boon discard-for-boon})

(def lost-in-the-woods {:name    :lost-in-the-woods
                        :type    :state
                        :trigger {:trigger           :at-start-turn
                                  :simultaneous-mode :auto
                                  :effects           [[:give-choice {:text    "You may discard a card to receive a Boon."
                                                                     :choice  ::discard-for-boon
                                                                     :options [:player :hand]
                                                                     :max     1}]]}})

(defn- fools-choice [boons]
  {:text    "Receive the Boons in any order."
   :choice  [::fool-receive-boon {:boons boons}]
   :options [:player :boons {:names (->> boons (map :name) set)}]
   :min     1
   :max     1})

(defn- fool-receive-boon [game {:keys [player-no card-name boons]}]
  (let [{{:keys [keep-until-clean-up?] :as boon} :card} (ut/get-card-idx game [:players player-no :boons] {:name card-name})
        remaining-boons (remove (comp #{card-name} :name) boons)]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat (when-not keep-until-clean-up?
                                                  [[:return-boon {:boon-name card-name}]])
                                                [[:receive-boon {:boon boon}]]
                                                (when (not-empty remaining-boons)
                                                  [[:give-choice (fools-choice remaining-boons)]]))})))

(defn- fools-errand [game {:keys [player-no]}]
  (let [{:keys [owner]} (get-in game [:artifacts :lost-in-the-woods])]
    (cond-> game
            (not= player-no owner)
            (as-> game (let [{:keys [deck discard]} (maybe-shuffle (:boons game) :number-of-cards 3)
                             [fool-boons deck] (split-at 3 deck)]
                         (-> game
                             (assoc :boons (merge (when (not-empty deck) {:deck deck})
                                                  (when discard {:discard discard})))
                             (update-in [:players player-no :boons] concat fool-boons)
                             (push-effect-stack {:player-no player-no
                                                 :effects   [[:take-artifact {:artifact-name :lost-in-the-woods}]
                                                             [:give-choice (fools-choice fool-boons)]]})))))))

(effects/register {::fool-receive-boon fool-receive-boon
                   ::fools-errand      fools-errand})

(def fool {:name     :fool
           :set      :nocturne
           :types    #{:action :fate}
           :cost     3
           :effects  [[::fools-errand]]
           :heirloom lucky-coin
           :setup    [[:setup-boons]
                      [:add-artifact {:artifact lost-in-the-woods}]]})

(def ghost-town {:name    :ghost-town
                 :set     :nocturne
                 :types   #{:night :duration}
                 :cost    3
                 :trigger {:trigger           :at-start-turn
                           :duration          :once
                           :simultaneous-mode :auto
                           :effects           [[:draw 1]
                                               [:give-actions 1]]}
                 :gain-to :hand})

(def guardian {:name    :guardian
               :set     :nocturne
               :types   #{:night :duration}
               :cost    2
               :effects [[:mark-unaffected]]
               :trigger {:trigger           :at-start-turn
                         :duration          :once
                         :simultaneous-mode :auto
                         :effects           [[:give-coins 1]
                                             [:clear-unaffected]]}
               :gain-to :hand})

(defn- idol-boon-or-curse [game {:keys [player-no]}]
  (let [idols-in-play (->> (get-in game [:players player-no :play-area])
                           (filter (comp #{:idol} :name))
                           count)]
    (cond
      (odd? idols-in-play) (receive-boon game {:player-no player-no})
      (even? idols-in-play) (attack-other-players game {:player-no player-no
                                                        :effects   [[:gain {:card-name :curse}]]}))))

(effects/register {::idol-boon-or-curse idol-boon-or-curse})

(def idol {:name       :idol
           :set        :nocturne
           :types      #{:treasure :attack :fate}
           :cost       5
           :coin-value 2
           :effects    [[::idol-boon-or-curse]]
           :setup      [[:setup-boons]]})

(defn- leprechaun-wish-or-hex [game {:keys [player-no]}]
  (let [play-area (get-in game [:players player-no :play-area])]
    (if (= 7 (count play-area))
      (gain game {:player-no player-no
                  :card-name :wish
                  :from      :extra-cards})
      (receive-hex game {:player-no player-no}))))

(effects/register {::leprechaun-wish-or-hex leprechaun-wish-or-hex})

(def leprechaun {:name    :leprechaun
                 :set     :nocturne
                 :types   #{:action :doom}
                 :cost    3
                 :effects [[:gain {:card-name :gold}]
                           [::leprechaun-wish-or-hex]]
                 :setup   [[:setup-extra-cards {:extra-cards [wish-pile]}]
                           [:setup-hexes]]})

(defn- monastery-trash [game {:keys [player-no]}]
  (let [gained-cards (count (get-in game [:players player-no :gained-cards]))]
    (cond-> game
            (pos? gained-cards) (give-choice {:player-no player-no
                                              :text      (str "Trash up to " gained-cards " card" (when (< 1 gained-cards) "s")
                                                              " from your hand or Coppers you have in play.")
                                              :choice    :trash-from-area
                                              :options   [:multi
                                                          [:player :hand]
                                                          [:player :play-area {:name :copper}]]
                                              :max       gained-cards}))))

(effects/register {::monastery-trash monastery-trash})

(def monastery {:name    :monastery
                :set     :nocturne
                :types   #{:night}
                :cost    2
                :effects [[::monastery-trash]]})

(defn- zombie-apprentice-trash [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:trash-from-hand {:card-name card-name}]
                                                    [:draw 3]
                                                    [:give-actions 1]]})))

(def zombie-apprentice {:name    :zombie-apprentice
                        :set     :nocturne
                        :types   #{:action :zombie}
                        :cost    3
                        :effects [[:give-choice {:text    "You may trash and Action card from your hand for +3 Cards and +1 Action."
                                                 :choice  ::zombie-apprentice-trash
                                                 :options [:player :hand {:type :action}]
                                                 :max     1}]]})

(defn- zombie-mason-trash [game {:keys [player-no]}]
  (let [{[card]  :deck
         discard :discard} (get-in game [:players player-no])
        cost (ut/get-cost game card)]
    (assert (or card (empty? discard)) "Discard was not properly shuffled for Zombie Mason.")
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:trash-from-topdeck]
                                                 [:give-choice {:text    (str "You may gain a card costing up to $" (inc cost) ".")
                                                                :choice  :gain
                                                                :options [:supply {:max-cost (inc cost)}]
                                                                :max     1}]]}))))

(def zombie-mason {:name    :zombie-mason
                   :set     :nocturne
                   :types   #{:action :zombie}
                   :cost    3
                   :effects [[:peek-deck 1]
                             [::zombie-mason-trash]]})

(def zombie-spy {:name    :zombie-spy
                 :set     :nocturne
                 :types   #{:action :zombie}
                 :cost    3
                 :effects [[:draw 1]
                           [:give-actions 1]
                           [:look-at 1]
                           [:give-choice {:text    "You may discard the top card of your deck."
                                          :choice  :discard-from-look-at
                                          :options [:player :look-at]}]
                           [:topdeck-all-look-at]]})

(effects/register {::zombie-apprentice-trash zombie-apprentice-trash
                   ::zombie-mason-trash      zombie-mason-trash})

(defn- necromancer-play-action [game {:keys [player-no card-name]}]
  (if card-name
    (let [{:keys [card]} (ut/get-card-idx game [:trash] {:name card-name
                                                         :face #{nil :up}})]
      (-> game
          (ut/update-in-vec [:trash] {:name card-name :face #{nil :up}} assoc :face :down)
          (push-effect-stack {:player-no player-no
                              :effects   [[:card-effect {:card card}]]})))
    game))

(defn- necromancer-setup [game _]
  (assoc game :trash (->> [zombie-apprentice
                           zombie-mason
                           zombie-spy]
                          (map ut/give-id!))))

(effects/register {::necromancer-play-action necromancer-play-action
                   ::necromancer-setup       necromancer-setup})

(def necromancer {:name    :necromancer
                  :set     :nocturne
                  :types   #{:action}
                  :cost    4
                  :effects [[:give-choice {:text    "Play a face up, non-Duration Action card from the Trash."
                                           :choice  ::necromancer-play-action
                                           :options [:trash {:face     :up
                                                             :not-type :duration
                                                             :type     :action}]
                                           :min     1
                                           :max     1}]]
                  :setup   [[::necromancer-setup]]})

(def night-watchman {:name    :night-watchman
                     :set     :nocturne
                     :types   #{:night}
                     :cost    3
                     :effects [[:look-at 5]
                               [:give-choice {:text    "Discard any number of the top 5 cards of your deck."
                                              :choice  :discard-from-look-at
                                              :options [:player :look-at]}]
                               [:topdeck-all-look-at]
                               #_[:give-choice {:text    "Put the rest back on top in any order."
                                                :choice  :topdeck-from-look-at
                                                :options [:player :look-at]
                                                :min     5}]]
                     :gain-to :hand})

(def goat {:name       :goat
           :set        :nocturne
           :types      #{:treasure :heirloom}
           :cost       2
           :coin-value 1
           :effects    [[:give-choice {:text    "You may trash a card from your hand."
                                       :choice  :trash-from-hand
                                       :options [:player :hand]
                                       :max     1}]]})

(defn- pixie-receive-boon [game {:keys [player-no card-id card-name boon]}]
  (let [{:keys [name effects keep-until-clean-up?]} boon]
    (cond-> game
            card-name (-> (cond-> keep-until-clean-up? (-> (update-in [:boons :discard] (partial remove #{boon}))
                                                           (update :boons ut/dissoc-if-empty :discard)
                                                           (update-in [:players player-no :boons] concat [boon])))
                          (push-effect-stack {:player-no player-no
                                              :effects   (concat [[:trash-from-play-area {:trash-card-id card-id}]]
                                                                 effects
                                                                 effects
                                                                 (when keep-until-clean-up?
                                                                   [[:add-trigger {:trigger {:trigger  :at-clean-up
                                                                                             :duration :once
                                                                                             :effects  [[:return-boon {:boon-name name}]]}}]]))})))))

(defn- pixie-give-choice [game {:keys [player-no card-id]}]
  (let [{[{:keys [name] :as boon} & deck] :deck
         discard                          :discard} (maybe-shuffle (:boons game))]
    (-> game
        (assoc :boons (merge {}
                             (when deck {:deck deck})
                             {:discard (concat discard [boon])}))
        (give-choice {:player-no player-no
                      :card-id   card-id
                      :text      (str "You may trash the Pixie to receive " (ut/format-name name) " twice.")
                      :choice    [::pixie-receive-boon {:boon boon}]
                      :options   [:player :play-area {:id card-id}]
                      :max       1}))))

(effects/register {::pixie-receive-boon pixie-receive-boon
                   ::pixie-give-choice  pixie-give-choice})

(def pixie {:name     :pixie
            :set      :nocturne
            :types    #{:action :fate}
            :cost     2
            :effects  [[:draw 1]
                       [:give-actions 1]
                       [::pixie-give-choice]]
            :heirloom goat
            :setup    [[:setup-boons]]})

(def cursed-gold {:name       :cursed-gold
                  :set        :nocturne
                  :types      #{:treasure :heirloom}
                  :cost       4
                  :coin-value 3
                  :effects    [[:gain {:card-name :curse}]]})

(defn- pooka-trash [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:trash-from-hand {:card-name card-name}]
                                                    [:draw 4]]})))

(effects/register {::pooka-trash pooka-trash})

(def pooka {:name     :pooka
            :set      :nocturne
            :types    #{:action}
            :cost     5
            :effects  [[:give-choice {:text    "You may trash a Treasure other than Cursed Gold from your hand, for +4 Cards."
                                      :choice  ::pooka-trash
                                      :options [:player :hand {:type     :treasure
                                                               :not-name #{:cursed-gold}}]
                                      :max     1}]]
            :heirloom cursed-gold})

(defn- sacred-grove-choice [game {:keys [choice] :as args}]
  (case choice
    :yes (receive-boon game args)
    :no game))

(defn- sacred-grove-receive-boon [game {:keys [player-no]}]
  (let [{[{:keys [name effects keep-until-clean-up?] :as boon} & deck] :deck
         discard                                                       :discard} (maybe-shuffle (:boons game))
        discard     (if keep-until-clean-up?
                      discard
                      (concat discard [boon]))
        gives-coin? (some (comp #{:give-coins} first) effects)]
    (-> game
        (assoc :boons (merge {}
                             (when deck {:deck deck})
                             (when discard {:discard discard})))
        (cond-> keep-until-clean-up? (update-in [:players player-no :boons] concat [boon]))
        (push-effect-stack {:player-no player-no
                            :effects   (concat [[:receive-boon {:boon boon}]]
                                               (when (not gives-coin?)
                                                 [[:other-players {:effects [[:give-choice {:text    (str "You may receive " (ut/format-name name) ".")
                                                                                            :choice  [::sacred-grove-choice {:boon boon}]
                                                                                            :options [:special
                                                                                                      {:option :yes :text "Yes please!"}
                                                                                                      {:option :no :text "No thank you."}]
                                                                                            :min     1
                                                                                            :max     1}]]}]]))}))))

(effects/register {::sacred-grove-choice       sacred-grove-choice
                   ::sacred-grove-receive-boon sacred-grove-receive-boon})

(def sacred-grove {:name    :sacred-grove
                   :set     :nocturne
                   :types   #{:action :fate}
                   :cost    5
                   :effects [[:give-buys 1]
                             [:give-coins 3]
                             [::sacred-grove-receive-boon]]
                   :setup   [[:setup-boons]]})

(defn- raider-attack [game {:keys [player-no card-names]}]
  (let [hand               (get-in game [:players player-no :hand])
        has-eligible-card? (some (comp card-names :name) hand)]
    (cond (< (count hand) 5) game
          has-eligible-card? (give-choice game {:player-no player-no
                                                :text      "Discard a copy of a card the attacker has in play."
                                                :choice    :discard-from-hand
                                                :options   [:player :hand {:names card-names}]
                                                :min       1
                                                :max       1})
          :else (reveal-hand game {:player-no player-no}))))

(defn- make-raider-attack [game {:keys [player-no]}]
  (let [card-names (->> (get-in game [:players player-no :play-area])
                        (map :name)
                        set)]
    (attack-other-players game {:player-no player-no
                                :effects   [[::raider-attack {:card-names card-names}]]})))

(effects/register {::raider-attack      raider-attack
                   ::make-raider-attack make-raider-attack})

(def raider {:name    :raider
             :set     :nocturne
             :types   #{:night :duration :attack}
             :cost    6
             :effects [[::make-raider-attack]]
             :trigger {:trigger           :at-start-turn
                       :duration          :once
                       :simultaneous-mode :auto
                       :effects           [[:give-coins 3]]}})

(defn- magic-lamp-genie [game {:keys [player-no card-id]}]
  (let [singular-cards-in-play (->> (get-in game [:players player-no :play-area])
                                    (map :name)
                                    frequencies
                                    (filter (comp #{1} second))
                                    count)
        {:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})]
    (cond-> game
            (and (<= 6 singular-cards-in-play)
                 card) (push-effect-stack {:player-no player-no
                                           :effects   [[:trash-this {:card-id card-id}]
                                                       [:gain {:card-name :wish :from :extra-cards}]
                                                       [:gain {:card-name :wish :from :extra-cards}]
                                                       [:gain {:card-name :wish :from :extra-cards}]]}))))

(effects/register {::magic-lamp-genie magic-lamp-genie})

(def magic-lamp {:name       :magic-lamp
                 :set        :nocturne
                 :types      #{:treasure :heirloom}
                 :cost       0
                 :coin-value 1
                 :effects    [[::magic-lamp-genie]]})

(def secret-cave-trigger {:trigger           :at-start-turn
                          :duration          :once
                          :simultaneous-mode :auto
                          :effects           [[:give-coins 3]]})

(defn- secret-cave-discard [game {:keys [player-no card-id card-names] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   (concat [[:discard-from-hand args]]
                                              (when (= 3 (count card-names))
                                                [[:add-trigger {:trigger secret-cave-trigger
                                                                :card-id card-id}]]))}))

(effects/register {::secret-cave-discard secret-cave-discard})

(def secret-cave {:name     :secret-cave
                  :set      :nocturne
                  :types    #{:action :duration}
                  :cost     3
                  :effects  [[:draw 1]
                             [:give-actions 1]
                             [:give-choice {:text      "You may discard 3 cards, for +$3 next turn."
                                            :choice    ::secret-cave-discard
                                            :options   [:player :hand]
                                            :min       3
                                            :max       3
                                            :optional? true}]]
                  :heirloom magic-lamp
                  :setup    [[:setup-extra-cards {:extra-cards [wish-pile]}]]})

(defn pasture-victory-points [cards]
  (->> cards
       (filter (comp #{:estate} :name))
       count))

(effects/register {::pasture-victory-points pasture-victory-points})

(defn- shepherd-discard-draw [game {:keys [player-no card-names]}]
  (cond-> game
          (not-empty card-names) (push-effect-stack {:player-no player-no
                                                     :effects   [[:reveal-from-hand {:card-names card-names}]
                                                                 [:discard-from-revealed {:card-names card-names}]
                                                                 [:draw (* 2 (count card-names))]]})))

(effects/register {::shepherd-discard-draw shepherd-discard-draw})

(def pasture {:name           :pasture
              :set            :nocturne
              :types          #{:treasure :victory :heirloom}
              :cost           2
              :coin-value     1
              :victory-points ::pasture-victory-points})

(def shepherd {:name     :shepherd
               :set      :nocturne
               :types    #{:action}
               :cost     4
               :effects  [[:give-actions 1]
                          [:give-choice {:text    "Discard any number of Victory cards."
                                         :choice  ::shepherd-discard-draw
                                         :options [:player :hand {:type :victory}]}]]
               :heirloom pasture})

(def skulk {:name    :skulk
            :set     :nocturne
            :types   #{:action :attack :doom}
            :cost    4
            :effects [[:give-buys 1]
                      [:others-receive-next-hex]]
            :on-gain [[:gain {:card-name :gold}]]
            :setup   [[:setup-hexes]]})

(defn- tormentor-imp-or-hex [game {:keys [player-no card-id]}]
  (let [no-other-cards-in-play? (->> (get-in game [:players player-no :play-area])
                                     (remove (comp #{card-id} :id))
                                     empty?)]
    (if no-other-cards-in-play?
      (gain game {:player-no player-no
                  :card-name :imp
                  :from      :extra-cards})
      (others-receive-next-hex game {:player-no player-no}))))

(effects/register {::tormentor-imp-or-hex tormentor-imp-or-hex})

(def tormentor {:name    :tormentor
                :set     :nocturne
                :types   #{:action :attack :doom}
                :cost    5
                :effects [[:give-coins 2]
                          [::tormentor-imp-or-hex]]
                :setup   [[:setup-extra-cards {:extra-cards [(:imp spirit-piles)]}]
                          [:setup-hexes]]})

(def pouch {:name       :pouch
            :set        :nocturne
            :types      #{:treasure :heirloom}
            :cost       2
            :coin-value 1
            :effects    [[:give-buys 1]]})

(def tracker {:name          :tracker
              :set           :nocturne
              :types         #{:action :fate}
              :cost          2
              :effects       [[:give-coins 1]
                              [:receive-boon]]
              :while-in-play {:on-gain [[:topdeck-gained-choice]]}
              :heirloom      pouch
              :setup         [[:setup-boons]]})

(defn- tragic-hero-demise [game {:keys [player-no card-id]}]
  (let [hand-size (count (get-in game [:players player-no :hand]))]
    (cond-> game
            (<= 8 hand-size) (push-effect-stack {:player-no player-no
                                                 :effects   [[:trash-from-play-area {:trash-card-id card-id}]
                                                             [:give-choice {:text    "Gain a Treasure."
                                                                            :choice  :gain
                                                                            :options [:supply {:type :treasure}]
                                                                            :min     1
                                                                            :max     1}]]}))))

(effects/register {::tragic-hero-demise tragic-hero-demise})

(def tragic-hero {:name    :tragic-hero
                  :set     :nocturne
                  :types   #{:action}
                  :cost    5
                  :effects [[:draw 3]
                            [:give-buys 1]
                            [::tragic-hero-demise]]})

(defn- werewolf-hunt [game {:keys [player-no]}]
  (let [phase (get-in game [:players player-no :phase])]
    (push-effect-stack game {:player-no player-no
                             :effects   [(if (= :night phase)
                                           [:others-receive-next-hex]
                                           [:draw 3])]})))

(effects/register {::werewolf-hunt werewolf-hunt})

(def werewolf {:name    :werewolf
               :set     :nocturne
               :types   #{:action :night :attack :doom}
               :cost    5
               :effects [[::werewolf-hunt]]
               :setup   [[:setup-hexes]]})

(def kingdom-cards [bard
                    blessed-village
                    cemetery
                    changeling
                    cobbler
                    conclave
                    crypt
                    cursed-village
                    den-of-sin
                    devils-workshop
                    exorcist
                    faithful-hound
                    fool
                    ghost-town
                    guardian
                    idol
                    leprechaun
                    monastery
                    necromancer
                    night-watchman
                    pixie
                    pooka
                    sacred-grove
                    raider
                    secret-cave
                    shepherd
                    skulk
                    tormentor
                    tracker
                    tragic-hero
                    werewolf])

; Doable without Boons / Hexes:
; Faithful Hound - On discard other than Clean Up
; Exorcist - viewing extra cards
