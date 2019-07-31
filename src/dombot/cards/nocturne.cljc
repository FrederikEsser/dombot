(ns dombot.cards.nocturne
  (:require [dombot.operations :refer [push-effect-stack give-choice move-card attack-other-players gain state-maintenance]]
            [dombot.cards.common :refer [reveal-hand add-trigger]]
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

(defn- will-o-wish-put-revealed-into-hand [game {:keys [player-no]}]
  (let [{:keys [id] :as card} (last (get-in game [:players player-no :revealed]))
        cost (ut/get-cost game card)]
    (cond-> game
            (and card (<= cost 2)) (move-card {:player-no    player-no
                                               :move-card-id id
                                               :from         :revealed
                                               :to           :hand}))))

(effects/register {::will-o-wish-put-revealed-into-hand will-o-wish-put-revealed-into-hand})

(def will-o-wisp {:name    :will-o'-wisp
                  :set     :nocturne
                  :types   #{:action :spirit}
                  :cost    0
                  :effects [[:draw 1]
                            [:give-actions 1]
                            [:reveal-from-deck 1]
                            [::will-o-wish-put-revealed-into-hand]
                            [:topdeck-all-revealed]]})

(def spirit-piles {:ghost        {:card ghost :pile-size 6}
                   :imp          {:card imp :pile-size 13}
                   :will-o'-wisp {:card will-o-wisp :pile-size 12}})

(def wish {:name    :wish
           :set     :nocturne
           :types   #{:action}
           :cost    0
           :effects [[:give-actions 1]
                     [:return-this-to-supply {:area :extra-cards}]
                     [:give-choice {:text    "Gain a card to your hand costing up to $6."
                                    :choice  :gain-to-hand
                                    :options [:supply {:max-cost 6}]
                                    :min     1
                                    :max     1}]]})

(def wish-pile {:card wish :pile-size 12})

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

(defn- changeling-exchange [game {:keys [player-no card-name gained-card-id from]
                                  :or   {from :supply} :as args}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:move-card {:move-card-id gained-card-id
                                                                 :from         :gaining
                                                                 :to           from}]
                                                    [:move-card {:card-name :changeling
                                                                 :from      :supply
                                                                 :to        :discard}]]})))

(defn- changeling-on-gain [game {:keys [player-no gained-card-id bought] :as args}]
  (let [{{:keys [name on-gain on-buy] :as card} :card} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})
        cost         (ut/get-cost game card)
        {:keys [pile-size]} (ut/get-pile-idx game :changeling)
        ignore-gain? (or (= :changeling name)
                         (and bought
                              (nil? on-gain)
                              (nil? on-buy)))]
    (cond-> game
            (and card
                 (<= 3 cost)
                 (pos? pile-size)
                 (not ignore-gain?)) (give-choice {:player-no player-no
                                                   :text      (str "You may exchange the gained " (ut/format-name name) " for a Changeling.")
                                                   :choice    [::changeling-exchange args]
                                                   :options   [:player :gaining {:id gained-card-id}]
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

(def kingdom-cards [cemetery
                    changeling
                    cobbler
                    conclave
                    crypt
                    den-of-sin
                    devils-workshop
                    exorcist
                    ghost-town
                    guardian
                    monastery
                    necromancer
                    night-watchman
                    pooka
                    raider
                    secret-cave
                    shepherd
                    tragic-hero])

; Doable without Boons / Hexes:
; Faithful Hound - On discard other than Clean Up
; Exorcist - viewing extra cards
