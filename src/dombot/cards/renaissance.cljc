(ns dombot.cards.renaissance
  (:require [dombot.operations :refer [push-effect-stack give-choice draw move-cards gain card-effect affect-other-players state-maintenance]]
            [dombot.cards.common :refer [reveal-hand reveal-from-deck add-trigger give-coins give-coffers give-villagers set-aside=>hand-trigger take-artifact]]
            [dombot.cards.dominion :as dominion]
            [dombot.cards.guilds :as guilds]
            [dombot.utils :as ut]
            [dombot.effects :as effects])
  (:refer-clojure :exclude [key]))

(def acting-troupe {:name    :acting-troupe
                    :set     :renaissance
                    :types   #{:action}
                    :cost    3
                    :effects [[:give-villagers 4]
                              [:trash-this]]})

(defn horn-at-clean-up [game {:keys [player-no]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:name :border-guard})]
    (cond-> game
            card (ut/update-in-vec [:players player-no :play-area] {:name :border-guard}
                                   assoc :at-clean-up [[:topdeck-this-from-play-area]]))))

(effects/register {::horn-at-clean-up horn-at-clean-up})

(def horn {:name    :horn
           :type    :artifact
           :trigger {:trigger :at-clean-up
                     :effects [[::horn-at-clean-up]]}})

(def lantern {:name :lantern
              :type :artifact})

(defn border-guard-choice [game {:keys [player-no choice]}]
  (case choice
    :horn (take-artifact game {:player-no player-no :artifact-name :horn})
    :lantern (take-artifact game {:player-no player-no :artifact-name :lantern})))

(defn- border-guard-number-of-revealed-cards [game player-no]
  (if (= player-no (get-in game [:artifacts :lantern :owner])) 3 2))

(defn border-guard-take-revealed [game {:keys [player-no] :as args}]
  (let [revealed           (get-in game [:players player-no :revealed])
        has-lantern?       (= player-no (get-in game [:artifacts :lantern :owner]))
        has-horn?          (= player-no (get-in game [:artifacts :horn :owner]))
        may-take-artifact? (and (= (border-guard-number-of-revealed-cards game player-no)
                                   (count revealed))
                                (every? :action (map (partial ut/get-types game) revealed))
                                (or (not has-lantern?) (not has-horn?)))]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat [[:put-revealed-into-hand args]]
                                                (when may-take-artifact?
                                                  [[:give-choice {:text    "Choose one:"
                                                                  :choice  ::border-guard-choice
                                                                  :options [:special
                                                                            {:option :lantern :text "Take the Lantern."}
                                                                            {:option :horn :text "Take the Horn."}]
                                                                  :min     1
                                                                  :max     1}]]))})))

(defn border-guard-reveal [game {:keys [player-no]}]
  (reveal-from-deck game {:player-no player-no
                          :arg       (border-guard-number-of-revealed-cards game player-no)}))

(effects/register {::border-guard-choice        border-guard-choice
                   ::border-guard-take-revealed border-guard-take-revealed
                   ::border-guard-reveal        border-guard-reveal})

(def border-guard {:name    :border-guard
                   :set     :renaissance
                   :types   #{:action}
                   :cost    2
                   :effects [[:give-actions 1]
                             [::border-guard-reveal]
                             [:give-choice {:text    "Put one of the revealed cards into your hand."
                                            :choice  ::border-guard-take-revealed
                                            :options [:player :revealed]
                                            :min     1
                                            :max     1}]
                             [:discard-all-revealed]]
                   :setup   [[:add-artifact {:artifact horn}]
                             [:add-artifact {:artifact lantern}]]})

(def cargo-ship-trigger {:trigger  :on-gain
                         :duration :turn
                         :effects  [[::cargo-ship-give-choice]]})

(defn cargo-ship-set-aside [game {:keys [player-no card-id card-name gained-card-id]}]
  (if card-name
    (let [{card     :card
           card-idx :idx} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})
          {trigger-idx :idx} (ut/get-trigger-idx game [:players player-no :triggers] {:card-id  card-id ; todo: Use trigger-id
                                                                                      :duration :turn})]
      (-> game
          (update-in [:players player-no :gaining] ut/vec-remove card-idx)
          (update-in [:players player-no :triggers] ut/vec-remove trigger-idx)
          (push-effect-stack {:player-no player-no
                              :effects   [[:add-trigger {:trigger (merge set-aside=>hand-trigger
                                                                         {:set-aside [card]})
                                                         :card-id card-id}]]})
          (state-maintenance player-no :gaining :cargo-ship)))
    game))

(defn cargo-ship-give-choice [game {:keys [player-no card-id gained-card-id]}]
  (let [{{:keys [name] :as card} :card} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})]
    (cond-> game
            card (give-choice {:player-no player-no
                               :card-id   card-id
                               :text      (str "You may set the gained " (ut/format-name name) " aside on Cargo Ship.")
                               :choice    [::cargo-ship-set-aside {:gained-card-id gained-card-id}]
                               :options   [:player :gaining {:id gained-card-id}]
                               :max       1}))))

(effects/register {::cargo-ship-set-aside   cargo-ship-set-aside
                   ::cargo-ship-give-choice cargo-ship-give-choice})

(def cargo-ship {:name    :cargo-ship
                 :set     :renaissance
                 :types   #{:action :duration}
                 :cost    3
                 :effects [[:give-coins 2]
                           [:add-trigger {:trigger cargo-ship-trigger}]]})

(def ducat {:name    :ducat
            :set     :renaissance
            :types   #{:treasure}
            :cost    2
            :effects [[:give-coffers 1]
                      [:give-buys 1]]
            :on-gain [[:give-choice {:text    "You may trash a Copper from your hand."
                                     :choice  :trash-from-hand
                                     :options [:player :hand {:name :copper}]
                                     :max     1}]]})

(defn- experiment-on-gain [game {:keys [player-no gained-by]}]
  (cond-> game
          (not= gained-by :experiment) (gain {:player-no player-no
                                              :card-name :experiment
                                              :gained-by :experiment})))

(effects/register {::experiment-on-gain experiment-on-gain})

(def experiment {:name    :experiment
                 :set     :renaissance
                 :types   #{:action}
                 :cost    3
                 :effects [[:draw 2]
                           [:give-actions 1]
                           [:return-this-to-supply]]
                 :on-gain [[::experiment-on-gain]]})

(def flag {:name    :flag
           :type    :artifact
           :trigger {:trigger :at-draw-hand
                     :effects [[:draw 1]]}})

(def flag-bearer {:name     :flag-bearer
                  :set      :renaissance
                  :types    #{:action}
                  :cost     4
                  :effects  [[:give-coins 2]]
                  :on-gain  [[:take-artifact {:artifact-name :flag}]]
                  :on-trash [[:take-artifact {:artifact-name :flag}]]
                  :setup    [[:add-artifact {:artifact flag}]]})

(defn hideout-trash [game {:keys [player-no card-name] :as args}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        types (ut/get-types game card)]
    (push-effect-stack game (merge args {:effects (concat [[:trash-from-hand {:card-name card-name}]]
                                                          (when (:victory types)
                                                            [[:gain {:card-name :curse}]]))}))))

(effects/register {::hideout-trash hideout-trash})

(def hideout {:name    :hideout
              :set     :renaissance
              :types   #{:action}
              :cost    4
              :effects [[:draw 1]
                        [:give-actions 2]
                        [:give-choice {:text    "Trash a card from your hand."
                                       :choice  ::hideout-trash
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]]})

(defn improve-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:name card-name})
        cost (inc (ut/get-cost game card))]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:trash-from-play-area {:card-name card-name}]
                                                 [:give-choice {:text    (str "Gain a card costing exactly $" cost ".")
                                                                :choice  :gain
                                                                :options [:supply {:cost cost}]
                                                                :min     1
                                                                :max     1}]]}))))

(defn improve-give-choice [game {:keys [player-no]}]
  (give-choice game {:player-no player-no
                     :text      "You may trash an Action card you would discard this turn to gain a card costing exactly $1 more than it."
                     :choice    ::improve-trash
                     :options   [:player :play-area {:type        :action
                                                     :leaves-play true}]
                     :max       1}))

(defn improve-clean-up [game {:keys [player-no card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})]
    (cond-> game
            card (ut/update-in-vec [:players player-no :play-area] {:id card-id}
                                   assoc :at-clean-up [[::improve-give-choice]]))))

(effects/register {::improve-trash       improve-trash
                   ::improve-give-choice improve-give-choice
                   ::improve-clean-up    improve-clean-up})

(def improve {:name    :improve
              :set     :renaissance
              :types   #{:action}
              :cost    3
              :effects [[:give-coins 2]
                        [::improve-clean-up]]})

(def inventor {:name    :inventor
               :set     :renaissance
               :types   #{:action}
               :cost    4
               :effects [[:give-choice {:text    "Gain a card costing up to $4."
                                        :choice  :gain
                                        :options [:supply {:max-cost 4}]
                                        :min     1
                                        :max     1}]
                         [:add-cost-reduction 1]]})

(def lackeys {:name    :lackeys
              :set     :renaissance
              :types   #{:action}
              :cost    2
              :effects [[:draw 2]]
              :on-gain [[:give-villagers 2]]})

(defn mountain-village-draw [game {:keys [player-no]}]
  (let [discard (get-in game [:players player-no :discard])]
    (if (empty? discard)
      (draw game {:player-no player-no
                  :arg       1})
      (give-choice game {:player-no player-no
                         :text      "Look through your discard pile and put a card from it into your hand."
                         :choice    :take-from-discard
                         :options   [:player :discard]
                         :min       1
                         :max       1}))))

(effects/register {::mountain-village-draw mountain-village-draw})

(def mountain-village {:name    :mountain-village
                       :set     :renaissance
                       :types   #{:action}
                       :cost    4
                       :effects [[:give-actions 2]
                                 [::mountain-village-draw]]})

(def old-witch {:name    :old-witch
                :set     :renaissance
                :types   #{:action :attack}
                :cost    5
                :effects [[:draw 3]
                          [:attack {:effects [[:gain {:card-name :curse}]
                                              [:give-choice {:text    "You may trash a Curse from your hand."
                                                             :choice  :trash-from-hand
                                                             :options [:player :hand {:name :curse}]
                                                             :max     1}]]}]]})

(def patron {:name      :patron
             :set       :renaissance
             :types     #{:action :reaction}
             :cost      4
             :effects   [[:give-villagers 1]
                         [:give-coins 2]]
             :on-reveal [[:give-coffers 1]]})

(def priest-trigger {:trigger  :on-trash
                     :duration :turn
                     :effects  [[:give-coins 2]]})

(def priest {:name    :priest
             :set     :renaissance
             :types   #{:action}
             :cost    4
             :effects [[:give-coins 2]
                       [:give-choice {:text    "Trash a card from your hand."
                                      :choice  :trash-from-hand
                                      :options [:player :hand]
                                      :min     1
                                      :max     1}]
                       [:add-trigger {:trigger priest-trigger}]]})

(defn recruiter-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-hand {:card-name card-name}]
                                         [:give-villagers cost]]})))

(effects/register {::recruiter-trash recruiter-trash})

(def recruiter {:name    :recruiter
                :set     :renaissance
                :types   #{:action}
                :cost    5
                :effects [[:draw 2]
                          [:give-choice {:text    "Trash a cards from your hand."
                                         :choice  ::recruiter-trash
                                         :options [:player :hand]
                                         :min     1
                                         :max     1}]]})

(defn research-set-aside [game {:keys [player-no card-id]}]
  (let [set-aside (get-in game [:players player-no :set-aside])]
    (-> game
        (cond->
          (not-empty set-aside) (push-effect-stack {:player-no player-no
                                                    :effects   [[:add-trigger {:trigger (merge set-aside=>hand-trigger
                                                                                               {:set-aside set-aside})
                                                                               :card-id card-id}]]}))
        (update-in [:players player-no] dissoc :set-aside))))

(defn research-trash [game {:keys [player-no card-name] :as args}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (push-effect-stack game (merge args {:effects [[:trash-from-hand {:card-name card-name}]
                                                   [:set-aside {:number-of-cards cost}]
                                                   [::research-set-aside]]}))))

(effects/register {::research-set-aside research-set-aside
                   ::research-trash     research-trash})

(def research {:name    :research
               :set     :renaissance
               :types   #{:action :duration}
               :cost    4
               :effects [[:give-actions 1]
                         [:give-choice {:text    "Trash a card from your hand."
                                        :choice  ::research-trash
                                        :options [:player :hand]
                                        :min     1
                                        :max     1}]]})

(defn- scepter-replay [game {:keys [player-no card-id card-name]}]
  (let [played-card-ids (set (get-in game [:players player-no :actions-played]))
        {:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:name card-name
                                                                              :id   played-card-ids})]
    (push-effect-stack game {:player-no player-no
                             :card-id   card-id
                             :effects   [[:card-effect {:card card}]
                                         [:register-repeated-play {:target-id (:id card)}]]})))

(defn- scepter-choice [game {:keys [player-no card-id choice]}]
  (let [played-card-ids (set (get-in game [:players player-no :actions-played]))]
    (case choice
      :coins (give-coins game {:player-no player-no :arg 2})
      :replay-action (give-choice game {:player-no player-no
                                        :card-id   card-id
                                        :text      "Replay an Action card you played this turn."
                                        :choice    ::scepter-replay
                                        :options   [:player :play-area {:type :action
                                                                        :ids  played-card-ids}]
                                        :min       1
                                        :max       1}))))

(effects/register {::scepter-replay scepter-replay
                   ::scepter-choice scepter-choice})

(def scepter {:name    :scepter
              :set     :renaissance
              :types   #{:treasure}
              :cost    5
              :effects [[:give-choice {:text    "Choose one:"
                                       :choice  ::scepter-choice
                                       :options [:special
                                                 {:option :coins :text "+$2"}
                                                 {:option :replay-action :text "Replay an Action card."}]
                                       :min     1
                                       :max     1}]]})

(def scholar {:name    :scholar
              :set     :renaissance
              :types   #{:action}
              :cost    5
              :effects [[:discard-all-hand]
                        [:draw 7]]})

(defn sculptor-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game card-name)
        types (ut/get-types game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:gain-to-hand {:card-name card-name}]
                                         (when (:treasure types) [:give-villagers 1])]})))

(effects/register {::sculptor-gain sculptor-gain})

(def sculptor {:name    :sculptor
               :set     :renaissance
               :types   #{:action}
               :cost    5
               :effects [[:give-choice {:text    "Gain a card to your hand costing up to $4."
                                        :choice  ::sculptor-gain
                                        :options [:supply {:max-cost 4}]
                                        :min     1
                                        :max     1}]]})

(defn seer-put-in-hand [game {:keys [player-no]}]
  (let [card-names (->> (get-in game [:players player-no :revealed])
                        (filter (comp #{2 3 4} (partial ut/get-cost game)))
                        (map :name))]
    (move-cards game {:player-no  player-no
                      :card-names card-names
                      :from       :revealed
                      :to         :hand})))

(effects/register {::seer-put-in-hand seer-put-in-hand})

(def seer {:name    :seer
           :set     :renaissance
           :types   #{:action}
           :cost    5
           :effects [[:draw 1]
                     [:give-actions 1]
                     [:reveal-from-deck 3]
                     [::seer-put-in-hand]
                     [:give-choice {:text    "Put the revealed cards back onto your deck in any order."
                                    :choice  :topdeck-from-revealed
                                    :options [:player :revealed]
                                    :min     3
                                    :max     3}]]})

(def silk-merchant {:name     :silk-merchant
                    :set      :renaissance
                    :types    #{:action}
                    :cost     4
                    :effects  [[:draw 2]
                               [:give-buys 1]]
                    :on-gain  [[:give-coffers 1]
                               [:give-villagers 1]]
                    :on-trash [[:give-coffers 1]
                               [:give-villagers 1]]})

(def spices {:name       :spices
             :set        :renaissance
             :types      #{:treasure}
             :cost       5
             :coin-value 2
             :effects    [[:give-buys 1]]
             :on-gain    [[:give-coffers 2]]})

(def treasure-chest {:name    :treasure-chest
                     :type    :artifact
                     :trigger {:trigger :at-start-buy
                               :effects [[:gain {:card-name :gold}]]}})

(defn- swashbuckler-check-discard [game {:keys [player-no]}]
  (let [{:keys [discard coffers]} (get-in game [:players player-no])]
    (cond-> game
            (not-empty discard) (push-effect-stack {:player-no player-no
                                                    :effects   (concat
                                                                 [[:give-coffers 1]]
                                                                 (when (and coffers (<= 3 coffers))
                                                                   [[:take-artifact {:artifact-name :treasure-chest}]]))}))))

(effects/register {::swashbuckler-check-discard swashbuckler-check-discard})

(def swashbuckler {:name    :swashbuckler
                   :set     :renaissance
                   :types   #{:action}
                   :cost    5
                   :effects [[:draw 3]
                             [::swashbuckler-check-discard]]
                   :setup   [[:add-artifact {:artifact treasure-chest}]]})

(def key {:name    :key
          :type    :artifact
          :trigger {:trigger :at-start-turn
                    :mode    :auto
                    :effects [[:give-coins 1]]}})

(defn treasurer-choice [game {:keys [player-no choice]}]
  (case choice
    :trash (give-choice game {:player-no player-no
                              :text      "Trash a Treasure from your hand."
                              :choice    :trash-from-hand
                              :options   [:player :hand {:type :treasure}]
                              :min       1
                              :max       1})
    :gain (give-choice game {:player-no player-no
                             :text      "Gain a Treasure from the trash to your hand."
                             :choice    :gain-from-trash-to-hand
                             :options   [:trash {:type :treasure}]
                             :min       1
                             :max       1})
    :key (take-artifact game {:player-no player-no :artifact-name :key})))

(effects/register {::treasurer-choice treasurer-choice})

(def treasurer {:name    :treasurer
                :set     :renaissance
                :types   #{:action}
                :cost    5
                :effects [[:give-coins 3]
                          [:give-choice {:text    "Choose one:"
                                         :choice  ::treasurer-choice
                                         :options [:special
                                                   {:option :trash :text "Trash a Treasure from your hand."}
                                                   {:option :gain :text "Gain a Treasure from the trash to your hand."}
                                                   {:option :key :text "Take the Key."}]
                                         :min     1
                                         :max     1}]]
                :setup   [[:add-artifact {:artifact key}]]})

(defn villain-attack [game {:keys [player-no]}]
  (let [hand               (get-in game [:players player-no :hand])
        has-eligible-card? (some (comp (partial <= 2) (partial ut/get-cost game)) hand)]
    (cond (< (count hand) 5) game
          has-eligible-card? (give-choice game {:player-no player-no
                                                :text      "Discard a card costing $2 or more."
                                                :choice    :discard-from-hand
                                                :options   [:player :hand {:min-cost 2}]
                                                :min       1
                                                :max       1})
          :else (reveal-hand game {:player-no player-no}))))

(effects/register {::villain-attack villain-attack})

(def villain {:name    :villain
              :set     :renaissance
              :types   #{:action :attack}
              :cost    5
              :effects [[:give-coffers 2]
                        [:attack {:effects [[::villain-attack]]}]]})

(def kingdom-cards [acting-troupe
                    border-guard
                    cargo-ship
                    ducat
                    experiment
                    flag-bearer
                    hideout
                    improve
                    inventor
                    lackeys
                    mountain-village
                    old-witch
                    patron
                    priest
                    recruiter
                    research
                    scepter
                    scholar
                    sculptor
                    seer
                    silk-merchant
                    spices
                    swashbuckler
                    treasurer
                    villain])

(def academy {:name    :academy
              :set     :renaissance
              :type    :project
              :cost    5
              :trigger {:trigger :on-gain
                        :type    :action
                        :effects [[:give-villagers 1]]}})

(def barracks {:name    :barracks
               :set     :renaissance
               :type    :project
               :cost    6
               :trigger {:trigger :at-start-turn
                         :mode    :auto
                         :effects [[:give-actions 1]]}})

(def canal {:name   :canal
            :set    :renaissance
            :type   :project
            :cost   7
            :on-buy [[:add-player-cost-reduction 1]]})

(def capitalism {:name :capitalism
                 :set  :renaissance
                 :type :project
                 :cost 5})

(def cathedral {:name    :cathedral
                :set     :renaissance
                :type    :project
                :cost    3
                :trigger {:trigger :at-start-turn
                          :mode    :manual
                          :effects [[:give-choice {:text    "Trash a card from your hand."
                                                   :choice  :trash-from-hand
                                                   :options [:player :hand]
                                                   :min     1
                                                   :max     1}]]}})

(def citadel {:name    :citadel
              :set     :renaissance
              :type    :project
              :cost    8
              :trigger {:trigger :play-first-action
                        :effects [[:card-effect]]}})

(def city-gate {:name    :city-gate
                :set     :renaissance
                :type    :project
                :cost    3
                :trigger {:trigger :at-start-turn
                          :mode    :manual
                          :effects [[:draw 1]
                                    [:give-choice {:text    "Put a card from your hand onto your deck."
                                                   :choice  :topdeck-from-hand
                                                   :options [:player :hand]
                                                   :min     1
                                                   :max     1}]]}})

(defn- crop-rotation-discard [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:draw 2]]})))

(effects/register {::crop-rotation-discard crop-rotation-discard})

(def crop-rotation {:name    :crop-rotation
                    :set     :renaissance
                    :type    :project
                    :cost    6
                    :trigger {:trigger :at-start-turn
                              :mode    :manual
                              :effects [[:give-choice {:text    "You may discard a Victory card for +2 Cards."
                                                       :choice  ::crop-rotation-discard
                                                       :options [:player :hand {:type :victory}]
                                                       :max     1}]]}})

(defn- exploration-at-end-buy [game {:keys [player-no]}]
  (let [bought-cards? (->> (get-in game [:players player-no :gained-cards])
                           (filter :bought)
                           not-empty)]
    (cond-> game
            (not bought-cards?) (push-effect-stack {:player-no player-no
                                                    :effects   [[:give-coffers 1]
                                                                [:give-villagers 1]]}))))

(effects/register {::exploration-at-end-buy exploration-at-end-buy})

(def exploration {:name    :exploration
                  :set     :renaissance
                  :type    :project
                  :cost    4
                  :trigger {:trigger :at-end-buy
                            :effects [[::exploration-at-end-buy]]}})

(def fair {:name    :fair
           :set     :renaissance
           :type    :project
           :cost    4
           :trigger {:trigger :at-start-turn
                     :mode    :auto
                     :effects [[:give-buys 1]]}})

(def fleet {:name    :fleet
            :set     :renaissance
            :type    :project
            :cost    5
            :trigger {:trigger  :at-end-game
                      :duration :once}})

(def guildhall {:name    :guildhall
                :set     :renaissance
                :type    :project
                :cost    5
                :trigger {:trigger :on-gain
                          :type    :treasure
                          :effects [[:give-coffers 1]]}})

(defn- innovation-play-action [game {:keys [player-no card-name gained-card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})]
    (cond-> game
            card-name (push-effect-stack {:player-no player-no
                                          :effects   [[:move-card {:move-card-id gained-card-id
                                                                   :from         :gaining
                                                                   :to           :play-area}]
                                                      [:card-effect {:card card}]]}))))

(defn- innovation-on-gain [game {:keys [player-no gained-card-id]}]
  (let [{{:keys [name]} :card} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})
        gained-actions (->> (get-in game [:players player-no :gained-cards])
                            (filter (comp :action (partial ut/get-types game)))
                            count)]
    (cond-> game
            (<= gained-actions 1) (give-choice {:player-no player-no
                                                :text      (str "You may play the gained " (ut/format-name name) ".")
                                                :choice    [::innovation-play-action {:gained-card-id gained-card-id}]
                                                :options   [:player :gaining {:id gained-card-id}]
                                                :max       1}))))

(effects/register {::innovation-play-action innovation-play-action
                   ::innovation-on-gain     innovation-on-gain})

(def innovation {:name    :innovation
                 :set     :renaissance
                 :type    :project
                 :cost    6
                 :trigger {:trigger :on-gain
                           :type    :action
                           :effects [[::innovation-on-gain]]}})

(defn- pageant-pay-for-coffers [game {:keys [player-no choice]}]
  (cond-> game
          (= :get-coffers choice) (push-effect-stack {:player-no player-no
                                                      :effects   [[:give-coins -1]
                                                                  [:give-coffers 1]]})))

(defn pageant-give-choice [game {:keys [player-no]}]
  (let [coins (get-in game [:players player-no :coins])]
    (cond-> game
            (pos? coins) (give-choice {:player-no player-no
                                       :text      "You may pay $1 for +1 Coffers."
                                       :choice    ::pageant-pay-for-coffers
                                       :options   [:special
                                                   {:option :get-coffers :text "+1 Coffers"}
                                                   {:option :decline :text "Decline"}]
                                       :min       1
                                       :max       1}))))

(effects/register {::pageant-pay-for-coffers pageant-pay-for-coffers
                   ::pageant-give-choice     pageant-give-choice})

(def pageant {:name    :pageant
              :set     :renaissance
              :type    :project
              :cost    3
              :trigger {:trigger :at-end-buy
                        :effects [[::pageant-give-choice]]}})

(def piazza {:name    :piazza
             :set     :renaissance
             :type    :project
             :cost    5
             :trigger {:trigger :at-start-turn
                       :mode    :manual
                       :effects [[:reveal-from-deck 1]
                                 [::guilds/herald-play-action]]}})

(defn- road-network-on-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game card-name)
        types (ut/get-types game card)]
    (cond-> game
            (:victory types) (draw {:player-no player-no
                                    :arg       1}))))

(defn- road-network-add-triggers [game {:keys [player-no]}]
  (affect-other-players game {:player-no player-no
                              :effects   [[:add-trigger {:trigger {:name     :road-network
                                                                   :duration :game
                                                                   :trigger  :on-gain
                                                                   :effects  [[::road-network-on-gain {:player-no player-no}]]}}]]}))

(effects/register {::road-network-on-gain      road-network-on-gain
                   ::road-network-add-triggers road-network-add-triggers})

(def road-network {:name   :road-network
                   :set    :renaissance
                   :type   :project
                   :cost   5
                   :on-buy [[::road-network-add-triggers]]})

(defn- sewers-on-trash [game {:keys [player-no trashed-by]}]
  (cond-> game
          (not= trashed-by :sewers) (give-choice {:player-no player-no
                                                  :text      "You may trash a card from your hand to the Sewers."
                                                  :choice    [:trash-from-hand {:trashed-by :sewers}]
                                                  :options   [:player :hand]
                                                  :max       1})))

(effects/register {::sewers-on-trash sewers-on-trash})

(def sewers {:name    :sewers
             :set     :renaissance
             :type    :project
             :cost    3
             :trigger {:trigger :on-trash
                       :effects [[::sewers-on-trash]]}})

(def silos {:name    :silos
            :set     :renaissance
            :type    :project
            :cost    4
            :trigger {:trigger :at-start-turn
                      :mode    :manual
                      :effects [[:give-choice {:text    "Discard any number of Coppers."
                                               :choice  ::dominion/cellar-sift
                                               :options [:player :hand {:name :copper}]}]]}})

(defn- sinister-plot-choice [game {:keys [player-no choice]}]
  (let [{:keys [tokens idx]} (->> (get-in game [:projects :sinister-plot :participants])
                                  (keep-indexed (fn [idx participant]
                                                  (when (= player-no (:player-no participant))
                                                    {:idx    idx
                                                     :tokens (:tokens participant)})))
                                  first)]
    (case choice
      :add-token (update-in game [:projects :sinister-plot :participants idx :tokens] ut/plus 1)
      :remove-tokens (-> game
                         (update-in [:projects :sinister-plot :participants idx] dissoc :tokens)
                         (cond-> tokens (draw {:player-no player-no :arg tokens}))))))

(defn- sinister-plot-give-choice [game {:keys [player-no]}]
  (let [tokens (or (->> (get-in game [:projects :sinister-plot :participants])
                        (keep (fn [participant]
                                (when (= player-no (:player-no participant))
                                  (:tokens participant))))
                        first)
                   0)]
    (give-choice game {:player-no player-no
                       :text      (str "Add a token to Sinister Plot, or remove your tokens for +" tokens " Card" (when (not= 1 tokens) "s") ".")
                       :choice    ::sinister-plot-choice
                       :options   [:special
                                   {:option :add-token :text "Add a token."}
                                   {:option :remove-tokens :text (str "Remove tokens, +" tokens " Card" (when (not= 1 tokens) "s"))}]
                       :min       1
                       :max       1})))

(effects/register {::sinister-plot-choice      sinister-plot-choice
                   ::sinister-plot-give-choice sinister-plot-give-choice})

(def sinister-plot {:name    :sinister-plot
                    :set     :renaissance
                    :type    :project
                    :cost    4
                    :trigger {:trigger :at-start-turn
                              :mode    :manual
                              :effects [[::sinister-plot-give-choice]]}})

(defn- star-chart-on-shuffle [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (if card-name
                                        [[:move-card {:card-name card-name
                                                      :from      :discard
                                                      :to        :star-chart}]
                                         [:do-shuffle]
                                         [:move-card {:card-name   card-name
                                                      :from        :star-chart
                                                      :to          :deck
                                                      :to-position :top}]]
                                        [[:do-shuffle]])}))

(effects/register {::star-chart-on-shuffle star-chart-on-shuffle})

(def star-chart {:name    :star-chart
                 :set     :renaissance
                 :type    :project
                 :cost    3
                 :trigger {:trigger :on-shuffle
                           :effects [[:put-deck-into-discard]
                                     [:give-choice {:text    "You may pick one card to go on top of your deck."
                                                    :choice  ::star-chart-on-shuffle
                                                    :options [:player :discard]
                                                    :max     1}]]}})

(def projects [academy
               barracks
               canal
               capitalism
               cathedral
               citadel
               city-gate
               crop-rotation
               exploration
               fair
               fleet
               guildhall
               innovation
               pageant
               piazza
               road-network
               sewers
               silos
               sinister-plot
               star-chart])
