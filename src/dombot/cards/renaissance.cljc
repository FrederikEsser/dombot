(ns dombot.cards.renaissance
  (:require [dombot.operations :refer [push-effect-stack give-choice draw move-cards card-effect]]
            [dombot.cards.common :refer [reveal-hand reveal-from-deck add-trigger give-coins give-coffers give-villagers]]
            [dombot.utils :as ut]
            [dombot.effects :as effects])
  (:refer-clojure :exclude [key]))

(defn take-artifact [game {:keys [player-no artifact-name]}]
  (let [{:keys [owner trigger]} (get-in game [:artifacts artifact-name])]
    (cond-> game
            (not= player-no owner) (-> (assoc-in [:artifacts artifact-name :owner] player-no)
                                       (cond->
                                         trigger (update-in [:players player-no :triggers] concat [(assoc trigger :duration artifact-name)])
                                         owner (-> (update-in [:players owner :triggers] (partial remove (comp #{artifact-name} :duration)))
                                                   (update-in [:players owner] ut/dissoc-if-empty :triggers)))))))

(effects/register {::take-artifact take-artifact})



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
           :trigger {:trigger :at-clean-up
                     :effects [[::horn-at-clean-up]]}})

(def lantern {:name :lantern})

(defn border-guard-choice [game {:keys [player-no choice]}]
  (case choice
    :horn (take-artifact game {:player-no player-no :artifact-name :horn})
    :lantern (take-artifact game {:player-no player-no :artifact-name :lantern})))

(defn- border-guard-number-of-revealed-cards [game player-no]
  (if (= player-no (get-in game [:artifacts :lantern :owner])) 3 2))

(defn border-guard-take-revealed [game {:keys [player-no] :as args}]
  (let [revealed (get-in game [:players player-no :revealed])
        may-take-artifact? (and (= (border-guard-number-of-revealed-cards game player-no)
                                   (count revealed))
                                (every? :action (map :types revealed)))]
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
                   :setup   [[::add-artifact {:artifact-name :horn}]
                             [::add-artifact {:artifact-name :lantern}]]})

(def cargo-ship-trigger {:trigger  :on-gain
                         :duration :once
                         :effects  [[::cargo-ship-give-choice]]})

(defn cargo-ship-set-aside [game {:keys [player-no card-id card-name gained-card-id from]}]
  (if card-name
    (let [{:keys [card idx]} (ut/get-card-idx game [:players player-no from] {:id gained-card-id})]
      (-> game
          (update-in [:players player-no from] ut/vec-remove idx)
          (ut/update-in-vec [:players player-no :play-area] {:id card-id}
                            (fn [cargo-ship]
                              (-> cargo-ship
                                  (update :set-aside concat [card])
                                  (update :at-start-turn concat [[[:put-set-aside-into-hand {:card-name card-name}]]]))))))
    (add-trigger game {:player-no player-no
                       :card-id   card-id
                       :trigger   cargo-ship-trigger})))

(defn cargo-ship-give-choice [game {:keys [player-no card-id gained-card-id card-name from]}]
  (let [card (ut/get-card-idx game [:players player-no from] {:id gained-card-id})]
    (if card
      (give-choice game {:player-no player-no
                         :card-id   card-id
                         :text      (str "You may set the gained " (ut/format-name card-name) " aside on Cargo Ship.")
                         :choice    [::cargo-ship-set-aside {:gained-card-id gained-card-id :from from}]
                         :options   [:player from {:id gained-card-id}]
                         :max       1})
      (add-trigger game {:player-no player-no
                         :card-id   card-id
                         :trigger   cargo-ship-trigger}))))

(effects/register {::cargo-ship-set-aside   cargo-ship-set-aside
                   ::cargo-ship-give-choice cargo-ship-give-choice})

(def cargo-ship {:name    :cargo-ship
                 :set     :renaissance
                 :types   #{:action :duration}
                 :cost    3
                 :effects [[:give-coins 2]
                           [:add-trigger {:trigger cargo-ship-trigger}]]})

(def ducat {:name       :ducat
            :set        :renaissance
            :types      #{:treasure}
            :cost       2
            :coin-value 0
            :effects    [[:give-coffers 1]
                         [:give-buys 1]]
            :on-gain    [[:give-choice {:text    "You may trash a Copper from your hand."
                                        :choice  :trash-from-hand
                                        :options [:player :hand {:name :copper}]
                                        :max     1}]]})

(def experiment {:name    :experiment
                 :set     :renaissance
                 :types   #{:action}
                 :cost    3
                 :effects [[:draw 2]
                           [:give-actions 1]
                           [:return-this-to-supply]]
                 :on-gain [[:do-gain {:card-name :experiment}]]}) ; todo: Handle other on-gain effects

(def flag {:name    :flag
           :trigger {:trigger :at-draw-hand
                     :effects [[:draw 1]]}})

(def flag-bearer {:name     :flag-bearer
                  :set      :renaissance
                  :types    #{:action}
                  :cost     4
                  :effects  [[:give-coins 2]]
                  :on-gain  [[::take-artifact {:artifact-name :flag}]]
                  :on-trash [[::take-artifact {:artifact-name :flag}]]
                  :setup    [[::add-artifact {:artifact-name :flag}]]})

(defn hideout-trash [game {:keys [player-no card-name] :as args}]
  (let [{{:keys [types]} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
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
  (ut/update-in-vec game [:players player-no :play-area] {:id card-id}
                    assoc :at-clean-up [[::improve-give-choice]]))

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

(defn researcher-set-aside [game {:keys [player-no card-id]}]
  (let [set-aside (get-in game [:players player-no :set-aside])]
    (-> game
        (cond-> (not-empty set-aside) (ut/update-in-vec [:players player-no :play-area] {:id card-id}
                                                        (fn [researcher]
                                                          (-> researcher
                                                              (update :set-aside concat set-aside)
                                                              (update :at-start-turn concat [(for [card-name (map :name set-aside)]
                                                                                               [:put-set-aside-into-hand {:card-name card-name}])])))))
        (update-in [:players player-no] dissoc :set-aside))))

(defn researcher-trash [game {:keys [player-no card-name] :as args}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (push-effect-stack game (merge args {:effects [[:trash-from-hand {:card-name card-name}]
                                                   [:set-aside {:number-of-cards cost}]
                                                   [::researcher-set-aside]]}))))

(effects/register {::researcher-set-aside researcher-set-aside
                   ::researcher-trash     researcher-trash})

(def researcher {:name    :researcher
                 :set     :renaissance
                 :types   #{:action :duration}
                 :cost    4
                 :effects [[:give-actions 1]
                           [:give-choice {:text    "Trash a card from your hand."
                                          :choice  ::researcher-trash
                                          :options [:player :hand]
                                          :min     1
                                          :max     1}]]})

(defn- scepter-replay [game {:keys [player-no card-name]}]
  (let [played-card-ids (set (get-in game [:players player-no :actions-played]))
        {:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:name card-name
                                                                              :id   played-card-ids})]
    (card-effect game {:player-no player-no
                       :card      card})))

(defn- scepter-choice [game {:keys [player-no choice]}]
  (let [played-card-ids (set (get-in game [:players player-no :actions-played]))]
    (case choice
      :coins (give-coins game {:player-no player-no :arg 2})
      :replay-action (give-choice game {:player-no player-no
                                        :text      "Replay an Action card you played this turn."
                                        :choice    ::scepter-replay
                                        :options   [:player :play-area {:type :action
                                                                        :ids  played-card-ids}]
                                        :min       1
                                        :max       1}))))

(effects/register {::scepter-replay scepter-replay
                   ::scepter-choice scepter-choice})

(def scepter {:name       :scepter
              :set        :renaissance
              :types      #{:treasure}
              :cost       5
              :coin-value 0
              :effects    [[:give-choice {:text    "Choose one:"
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
  (let [{{:keys [types]} :card} (ut/get-pile-idx game card-name)]
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

(defn seer-put-in-hand [game {:keys [player-no] :as args}]
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
                     :trigger {:trigger :at-start-buy
                               :effects [[:gain {:card-name :gold}]]}})

(defn- swashbuckler-check-discard [game {:keys [player-no]}]
  (let [{:keys [discard coffers]} (get-in game [:players player-no])]
    (cond-> game
            (not-empty discard) (push-effect-stack {:player-no player-no
                                                    :effects   (concat
                                                                 [[:give-coffers 1]]
                                                                 (when (and coffers (<= 3 coffers))
                                                                   [[::take-artifact {:artifact-name :treasure-chest}]]))}))))

(effects/register {::swashbuckler-check-discard swashbuckler-check-discard})

(def swashbuckler {:name    :swashbuckler
                   :set     :renaissance
                   :types   #{:action}
                   :cost    5
                   :effects [[:draw 3]
                             [::swashbuckler-check-discard]]
                   :setup   [[::add-artifact {:artifact-name :treasure-chest}]]})

(def key {:name    :key
          :trigger {:trigger :at-start-turn
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
                :setup   [[::add-artifact {:artifact-name :key}]]})

(defn villain-attack [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])
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
                    researcher
                    scepter
                    scholar
                    sculptor
                    seer
                    silk-merchant
                    spices
                    swashbuckler
                    treasurer
                    villain])

(def artifacts {:flag           flag
                :horn           horn
                :key            key
                :lantern        lantern
                :treasure-chest treasure-chest})

(defn add-artifact [game {:keys [artifact-name]}]
  (assoc-in game [:artifacts artifact-name] (get artifacts artifact-name)))

(effects/register {::add-artifact add-artifact})

(defn academy-on-gain [game {:keys [player-no gained-card-id from]}]
  (let [{{:keys [types]} :card} (ut/get-card-idx game [:players player-no from] {:id gained-card-id})]
    (cond-> game
            (:action types) (give-villagers {:player-no player-no :arg 1}))))

(effects/register {::academy-on-gain academy-on-gain})

(def academy {:name    :academy
              :set     :renaissance
              :type    :project
              :cost    5
              :trigger {:trigger :on-gain
                        :effects [[::academy-on-gain]]}})

(def barracks {:name    :barracks
               :set     :renaissance
               :type    :project
               :cost    6
               :trigger {:trigger :at-start-turn
                         :effects [[:give-actions 1]]}})

(def cathedral {:name    :cathedral
                :set     :renaissance
                :type    :project
                :cost    3
                :trigger {:trigger :at-start-turn
                          :effects [[:give-choice {:text    "Trash a card from your hand."
                                                   :choice  :trash-from-hand
                                                   :options [:player :hand]
                                                   :min     1
                                                   :max     1}]]}})

(def city-gate {:name    :city-gate
                :set     :renaissance
                :type    :project
                :cost    3
                :trigger {:trigger :at-start-turn
                          :effects [[:draw 1]
                                    [:give-choice {:text    "Put a card from your hand onto your deck."
                                                   :choice  :topdeck-from-hand
                                                   :options [:player :hand]
                                                   :min     1
                                                   :max     1}]]}})

(def fair {:name    :fair
           :set     :renaissance
           :type    :project
           :cost    4
           :trigger {:trigger :at-start-turn
                     :effects [[:give-buys 1]]}})

(defn guildhall-on-gain [game {:keys [player-no gained-card-id from]}]
  (let [{{:keys [types]} :card} (ut/get-card-idx game [:players player-no from] {:id gained-card-id})]
    (cond-> game
            (:treasure types) (give-coffers {:player-no player-no :arg 1}))))

(effects/register {::guildhall-on-gain guildhall-on-gain})

(def guildhall {:name    :guildhall
                :set     :renaissance
                :type    :project
                :cost    5
                :trigger {:trigger :on-gain
                          :effects [[::guildhall-on-gain]]}})

(def projects [academy
               barracks
               cathedral
               city-gate
               fair
               guildhall])

