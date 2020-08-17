(ns dombot.cards.dark-ages
  (:require [dombot.operations :refer [push-effect-stack give-choice gain]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]
            [clojure.set :refer [intersection]]))

(defn- hovel-on-buy [game {:keys [player-no card]}]
  (let [types (ut/get-types game card)]
    (cond-> game
            (:victory types) (give-choice {:player-no player-no
                                           :text      "You may trash a Hovel from your hand."
                                           :choice    :trash-from-hand
                                           :options   [:player :hand {:name :hovel}]
                                           :max       1}))))

(effects/register {::hovel-on-buy hovel-on-buy})

(def hovel {:name     :hovel
            :set      :dark-ages
            :types    #{:reaction :shelter}
            :cost     1
            :reaction {:on-buy [[::hovel-on-buy]]}})

(def necropolis {:name    :necropolis
                 :set     :dark-ages
                 :types   #{:action :shelter}
                 :cost    1
                 :effects [[:give-actions 2]]})

(def overgrown-estate {:name     :overgrown-estate
                       :set      :dark-ages
                       :types    #{:victory :shelter}
                       :cost     1
                       :on-trash [[:draw 1]]})

(def shelters [hovel
               necropolis
               overgrown-estate])

(def abandoned-mine {:name    :abandoned-mine
                     :set     :dark-ages
                     :types   #{:action :ruins}
                     :cost    0
                     :effects [[:give-coins 1]]})

(def ruined-library {:name    :ruined-library
                     :set     :dark-ages
                     :types   #{:action :ruins}
                     :cost    0
                     :effects [[:draw 1]]})

(def ruined-market {:name    :ruined-market
                    :set     :dark-ages
                    :types   #{:action :ruins}
                    :cost    0
                    :effects [[:give-buys 1]]})

(def ruined-village {:name    :ruined-village
                     :set     :dark-ages
                     :types   #{:action :ruins}
                     :cost    0
                     :effects [[:give-actions 1]]})

(defn- survivors-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :discard [[:discard-all-look-at]]
                                        :topdeck [[:give-choice {:text    "Put them back in any order."
                                                                 :choice  :topdeck-from-look-at
                                                                 :options [:player :look-at]
                                                                 :min     2
                                                                 :max     2}]])}))

(defn- survivors-look [game {:keys [player-no]}]
  (let [look-at (get-in game [:players player-no :look-at])]
    (cond-> game
            (not-empty look-at) (push-effect-stack {:player-no player-no
                                                    :effects   [[:give-choice {:text    "Look at the top 2 cards of your deck."
                                                                               :choice  ::survivors-choice
                                                                               :options [:special
                                                                                         {:option :discard :text "Discard them"}
                                                                                         {:option :topdeck :text "Put them back"}]
                                                                               :min     1
                                                                               :max     1}]]}))))

(effects/register {::survivors-choice survivors-choice
                   ::survivors-look   survivors-look})

(def survivors {:name    :survivors
                :set     :dark-ages
                :types   #{:action :ruins}
                :cost    0
                :effects [[:look-at 2]
                          [::survivors-look]]})

(def ruins [abandoned-mine
            ruined-library
            ruined-market
            ruined-village
            survivors])

(defn- setup-ruins [{:keys [players] :as game} _]
  (if (ut/get-pile-idx game :supply :ruins #{:include-empty-split-piles})
    game
    (let [split-pile (->> ruins
                          (mapcat (partial repeat 10))
                          (map (fn [ruin]
                                 {:card ruin :pile-size 1}))
                          shuffle
                          (take (* 10 (dec (count players)))))
          ruins-pile {:split-pile (vec (concat split-pile
                                               [{:card {:name  :ruins
                                                        :types #{:action :ruins}
                                                        :cost  0}}]))
                      :hidden?    true}]
      (update game :supply (fn insert [supply]
                             (vec (concat (take-while (comp not #{:copper} :name :card) supply)
                                          [ruins-pile]
                                          (drop-while (comp not #{:copper} :name :card) supply))))))))

(defn- gain-ruins [game {:keys [player-no]}]
  (let [{:keys [idx]} (ut/get-pile-idx game :supply :ruins #{:include-empty-split-piles})
        {:keys [card pile-size]} (-> (get-in game [:supply idx])
                                     ut/access-top-card)]
    (cond-> game
            (pos? pile-size) (gain {:player-no player-no
                                    :card-name (:name card)}))))

(effects/register {::setup-ruins setup-ruins
                   ::gain-ruins  gain-ruins})

(def spoils {:name       :spoils
             :set        :dark-ages
             :types      #{:treasure}
             :cost       0
             :coin-value 3
             :effects    [[:return-this-to-supply {:area :extra-cards}]]})

(def altar {:name    :altar
            :set     :dark-ages
            :types   #{:action}
            :cost    6
            :effects [[:give-choice {:text    "Trash a card from your hand."
                                     :choice  :trash-from-hand
                                     :options [:player :hand]
                                     :min     1
                                     :max     1}]
                      [:give-choice {:text    "Gain a card costing up to $5."
                                     :choice  :gain
                                     :options [:supply {:max-cost 5}]
                                     :min     1
                                     :max     1}]]})

(def armory {:name    :armory
             :set     :dark-ages
             :types   #{:action}
             :cost    4
             :effects [[:give-choice {:text    "Gain a card onto your deck costing up to $4."
                                      :choice  :gain-to-topdeck
                                      :options [:supply {:max-cost 4}]
                                      :min     1
                                      :max     1}]]})

(def bandit-camp {:name    :bandit-camp
                  :set     :dark-ages
                  :types   #{:action}
                  :cost    5
                  :effects [[:draw 1]
                            [:give-actions 2]
                            [:gain {:card-name :spoils :from :extra-cards}]]
                  :setup   [[:setup-extra-cards {:extra-cards [{:card spoils :pile-size 15}]}]]})

(def beggar {:name      :beggar
             :set       :dark-ages
             :types     #{:action :reaction}
             :cost      2
             :effects   [[:gain-to-hand {:card-name :copper}]
                         [:gain-to-hand {:card-name :copper}]
                         [:gain-to-hand {:card-name :copper}]]
             :reacts-to :attack
             :reaction  [[:discard-from-hand {:card-name :beggar}]
                         [:gain-to-topdeck {:card-name :silver}]
                         [:gain {:card-name :silver}]]})

(defn- catacombs-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :take [[:take-all-from-look-at]]
                                        :discard [[:discard-all-look-at]
                                                  [:draw 3]])}))

(defn- catacombs-on-trash [game {:keys [player-no card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:trash] {:id card-id})
        cost (ut/get-cost game card)]
    (give-choice game {:player-no player-no
                       :text      (str "Gain a card cheaper than " (ut/format-cost cost) ".")
                       :choice    :gain
                       :options   [:supply {:costs-less-than cost}]
                       :min       1
                       :max       1})))

(effects/register {::catacombs-choice   catacombs-choice
                   ::catacombs-on-trash catacombs-on-trash})

(def catacombs {:name     :catacombs
                :set      :dark-ages
                :types    #{:action}
                :cost     5
                :effects  [[:look-at 3]
                           [:give-choice {:text    "Choose one:"
                                          :choice  ::catacombs-choice
                                          :options [:special
                                                    {:option :take :text "Put the cards into your hand"}
                                                    {:option :discard :text "Discard them and +3 Cards"}]
                                          :min     1
                                          :max     1}]]
                :on-trash [[::catacombs-on-trash]]})

(defn count-bad-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [(case choice
                                         :discard [:give-choice {:text    "Discard 2 cards."
                                                                 :choice  :discard-from-hand
                                                                 :options [:player :hand]
                                                                 :min     2
                                                                 :max     2}]
                                         :topdeck [:give-choice {:text    "Put a card from your hand onto your deck."
                                                                 :choice  :topdeck-from-hand
                                                                 :options [:player :hand]
                                                                 :min     1
                                                                 :max     1}]
                                         :copper [:gain {:card-name :copper}])]}))

(defn count-good-choice [game {:keys [player-no choice]}]
  (let [hand (->> (get-in game [:players player-no :hand])
                  (map :name))]
    (push-effect-stack game {:player-no player-no
                             :effects   [(case choice
                                           :coins [:give-coins 3]
                                           :trash [:trash-from-hand {:card-names hand}]
                                           :duchy [:gain {:card-name :duchy}])]})))

(effects/register {::count-bad-choice  count-bad-choice
                   ::count-good-choice count-good-choice})

(def count' {:name    :count
             :set     :dark-ages
             :types   #{:action}
             :cost    5
             :effects [[:give-choice {:text    "Choose one:"
                                      :choice  ::count-bad-choice
                                      :options [:special
                                                {:option :discard :text "Discard 2 cards"}
                                                {:option :topdeck :text "Topdeck a card"}
                                                {:option :copper :text "Gain a Copper"}]
                                      :min     1
                                      :max     1}]
                       [:give-choice {:text    "Choose one:"
                                      :choice  ::count-good-choice
                                      :options [:special
                                                {:option :coins :text "+$3"}
                                                {:option :trash :text "Trash your hand"}
                                                {:option :duchy :text "Gain a Duchy"}]
                                      :min     1
                                      :max     1}]]})

(defn- counterfeit-treasure [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:repeat-action {:card-name card-name
                                                                     :times     2}]
                                                    [:trash-from-play-area {:card-name card-name}]]})))

(effects/register {::counterfeit-treasure counterfeit-treasure})

(def counterfeit {:name            :counterfeit
                  :set             :dark-ages
                  :types           #{:treasure}
                  :cost            5
                  :coin-value      1
                  :effects         [[:give-buys 1]
                                    [:give-choice {:text    "You may play a Treasure from your hand twice."
                                                   :choice  ::counterfeit-treasure
                                                   :options [:player :hand {:type :treasure}]
                                                   :max     1}]]
                  :auto-play-index -1})

(def cultist {:name     :cultist
              :set      :dark-ages
              :types    #{:action :attack :looter}
              :cost     5
              :effects  [[:draw 2]
                         [:attack {:effects [[::gain-ruins]]}]
                         [:give-choice {:text    "You may play a Cultist from your hand."
                                        :choice  :play-from-hand
                                        :options [:player :hand {:name :cultist}]
                                        :max     1}]]
              :on-trash [[:draw 3]]
              :setup    [[::setup-ruins]]})

(defn death-cart-trash [game {:keys [player-no choice]}]
  (cond-> game
          choice (push-effect-stack {:player-no player-no
                                     :effects   [[:trash-from-area {:choice choice}]
                                                 [:give-coins 5]]})))

(effects/register {::death-cart-trash death-cart-trash})

(def death-cart {:name    :death-cart
                 :set     :dark-ages
                 :types   #{:action :looter}
                 :cost    4
                 :effects [[:give-choice {:text    "You may trash this or an Action card from your hand, for $5."
                                          :choice  ::death-cart-trash
                                          :options [:mixed
                                                    [:player :hand {:type :action}]
                                                    [:player :play-area {:this true}]]
                                          :max     1}]]
                 :on-gain [[::gain-ruins]
                           [::gain-ruins]]
                 :setup   [[::setup-ruins]]})

(defn feodum-victory-points [cards]
  (let [silver-count (->> cards
                          (filter (comp #{:silver} :name))
                          count)]
    {:victory-points (quot silver-count 3)
     :notes          (str silver-count " Silver" (when (not= 1 silver-count) "s"))}))

(effects/register {::feodum-victory-points feodum-victory-points})

(def feodum {:name           :feodum
             :set            :dark-ages
             :types          #{:victory}
             :cost           4
             :victory-points ::feodum-victory-points
             :on-trash       [[:gain {:card-name :silver}]
                              [:gain {:card-name :silver}]
                              [:gain {:card-name :silver}]]})

(defn- forager-give-coins [game {:keys [player-no]}]
  (let [different-treasures (->> game
                                 :trash
                                 (filter (comp :treasure (partial ut/get-types game)))
                                 (map :name)
                                 distinct
                                 count)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:give-coins different-treasures]]})))

(effects/register {::forager-give-coins forager-give-coins})

(def forager {:name    :forager
              :set     :dark-ages
              :types   #{:action}
              :cost    3
              :effects [[:give-actions 1]
                        [:give-buys 1]
                        [:give-choice {:text    "Trash a card from your hand."
                                       :choice  :trash-from-hand
                                       :options [:player :hand]
                                       :min     1
                                       :max     1}]
                        [::forager-give-coins]]})

(defn- fortress-trashed [game {:keys [player-no card-id]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:move-card {:move-card-id card-id
                                                    :from         :trash
                                                    :to           :hand}]]}))

(effects/register {::fortress-trashed fortress-trashed})

(def fortress {:name     :fortress
               :set      :dark-ages
               :types    #{:action}
               :cost     4
               :effects  [[:draw 1]
                          [:give-actions 2]]
               :on-trash [[::fortress-trashed]]})

(defn- graverobber-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :gain [[:give-choice {:text    "Gain a card from the trash costing from $3 to $6, onto your deck."
                                                              :choice  [:gain-from-trash {:to          :deck
                                                                                          :to-position :top}]
                                                              :options [:trash {:min-cost 3
                                                                                :max-cost 6}]
                                                              :min     1
                                                              :max     1}]]
                                        :upgrade [[:give-choice {:text    "Trash an Action card from your hand and gain a card costing up to $3 more than it."
                                                                 :choice  [:trash-and-gain {:extra-cost 3}]
                                                                 :options [:player :hand {:type :action}]
                                                                 :min     1
                                                                 :max     1}]])}))

(effects/register {::graverobber-choice graverobber-choice})

(def graverobber {:name    :graverobber
                  :set     :dark-ages
                  :types   #{:action}
                  :cost    5
                  :effects [[:give-choice {:text    (str "Choose one: Gain a card from the trash costing from $3 to $6, onto your deck; "
                                                         "or trash an Action card from your hand and gain a card costing up to $3 more than it.")
                                           :choice  ::graverobber-choice
                                           :options [:special
                                                     {:option :gain :text "Gain from Trash"}
                                                     {:option :upgrade :text "Trash an Action"}]
                                           :min     1
                                           :max     1}]]})

(defn- madman-return [game {:keys [player-no card-id]}]
  (let [madman-in-play (ut/get-card-idx game [:players player-no :play-area] {:id card-id})
        cards-in-hand  (->> (get-in game [:players player-no :hand])
                            count)]
    (cond-> game
            madman-in-play (push-effect-stack {:player-no player-no
                                               :effects   [[:return-this-to-supply {:card-id card-id
                                                                                    :area    :extra-cards}]
                                                           [:draw cards-in-hand]]}))))

(effects/register {::madman-return madman-return})

(def madman {:name    :madman
             :set     :dark-ages
             :types   #{:action}
             :cost    0
             :effects [[:give-actions 2]
                       [::madman-return]]})

(defn- hermit-trash [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:trash-from-play-area {:card-name :hermit}]
                                                    [:gain {:card-name :madman
                                                            :from      :extra-cards}]]})))

(defn bought-no-cards? [game player-no]
  (->> (get-in game [:players player-no :gained-cards])
       (filter :bought)
       empty?))

(effects/register {::hermit-trash     hermit-trash
                   ::bought-no-cards? bought-no-cards?})

(def hermit {:name              :hermit
             :set               :dark-ages
             :types             #{:action}
             :cost              3
             :effects           [[:give-choice {:text    "You may trash a non-Treasure card from your discard pile or hand."
                                                :choice  :trash-from-area
                                                :options [:mixed
                                                          [:player :hand {:not-type :treasure}]
                                                          [:player :discard {:not-type :treasure}]]
                                                :max     1}]
                                 [:give-choice {:text    "Gain a card costing up to $3."
                                                :choice  :gain
                                                :options [:supply {:max-cost 3}]
                                                :min     1
                                                :max     1}]]
             :trigger-condition ::bought-no-cards?
             :trigger-mode      :auto
             :at-clean-up       [[:trash-this]
                                 [:gain {:card-name :madman :from :extra-cards}]]
             :setup             [[:setup-extra-cards {:extra-cards [{:card madman :pile-size 10}]}]]})

(defn- hunting-grounds-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :duchy [[:gain {:card-name :duchy}]]
                                        :estates [[:gain {:card-name :estate}]
                                                  [:gain {:card-name :estate}]
                                                  [:gain {:card-name :estate}]])}))

(defn- hunting-grounds-gain [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case card-name
                                        :duchy [[:gain {:card-name :duchy}]]
                                        :estate (repeat 3 [:gain {:card-name :estate}]))}))



(effects/register {::hunting-grounds-gain   hunting-grounds-gain
                   ::hunting-grounds-choice hunting-grounds-choice})

(def hunting-grounds {:name     :hunting-grounds
                      :set      :dark-ages
                      :types    #{:action}
                      :cost     6
                      :effects  [[:draw 4]]
                      :on-trash [[:give-choice {:text    "Gain a Duchy or 3 Estates."
                                                :choice  ::hunting-grounds-gain
                                                :options [:supply {:names #{:duchy :estate}
                                                                   :all   true}]
                                                :min     1
                                                :max     1}]]})

(defn- ironmonger-check-revealed [game {:keys [player-no]}]
  (let [{:keys [name] :as card} (last (get-in game [:players player-no :revealed]))
        types (ut/get-types game card)]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:give-choice {:text    (str "You may discard the revealed " (ut/format-name name) ".")
                                                                :choice  :discard-from-revealed
                                                                :options [:player :revealed]
                                                                :max     1}]
                                                 [:topdeck-all-revealed]
                                                 (when (:action types)
                                                   [:give-actions 1])
                                                 (when (:treasure types)
                                                   [:give-coins 1])
                                                 (when (:victory types)
                                                   [:draw 1])]}))))

(effects/register {::ironmonger-check-revealed ironmonger-check-revealed})

(def ironmonger {:name    :ironmonger
                 :set     :dark-ages
                 :types   #{:action}
                 :cost    4
                 :effects [[:draw 1]
                           [:give-actions 1]
                           [:reveal-from-deck 1]
                           [::ironmonger-check-revealed]]})

(def junk-dealer {:name    :junk-dealer
                  :set     :dark-ages
                  :types   #{:action}
                  :cost    5
                  :effects [[:draw 1]
                            [:give-actions 1]
                            [:give-coins 1]
                            [:give-choice {:text    "Trash a card from your hand."
                                           :choice  :trash-from-hand
                                           :options [:player :hand]
                                           :min     1
                                           :max     1}]]})

(defn- knight-trash [game {:keys [player-no card-name attacking-player-no attacking-knight-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :revealed] {:name card-name})
        types (ut/get-types game card)]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:trash-from-revealed {:card-name card-name}]]})
        (cond-> (:knight types) (push-effect-stack {:player-no attacking-player-no
                                                    :effects   [[:trash-from-play-area {:trash-card-id attacking-knight-id}]]})))))

(defn- knight-attack [game {:keys [player-no attacking-player-no card-id]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:reveal-from-deck 2]
                                       [:give-choice {:text    "Trash a revealed card costing from $3 to $6."
                                                      :choice  [::knight-trash {:attacking-player-no attacking-player-no
                                                                                :attacking-knight-id card-id}]
                                                      :options [:player :revealed {:min-cost 3
                                                                                   :max-cost 6}]
                                                      :min     1
                                                      :max     1}]
                                       [:discard-all-revealed]]}))

(def dame-anna {:name    :dame-anna
                :set     :dark-ages
                :types   #{:action :attack :knight}
                :cost    5
                :effects [[:give-choice {:text    "You may trash up to 2 cards from your hand."
                                         :choice  :trash-from-hand
                                         :options [:player :hand]
                                         :max     2}]
                          [:attack {:effects [[::knight-attack]]}]]})

(def dame-josephine {:name           :dame-josephine
                     :set            :dark-ages
                     :types          #{:action :attack :knight :victory}
                     :victory-points 2
                     :cost           5
                     :effects        [[:attack {:effects [[::knight-attack]]}]]})

(def dame-molly {:name    :dame-molly
                 :set     :dark-ages
                 :types   #{:action :attack :knight}
                 :cost    5
                 :effects [[:give-actions 2]
                           [:attack {:effects [[::knight-attack]]}]]})

(def dame-natalie {:name    :dame-natalie
                   :set     :dark-ages
                   :types   #{:action :attack :knight}
                   :cost    5
                   :effects [[:give-choice {:text    "You may gain a card costing up to $3."
                                            :choice  :gain
                                            :options [:supply {:max-cost 3}]
                                            :max     1}]
                             [:attack {:effects [[::knight-attack]]}]]})

(def dame-sylvia {:name    :dame-sylvia
                  :set     :dark-ages
                  :types   #{:action :attack :knight}
                  :cost    5
                  :effects [[:give-coins 2]
                            [:attack {:effects [[::knight-attack]]}]]})

(def sir-bailey {:name    :sir-bailey
                 :set     :dark-ages
                 :types   #{:action :attack :knight}
                 :cost    5
                 :effects [[:draw 1]
                           [:give-actions 1]
                           [:attack {:effects [[::knight-attack]]}]]})

(def sir-destry {:name    :sir-destry
                 :set     :dark-ages
                 :types   #{:action :attack :knight}
                 :cost    5
                 :effects [[:draw 2]
                           [:attack {:effects [[::knight-attack]]}]]})

(def sir-martin {:name    :sir-martin
                 :set     :dark-ages
                 :types   #{:action :attack :knight}
                 :cost    4
                 :effects [[:give-buys 2]
                           [:attack {:effects [[::knight-attack]]}]]})

(def sir-michael {:name    :sir-michael
                  :set     :dark-ages
                  :types   #{:action :attack :knight}
                  :cost    5
                  :effects [[:attack {:effects [[:discard-down-to 3]]}]
                            [:attack {:effects [[::knight-attack]]}]]})

(def sir-vander {:name     :sir-vander
                 :set      :dark-ages
                 :types    #{:action :attack :knight}
                 :cost     5
                 :effects  [[:attack {:effects [[::knight-attack]]}]]
                 :on-trash [[:gain {:card-name :gold}]]})

(def knights {:name       :knights
              :set        :dark-ages
              :types      #{:action :attack :knight}
              :cost       5
              :split-pile ::knights-pile})

(defn knights-pile [_]
  {:split-pile (-> (shuffle [{:card dame-anna :pile-size 1}
                             {:card dame-josephine :pile-size 1}
                             {:card dame-molly :pile-size 1}
                             {:card dame-natalie :pile-size 1}
                             {:card dame-sylvia :pile-size 1}
                             {:card sir-bailey :pile-size 1}
                             {:card sir-destry :pile-size 1}
                             {:card sir-martin :pile-size 1}
                             {:card sir-michael :pile-size 1}
                             {:card sir-vander :pile-size 1}])
                   (concat [{:card knights}])
                   vec)
   :hidden?    true})

(effects/register {::knight-trash  knight-trash
                   ::knight-attack knight-attack
                   ::knights-pile  knights-pile})

(def marauder {:name    :marauder
               :set     :dark-ages
               :types   #{:action :attack :looter}
               :cost    4
               :effects [[:gain {:card-name :spoils :from :extra-cards}]
                         [:attack {:effects [[::gain-ruins]]}]]
               :setup   [[:setup-extra-cards {:extra-cards [{:card spoils :pile-size 15}]}]
                         [::setup-ruins]]})

(defn- market-square-discard [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name :market-square}]
                                                    [:gain {:card-name :gold}]]})))

(effects/register {::market-square-discard market-square-discard})

(def market-square {:name     :market-square
                    :set      :dark-ages
                    :types    #{:action :reaction}
                    :cost     3
                    :effects  [[:draw 1]
                               [:give-actions 1]
                               [:give-buys 1]]
                    :reaction {:on-trash [[:give-choice {:text    "You may discard a Market Square to gain a Gold."
                                                         :choice  ::market-square-discard
                                                         :options [:player :hand {:name :market-square}]
                                                         :max     1}]]}})

(def mystic {:name    :mystic
             :set     :dark-ages
             :types   #{:action}
             :cost    5
             :effects [[:give-actions 1]
                       [:give-coins 2]
                       [:peek-deck 1]
                       [:name-a-card {:effect :draw-named-card}]]})

(defn pillage-attack [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (<= 5 (count hand)) (push-effect-stack {:player-no player-no
                                                    :effects   [[:reveal-hand]
                                                                [:give-choice {:text    "Discard a card (attacker chooses)."
                                                                               :choice  :discard-from-hand
                                                                               :options [:player :hand]
                                                                               :min     1
                                                                               :max     1}]]}))))

(effects/register {::pillage-attack pillage-attack})

(def pillage {:name    :pillage
              :set     :dark-ages
              :types   #{:action :attack}
              :cost    5
              :effects [[:trash-this]
                        [:attack {:effects [[::pillage-attack]]}]
                        [:gain {:card-name :spoils :from :extra-cards}]
                        [:gain {:card-name :spoils :from :extra-cards}]]
              :setup   [[:setup-extra-cards {:extra-cards [{:card spoils :pile-size 15}]}]]})

(defn- poor-house-lose-coins [game {:keys [player-no]}]
  (let [{:keys [coins hand]} (get-in game [:players player-no])
        treasures-in-hand (->> hand
                               (filter (comp :treasure (partial ut/get-types game)))
                               count)]
    (assoc-in game [:players player-no :coins] (max 0 (- coins treasures-in-hand)))))

(effects/register {::poor-house-lose-coins poor-house-lose-coins})

(def poor-house {:name    :poor-house
                 :set     :dark-ages
                 :types   #{:action}
                 :cost    1
                 :effects [[:give-coins 4]
                           [:reveal-hand]
                           [::poor-house-lose-coins]]})

(defn- rats-trash [game {:keys [player-no]}]
  (let [all-rats? (->> (get-in game [:players player-no :hand])
                       (every? (comp #{:rats} :name)))]
    (push-effect-stack game {:player-no player-no
                             :effects   (if all-rats?
                                          [[:reveal-hand]]
                                          [[:give-choice {:text    "Trash a card from your hand other than a Rats."
                                                          :choice  :trash-from-hand
                                                          :options [:player :hand {:not-names #{:rats}}]
                                                          :min     1
                                                          :max     1}]])})))

(defn rats-20 [game _]
  (let [{:keys [idx]} (ut/get-pile-idx game :rats)]
    (assoc-in game [:supply idx :pile-size] 20)))

(effects/register {::rats-trash rats-trash
                   ::rats-20    rats-20})

(def rats {:name     :rats
           :set      :dark-ages
           :types    #{:action}
           :cost     4
           :effects  [[:draw 1]
                      [:give-actions 1]
                      [:gain {:card-name :rats}]
                      [::rats-trash]]
           :on-trash [[:draw 1]]
           :setup    [[::rats-20]]})

(defn- rebuild-revealed [game {:keys [player-no card-name]}]
  (let [{:keys [revealed deck discard]} (get-in game [:players player-no])
        {:keys [name] :as card} (last revealed)
        types    (ut/get-types game card)
        max-cost (-> (ut/get-cost game card)
                     (ut/add-to-cost 3))]
    (cond (and (:victory types)
               (not= name card-name)) (push-effect-stack game {:player-no player-no
                                                               :effects   [[:trash-from-revealed {:card-name name}]
                                                                           [:discard-all-revealed]
                                                                           [:give-choice {:text    (str "Gain a Victory costing up to " (ut/format-cost max-cost) ".")
                                                                                          :choice  :gain
                                                                                          :options [:supply {:type     :victory
                                                                                                             :max-cost max-cost}]
                                                                                          :min     1
                                                                                          :max     1}]]})
          (not-empty (concat deck discard)) (push-effect-stack game {:player-no player-no
                                                                     :effects   [[:reveal-from-deck 1]
                                                                                 [::rebuild-revealed {:card-name card-name}]]})
          :else game)))

(defn- rebuild-reveal [game {:keys [player-no choice]}]
  (let [{:keys [card-name]} choice]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:reveal-from-deck 1]
                                         [::rebuild-revealed {:card-name card-name}]
                                         [:discard-all-revealed]]})))

(effects/register {::rebuild-revealed rebuild-revealed
                   ::rebuild-reveal   rebuild-reveal})

(def rebuild {:name    :rebuild
              :set     :dark-ages
              :types   #{:action}
              :cost    5
              :effects [[:give-actions 1]
                        [:peek-deck 1]
                        [:name-a-card {:effect ::rebuild-reveal}]]})

(defn- rogue-gain-or-attack [game {:keys [player-no]}]
  (let [cards-in-trash? (->> game
                             :trash
                             (filter (comp (partial ut/costs-between 3 6) (partial ut/get-cost game)))
                             not-empty)]
    (push-effect-stack game {:player-no player-no
                             :effects   (if cards-in-trash?
                                          [[:give-choice {:text    "Gain a card costing from $3 to $6 from the trash."
                                                          :choice  :gain-from-trash
                                                          :options [:trash {:min-cost 3
                                                                            :max-cost 6}]
                                                          :min     1
                                                          :max     1}]]
                                          [[:attack {:effects [[:reveal-from-deck 2]
                                                               [:give-choice {:text    "Trash a revealed card costing from $3 to $6."
                                                                              :choice  :trash-from-revealed
                                                                              :options [:player :revealed {:min-cost 3
                                                                                                           :max-cost 6}]
                                                                              :min     1
                                                                              :max     1}]
                                                               [:discard-all-revealed]]}]])})))

(effects/register {::rogue-gain-or-attack rogue-gain-or-attack})

(def rogue {:name    :rogue
            :set     :dark-ages
            :types   #{:action :attack}
            :cost    5
            :effects [[:give-coins 2]
                      [::rogue-gain-or-attack]]})

(defn- sage-reveal [game {:keys [player-no]}]
  (let [{:keys [revealed deck discard]} (get-in game [:players player-no])
        {:keys [name] :as card} (last revealed)
        cost (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   (cond
                                          (ut/costs-at-least 3 cost) [[:take-from-revealed {:card-name name}]]
                                          (not-empty (concat deck discard)) [[:reveal-from-deck 1]
                                                                             [::sage-reveal]])})))

(effects/register {::sage-reveal sage-reveal})

(def sage {:name    :sage
           :set     :dark-ages
           :types   #{:action}
           :cost    3
           :effects [[:give-actions 1]
                     [:reveal-from-deck 1]
                     [::sage-reveal]
                     [:discard-all-revealed]]})

(defn- scavenger-discard [game {:keys [player-no choice]}]
  (cond-> game
          (= :yes choice) (push-effect-stack {:player-no player-no
                                              :effects   [[:put-deck-into-discard]]})))

(defn- scavenger-choice [game {:keys [player-no]}]
  (let [deck (get-in game [:players player-no :deck])]
    (cond-> game
            (not-empty deck) (give-choice {:player-no player-no
                                           :text      "You may put your deck into your discard pile."
                                           :choice    ::scavenger-discard
                                           :options   [:special
                                                       {:option :yes :text "Yes"}
                                                       {:option :no :text "No"}]
                                           :min       1
                                           :max       1}))))

(effects/register {::scavenger-discard scavenger-discard
                   ::scavenger-choice  scavenger-choice})

(def scavenger {:name    :scavenger
                :set     :dark-ages
                :types   #{:action}
                :cost    4
                :effects [[:give-coins 2]
                          [::scavenger-choice]
                          [:give-choice {:text    "Look through your discard pile and put one card from it onto your deck."
                                         :choice  :topdeck-from-discard
                                         :options [:player :discard]
                                         :min     1
                                         :max     1}]]})

(defn squire-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [(case choice
                                         :actions [:give-actions 2]
                                         :buys [:give-buys 2]
                                         :silver [:gain {:card-name :silver}])]}))

(effects/register {::squire-choice squire-choice})

(def squire {:name     :squire
             :set      :dark-ages
             :types    #{:action}
             :cost     2
             :effects  [[:give-coins 1]
                        [:give-choice {:text    "Choose one:"
                                       :choice  ::squire-choice
                                       :options [:special
                                                 {:option :actions :text "+2 Actions"}
                                                 {:option :buys :text "+2 Buys"}
                                                 {:option :silver :text "Gain a Silver"}]
                                       :min     1
                                       :max     1}]]
             :on-trash [[:give-choice {:text    "Gain an Attack card."
                                       :choice  :gain
                                       :options [:supply {:type :attack}]
                                       :min     1
                                       :max     1}]]})

(defn storeroom-sift [game {:keys [player-no card-names]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:discard-from-hand {:card-names card-names}]
                                       [:draw (count card-names)]
                                       [:give-choice {:text    "Discard any number of cards for +$1 each."
                                                      :choice  :discard-for-coins
                                                      :options [:player :hand]}]]}))

(effects/register {::storeroom-sift storeroom-sift})

(def storeroom {:name    :storeroom
                :set     :dark-ages
                :types   #{:action}
                :cost    3
                :effects [[:give-buys 1]
                          [:give-choice {:text    "Discard any number of cards, then draw that many."
                                         :choice  ::storeroom-sift
                                         :options [:player :hand]}]]})

(defn- mercenary-trash [game {:keys [player-no card-name card-names] :as args}]
  (cond-> game
          (or card-name card-names) (push-effect-stack {:player-no player-no
                                                        :effects   (concat [[:trash-from-hand args]]
                                                                           (when (= 2 (count card-names))
                                                                             [[:draw 2]
                                                                              [:give-coins 2]
                                                                              [:attack {:effects [[:discard-down-to 3]]}]]))})))

(effects/register {::mercenary-trash mercenary-trash})

(def mercenary {:name    :mercenary
                :set     :dark-ages
                :types   #{:action :attack}
                :cost    0
                :effects [[:give-choice {:text      "You may trash 2 cards from your hand"
                                         :choice    ::mercenary-trash
                                         :options   [:player :hand]
                                         :min       2
                                         :max       2
                                         :optional? true}]]})

(defn- urchin-trash [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:trash-from-play-area {:card-name :urchin}]
                                                    [:gain {:card-name :mercenary
                                                            :from      :extra-cards}]]})))

(effects/register {::urchin-trash urchin-trash})

(def urchin {:name          :urchin
             :set           :dark-ages
             :types         #{:action :attack}
             :cost          3
             :effects       [[:draw 1]
                             [:give-actions 1]
                             [:attack {:effects [[:discard-down-to 4]]}]]
             :while-in-play {:play-attack [[:give-choice {:text    "You may trash an Urchin to gain a Mercenary."
                                                          :choice  ::urchin-trash
                                                          :options [:player :play-area {:name :urchin}]
                                                          :max     1}]]}
             :setup         [[:setup-extra-cards {:extra-cards [{:card mercenary :pile-size 10}]}]]})

(defn- vagrant-check-revealed [game {:keys [player-no]}]
  (let [{:keys [name] :as card} (last (get-in game [:players player-no :revealed]))
        take-card? (->> (ut/get-types game card)
                        (intersection #{:curse :ruins :shelter :victory})
                        not-empty)]
    (cond-> game
            take-card? (push-effect-stack {:player-no player-no
                                           :effects   [[:take-from-revealed {:card-name name}]]}))))

(effects/register {::vagrant-check-revealed vagrant-check-revealed})

(def vagrant {:name    :vagrant
              :set     :dark-ages
              :types   #{:action}
              :cost    2
              :effects [[:draw 1]
                        [:give-actions 1]
                        [:reveal-from-deck 1]
                        [::vagrant-check-revealed]
                        [:topdeck-all-revealed]]})

(def wandering-minstrel {:name    :wandering-minstrel
                         :set     :dark-ages
                         :types   #{:action}
                         :cost    4
                         :effects [[:draw 1]
                                   [:give-actions 2]
                                   [:reveal-from-deck 3]
                                   [:give-choice {:text    "Put the Action cards back in any order."
                                                  :choice  :topdeck-from-revealed
                                                  :options [:player :revealed {:type :action}]
                                                  :min     3}]
                                   [:discard-all-revealed]]})

(def kingdom-cards [altar
                    armory
                    bandit-camp
                    beggar
                    catacombs
                    count'
                    counterfeit
                    cultist
                    death-cart
                    feodum
                    forager
                    fortress
                    graverobber
                    hermit
                    hunting-grounds
                    ironmonger
                    junk-dealer
                    knights
                    marauder
                    market-square
                    mystic
                    pillage
                    poor-house
                    rats
                    rebuild
                    rogue
                    sage
                    scavenger
                    squire
                    storeroom
                    urchin
                    vagrant
                    wandering-minstrel])
