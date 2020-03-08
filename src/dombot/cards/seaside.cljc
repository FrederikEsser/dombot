(ns dombot.cards.seaside
  (:require [dombot.operations :refer [move-card move-cards give-choice push-effect-stack]]
            [dombot.cards.common :refer [reveal-hand gain-to-hand discard-all-look-at trash-from-revealed set-aside=>hand-trigger]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn ambassador-reveal [game {:keys [card-name] :as args}]
  (push-effect-stack game (merge args
                                 {:effects [[:reveal {:card-name card-name}]
                                            [:give-choice {:text    "Return up to 2 copies of it to the Supply."
                                                           :choice  :return-to-supply
                                                           :options [:player :hand {:name card-name}]
                                                           :max     2}]
                                            [:attack {:effects [[:gain {:card-name card-name}]]}]]})))

(effects/register {::ambassador-reveal ambassador-reveal})

(def ambassador {:name    :ambassador
                 :set     :seaside
                 :types   #{:action :attack}
                 :cost    3
                 :effects [[:give-choice {:text    "Reveal a card from your hand."
                                          :choice  ::ambassador-reveal
                                          :options [:player :hand]
                                          :min     1
                                          :max     1}]]})

(def bazaar {:name    :bazaar
             :set     :seaside
             :types   #{:action}
             :cost    5
             :effects [[:draw 1]
                       [:give-actions 2]
                       [:give-coins 1]]})

(def caravan {:name    :caravan
              :set     :seaside
              :types   #{:action :duration}
              :cost    4
              :effects [[:draw 1]
                        [:give-actions 1]]
              :trigger {:event    :at-start-turn
                        :duration :once
                        :mode     :semi
                        :effects  [[:draw 1]]}})

(defn cutpurse-attack [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (if (some (comp #{:copper} :name) hand)
      (give-choice game {:player-no player-no
                         :text      "Discard a Copper."
                         :choice    :discard-from-hand
                         :options   [:player :hand {:name :copper}]
                         :min       1
                         :max       1})
      (-> game
          (reveal-hand {:player-no player-no})))))

(effects/register {::cutpurse-attack cutpurse-attack})

(def cutpurse {:name    :cutpurse
               :set     :seaside
               :types   #{:action :attack}
               :cost    4
               :effects [[:give-coins 2]
                         [:attack {:effects [[::cutpurse-attack]]}]]})

(defn embargo-place-token [game {:keys [card-name]}]
  (let [{:keys [idx]} (ut/get-pile-idx game card-name)]
    (-> game
        (update-in [:supply idx :tokens :embargo :number-of-tokens] ut/plus 1)
        (assoc-in [:supply idx :tokens :embargo :on-buy] [[:gain {:card-name :curse}]]))))

(effects/register {::embargo-place-token embargo-place-token})

(def embargo {:name    :embargo
              :set     :seaside
              :types   #{:action}
              :cost    2
              :effects [[:give-coins 2]
                        [:trash-this]
                        [:give-choice {:text    "Add an Embargo token to a Supply pile."
                                       :choice  ::embargo-place-token
                                       :options [:supply]
                                       :min     1
                                       :max     1}]]})

(defn explorer-choice [game {:keys [player-no card-name]}]
  (if (= :province card-name)
    (push-effect-stack game {:player-no player-no
                             :effects   [[:gain-to-hand {:card-name :gold}]
                                         [:reveal {:card-name :province}]]}) ; hack to make revealed card show af gain to hand
    (gain-to-hand game {:player-no player-no
                        :card-name :silver})))

(defn explorer-reveal-province [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (if (some (comp #{:province} :name) hand)
      (give-choice game {:player-no player-no
                         :text      "You may reveal a Province from your hand."
                         :choice    ::explorer-choice
                         :options   [:player :hand {:name :province}]
                         :max       1})
      (gain-to-hand game {:player-no player-no
                          :card-name :silver}))))

(effects/register {::explorer-reveal-province explorer-reveal-province
                   ::explorer-choice          explorer-choice})

(def explorer {:name    :explorer
               :set     :seaside
               :types   #{:action}
               :cost    5
               :effects [[::explorer-reveal-province]]})

(def fishing-village {:name    :fishing-village
                      :set     :seaside
                      :types   #{:action :duration}
                      :cost    3
                      :effects [[:give-actions 2]
                                [:give-coins 1]]
                      :trigger {:event    :at-start-turn
                                :duration :once
                                :mode     :auto
                                :effects  [[:give-actions 1]
                                           [:give-coins 1]]}})

(def ghost-ship {:name    :ghost-ship
                 :set     :seaside
                 :types   #{:action :attack}
                 :cost    5
                 :effects [[:draw 2]
                           [:attack {:effects [[:topdeck-down-to 3]]}]]})

(defn haven-set-aside [game {:keys [player-no card-id card-name]}]
  (let [{:keys [card idx]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (-> game
        (update-in [:players player-no :hand] ut/vec-remove idx)
        (push-effect-stack {:player-no player-no
                            :effects   [[:add-trigger {:trigger (merge set-aside=>hand-trigger
                                                                       {:set-aside [card]})
                                                       :card-id card-id}]]}))))

(effects/register {::haven-set-aside haven-set-aside})

(def haven {:name    :haven
            :set     :seaside
            :types   #{:action :duration}
            :cost    2
            :effects [[:draw 1]
                      [:give-actions 1]
                      [:give-choice {:text    "Set aside a card from your hand."
                                     :choice  ::haven-set-aside
                                     :options [:player :hand]
                                     :min     1
                                     :max     1}]]})

(defn island-put-island [game {:keys [player-no card-id]}]
  (let [island (ut/get-card-idx game [:players player-no :play-area] {:id card-id})]
    (cond-> game
            island (move-card {:player-no    player-no
                               :move-card-id card-id
                               :from         :play-area
                               :to           :island-mat}))))

(defn island-put [game args]
  (move-card game (merge args {:from :hand
                               :to   :island-mat})))

(effects/register {::island-put-island island-put-island
                   ::island-put        island-put})

(def island {:name           :island
             :set            :seaside
             :types          #{:action :victory}
             :cost           4
             :effects        [[::island-put-island]
                              [:give-choice {:text    "Put a card from your hand on the Island Mat."
                                             :choice  ::island-put
                                             :options [:player :hand]
                                             :min     1
                                             :max     1}]]
             :victory-points 2})

(def lighthouse {:name    :lighthouse
                 :set     :seaside
                 :types   #{:action :duration}
                 :cost    2
                 :effects [[:give-actions 1]
                           [:give-coins 1]
                           [:mark-unaffected]]
                 :trigger {:event    :at-start-turn
                           :duration :once
                           :mode     :auto
                           :effects  [[:give-coins 1]
                                      [:clear-unaffected]]}})

(def lookout {:name    :lookout
              :set     :seaside
              :types   #{:action}
              :cost    3
              :effects [[:give-actions 1]
                        [:look-at 3]
                        [:give-choice {:text    "Trash one of the top 3 cards of your deck."
                                       :choice  :trash-from-look-at
                                       :options [:player :look-at]
                                       :min     1
                                       :max     1}]
                        [:give-choice {:text    "Discard one of the top 3 cards of your deck."
                                       :choice  :discard-from-look-at
                                       :options [:player :look-at]
                                       :min     1
                                       :max     1}]
                        [:move-card {:from          :look-at
                                     :from-position :top
                                     :to            :deck
                                     :to-position   :top}]]})

(def merchant-ship {:name    :merchant-ship
                    :set     :seaside
                    :types   #{:action :duration}
                    :cost    5
                    :effects [[:give-coins 2]]
                    :trigger {:event    :at-start-turn
                              :duration :once
                              :mode     :auto
                              :effects  [[:give-coins 2]]}})

(defn native-village-choice [game {:keys [player-no choice]}]
  (let [native-village-cards (->> (get-in game [:players player-no :native-village-mat])
                                  (map :name))]
    (case choice
      :put (move-card game {:player-no     player-no
                            :from          :deck
                            :from-position :top
                            :to            :native-village-mat})
      :take (move-cards game {:player-no  player-no
                              :card-names native-village-cards
                              :from       :native-village-mat
                              :to         :hand}))))

(effects/register {::native-village-choice native-village-choice})

(def native-village {:name    :native-village
                     :set     :seaside
                     :types   #{:action}
                     :cost    2
                     :effects [[:give-actions 2]
                               [:give-choice {:text    "Choose one:"
                                              :choice  ::native-village-choice
                                              :options [:special
                                                        {:option :put :text "Put the top card of your deck face down on your Native Village mat."}
                                                        {:option :take :text "Put all the cards from your mat into your hand."}]
                                              :min     1
                                              :max     1}]]})

(defn navigator-choice [game {:keys [player-no choice]}]
  (assert choice "No choice specified for Navigator.")
  (let [look-at (get-in game [:players player-no :look-at])]
    (case choice
      :discard (discard-all-look-at game {:player-no player-no})
      :topdeck-same-order (move-cards game {:player-no   player-no
                                            :card-names  (->> look-at (map :name) reverse)
                                            :from        :look-at
                                            :to          :deck
                                            :to-position :top})
      :topdeck (give-choice game {:player-no player-no
                                  :text      "Put them back on your deck in any order."
                                  :choice    :topdeck-from-look-at
                                  :options   [:player :look-at]
                                  :min       5
                                  :max       5}))))

(effects/register {::navigator-choice navigator-choice})

(def navigator {:name    :navigator
                :set     :seaside
                :types   #{:action}
                :cost    4
                :effects [[:give-coins 2]
                          [:look-at 5]
                          [:give-choice {:text    "Look at the top 5 cards of your deck."
                                         :choice  ::navigator-choice
                                         :options [:special
                                                   {:option :discard :text "Discard them all."}
                                                   {:option :topdeck-same-order :text "Put them back on your deck in the same order."}
                                                   {:option :topdeck :text "Put them back on your deck in any order."}]
                                         :min     1
                                         :max     1}]]})

(defn outpost-extra-turn [game {:keys [player-no card-id]}]
  (push-effect-stack game {:player-no player-no
                           :card-id   card-id
                           :effects   [[:clean-up {:extra-turn?     true
                                                   :number-of-cards 3}]
                                       [:start-turn]]}))

(def outpost-trigger {:event    :at-end-turn
                      :duration :once
                      :effects  [[::outpost-extra-turn]]})

(defn outpost-give-extra-turn [game {:keys [player-no card-id]}]
  (let [{:keys [previous-turn-was-yours?]} (get-in game [:players player-no])]

    (cond-> game
            (not previous-turn-was-yours?) (-> (assoc-in [:players player-no :previous-turn-was-yours?] true)
                                               (push-effect-stack {:player-no player-no
                                                                   :effects   [[:add-trigger {:trigger outpost-trigger
                                                                                              :card-id card-id}]]})))))

(effects/register {::outpost-extra-turn      outpost-extra-turn
                   ::outpost-give-extra-turn outpost-give-extra-turn})

(def outpost {:name    :outpost
              :set     :seaside
              :types   #{:action :duration}
              :cost    5
              :effects [[::outpost-give-extra-turn]]})

(defn pearl-diver-choice [game {:keys [player-no choice]}]
  (let [[{:keys [name]}] (get-in game [:players player-no :look-at])]
    (move-card game {:player-no   player-no
                     :card-name   name
                     :from        :look-at
                     :to          :deck
                     :to-position choice})))

(defn pearl-diver-give-choice [game {:keys [player-no]}]
  (let [[{:keys [name]}] (get-in game [:players player-no :look-at])]
    (cond-> game
            name (give-choice {:player-no player-no
                               :text      (str "Choose where to put the " (ut/format-name name) ":")
                               :choice    ::pearl-diver-choice
                               :options   [:special
                                           {:option :top :text "Put it on top of your deck."}
                                           {:option :bottom :text "Leave it at the bottom of your deck."}]
                               :min       1
                               :max       1}))))

(effects/register {::pearl-diver-choice      pearl-diver-choice
                   ::pearl-diver-give-choice pearl-diver-give-choice})

(def pearl-diver {:name    :pearl-diver
                  :set     :seaside
                  :types   #{:action}
                  :cost    2
                  :effects [[:draw 1]
                            [:give-actions 1]
                            [:look-at {:arg 1 :from-position :bottom}]
                            [::pearl-diver-give-choice]]})

(defn pirate-ship-trash [game {:keys [attacker] :as args}]
  (-> game
      (assoc-in [:players attacker :pirate-ship-trashed?] true)
      (trash-from-revealed args)))

(defn pirate-ship-add-coin [game {:keys [player-no]}]
  (let [pirate-ship-trashed? (get-in game [:players player-no :pirate-ship-trashed?])]
    (-> game
        (update-in [:players player-no] dissoc :pirate-ship-trashed?)
        (cond-> pirate-ship-trashed? (update-in [:players player-no :pirate-ship-coins] ut/plus 1)))))

(defn pirate-ship-choice [game {:keys [player-no choice]}]
  (let [pirate-ship-coins (or (get-in game [:players player-no :pirate-ship-coins]) 0)]
    (case choice
      :coins (update-in game [:players player-no :coins] + pirate-ship-coins)
      :attack (push-effect-stack game {:player-no player-no
                                       :effects   [[:attack {:effects [[:reveal-from-deck 2]
                                                                       [:give-choice {:text     "Trash a revealed Treasure (attacker chooses)."
                                                                                      :choice   ::pirate-ship-trash
                                                                                      :options  [:player :revealed {:type :treasure}]
                                                                                      :min      1
                                                                                      :max      1
                                                                                      :attacker player-no}]
                                                                       [:discard-all-revealed]]}]
                                                   [::pirate-ship-add-coin]]}))))

(defn pirate-ship-give-choice [game {:keys [player-no] :as args}]
  (let [pirate-ship-coins (or (get-in game [:players player-no :pirate-ship-coins]) 0)]
    (give-choice game (merge args {:text    "Choose one:"
                                   :choice  ::pirate-ship-choice
                                   :options [:special
                                             {:option :coins :text (str "+$" pirate-ship-coins)}
                                             {:option :attack :text "Attack other players."}]
                                   :min     1
                                   :max     1}))))

(effects/register {::pirate-ship-trash       pirate-ship-trash
                   ::pirate-ship-add-coin    pirate-ship-add-coin
                   ::pirate-ship-choice      pirate-ship-choice
                   ::pirate-ship-give-choice pirate-ship-give-choice})

(def pirate-ship {:name    :pirate-ship
                  :set     :seaside
                  :types   #{:action :attack}
                  :cost    4
                  :effects [[::pirate-ship-give-choice]]})

(defn salvager-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-hand {:card-name card-name}]
                                         [:give-coins cost]]})))

(effects/register {::salvager-trash salvager-trash})

(def salvager {:name    :salvager
               :set     :seaside
               :types   #{:action}
               :cost    4
               :effects [[:give-buys 1]
                         [:give-choice {:text    "Trash a card from your hand."
                                        :choice  ::salvager-trash
                                        :options [:player :hand]
                                        :min     1
                                        :max     1}]]})

(def sea-hag {:name    :sea-hag
              :set     :seaside
              :types   #{:action :attack}
              :cost    4
              :effects [[:attack {:effects [[:discard-from-topdeck 1]
                                            [:gain-to-topdeck {:card-name :curse}]]}]]})

(defn smugglers-give-choice [{:keys [players] :as game} {:keys [player-no] :as args}]
  (let [prev-player      (mod (dec player-no) (count players))
        valid-card-names (->> (get-in game [:players prev-player :gained-cards])
                              (keep (fn [{:keys [name] :as card}]
                                      (when (<= (ut/get-cost game card) 6)
                                        name)))
                              set)]
    (give-choice game (merge args
                             {:text    "Gain a card costing up to $6 that the player to the right gained on their last turn."
                              :choice  :gain
                              :options [:supply {:names valid-card-names
                                                 :all   true}]
                              :min     1
                              :max     1}))))

(effects/register {::smugglers-give-choice smugglers-give-choice})

(def smugglers {:name    :smugglers
                :set     :seaside
                :types   #{:action}
                :cost    3
                :effects [[::smugglers-give-choice]]})

(def tactician-trigger {:event    :at-start-turn
                        :duration :once
                        :mode     :semi
                        :effects  [[:draw 5]
                                   [:give-actions 1]
                                   [:give-buys 1]]})

(defn tactician-discard [game {:keys [player-no card-id]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (pos? (count hand)) (push-effect-stack {:player-no player-no
                                                    :effects   [[:move-cards {:player-no  player-no
                                                                              :card-names (map :name hand)
                                                                              :from       :hand
                                                                              :to         :discard}]
                                                                [:add-trigger {:trigger tactician-trigger
                                                                               :card-id card-id}]]}))))

(effects/register {::tactician-discard tactician-discard})

(def tactician {:name    :tactician
                :set     :seaside
                :types   #{:action :duration}
                :cost    5
                :effects [[::tactician-discard]]})

(defn treasure-map-trash [game {:keys [player-no card-id] :as args}]
  (let [{this-treasure-map :card} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})
        {another-treasure-map :card} (ut/get-card-idx game [:players player-no :hand] {:name :treasure-map})]
    (push-effect-stack game (merge args {:effects (concat [(when this-treasure-map
                                                             [:trash-this])]
                                                          (when another-treasure-map
                                                            [[:trash-from-hand {:card-name :treasure-map}]])
                                                          (when (and this-treasure-map another-treasure-map)
                                                            (repeat 4 [:gain-to-topdeck {:card-name :gold}])))}))))

(effects/register {::treasure-map-trash treasure-map-trash})

(def treasure-map {:name    :treasure-map
                   :set     :seaside
                   :types   #{:action}
                   :cost    4
                   :effects [[::treasure-map-trash]]})

(defn bought-no-victory-cards? [game player-no]
  (let [bought-victory-cards (->> (get-in game [:players player-no :gained-cards])
                                  (filter :bought)
                                  (filter (comp :victory (partial ut/get-types game))))]
    (empty? bought-victory-cards)))

(effects/register {::bought-no-victory-cards? bought-no-victory-cards?})

(def treasury {:name              :treasury
               :set               :seaside
               :types             #{:action}
               :cost              5
               :effects           [[:draw 1]
                                   [:give-actions 1]
                                   [:give-coins 1]]
               :trigger-condition ::bought-no-victory-cards?
               :at-clean-up       [[:topdeck-from-play-area {:card-name :treasury}]]})

(def warehouse {:name    :warehouse
                :set     :seaside
                :types   #{:action}
                :cost    3
                :effects [[:give-actions 1]
                          [:draw 3]
                          [:give-choice {:text    "Discard 3 cards."
                                         :choice  :discard-from-hand
                                         :options [:player :hand]
                                         :min     3
                                         :max     3}]]})

(def wharf {:name    :wharf
            :set     :seaside
            :types   #{:action :duration}
            :cost    5
            :effects [[:draw 2]
                      [:give-buys 1]]
            :trigger {:event    :at-start-turn
                      :duration :once
                      :mode     :semi
                      :effects  [[:draw 2]
                                 [:give-buys 1]]}})

(def kingdom-cards [ambassador
                    bazaar
                    caravan
                    cutpurse
                    embargo
                    explorer
                    fishing-village
                    ghost-ship
                    haven
                    island
                    lighthouse
                    lookout
                    merchant-ship
                    native-village
                    navigator
                    outpost
                    pearl-diver
                    pirate-ship
                    salvager
                    sea-hag
                    smugglers
                    tactician
                    treasure-map
                    treasury
                    warehouse
                    wharf])
