(ns dombot.cards.seaside
  (:require [dombot.operations :refer [move-card move-cards give-choice push-effect-stack]]
            [dombot.cards.common :refer [reveal-hand]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def bazaar {:name    :bazaar
             :set     :seaside
             :types   #{:action}
             :cost    5
             :effects [[:draw 1]
                       [:give-actions 2]
                       [:give-coins 1]]})

(def caravan {:name     :caravan
              :set      :seaside
              :types    #{:action :duration}
              :cost     4
              :effects  [[:draw 1]
                         [:give-actions 1]]
              :duration [[:draw 1]]})

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

(def fishing-village {:name     :fishing-village
                      :set      :seaside
                      :types    #{:action :duration}
                      :cost     3
                      :effects  [[:give-actions 2]
                                 [:give-coins 1]]
                      :duration [[:give-actions 1]
                                 [:give-coins 1]]})

(def ghost-ship {:name    :ghost-ship
                 :set     :seaside
                 :types   #{:action :attack}
                 :cost    5
                 :effects [[:draw 2]
                           [:attack {:effects [[:topdeck-down-to 3]]}]]})

(defn haven-put-in-hand [game {:keys [player-no card-id card-name]}]
  game
  (let [{haven :card} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})
        {:keys [idx card]} (ut/get-card-idx haven [:set-aside] {:name card-name})]
    (-> game
        (ut/update-in-vec [:players player-no :play-area] {:id card-id} update :set-aside ut/vec-remove idx)
        (update-in [:players player-no :hand] concat [card]))))

(defn haven-set-aside [game {:keys [player-no card-id card-name]}]
  (let [{:keys [card idx]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (-> game
        (update-in [:players player-no :hand] ut/vec-remove idx)
        (ut/update-in-vec [:players player-no :play-area] {:id card-id}
                          (fn [haven]
                            (-> haven
                                (update :set-aside concat [card])
                                (update :at-start-turn concat [[[::haven-put-in-hand {:card-name card-name}]]])))))))

(effects/register {::haven-put-in-hand haven-put-in-hand
                   ::haven-set-aside   haven-set-aside})

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

(def lighthouse {:name     :lighthouse
                 :set      :seaside
                 :types    #{:action :duration}
                 :cost     2
                 :effects  [[:give-actions 1]
                            [:give-coins 1]
                            [:mark-unaffected]]
                 :duration [[:give-coins 1]
                            [:clear-unaffected]]})

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

(def merchant-ship {:name     :merchant-ship
                    :set      :seaside
                    :types    #{:action :duration}
                    :cost     5
                    :effects  [[:give-coins 2]]
                    :duration [[:give-coins 2]]})

(defn outpost-extra-turn [game {:keys [player-no card-id]}]
  (push-effect-stack game {:player-no player-no
                           :card-id   card-id
                           :effects   [[:clean-up {:extra-turn?     true
                                                   :number-of-cards 3}]
                                       [:start-turn]]}))

(defn outpost-give-extra-turn [game {:keys [player-no card-id]}]
  (let [{:keys [previous-turn-was-yours?]} (get-in game [:players player-no])]
    (cond-> game
            (not previous-turn-was-yours?) (-> (assoc-in [:players player-no :previous-turn-was-yours?] true)
                                               (ut/update-in-vec [:players player-no :play-area] {:id card-id}
                                                                 assoc :at-end-turn [[::outpost-extra-turn]])))))

(effects/register {::outpost-extra-turn      outpost-extra-turn
                   ::outpost-give-extra-turn outpost-give-extra-turn})

(def outpost {:name    :outpost
              :set     :seaside
              :types   #{:action :duration}
              :cost    5
              :effects [[::outpost-give-extra-turn]]})

(def sea-hag {:name    :sea-hag
              :set     :seaside
              :types   #{:action :attack}
              :cost    4
              :effects [[:attack {:effects [[:discard-from-topdeck 1]
                                            [:gain-to-topdeck {:card-name :curse}]]}]]})

(defn tactician-discard [game {:keys [player-no card-id]}]
  (let [hand (get-in game [:players player-no :hand])]
    (if (< 0 (count hand))
      (move-cards game {:player-no  player-no
                        :card-names (map :name hand)
                        :from       :hand
                        :to         :discard})
      (ut/update-in-vec game [:players player-no :play-area] {:id card-id} update :at-start-turn drop-last))))

(effects/register {::tactician-discard tactician-discard})

(def tactician {:name     :tactician
                :set      :seaside
                :types    #{:action :duration}
                :cost     5
                :effects  [[::tactician-discard]]
                :duration [[:draw 5]
                           [:give-actions 1]
                           [:give-buys 1]]})

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

(def wharf {:name     :wharf
            :set      :seaside
            :types    #{:action :duration}
            :cost     5
            :effects  [[:draw 2]
                       [:give-buys 1]]
            :duration [[:draw 2]
                       [:give-buys 1]]})

(def kingdom-cards [bazaar
                    caravan
                    cutpurse
                    fishing-village
                    ghost-ship
                    haven
                    island
                    lighthouse
                    lookout
                    merchant-ship
                    outpost
                    sea-hag
                    tactician
                    warehouse
                    wharf])
