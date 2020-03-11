(ns dombot.cards.promos
  (:require [dombot.operations :refer [move-card move-cards push-effect-stack]]
            [dombot.cards.common :refer [set-aside=>hand-trigger add-trigger]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))


(defn- captain-play-from-supply [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game card-name)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:card-effect {:card card}]]})))

(effects/register {::captain-play-from-supply captain-play-from-supply})

(def captain-choice {:text    "Play a non-Duration Action card from the Supply costing up to $4."
                     :choice  ::captain-play-from-supply
                     :options [:supply {:type     :action
                                        :not-type :duration
                                        :max-cost 4}]
                     :min     1
                     :max     1})

(def captain {:name    :captain
              :set     :promos
              :types   #{:action :duration}
              :cost    6
              :effects [[:give-choice captain-choice]]
              :trigger {:event    :at-start-turn
                        :duration :once
                        :mode     :complex
                        :effects  [[:give-choice captain-choice]]}})

(defn- church-set-aside [game {:keys [player-no card-id]}]
  (let [set-aside (get-in game [:players player-no :church-set-aside])]
    (-> game
        (update-in [:players player-no] dissoc :church-set-aside)
        (add-trigger {:player-no player-no
                      :card-id   card-id
                      :trigger   (merge {:event    :at-start-turn
                                         :duration :once
                                         :mode     :manual
                                         :effects  [[:put-set-aside-into-hand]
                                                    [:give-choice {:text    "You may trash a card from your hand."
                                                                   :choice  :trash-from-hand
                                                                   :options [:player :hand]
                                                                   :max     1}]]}
                                        (when (not-empty set-aside)
                                          {:set-aside set-aside}))}))))

(defn church-choice [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name card-names) (move-cards (merge args {:from :hand
                                                             :to   :church-set-aside}))))

(effects/register {::church-set-aside church-set-aside
                   ::church-choice    church-choice})

(def church {:name    :church
             :set     :promos
             :types   #{:action :duration}
             :cost    3
             :effects [[:give-actions 1]
                       [:give-choice {:text    "Set aside up to 3 cards from your hand for next turn."
                                      :choice  ::church-choice
                                      :options [:player :hand]
                                      :max     3}]
                       [::church-set-aside]]})

(defn- dismantle-trash [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        cost (ut/get-cost game card)]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat [[:trash-from-hand {:card-name card-name}]]
                                                (when (ut/costs-at-least 1 cost)
                                                  [[:give-choice {:text    (str "Gain a card costing less than " (ut/format-cost cost) ".")
                                                                  :choice  :gain
                                                                  :options [:supply {:costs-less-than cost}]
                                                                  :min     1
                                                                  :max     1}]
                                                   [:gain {:card-name :gold}]]))})))

(effects/register {::dismantle-trash dismantle-trash})

(def dismantle {:name    :dismantle
                :set     :promos
                :types   #{:action}
                :cost    4
                :effects [[:give-choice {:text    "Trash a card from your hand."
                                         :choice  ::dismantle-trash
                                         :options [:player :hand]
                                         :min     1
                                         :max     1}]]})

(def envoy {:name    :envoy
            :set     :promos
            :types   #{:action}
            :cost    4
            :effects [[:reveal-from-deck 5]
                      [:give-choice {:text       "Choose which of the revealed cards will be discarded."
                                     :choice     :discard-from-revealed
                                     :options    [:player :revealed]
                                     :min        1
                                     :max        1
                                     :hide-hand? true}]
                      [:put-all-revealed-into-hand]]})

(defn stash-put [game {:keys [player-no position]}]
  (move-card game {:player-no   player-no
                   :card-name   :stash
                   :from        :stash
                   :to          :deck
                   :to-position position}))

(effects/register {::stash-put stash-put})

(def stash {:name           :stash
            :set            :promos
            :types          #{:treasure}
            :cost           5
            :coin-value     2
            :before-shuffle [[:move-card {:card-name :stash
                                          :from      :discard
                                          :to        :stash}]]
            :after-shuffle  [[:give-choice {:text    "Put the Stash anywhere in your deck."
                                            :choice  ::stash-put
                                            :options [:deck-position]
                                            :min     1
                                            :max     1}]]})

(def kingdom-cards [captain
                    church
                    dismantle
                    envoy
                    stash])
