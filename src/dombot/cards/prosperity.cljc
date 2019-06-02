(ns dombot.cards.prosperity
  (:require [dombot.operations :refer [#_move-cards push-effect-stack give-choice]]
            [dombot.cards.common :refer [give-coins]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def colony {:name :colony :types #{:victory} :cost 11 :victory-points 10})

(def platinum {:name :platinum :types #{:treasure} :cost 9 :coin-value 5})

(defn- bank-give-coins [game {:keys [player-no]}]
  (let [number-of-treasures-in-play (->> (get-in game [:players player-no :play-area])
                                         (filter (comp :treasure :types))
                                         count)]
    (give-coins game {:player-no player-no :arg number-of-treasures-in-play})))

(effects/register {::bank-give-coins bank-give-coins})

(def bank {:name            :bank
           :set             :prosperity
           :types           #{:treasure}
           :cost            7
           :coin-value      0
           :effects         [[::bank-give-coins]]
           :auto-play-index 1})

(defn- city-effects [game {:keys [player-no]}]
  (let [empty-piles (ut/empty-supply-piles game)]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat [[:draw 1]
                                                 [:give-actions 2]]
                                                (when (<= 1 empty-piles)
                                                  [[:draw 1]])
                                                (when (<= 2 empty-piles)
                                                  [[:give-buys 1]
                                                   [:give-coins 1]]))})))

(effects/register {::city-effects city-effects})

(def city {:name    :city
           :set     :prosperity
           :types   #{:action}
           :cost    5
           :effects [[::city-effects]]})

(def expand {:name    :expand
             :set     :prosperity
             :types   #{:action}
             :cost    7
             :effects [[:give-choice {:text    "Trash a card from your hand."
                                      :choice  [:trash-and-gain {:extra-cost 3}]
                                      :options [:player :hand]
                                      :min     1
                                      :max     1}]]})

(defn- forge-trash [game {:keys [player-no card-names]}]
  (let [total-cost (->> card-names
                        (map (fn [card-name]
                               (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
                                 (ut/get-cost game card))))
                        (apply + 0))]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:trash-from-hand {:card-names card-names}]
                                         [:give-choice {:text    (str "Gain a card costing exactly " total-cost ".")
                                                        :choice  :gain
                                                        :options [:supply {:cost total-cost}]
                                                        :min     1
                                                        :max     1}]]})))

(effects/register {::forge-trash forge-trash})

(def forge {:name    :forge
            :set     :prosperity
            :types   #{:action}
            :cost    7
            :effects [[:give-choice {:text    "Trash any number of cards from your hand."
                                     :choice  ::forge-trash
                                     :options [:player :hand]}]]})

(def kings-court {:name    :king's-court
                  :set     :prosperity
                  :types   #{:action}
                  :cost    7
                  :effects [[:give-choice {:text    "You may play an Action card from your hand three times."
                                           :choice  [:repeat-action {:times 3}]
                                           :options [:player :hand {:type :action}]
                                           :max     1}]]})

(defn- vault-discard [game {:keys [player-no card-names]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:discard-from-hand {:card-names card-names}]
                                       [:give-coins (count card-names)]]}))

(defn- vault-discard-2 [game {:keys [player-no card-names] :as args}]
  (-> game
      (push-effect-stack {:player-no player-no
                          :effects   (concat [[:discard-from-hand args]]
                                             (when (= 2 (count card-names))
                                               [[:draw 1]]))})))

(effects/register {::vault-discard   vault-discard
                   ::vault-discard-2 vault-discard-2})

(def vault {:name    :vault
            :set     :prosperity
            :types   #{:action}
            :cost    5
            :effects [[:draw 2]
                      [:give-choice {:text    "Discard any number of cards for +$1 each."
                                     :choice  ::vault-discard
                                     :options [:player :hand]}]
                      [:other-players {:effects [[:give-choice {:text      "You may discard 2 cards, to draw a card."
                                                                :choice    ::vault-discard-2
                                                                :options   [:player :hand]
                                                                :min       2
                                                                :max       2
                                                                :optional? true}]]}]]})

(def workers-village {:name    :worker's-village
                      :set     :prosperity
                      :types   #{:action}
                      :cost    4
                      :effects [[:draw 1]
                                [:give-actions 2]
                                [:give-buys 1]]})

(def kingdom-cards [bank
                    city
                    forge
                    expand
                    kings-court
                    vault
                    workers-village])