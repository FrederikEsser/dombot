(ns dombot.cards.prosperity
  (:require [dombot.operations :refer [#_move-cards push-effect-stack give-choice]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

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

(def workers-village {:name    :worker's-village
                      :set     :prosperity
                      :types   #{:action}
                      :cost    4
                      :effects [[:draw 1]
                                [:give-actions 2]
                                [:give-buys 1]]})

(def kingdom-cards [forge
                    expand
                    kings-court
                    workers-village])