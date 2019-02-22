(ns dombot.cards.seaside
  (:require [dombot.operations :refer [move-cards]]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def caravan {:name     :caravan
              :set      :seaside
              :types    #{:action :duration}
              :cost     4
              :effects  [[:draw 1]
                         [:give-actions 1]]
              :duration [[:draw 1]]})

(def fishing-village {:name     :fishing-village
                      :set      :seaside
                      :types    #{:action :duration}
                      :cost     3
                      :effects  [[:give-actions 2]
                                 [:give-coins 1]]
                      :duration [[:give-actions 1]
                                 [:give-coins 1]]})

(defn haven-set-aside [game {:keys [player-no card-name]}]
  (let [{:keys [card idx]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        haven-idx (-> game (get-in [:players player-no :play-area]) count dec)]
    (-> game
        (update-in [:players player-no :hand] ut/vec-remove idx)
        (update-in [:players player-no :play-area haven-idx :set-aside] concat [card]))))

(defn haven-put-in-hand [game {:keys [player-no card-id]}]
  game
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})]
    (-> game
        (ut/update-in-vec [:players player-no :play-area] {:id card-id} dissoc :set-aside)
        (update-in [:players player-no :hand] concat (:set-aside card)))))

(effects/register {::haven-set-aside   haven-set-aside
                   ::haven-put-in-hand haven-put-in-hand})

(def haven {:name     :haven
            :set      :seaside
            :types    #{:action :duration}
            :cost     2
            :effects  [[:draw 1]
                       [:give-actions 1]
                       [:give-choice {:text    "Set aside a card from your hand."
                                      :choice  ::haven-set-aside
                                      :options [:player :hand]
                                      :min     1
                                      :max     1}]]
            :duration [[::haven-put-in-hand]]})

(def lighthouse {:name     :lighthouse
                 :set      :seaside
                 :types    #{:action :duration}
                 :cost     2
                 :effects  [[:give-actions 1]
                            [:give-coins 1]
                            [:mark-unaffected]]
                 :duration [[:give-coins 1]
                            [:clear-unaffected]]})

(def merchant-ship {:name     :merchant-ship
                    :set      :seaside
                    :types    #{:action :duration}
                    :cost     5
                    :effects  [[:give-coins 2]]
                    :duration [[:give-coins 2]]})

(defn tactician-discard [game {:keys [player-no card-id]}]
  (let [hand (get-in game [:players player-no :hand])]
    (if (< 0 (count hand))
      (move-cards game {:player-no  player-no
                        :card-names (map :name hand)
                        :from       :hand
                        :to         :discard})
      (ut/update-in-vec game [:players player-no :play-area] {:id card-id} update :next-turn drop-last))))

(effects/register {::tactician-discard tactician-discard})

(def tactician {:name     :tactician
                :set      :seaside
                :types    #{:action :duration}
                :cost     5
                :effects  [[::tactician-discard]]
                :duration [[:draw 5]
                           [:give-actions 1]
                           [:give-buys 1]]})

(def wharf {:name     :wharf
            :set      :seaside
            :types    #{:action :duration}
            :cost     5
            :effects  [[:draw 2]
                       [:give-buys 1]]
            :duration [[:draw 2]
                       [:give-buys 1]]})

(def kingdom-cards [caravan
                    fishing-village
                    haven
                    lighthouse
                    merchant-ship
                    tactician
                    wharf])
