(ns dombot.cards.common
  (:require [dombot.operations :refer [gain move-card move-cards give-choice card-effect]]
            [dombot.utils :as ut]))

(defn gain-to-hand [game player-no card-name]
  (gain game player-no card-name {:to :hand}))

(defn gain-to-topdeck [game player-no card-name]
  (gain game player-no card-name {:to          :deck
                                  :to-position :top}))

(defn topdeck-from-hand [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name   card-name
                                          :from        :hand
                                          :to          :deck
                                          :to-position :top})))

(defn discard-from-hand [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :hand
                              :to         :discard}))

(defn trash-from-hand [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :hand
                              :to         :trash}))

(defn discard-all-revealed [game player-no]
  (let [{:keys [reveal]} (get-in game [:players player-no])]
    (-> game
        (move-cards player-no {:card-names (map :name reveal)
                               :from       :reveal
                               :to         :discard}))))

(defn trash-from-revealed [game player-no card-name]
  (-> game
      (move-card player-no {:card-name card-name
                            :from      :reveal
                            :to        :trash})))

(defn topdeck-from-discard [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name   card-name
                                          :from        :discard
                                          :to          :deck
                                          :to-position :top})))

(defn give-topdeck-choice [game player-no]
  (-> game
      (give-choice player-no {:text       "Put a card from your hand onto your deck."
                              :choice-fn  topdeck-from-hand
                              :options-fn (ut/player-area :hand)
                              :min        1
                              :max        1})))

(defn play-action-twice [game player-no card-name]
  (if card-name
    (let [player (get-in game [:players player-no])
          {:keys [card]} (ut/get-card-idx player :hand card-name)]
      (-> game
          (move-card player-no {:card-name card-name
                                :from      :hand
                                :to        :play-area})
          (card-effect player-no card)
          (card-effect player-no card)))
    game))
