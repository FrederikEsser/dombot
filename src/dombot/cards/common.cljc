(ns dombot.cards.common
  (:require [dombot.operations :refer [gain move-card move-cards give-choice card-effect push-effect-stack]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn give-actions [game player-no n]
  (update-in game [:players player-no :actions] + n))

(defn give-money [game player-no n]
  (update-in game [:players player-no :coins] + n))

(defn give-buys [game player-no n]
  (update-in game [:players player-no :buys] + n))

(defn gain-to-hand [game player-no card-name]
  (gain game player-no card-name {:to :hand}))

(defn gain-to-topdeck [game player-no card-name]
  (gain game player-no card-name {:to          :deck
                                  :to-position :top}))

(defn play-from-hand [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name card-name
                                          :from      :hand
                                          :to        :play-area})))

(defn play-from-discard [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name     card-name
                                          :from          :discard
                                          :from-position :bottom
                                          :to            :play-area})))

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

(defn reveal-hand [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (assoc-in game [:players player-no :revealed-cards :hand] (count hand))))

(defn discard-down-to [game player-no n]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (> (count hand) n) (give-choice player-no {:text    (str "Discard down to " n " cards in hand.")
                                                       :choice  :discard-from-hand
                                                       :options [:player :hand]
                                                       :min     (- (count hand) n)
                                                       :max     (- (count hand) n)}))))

(defn trash-from-hand [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :hand
                              :to         :trash}))

(defn reveal-from-deck [game player-no number-of-cards]
  (move-cards game player-no {:number-of-cards number-of-cards
                              :from            :deck
                              :from-position   :top
                              :to              :revealed}))

(defn look-at [game player-no number-of-cards]
  (move-cards game player-no {:number-of-cards number-of-cards
                              :from            :deck
                              :from-position   :top
                              :to              :look-at}))

(defn discard-all-revealed [game player-no]
  (let [revealed (get-in game [:players player-no :revealed])]
    (move-cards game player-no {:card-names (map :name revealed)
                                :from       :revealed
                                :to         :discard})))

(defn discard-all-set-aside [game player-no]
  (let [set-aside (get-in game [:players player-no :set-aside])]
    (move-cards game player-no {:card-names (map :name set-aside)
                                :from       :set-aside
                                :to         :discard})))

(defn trash-from-revealed [game player-no card-name]
  (move-card game player-no {:card-name card-name
                             :from      :revealed
                             :to        :trash}))

(defn topdeck-from-discard [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name   card-name
                                          :from        :discard
                                          :to          :deck
                                          :to-position :top})))

(defn play-action-twice [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {card :card} (ut/get-card-idx player :hand card-name)]
    (cond-> game
            card-name (push-effect-stack player-no [[:play card-name]
                                                    [:card-effect card]
                                                    [:card-effect card]]))))

(defn add-trigger [game player-no trigger]
  (-> game
      (update-in [:players player-no :triggers] concat [trigger])))

(defn topdeck-from-look-at [game player-no card-names]
  (-> game
      (move-cards player-no {:card-names  card-names
                             :from        :look-at
                             :to          :deck
                             :to-position :top})))

(defn discard-from-look-at [game player-no card-names]
  (-> game
      (move-cards player-no {:card-names card-names
                             :from       :look-at
                             :to         :discard})))

(defn trash-from-look-at [game player-no card-names]
  (-> game
      (move-cards player-no {:card-names card-names
                             :from       :look-at
                             :to         :trash})))

(defn discard-from-topdeck [game player-no number-of-cards]
  (-> game
      (move-card player-no {:number-of-cards number-of-cards
                            :from            :deck
                            :from-position   :top
                            :to              :discard})))

(effects/register {:give-actions          give-actions
                   :give-money            give-money
                   :give-buys             give-buys
                   :gain-to-hand          gain-to-hand
                   :gain-to-topdeck       gain-to-topdeck
                   :play                  play-from-hand
                   :play-from-discard     play-from-discard
                   :topdeck-from-hand     topdeck-from-hand
                   :discard-from-hand     discard-from-hand
                   :discard-down-to       discard-down-to
                   :trash-from-hand       trash-from-hand
                   :reveal-hand           reveal-hand
                   :reveal-from-deck      reveal-from-deck
                   :look-at               look-at
                   :discard-all-revealed  discard-all-revealed
                   :trash-from-revealed   trash-from-revealed
                   :discard-all-set-aside discard-all-set-aside
                   :topdeck-from-look-at  topdeck-from-look-at
                   :discard-from-look-at  discard-from-look-at
                   :trash-from-look-at    trash-from-look-at
                   :topdeck-from-discard  topdeck-from-discard
                   :discard-from-topdeck  discard-from-topdeck
                   :play-action-twice     play-action-twice
                   :add-trigger           add-trigger})
