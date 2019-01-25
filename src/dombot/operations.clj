(ns dombot.operations
  (:require [dombot.utils :as ut]))

(defn start-round
  ([player]
   (assoc player :actions 1
                 :coins 0
                 :buys 1))
  ([game player-no]
   (-> game
       (update-in [:players player-no] start-round))))

(defn get-pile-idx [game card-name]
  (->> game
       :supply
       (keep-indexed (fn [idx pile]
                       (when ((comp #{card-name} :name :card) pile) (merge pile {:idx idx}))))
       first))

(defn gain [game player-no card-name]
  (let [{:keys [idx card]} (get-pile-idx game card-name)
        pile-size (get-in game [:supply idx :pile-size])]
    (assert pile-size)
    (if (< 0 pile-size)
      (-> game
          (update-in [:supply idx :pile-size] dec)
          (update-in [:players player-no :discard] concat [card]))
      game)))

(defn buy-card [game player-no card-name]
  (let [{:keys [buys coins]} (get-in game [:players player-no])
        {{:keys [cost]} :card
         pile-size      :pile-size} (get-pile-idx game card-name)]
    (assert (and buys (> buys 0)))
    (assert (and coins cost (>= coins cost)))
    (assert (and pile-size (< 0 pile-size)))
    (-> game
        (gain player-no card-name)
        (update-in [:players player-no :coins] - cost)
        (update-in [:players player-no :buys] - 1))))

(defn shuffle-discard [{:keys [deck discard] :as player}]
  (assert (empty? deck))
  (-> player
      (assoc :deck (shuffle discard))
      (assoc :discard [])))

(defn draw-one [{:keys [:deck :discard] :as player}]
  (if (empty? deck)
    (if (empty? discard)
      player
      (-> player
          shuffle-discard
          draw-one))
    (let [card (first deck)]
      (-> player
          (update :deck (partial drop 1))
          (update :hand conj card)))))

(defn draw
  ([player n]
   (ut/redupeat player draw-one n))
  ([game player-no n]
   (-> game
       (update-in [:players player-no] draw n))))

(defn clean-up
  ([{:keys [play-area hand] :as player}]
   (-> player
       (update :discard concat play-area hand)
       (assoc :play-area []
              :hand [])
       (dissoc :triggers)
       (draw 5)))
  ([game player-no]
   (-> game
       (update-in [:players player-no] clean-up))))

(defn get-hand-idx [player card-name]
  (->> player
       :hand
       (keep-indexed (fn [idx {:keys [name] :as card}]
                       (when (= card-name name) {:idx idx :card card})))
       first))

(defn put-in-play [player card-name]
  (let [{:keys [idx card]} (get-hand-idx player card-name)]
    (assert card)
    (-> player
        (update :hand ut/vec-remove idx)
        (update :play-area concat [card]))))

(defn apply-triggers [game player-no trigger-id]
  (let [{:keys [triggers]} (get-in game [:players player-no])
        apply-trigger (fn [game' {:keys [trigger-fn]}] (trigger-fn game' player-no))
        matching-triggers (filter (comp #{trigger-id} :trigger-id) triggers)]
    (-> (reduce apply-trigger game matching-triggers)
        (update-in [:players player-no :triggers] (partial remove (comp #{trigger-id} :trigger-id))))))

(defn play [game player-no card-name]
  (let [{:keys [actions triggers] :as player} (get-in game [:players player-no])
        {{:keys [type action-fn coin-value]} :card} (get-hand-idx player card-name)]
    (assert type)
    (cond
      (:action type) (assert (and action-fn actions (< 0 actions)))
      (:treasure type) (assert coin-value)
      :else (assert false))
    (-> game
        (update-in [:players player-no] put-in-play card-name)
        (cond->
          (:action type) (update-in [:players player-no :actions] - 1)
          action-fn (action-fn player-no)
          coin-value (update-in [:players player-no :coins] + coin-value)
          (not-empty triggers) (apply-triggers player-no [:play card-name])))))

(defn play-treasures [game player-no]
  (let [{:keys [hand]} (get-in game [:players player-no])
        treasures (->> hand
                       (filter (comp :treasure :type))
                       (map :name))]
    (reduce (fn [game' card-name] (play game' player-no card-name)) game treasures)))

(defn do-for-other-players [{:keys [players] :as game} player-no f & args]
  (let [other-player-nos (->> players
                              (keep-indexed (fn [idx _] (when (not= idx player-no) idx))))]
    (reduce (fn [game' other-player-no]
              (apply f game' other-player-no args))
            game
            other-player-nos)))

(defn get-victory-points [cards {:keys [victory-points]}]
  (if (fn? victory-points)
    (victory-points cards)
    victory-points))

(defn calc-victory-points [{:keys [deck discard hand play-area]}]
  (let [cards (concat deck discard hand play-area)]
    (->> cards
         (filter :victory-points)
         (map (partial get-victory-points cards))
         (apply + 0))))

(defn game-ended? [{:keys [supply] :as game}]
  (let [{:keys [pile-size]} (get-pile-idx game :province)
        empty-piles (->> supply
                         (filter (comp zero? :pile-size)))]
    (or (zero? pile-size)
        (>= (count empty-piles) 3))))

(defn view-player [player]
  (-> player
      (update :hand ut/frequencies-of :name)
      (update :play-area ut/frequencies-of :name)
      (update :deck count)
      (update :discard count)
      (dissoc :triggers)
      (assoc :victory-points (calc-victory-points player))))

(defn view-end-player [{:keys [deck discard hand play-area] :as player}]
  (let [cards (concat deck discard hand play-area)]
    {:cards          (ut/frequencies-of cards :name)
     :victory-points (calc-victory-points player)}))

(defn view-supply [supply]
  (->> supply
       (map (fn [{:keys [card pile-size]}]
              [(:name card) pile-size]))
       (into {})))

(defn view-game [{:keys [supply players current-player] :as game}]
  (if (game-ended? game)
    {:players (map view-end-player players)}
    {:supply         (view-supply supply)
     :player         (view-player (get players current-player))
     :current-player current-player}))

