(ns dombot.operations
  (:require [dombot.utils :as ut]))

(defn start-round [player]
  (assoc player :actions 1
                :coins 0
                :buys 1))

(defn get-pile-idx [game card-name]
  (->> game
       :supply
       (keep-indexed (fn [idx pile]
                       (when ((comp #{card-name} :name :card) pile) {:idx idx :card (:card pile)})))
       first))

(defn gain [game player-no card-name]
  (let [{:keys [idx card]} (get-pile-idx game card-name)
        pile-size (get-in game [:supply idx :count])]
    (assert (and pile-size (< 0 pile-size)))
    (-> game
        (update-in [:supply idx :count] dec)
        (update-in [:players player-no :discard] concat [card]))))

(defn buy-card [game player-no card-name]
  (let [{:keys [buys coins]} (get-in game [:players player-no])
        {{:keys [cost]} :card} (get-pile-idx game card-name)]
    (assert (and buys (> buys 0)))
    (assert (and coins cost (>= coins cost)))
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

(defn draw [player n]
  (ut/redupeat player draw-one n))

(defn clean-up [{:keys [play-area hand] :as player}]
  (-> player
      (update :discard concat play-area hand)
      (assoc :play-area []
             :hand [])
      (draw 5)))

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

(defn play-treasure [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {{:keys [coin-value]} :card} (get-hand-idx player card-name)]
    (assert coin-value)
    (-> game
        (update-in [:players player-no] put-in-play card-name)
        (update-in [:players player-no :coins] + coin-value))))

(defn play-treasures [game player-no]
  (let [{:keys [hand]} (get-in game [:players player-no])
        treasures (->> hand
                       (filter (comp :treasure :type))
                       (map :name))]
    (reduce (fn [game' card-name] (play-treasure game' player-no card-name)) game treasures)))

(defn play-action [game player-no card-name]
  (let [{:keys [actions] :as player} (get-in game [:players player-no])
        {{:keys [action-fn]} :card} (get-hand-idx player card-name)]
    (assert (and action-fn actions (< 0 actions)))
    (-> game
        (update-in [:players player-no] put-in-play card-name)
        (update-in [:players player-no :actions] - 1)
        (action-fn player-no))))

(defn play [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {{:keys [type]} :card} (get-hand-idx player card-name)]
    (assert type)
    (cond
      (:action type) (play-action game player-no card-name)
      (:treasure type) (play-treasure game player-no card-name)
      :else (assert false))))

(defn get-vp [deck {:keys [:vp]}]
  (if (fn? vp)
    (vp deck)
    vp))

(defn calc-victory-points [cards]
  (->> cards
       (filter (comp :victory :type))
       (map (partial get-vp cards))
       (apply +)))

(defn view-player [player]
  (-> player
      (update :hand ut/frequencies-of :name)
      (update :play-area ut/frequencies-of :name)
      (update :deck count)
      (update :discard count)))

(defn view-supply [supply]
  (->> supply
       (map (fn [{:keys [card count]}]
              [(:name card) count]))
       (into {})))

(defn view-game [{:keys [supply players current-player] :as game}]
  {:supply         (view-supply supply)
   :player         (view-player (get players current-player))
   :current-player current-player})
