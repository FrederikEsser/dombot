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

(defn gain [game player-no card-name & [{:keys [to]
                                         :or   {to :discard}}]]
  (let [{:keys [idx card]} (ut/get-pile-idx game card-name)
        pile-size (get-in game [:supply idx :pile-size])]
    (assert pile-size (str "Gain error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (cond-> game
            (< 0 pile-size) (-> (update-in [:supply idx :pile-size] dec)
                                (update-in [:players player-no to] concat [card])))))

(defn gain-to-hand [game player-no card-name]
  (gain game player-no card-name {:to :hand}))

(defn buy-card [game player-no card-name]
  (let [{:keys [buys coins]} (get-in game [:players player-no])
        {{:keys [cost]} :card
         pile-size      :pile-size
         :as            supply-pile} (ut/get-pile-idx game card-name)]
    (assert (and buys (> buys 0)) "Buy error: You have no more buys.")
    (assert supply-pile (str "Buy error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (assert (and coins cost (>= coins cost)) (str "Buy error: " (ut/format-name card-name) " costs " cost " and you only have " coins " coins."))
    (assert (and pile-size (< 0 pile-size)) (str "Buy error: " (ut/format-name card-name) " supply is empty."))
    (-> game
        (gain player-no card-name)
        (update-in [:players player-no :coins] - cost)
        (update-in [:players player-no :buys] - 1))))

(defn shuffle-discard [{:keys [deck discard] :as player}]
  (assert (empty? deck) "Shuffle error: Your deck is not empty.")
  (-> player
      (assoc :deck (shuffle discard))
      (assoc :discard [])))

(defn- draw-one [{:keys [:deck :discard] :as player}]
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

(defn move-card [game player-no {:keys [card-name from from-position to to-position] :as args}]
  (let [{:keys [deck discard] :as player} (get-in game [:players player-no])]
    (if (and (= :deck from) (empty? deck) (not-empty discard))
      (-> game
          (update-in [:players player-no] shuffle-discard)
          (move-card player-no args))
      (let [{:keys [idx card]} (case from-position
                                 :top {:idx 0 :card (first (get player from))}
                                 :bottom {:idx (dec (count (get player from))) :card (last (get player from))}
                                 (ut/get-card-idx player from card-name))
            from-path (if (= from :trash)
                        [:trash]
                        [:players player-no from])
            to-path (if (= to :trash)
                      [:trash]
                      [:players player-no to])
            add-card-to-coll (fn [coll card']
                               (case to-position
                                 :top (concat [card'] coll)
                                 (concat coll [card'])))]
        (when card-name
          (assert card (str "Move error: There is no " (ut/format-name card-name) " in your " (ut/format-name from) ".")))
        (cond-> game
                card (-> (update-in from-path ut/vec-remove idx)
                         (update-in to-path add-card-to-coll card)))))))

(defn- apply-triggers [game player-no trigger-id]
  (let [{:keys [triggers]} (get-in game [:players player-no])
        apply-trigger (fn [game' {:keys [trigger-fn]}] (trigger-fn game' player-no))
        matching-triggers (filter (comp #{trigger-id} :trigger-id) triggers)]
    (-> (reduce apply-trigger game matching-triggers)
        (update-in [:players player-no :triggers] (partial remove (comp #{trigger-id} :trigger-id))))))

(defn play [game player-no card-name]
  (let [{:keys [actions triggers] :as player} (get-in game [:players player-no])
        {{:keys [type action-fn coin-value] :as card} :card} (ut/get-card-idx player :hand card-name)]
    (assert card (str "Play error: There is no " (ut/format-name card-name) " in your Hand."))
    (assert type (str "Play error: " (ut/format-name card-name) " has no type."))
    (cond
      (:action type) (do (assert action-fn (str "Play error: " (ut/format-name card-name) " has no action function."))
                         (assert (and actions (< 0 actions)) "Play error: You have no more actions."))
      (:treasure type) (assert coin-value (str "Play error: " (ut/format-name card-name) " has no coin value"))
      :else (assert false (str "Play error: " (ut/format-type type) " cards cannot be played.")))
    (-> game
        (move-card player-no {:card-name card-name
                              :from      :hand
                              :to        :play-area})
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

(defn push-effect-stack [game player-no item]
  (update-in game [:players player-no :effect-stack] (partial concat [item])))

(defn pop-effect-stack [game player-no]
  (update-in game [:players player-no :effect-stack] (partial drop 1)))

(defn- chose-single [game player-no selection]
  (if (coll? selection)
    (assert (<= (count selection) 1) "Chose error: You can only pick 1 option."))
  (let [{[{:keys [choice-fn options min]}] :effect-stack} (get-in game [:players player-no])
        single-selection (if (coll? selection)
                           (first selection)
                           selection)]
    (if (= min 1)
      (assert single-selection "Chose error: You must pick an option"))
    (when single-selection
      (assert ((set options) single-selection) (str "Chose error: " (ut/format-name single-selection) " is not a valid choice.")))

    (-> game
        (pop-effect-stack player-no)
        (choice-fn player-no single-selection))))

(defn- chose-multi [game player-no selection]
  (let [{[{:keys [choice-fn options min max]}] :effect-stack} (get-in game [:players player-no])
        valid-choices (-> options set)
        multi-selection (if (coll? selection)
                          selection
                          (if selection
                            [selection]
                            []))]

    (when min
      (assert (<= min (count multi-selection)) (str "Chose error: You must pick at least " min " options.")))
    (when max
      (assert (<= (count multi-selection) max) (str "Chose error: You can only pick " max " options.")))
    (doseq [sel multi-selection]
      (assert (valid-choices sel) (str "Chose error: " (ut/format-name sel) " is not a valid choice.")))

    (-> game
        (pop-effect-stack player-no)
        (choice-fn player-no multi-selection))))

(defn check-stack [game player-no]
  (let [{[{:keys [action-fn]}] :effect-stack} (get-in game [:players player-no])]
    (cond-> game
            action-fn (-> (pop-effect-stack player-no)
                          (action-fn player-no)
                          (check-stack player-no)))))

(defn chose [game player-no selection]
  (let [{[{:keys [choice-fn options min max]}] :effect-stack} (get-in game [:players player-no])
        chose-fn (if (= max 1) chose-single chose-multi)]
    (assert choice-fn "Chose error: You don't have a choice to make.")
    (assert (not-empty options) "Chose error: Choice has no options")
    (assert (or (nil? min) (nil? max) (<= min max)))

    (-> game
        (chose-fn player-no selection)
        (check-stack player-no))))

(defn give-choice [game player-no {:keys [options-fn options min max] :as args}]
  (let [options (if options-fn (options-fn game player-no) options)
        args (cond-> args
                     min (update :min clojure.core/min (count options))
                     max (update :max clojure.core/min (count options)))]
    (-> game
        (cond-> (not-empty options) (push-effect-stack player-no (-> args
                                                                     (dissoc :options-fn)
                                                                     (assoc :options options))))
        (check-stack player-no))))

(defn- get-victory-points [cards {:keys [victory-points]}]
  (if (fn? victory-points)
    (victory-points cards)
    victory-points))

(defn calc-victory-points [{:keys [deck discard hand play-area]}]
  (let [cards (concat deck discard hand play-area)]
    (->> cards
         (filter :victory-points)
         (map (partial get-victory-points cards))
         (apply + 0))))

(defn game-ended? [game]
  (let [{province-pile-size :pile-size} (ut/get-pile-idx game :province)]
    (or (zero? province-pile-size)
        (>= (ut/empty-supply-piles game) 3))))

(defn view-discard [discard]
  (if (empty? discard)
    :empty
    (let [size (count discard)
          variance (Math/round (/ size 4.0))]
      {:top         (-> discard last :name)
       :approx-size (- (+ size
                          (rand-int (inc variance)))
                       (rand-int (inc variance)))})))

(defn view-player [{[{:keys [options text]}] :effect-stack :as player}]
  (-> player
      (update :hand ut/frequencies-of :name)
      (update :play-area ut/frequencies-of :name)
      (update :deck count)
      (update :discard view-discard)
      (cond-> options (assoc :choice {:text    text
                                      :options options}))
      (dissoc :effect-stack)
      (dissoc :triggers)
      (assoc :victory-points (calc-victory-points player))))

(defn- view-end-player [{:keys [name deck discard hand play-area] :as player}]
  (let [cards (concat deck discard hand play-area)]
    {:name           name
     :cards          (ut/frequencies-of cards :name)
     :victory-points (calc-victory-points player)}))

(defn view-supply [supply]
  (->> supply
       (sort-by (comp (juxt (comp first :type) :cost :name) :card))
       (map (fn [{{:keys [name cost]} :card
                  :keys               [pile-size]}]
              {:card name :price cost :count pile-size}))))

(defn view-game [{:keys [supply players trash current-player] :as game}]
  (if (game-ended? game)
    {:players (map view-end-player players)}
    {:supply         (view-supply supply)
     :player         (view-player (get players current-player))
     :trash          (ut/frequencies-of trash :name)
     :current-player (get-in players [current-player :name])}))

