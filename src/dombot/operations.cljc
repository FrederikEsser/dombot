(ns dombot.operations
  (:require [dombot.utils :as ut]
            [dombot.effects :as effects]
            [dombot.front-end-model :as model]))

(defn start-turn
  ([player]
   (assoc player :actions 1
                 :coins 0
                 :buys 1
                 :phase :action))
  ([game player-no]
   (-> game
       (update-in [:players player-no] start-turn))))

(defn set-approx-discard-size [game player-no & [n]]
  (let [{:keys [discard approx-discard-size]} (get-in game [:players player-no])
        size (count discard)
        variance (Math/round (/ size 4.0))
        approx-size (or n
                        (- (+ size
                              (rand-int (inc variance)))
                           (rand-int (inc variance))))]
    (cond-> game
            approx-discard-size (assoc-in [:players player-no :approx-discard-size] approx-size))))

(defn gain [game player-no card-name & [{:keys [to to-position]
                                         :or   {to :discard}}]]
  (let [{:keys [idx card]} (ut/get-pile-idx game card-name)
        pile-size (get-in game [:supply idx :pile-size])
        add-card-to-coll (fn [coll card]
                           (case to-position
                             :top (concat [card] coll)
                             (concat coll [card])))]
    (assert pile-size (str "Gain error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (cond-> game
            (< 0 pile-size) (-> (update-in [:supply idx :pile-size] dec)
                                (update-in [:players player-no to] add-card-to-coll card)
                                (cond-> (= to :discard) (set-approx-discard-size player-no))))))

(defn buy-card [{:keys [effect-stack] :as game} player-no card-name]
  (let [{:keys [buys coins phase]} (get-in game [:players player-no])
        {{:keys [cost]} :card
         pile-size      :pile-size
         :as            supply-pile} (ut/get-pile-idx game card-name)]
    (assert (empty? effect-stack) "You can't buy cards when you have a choice to make.")
    (assert (and buys (> buys 0)) "Buy error: You have no more buys.")
    (assert supply-pile (str "Buy error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (assert (and coins cost (>= coins cost)) (str "Buy error: " (ut/format-name card-name) " costs " cost " and you only have " coins " coins."))
    (assert (and pile-size (< 0 pile-size)) (str "Buy error: " (ut/format-name card-name) " supply is empty."))
    (when phase
      (assert (#{:action :pay :buy} phase) (str "You can't buy cards when you're in the " (ut/format-name phase) " phase.")))
    (-> game
        (cond-> phase (assoc-in [:players player-no :phase] :buy))
        (gain player-no card-name)
        (update-in [:players player-no :coins] - cost)
        (update-in [:players player-no :buys] - 1))))

(defn shuffle-discard [{:keys [deck discard] :as player}]
  (assert (empty? deck) "Shuffle error: Your deck is not empty.")
  (-> player
      (assoc :deck (shuffle discard))
      (assoc :discard [])))

(defn move-card [game player-no {:keys [card-name from from-position to to-position] :as args}]
  (let [{:keys [deck discard] :as player} (get-in game [:players player-no])]
    (if (and (= :deck from) (empty? deck) (not-empty discard))
      (-> game
          (update-in [:players player-no] shuffle-discard)
          (move-card player-no args))
      (let [{:keys [idx card]} (case from-position
                                 :bottom {:idx (dec (count (get player from))) :card (last (get player from))}
                                 :top {:idx 0 :card (first (get player from))}
                                 (if card-name
                                   (ut/get-card-idx player from card-name)
                                   {:idx 0 :card (first (get player from))}))
            from-path (if (= from :trash)
                        [:trash]
                        [:players player-no from])
            to-path (if (= to :trash)
                      [:trash]
                      [:players player-no to])
            add-card-to-coll (fn [coll card]
                               (case to-position
                                 :top (concat [card] coll)
                                 (concat coll [card])))]
        (when card-name
          (assert card (str "Move error: There is no " (ut/format-name card-name) " in your " (ut/format-name from) ".")))
        (cond-> game
                card (-> (update-in from-path ut/vec-remove idx)
                         (update-in to-path add-card-to-coll card)
                         (cond-> (and (= :deck from) (:can-undo? game)) (assoc :can-undo? false)
                                 (or (= from :discard) (= to :discard)) (set-approx-discard-size player-no))))))))

(defn move-cards [game player-no {:keys [card-names number-of-cards from-position] :as args}]
  (assert (or card-names
              (and number-of-cards from-position)) "Can't move unspecified cards.")
  (if number-of-cards
    (ut/redupeat game number-of-cards move-card player-no args)
    (reduce (fn [game card-name] (move-card game player-no (assoc args :card-name card-name)))
            game
            (ut/ensure-coll card-names))))

(defn draw [game player-no number-of-cards]
  (move-cards game player-no {:number-of-cards number-of-cards
                              :from            :deck
                              :from-position   :top
                              :to              :hand}))

(defn push-effect-stack [game player-no data]
  (update game :effect-stack (partial concat (if (sequential? data)
                                               (->> data
                                                    (map (fn [effect] {:player-no player-no
                                                                       :effect    effect})))
                                               [(assoc data :player-no player-no)]))))

(defn pop-effect-stack [{:keys [effect-stack] :as game}]
  (if (= 1 (count effect-stack))
    (dissoc game :effect-stack)
    (update game :effect-stack (partial drop 1))))

(defn affect-other-players [{:keys [players] :as game} player-no {:keys [effects unaffected]}]
  (let [other-player-nos (->> (range 1 (count players))
                              (map (fn [n] (-> n (+ player-no) (mod (count players)))))
                              (remove (set unaffected))
                              reverse)]
    (reduce (fn [game other-player-no]
              (push-effect-stack game other-player-no effects))
            game
            other-player-nos)))

(defn do-effect [game player-no [name args]]
  (let [effect-fn (effects/get-effect name)]
    (if args
      (effect-fn game player-no args)
      (effect-fn game player-no))))

(defn check-stack [game]
  (let [[{:keys [player-no effect]}] (get game :effect-stack)]
    (cond-> game
            effect (-> pop-effect-stack
                       (do-effect player-no effect)
                       check-stack))))

(defn- choose-single [game selection]
  (if (coll? selection)
    (assert (<= (count selection) 1) "Choose error: You can only pick 1 option."))
  (let [[{:keys [player-no choice options min]}] (get game :effect-stack)
        choice-fn (effects/get-effect choice)
        valid-choices (set options)
        single-selection (if (coll? selection)
                           (first selection)
                           selection)]
    (if (= min 1)
      (assert single-selection "Choose error: You must pick an option"))
    (when single-selection
      (assert (valid-choices single-selection) (str "Choose error: " (ut/format-name single-selection) " is not a valid option.")))

    (-> game
        pop-effect-stack
        (choice-fn player-no single-selection))))

(defn- choose-multi [game selection]
  (let [[{:keys [player-no choice options min max]}] (get game :effect-stack)
        choice-fn (effects/get-effect choice)
        valid-choices (set options)
        multi-selection (if (coll? selection)
                          selection
                          (if selection
                            [selection]
                            []))]

    (when min
      (assert (<= min (count multi-selection)) (str "Choose error: You must pick at least " min " options.")))
    (when max
      (assert (<= (count multi-selection) max) (str "Choose error: You can only pick " max " options.")))
    (doseq [sel multi-selection]
      (assert (valid-choices sel) (str "Choose error: " (ut/format-name sel) " is not a valid choice.")))

    (-> game
        pop-effect-stack
        (choice-fn player-no multi-selection))))

(defn choose [game selection]
  (let [[{:keys [choice options min max]}] (get game :effect-stack)
        choose-fn (if (= max 1) choose-single choose-multi)]
    (assert choice "Choose error: You don't have a choice to make.")
    (assert (not-empty options) "Choose error: Choice has no options")
    (assert (or (nil? min) (nil? max) (<= min max)))

    (-> game
        (choose-fn selection)
        check-stack)))

(defn get-source [{[name arg & [{:keys [last]}]] :options}]
  (if (= :supply name)
    {:source :supply}
    (merge {:source arg}
           (when (and (= :discard arg)
                      (not last))
             {:reveal-source true}))))

(defn- ?reveal-discard-size [game player-no {:keys [source reveal-source]}]
  (let [discard (get-in game [:players player-no :discard])]
    (cond-> game
            (and (= :discard source) reveal-source) (set-approx-discard-size player-no (count discard)))))

(defn give-choice [{:keys [mode] :as game} player-no {[opt-name & opt-args] :options
                                                      :keys                 [min max]
                                                      :as                   choice}]
  (let [options (apply (effects/get-effect opt-name) game player-no opt-args)
        {:keys [min max] :as choice'} (-> choice
                                          (assoc :options options)
                                          (merge (get-source choice))
                                          (cond-> min (update :min clojure.core/min (count options))
                                                  max (update :max clojure.core/min (count options))))
        swiftable (and (= :swift mode)
                       (not-empty options)
                       (apply = options)
                       (= min (or max (count options))))]
    (-> game
        (cond-> (not-empty options) (push-effect-stack player-no choice')
                swiftable (choose (take min options)))
        (?reveal-discard-size player-no choice')
        check-stack)))

(defn- apply-triggers [game player-no trigger]
  (let [triggers (get-in game [:players player-no :triggers])
        apply-trigger (fn [game {:keys [effects]}] (push-effect-stack game player-no effects))
        matching-triggers (filter (comp #{trigger} :trigger) triggers)]
    (-> (reduce apply-trigger game matching-triggers)
        (update-in [:players player-no :triggers] (partial remove (comp #{trigger} :trigger))))))

(defn reveal-reaction [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {{:keys [reaction]} :card} (ut/get-card-idx player :hand card-name)] ; TODO: Handle reactions that are not in hand
    (cond-> game
            reaction (push-effect-stack player-no reaction))))

(defn card-effect [game player-no {:keys [name type effects] :as card}]
  (cond-> game
          (:action type) (push-effect-stack player-no effects)
          (:attack type) (affect-other-players player-no {:effects [[:give-choice {:text    (str "You may reveal a Reaction to react to the " (ut/format-name name) " Attack.")
                                                                                   :choice  :reveal-reaction
                                                                                   :options [:player :hand {:type      :reaction
                                                                                                            :reacts-to :attack}]
                                                                                   :max     1}]]})))

(effects/register {:gain            gain
                   :draw            draw
                   :other-players   affect-other-players
                   :attack          affect-other-players
                   :give-choice     give-choice
                   :reveal-reaction reveal-reaction
                   :card-effect     card-effect})

(defn play [{:keys [effect-stack] :as game} player-no card-name]
  (let [{:keys [phase actions triggers] :as player} (get-in game [:players player-no])
        {{:keys [type effects coin-value] :as card} :card} (ut/get-card-idx player :hand card-name)]
    (assert (empty? effect-stack) "You can't play cards when you have a choice to make.")
    (assert card (str "Play error: There is no " (ut/format-name card-name) " in your Hand."))
    (assert type (str "Play error: " (ut/format-name card-name) " has no type."))
    (cond
      (:action type) (do (assert effects (str "Play error: " (ut/format-name card-name) " has no effect."))
                         (assert (and actions (< 0 actions)) "Play error: You have no more actions."))
      (:treasure type) (assert coin-value (str "Play error: " (ut/format-name card-name) " has no coin value"))
      :else (assert false (str "Play error: " (ut/format-type type) " cards cannot be played.")))
    (when phase
      (assert (or (and (:action type)
                       (#{:action} phase))
                  (and (:treasure type)
                       (#{:action :pay} phase)))
              (str "You can't play " (ut/format-type type) " cards when you're in the " (ut/format-name phase) " phase.")))
    (-> game
        (move-card player-no {:card-name card-name
                              :from      :hand
                              :to        :play-area})
        (card-effect player-no card)
        (cond->
          phase (assoc-in [:players player-no :phase] (cond (:action type) :action
                                                            (:treasure type) :pay))
          (:action type) (update-in [:players player-no :actions] - 1)
          coin-value (update-in [:players player-no :coins] + coin-value) ; todo: put treasures on the stack
          (not-empty triggers) (apply-triggers player-no [:play card-name]))
        check-stack)))

(defn play-treasures [game player-no]
  (let [{:keys [hand]} (get-in game [:players player-no])
        treasures (->> hand
                       (filter (comp :treasure :type))
                       (map :name))]
    (reduce (fn [game card-name] (play game player-no card-name)) game treasures))) ; TODO: Stack treasures separately

(defn clean-up
  ([{:keys [play-area hand] :as player}]
   (-> player
       (update :discard concat play-area hand)
       (assoc :play-area []
              :hand []
              :actions 0
              :coins 0
              :buys 0
              :phase :out-of-turn)
       (dissoc :triggers)))
  ([{:keys [effect-stack] :as game} player-no]
   (assert (empty? effect-stack) "You can't end your turn when you have a choice to make.")
   (-> game
       (update-in [:players player-no] clean-up)
       (set-approx-discard-size player-no)
       (draw player-no 5)
       (dissoc :revealed))))

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

(defn view-player [{:keys [look-at]
                    :as   player}]
  (-> player
      (update :hand ut/frequencies-of :name)
      (update :play-area ut/frequencies-of :name)
      (update :look-at ut/frequencies-of :name)
      (cond-> (empty? look-at) (dissoc :look-at))
      (update :deck count)
      (update :discard view-discard)
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

(defn view-game [{:keys [supply players trash effect-stack current-player revealed] :as game}]
  (if (game-ended? game)
    {:players (map view-end-player players)}
    #_(model/model-game game)
    (let [[{:keys [player-no text options]}] effect-stack
          revealed (->> revealed
                        (map (fn [[player-no hand]]
                               {:player (get-in players [player-no :name])
                                :hand   (map :name hand)})))]
      (cond-> {:supply         (view-supply supply)
               :player         (view-player (get players current-player))
               :trash          (ut/frequencies-of trash :name)
               :current-player (get-in players [current-player :name])}
              (not-empty revealed) (assoc :revealed revealed)
              (or text (not-empty options)) (assoc :choice {:text    text
                                                            :player  (get-in players [player-no :name])
                                                            :options options})))))
