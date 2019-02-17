(ns dombot.operations
  (:require [dombot.utils :as ut]
            [dombot.effects :as effects]
            [dombot.front-end-view :as view]
            [clojure.set]))

(defn game-ended? [game]
  (let [{province-pile-size :pile-size} (ut/get-pile-idx game :province)]
    (or (and province-pile-size (zero? province-pile-size))
        (>= (ut/empty-supply-piles game) 3))))

(defn push-effect-stack
  ([game player-no data {:keys [card-id]}]
   (update game :effect-stack (partial concat (if (sequential? data)
                                                (->> data
                                                     (map (fn [effect]
                                                            (when effect
                                                              (merge {:player-no player-no
                                                                      :effect    effect}
                                                                     (when card-id
                                                                       {:card-id card-id})))))
                                                     (remove nil?))
                                                (when data
                                                  [(merge data
                                                          {:player-no player-no}
                                                          (when card-id
                                                            {:card-id card-id}))])))))
  ([game player-no data]
   (push-effect-stack game player-no data nil)))

(defn pop-effect-stack [{:keys [effect-stack] :as game}]
  (if (= 1 (count effect-stack))
    (dissoc game :effect-stack)
    (update game :effect-stack (partial drop 1))))

(defn do-effect [game player-no [name args] {:keys [card-id]}]
  (let [effect-fn (effects/get-effect name)
        args (cond-> args
                     (and (map? args) card-id) (assoc :card-id card-id))]
    (if args
      (effect-fn game player-no args)
      (effect-fn game player-no))))

(defn check-stack [game]
  (let [[{:keys [player-no card-id effect] :as top}] (get game :effect-stack)]
    (cond-> game
            effect (-> pop-effect-stack
                       (do-effect player-no effect {:card-id card-id})
                       check-stack))))

(defn start-turn
  ([player]
   (assoc player :actions 1
                 :coins 0
                 :buys 1
                 :phase :action))
  ([game player-no]
   (-> (cond-> game
               (not (game-ended? game)) (update-in [:players player-no] start-turn))
       check-stack)))

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

(defn increase-revealed-number-of-cards [game player-no area]
  (cond-> game
          (not= :trash area) (update-in [:players player-no :revealed-cards area] #(if % (inc %) 1))))

(defn reset-revealed-number-of-cards [game player-no area]
  (let [revealed-cards (get-in game [:players player-no :revealed-cards])]
    (cond-> game
            revealed-cards (update-in [:players player-no :revealed-cards] dissoc area))))

(defn state-maintenance [game player-no from to]
  (let [from-cards (get-in game [:players player-no from])]
    (cond-> game
            (and (= from :deck) (:can-undo? game)) (assoc :can-undo? false)
            (or (= from :discard) (= to :discard)) (set-approx-discard-size player-no)
            (= from :revealed) (increase-revealed-number-of-cards player-no to)
            (not= from :revealed) (-> (reset-revealed-number-of-cards player-no from)
                                      (reset-revealed-number-of-cards player-no to))
            (empty? from-cards) (update-in [:players player-no] dissoc from))))

(defn gain [game player-no card-name & [{:keys [to to-position]
                                         :or   {to :discard}}]]
  (let [{:keys [idx card pile-size]} (ut/get-pile-idx game card-name)
        to-path (if (= to :trash)
                  [:trash]
                  [:players player-no to])
        add-card-to-coll (fn [coll card]
                           (case to-position
                             :top (concat [card] coll)
                             (concat coll [card])))]
    (assert pile-size (str "Gain error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (cond-> game
            (< 0 pile-size) (-> (update-in [:supply idx :pile-size] dec)
                                (update-in to-path add-card-to-coll (ut/give-id! card))
                                (state-maintenance player-no :supply to)))))

(defn buy-card [{:keys [effect-stack] :as game} player-no card-name]
  (let [{:keys [buys coins phase]} (get-in game [:players player-no])
        {:keys [card pile-size] :as supply-pile} (ut/get-pile-idx game card-name)
        cost (ut/get-cost game card)]
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
        (update-in [:players player-no :buys] - 1)
        check-stack)))

(defn do-shuffle
  ([{:keys [discard] :as player}]
   (-> player
       (cond-> (not-empty discard) (update :deck concat (shuffle discard)))
       (dissoc :discard)))
  ([game player-no]
   (-> game
       (update-in [:players player-no] do-shuffle)
       (state-maintenance player-no :discard :deck))))

(defn shuffle-discard [game player-no]
  (let [discard (get-in game [:players player-no :discard])
        before (->> discard
                    (keep (comp :shuffle :before-triggers))
                    (apply concat))
        after (->> discard
                   (keep (comp :shuffle :after-triggers))
                   (apply concat))]
    (push-effect-stack game player-no (concat before
                                              [[:do-shuffle]]
                                              after))))

(effects/register {:do-shuffle do-shuffle
                   :shuffle    shuffle-discard})

(defn peek-deck [game player-no number-of-cards]
  (let [{:keys [deck discard]} (get-in game [:players player-no])]
    (cond-> game
            (and (< (count deck) number-of-cards) (not-empty discard)) (shuffle-discard player-no))))

(effects/register {:peek-deck peek-deck})

(defn move-card [game player-no {:keys [card-name from from-position to to-position to-player] :as args}]
  (let [{:keys [deck discard] :as player} (get-in game [:players player-no])]
    (if (and (= :deck from) (empty? deck) (not-empty discard))
      (push-effect-stack game player-no [[:shuffle]
                                         [:move-card args]])
      (let [from-path (if (= from :trash)
                        [:trash]
                        [:players player-no from])
            {:keys [idx card]} (case from-position
                                 :bottom {:idx (dec (count (get player from))) :card (last (get player from))}
                                 :top {:idx 0 :card (first (get player from))}
                                 (if card-name
                                   (ut/get-card-idx game from-path card-name)
                                   {:idx 0 :card (first (get player from))}))
            to-path (if (= to :trash)
                      [:trash]
                      [:players (or to-player player-no) to])
            add-card-to-coll (fn [coll card]
                               (let [coll (vec coll)]
                                 (if (empty? coll)
                                   [card]
                                   (cond
                                     (= :top to-position) (concat [card] coll)
                                     (integer? to-position) (concat (subvec coll 0 to-position)
                                                                    [card]
                                                                    (subvec coll to-position (count coll)))
                                     :else (concat coll [card])))))]
        (when card-name
          (assert card (str "Move error: There is no " (ut/format-name card-name) " in your " (ut/format-name from) ".")))
        (cond-> game
                card (-> (update-in from-path ut/vec-remove idx)
                         (update-in to-path add-card-to-coll card)
                         (state-maintenance player-no from to)))))))

(effects/register {:move-card move-card})

(defn move-cards [game player-no {:keys [card-names number-of-cards from-position] :as args}]
  (assert (or card-names
              (and number-of-cards from-position)) "Can't move unspecified cards.")
  (if number-of-cards
    (cond-> game
            (< 0 number-of-cards)
            (push-effect-stack player-no
                               (repeat number-of-cards [:move-card (dissoc args :number-of-cards)])))
    (let [card-names (ut/ensure-coll card-names)]
      (cond-> game
              (not-empty card-names)
              (push-effect-stack player-no
                                 (map (fn [card-name]
                                        [:move-card (-> args
                                                        (dissoc :card-names)
                                                        (assoc :card-name card-name))])
                                      card-names))))))

(defn draw [game player-no number-of-cards]
  (move-cards game player-no {:number-of-cards number-of-cards
                              :from            :deck
                              :from-position   :top
                              :to              :hand}))

(defn affect-other-players [{:keys [players] :as game} player-no {:keys [effects attack all at-once]}]
  (let [player-nos (cond-> (->> (range 1 (count players))
                                (map (fn [n] (-> n (+ player-no) (mod (count players)))))
                                (remove (fn [n] (and attack (get-in game [:players n :unaffected])))))
                           (not at-once) reverse
                           all (concat [player-no]))]
    (reduce (fn [game other-player-no]
              (push-effect-stack game other-player-no effects))
            game
            player-nos)))

(defn attack-other-players [game player-no args]
  (affect-other-players game player-no (assoc args :attack true)))

(defn affect-all-players [game player-no args]
  (affect-other-players game player-no (assoc args :all true)))

(effects/register {:other-players affect-other-players
                   :attack        attack-other-players
                   :all-players   affect-all-players})

(defn- choose-single [game valid-choices selection]
  (if (coll? selection)
    (assert (<= (count selection) 1) "Choose error: You can only pick 1 option."))
  (let [[{:keys [player-no choice min optional?]}] (get game :effect-stack)
        choice-fn (effects/get-effect choice)
        single-selection (if (coll? selection)
                           (first selection)
                           selection)]
    (if (= min 1)
      (assert (or single-selection optional?) "Choose error: You must pick an option"))
    (when single-selection
      (assert (valid-choices single-selection) (str "Choose error: " (ut/format-name single-selection) " is not a valid option.")))

    (-> game
        pop-effect-stack
        (choice-fn player-no single-selection))))

(defn- choose-multi [game valid-choices selection]
  (let [[{:keys [player-no choice min max optional?]}] (get game :effect-stack)
        choice-fn (effects/get-effect choice)
        multi-selection (if (coll? selection)
                          selection
                          (if selection
                            [selection]
                            []))]

    (when min
      (assert (or (<= min (count multi-selection))
                  (and optional? (empty? multi-selection))) (str "Choose error: You must pick at least " min " options.")))
    (when max
      (assert (<= (count multi-selection) max) (str "Choose error: You can only pick " max " options.")))
    (doseq [sel multi-selection]
      (assert (valid-choices sel) (str "Choose error: " (ut/format-name sel) " is not a valid choice.")))

    (-> game
        pop-effect-stack
        (choice-fn player-no multi-selection))))

(defn choose [game selection]
  (let [[{:keys [choice options min max]}] (get game :effect-stack)
        choose-fn (if (= max 1) choose-single choose-multi)
        valid-choices (->> options (map (fn [{:keys [option] :as option-data}] (or option option-data))) set)]
    (assert choice "Choose error: You don't have a choice to make.")
    (assert (not-empty options) "Choose error: Choice has no options")
    (assert (or (nil? min) (nil? max) (<= min max)))

    (-> game
        (choose-fn valid-choices selection)
        check-stack)))

(defn get-source [{[name arg & [{:keys [last]}]] :options}]
  (if (= :player name)
    (merge {:source arg}
           (when (and (= :discard arg)
                      (not last))
             {:reveal-source true}))
    {:source name}))

(defn- ?reveal-discard-size [game player-no {:keys [source reveal-source]}]
  (let [discard (get-in game [:players player-no :discard])]
    (cond-> game
            (and (= :discard source) reveal-source) (set-approx-discard-size player-no (count discard)))))

(defn give-choice [{:keys [mode] :as game} player-no {[opt-name & opt-args] :options
                                                      :keys                 [min max optional? card-id]
                                                      :as                   choice}]
  (let [opt-fn (effects/get-option opt-name)
        options (apply opt-fn game player-no card-id opt-args)
        {:keys [min max] :as choice'} (-> choice
                                          (assoc :options options)
                                          (merge (get-source choice))
                                          (cond-> min (update :min clojure.core/min (count options))
                                                  max (update :max clojure.core/min (count options))))
        swiftable (and (= :swift mode)
                       (not-empty options)
                       (apply = options)
                       (= min (or max (count options)))
                       (not optional?))]
    (-> game
        (cond-> (not-empty options) (push-effect-stack player-no choice' {:card-id card-id})
                swiftable (choose (take min options)))
        (?reveal-discard-size player-no choice')
        check-stack)))

(defn remove-trigger [game player-no trigger]
  (-> game
      (update-in [:players player-no :triggers] (partial remove (comp #{trigger} :trigger)))
      (as-> game
            (let [triggers (get-in game [:players player-no :triggers])]
              (cond-> game
                      (empty? triggers) (update-in [:players player-no] dissoc :triggers))))))

(defn- apply-triggers [game player-no trigger]
  (let [triggers (get-in game [:players player-no :triggers])
        apply-trigger (fn [game {:keys [effects]}] (push-effect-stack game player-no effects))
        matching-triggers (filter (comp #{trigger} :trigger) triggers)]
    (-> (reduce apply-trigger game matching-triggers)
        (remove-trigger player-no trigger))))

(def reaction-choice [[:give-choice {:text    (str "You may reveal a Reaction to react to the Attack.")
                                     :choice  :reveal-reaction
                                     :options [:player :hand {:type      :reaction
                                                              :reacts-to :attack}]
                                     :max     1}]])

(defn reveal-reaction [game player-no card-name]
  (let [{{:keys [reaction]} :card} (ut/get-card-idx game [:players player-no :hand] card-name)] ; TODO: Handle reactions that are not in hand
    (cond-> game
            reaction (push-effect-stack player-no (concat reaction
                                                          reaction-choice)))))

(defn clear-unaffected [game player-no]
  (update-in game [:players player-no] dissoc :unaffected))

(effects/register {:clear-unaffected clear-unaffected})

(defn card-effect [game player-no {:keys [id types effects]}]
  (let [{:keys [actions-played]} (get-in game [:players player-no])]
    (cond-> game
            (and (:action types)
                 actions-played) (update-in [:players player-no :actions-played] inc)
            (:attack types) (affect-other-players player-no {:effects [[:clear-unaffected]]})
            (:action types) (push-effect-stack player-no effects {:card-id id})
            (:attack types) (affect-other-players player-no {:effects reaction-choice}))))

(effects/register {:gain            gain
                   :draw            draw
                   :give-choice     give-choice
                   :reveal-reaction reveal-reaction
                   :card-effect     card-effect})

(defn play [{:keys [effect-stack] :as game} player-no card-name]
  (let [{:keys [phase actions triggers]} (get-in game [:players player-no])
        {{:keys [types effects coin-value] :as card} :card} (ut/get-card-idx game [:players player-no :hand] card-name)]
    (assert (empty? effect-stack) "You can't play cards when you have a choice to make.")
    (assert card (str "Play error: There is no " (ut/format-name card-name) " in your Hand."))
    (assert types (str "Play error: " (ut/format-name card-name) " has no types."))
    (cond
      (:action types) (do (assert effects (str "Play error: " (ut/format-name card-name) " has no effect."))
                          (assert (and actions (< 0 actions)) "Play error: You have no more actions."))
      (:treasure types) (assert coin-value (str "Play error: " (ut/format-name card-name) " has no coin value"))
      :else (assert false (str "Play error: " (ut/format-types types) " cards cannot be played.")))
    (when phase
      (assert (or (and (:action types)
                       (#{:action} phase))
                  (and (:treasure types)
                       (#{:action :pay} phase)))
              (str "You can't play " (ut/format-types types) " cards when you're in the " (ut/format-name phase) " phase.")))
    (-> game
        (move-card player-no {:card-name card-name
                              :from      :hand
                              :to        :play-area})
        (card-effect player-no card)
        (cond->
          phase (assoc-in [:players player-no :phase] (cond (:action types) :action
                                                            (:treasure types) :pay))
          (:action types) (update-in [:players player-no :actions] - 1)
          coin-value (update-in [:players player-no :coins] + coin-value) ; todo: put treasures on the stack
          (not-empty triggers) (apply-triggers player-no [:play card-name]))
        check-stack)))

(defn play-treasures [game player-no]
  (let [{:keys [hand]} (get-in game [:players player-no])
        treasures (->> hand
                       (filter (comp :treasure :types))
                       (map :name))]
    (reduce (fn [game card-name] (play game player-no card-name)) game treasures))) ; TODO: Stack treasures separately

(defn- get-victory-points [cards {:keys [victory-points]}]
  (if (keyword? victory-points)
    (let [vp-fn (effects/get-effect victory-points)]
      (vp-fn cards))
    victory-points))

(defn calc-victory-points [{:keys [deck discard hand play-area]}]
  (let [cards (concat deck discard hand play-area)]
    (->> cards
         (filter :victory-points)
         (map (partial get-victory-points cards))
         (apply + 0))))

(def calc-score (juxt calc-victory-points (comp - :number-of-turns)))

(defn end-game-for-player [best-score {:keys [deck discard] :as player}]
  (let [victory-points (calc-victory-points player)]
    (-> player
        (update :hand concat deck discard)
        (dissoc :deck :discard)
        (assoc :phase :end-of-game
               :victory-points victory-points
               :winner (= best-score (calc-score player))))))

(defn check-game-ended [{:keys [players] :as game}]
  (let [best-score (->> players
                        (map calc-score)
                        sort
                        last)]
    (cond-> game
            (game-ended? game) (-> (update :players (partial mapv (partial end-game-for-player best-score)))))))

(defn clean-up
  ([{:keys [play-area hand] :as player}]
   (let [used-cards (concat hand play-area)]
     (-> player
         (cond-> (not-empty used-cards) (update :discard concat used-cards))
         (dissoc :hand :play-area)
         (assoc :actions 0
                :coins 0
                :buys 0
                :actions-played 0
                :phase :out-of-turn)
         (dissoc :triggers)
         (update :number-of-turns inc))))
  ([{:keys [effect-stack] :as game} player-no]
   (assert (empty? effect-stack) "You can't end your turn when you have a choice to make.")
   (-> game
       (update-in [:players player-no] clean-up)
       (set-approx-discard-size player-no)
       (draw player-no 5)
       (update :players (partial mapv (fn [player] (dissoc player :revealed-cards))))
       (dissoc :cost-reductions)
       check-game-ended)))
