(ns dombot.operations
  (:require [dombot.utils :as ut]
            [dombot.effects :as effects]
            [clojure.set]))

(defn game-ended? [game]
  (let [{province-pile-size :pile-size} (ut/get-pile-idx game :province)]
    (or (and province-pile-size (zero? province-pile-size))
        (>= (ut/empty-supply-piles game) 3))))

(defn push-effect-stack [game {:keys [player-no card-id effects choice]}]
  (update game :effect-stack (partial concat (cond effects (->> effects
                                                                (map (fn [effect]
                                                                       (when effect
                                                                         (merge {:player-no player-no
                                                                                 :effect    effect}
                                                                                (when card-id
                                                                                  {:card-id card-id})))))
                                                                (remove nil?))
                                                   choice [(merge choice
                                                                  {:player-no player-no}
                                                                  (when card-id
                                                                    {:card-id card-id}))]
                                                   :else []))))

(defn pop-effect-stack [{:keys [effect-stack] :as game}]
  (if (= 1 (count effect-stack))
    (dissoc game :effect-stack)
    (update game :effect-stack (partial drop 1))))

(defn do-effect [game {:keys       [player-no card-id]
                       [name args] :effect}]
  (let [effect-fn (effects/get-effect name)
        args (merge {:player-no player-no}
                    (when card-id
                      {:card-id card-id})
                    (cond (map? args) args
                          args {:arg args}))]
    (effect-fn game args)))

(defn check-stack [game]
  (let [[{:keys [player-no card-id effect] :as top}] (get game :effect-stack)]
    (cond-> game
            effect (-> pop-effect-stack
                       (do-effect {:player-no player-no
                                   :card-id   card-id
                                   :effect    effect})
                       check-stack))))

(defn stay-in-play [{:keys [at-start-turn at-end-turn]}]
  (or (not-empty at-start-turn)
      (not-empty at-end-turn)))

(defn duration-effects [game player-no]
  (let [duration-cards (->> (get-in game [:players player-no :play-area])
                            (filter (comp not-empty :at-start-turn)))
        do-duration-effect (fn [game {:keys [id at-start-turn]}]
                             (-> game
                                 (ut/update-in-vec [:players player-no :play-area] {:id id} dissoc :at-start-turn)
                                 (cond-> at-start-turn (push-effect-stack {:player-no player-no
                                                                           :card-id   id
                                                                           :effects   (apply concat at-start-turn)}))))]
    (reduce do-duration-effect game (reverse duration-cards))))

(defn start-turn
  ([player]
   (assoc player :actions 1
                 :coins 0
                 :buys 1
                 :phase :action))
  ([game {:keys [player-no]}]
   (if (game-ended? game)
     game
     (-> game
         (assoc :current-player player-no)
         (update-in [:players player-no] start-turn)
         (duration-effects player-no)
         check-stack))))

(effects/register {:start-turn start-turn})

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

(defn gain [game {:keys [player-no card-name to to-position]
                  :or   {to :discard}}]
  (assert card-name "No card-name specified for gain.")
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

(effects/register {:gain gain})

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
        (gain {:player-no player-no
               :card-name card-name})
        (update-in [:players player-no :coins] - cost)
        (update-in [:players player-no :buys] - 1)
        check-stack)))

(defn do-shuffle
  ([{:keys [discard] :as player}]
   (-> player
       (cond-> (not-empty discard) (update :deck concat (shuffle discard)))
       (dissoc :discard)))
  ([game {:keys [player-no]}]
   (-> game
       (update-in [:players player-no] do-shuffle)
       (state-maintenance player-no :discard :deck))))

(defn shuffle-discard [game {:keys [player-no]}]
  (let [discard (get-in game [:players player-no :discard])
        before (->> discard
                    (keep (comp :shuffle :before-triggers))
                    (apply concat))
        after (->> discard
                   (keep (comp :shuffle :after-triggers))
                   (apply concat))]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat before
                                                [[:do-shuffle]]
                                                after)})))

(effects/register {:do-shuffle do-shuffle
                   :shuffle    shuffle-discard})

(defn peek-deck [game {:keys [player-no arg]}]
  (let [{:keys [deck discard]} (get-in game [:players player-no])]
    (cond-> game
            (and (< (count deck) arg) (not-empty discard)) (shuffle-discard {:player-no player-no}))))

(effects/register {:peek-deck peek-deck})

(defn move-card [game {:keys [player-no card-name move-card-id from from-position to to-position to-player] :as args}]
  (assert (or card-name move-card-id from-position) (str "Can't move unspecified card: " args))
  (let [{:keys [deck discard] :as player} (get-in game [:players player-no])]
    (if (and (= :deck from) (empty? deck) (not-empty discard))
      (push-effect-stack game {:player-no player-no
                               :effects   [[:shuffle]
                                           [:move-card args]]})
      (let [from-path (if (= from :trash)
                        [:trash]
                        [:players player-no from])
            {:keys [idx card]} (case from-position
                                 :bottom {:idx (dec (count (get player from))) :card (last (get player from))}
                                 :top {:idx 0 :card (first (get player from))}
                                 (cond
                                   card-name (ut/get-card-idx game from-path {:name card-name})
                                   move-card-id (ut/get-card-idx game from-path {:id move-card-id})
                                   :else {:idx 0 :card (first (get player from))}))
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
        (when move-card-id
          (assert card (str "Move error: Card-id " move-card-id " is not in your " (ut/format-name from) ".")))
        (cond-> game
                card (-> (update-in from-path ut/vec-remove idx)
                         (update-in to-path add-card-to-coll card)
                         (state-maintenance player-no from to)))))))

(effects/register {:move-card move-card})

(defn move-cards [game {:keys [player-no card-name card-names number-of-cards from-position] :as args}]
  (assert (or card-name
              card-names
              (and number-of-cards from-position)) (str "Can't move unspecified cards: " args))
  (if number-of-cards
    (cond-> game
            (< 0 number-of-cards)
            (push-effect-stack {:player-no player-no
                                :effects   (repeat number-of-cards [:move-card (dissoc args :number-of-cards)])}))
    (let [card-names (or card-names [card-name])]
      (cond-> game
              (not-empty card-names)
              (push-effect-stack {:player-no player-no
                                  :effects   (map (fn [card-name]
                                                    [:move-card (-> args
                                                                    (dissoc :card-names)
                                                                    (assoc :card-name card-name))])
                                                  card-names)})))))

(defn draw [game {:keys [player-no arg]}]
  (move-cards game {:player-no       player-no
                    :number-of-cards arg
                    :from            :deck
                    :from-position   :top
                    :to              :hand}))

(effects/register {:draw draw})

(defn mark-unaffected [game {:keys [player-no card-id works]}]
  (let [tag (if works {:works works} {:card-id card-id})]
    (update-in game [:players player-no :unaffected] conj tag)))

(defn is-unaffected?
  ([{:keys [unaffected]}]
   (not-empty unaffected))
  ([game player-no]
   (-> (get-in game [:players player-no])
       is-unaffected?)))

(defn clear-unaffected [game {:keys [player-no card-id works]}]
  (let [criteria (if works {:works works} {:card-id card-id})]
    (-> game
        (update-in [:players player-no :unaffected] (partial remove (ut/match criteria)))
        (update-in [:players player-no] ut/dissoc-if-empty :unaffected))))

(effects/register {:mark-unaffected  mark-unaffected
                   :clear-unaffected clear-unaffected})

(defn affect-other-players [{:keys [players] :as game} {:keys [player-no effects attack all at-once]}]
  (let [player-nos (cond-> (->> (range 1 (count players))
                                (map (fn [n] (-> n (+ player-no) (mod (count players)))))
                                (remove (fn [n] (and attack (is-unaffected? game n)))))
                           (not at-once) reverse
                           all (concat [player-no]))]
    (reduce (fn [game other-player-no]
              (push-effect-stack game {:player-no other-player-no
                                       :effects   effects}))
            game
            player-nos)))

(defn attack-other-players [game args]
  (affect-other-players game (assoc args :attack true)))

(defn affect-all-players [game args]
  (affect-other-players game (assoc args :all true)))

(effects/register {:other-players affect-other-players
                   :attack        attack-other-players
                   :all-players   affect-all-players})

(defn- choose-single [game valid-choices selection]
  (if (coll? selection)
    (assert (<= (count selection) 1) "Choose error: You can only pick 1 option."))
  (let [[{:keys [player-no card-id choice source min optional?]}] (get game :effect-stack)
        choice-fn (effects/get-effect choice)
        arg-name (case source
                   :deck-position :position
                   :special :choice
                   :card-name)
        single-selection (if (coll? selection)
                           (first selection)
                           selection)]
    (if (= min 1)
      (assert (or single-selection optional?) "Choose error: You must pick an option"))
    (when single-selection
      (assert (valid-choices single-selection) (str "Choose error: " (ut/format-name single-selection) " is not a valid option.")))

    (-> game
        pop-effect-stack
        (choice-fn {:player-no player-no
                    :card-id   card-id
                    arg-name   single-selection}))))

(defn- choose-multi [game valid-choices selection]
  (let [[{:keys [player-no card-id choice source min max optional?]}] (get game :effect-stack)
        choice-fn (effects/get-effect choice)
        arg-name (case source
                   :deck-position :position
                   :special :choices
                   :card-names)
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
        (choice-fn {:player-no player-no
                    :card-id   card-id
                    arg-name   multi-selection}))))

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

(defn give-choice [{:keys [mode] :as game} {:keys                 [player-no card-id min max optional?]
                                            [opt-name & opt-args] :options
                                            :as                   choice}]
  (let [opt-fn (effects/get-option opt-name)
        options (apply opt-fn game player-no card-id opt-args)
        {:keys [min max] :as choice} (-> choice
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
        (cond-> (not-empty options) (push-effect-stack {:player-no player-no
                                                        :card-id   card-id
                                                        :choice    choice})
                swiftable (choose (take min options)))
        (?reveal-discard-size player-no choice)
        check-stack)))

(effects/register {:give-choice give-choice})

(defn remove-trigger [game player-no trigger]
  (-> game
      (update-in [:players player-no :triggers] (partial remove (ut/match {:trigger trigger})))
      (update-in [:players player-no] ut/dissoc-if-empty :triggers)))

(defn- apply-triggers [game player-no trigger]
  (let [triggers (get-in game [:players player-no :triggers])
        apply-trigger (fn [game {:keys [effects]}] (push-effect-stack game {:player-no player-no
                                                                            :effects   effects}))
        matching-triggers (filter (comp #{trigger} :trigger) triggers)]
    (-> (reduce apply-trigger game matching-triggers)
        (remove-trigger player-no trigger))))

(def reaction-choice [[:give-choice {:text    (str "You may reveal a Reaction to react to the Attack.")
                                     :choice  :reveal-reaction
                                     :options [:player :hand {:type      :reaction
                                                              :reacts-to :attack}]
                                     :max     1}]])

(defn reveal-reaction [game {:keys [player-no card-name]}]
  (if card-name
    (let [{{:keys [reaction]} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})] ; TODO: Handle reactions that are not in hand
      (assert reaction (str "Revealed Reaction " card-name " has no reaction effects specified."))
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   (concat reaction reaction-choice)})))
    game))

(effects/register {:reveal-reaction reveal-reaction})

(defn card-effect [game {:keys                               [player-no]
                         {:keys [id types effects duration]} :card}]
  (let [{:keys [actions-played]} (get-in game [:players player-no])]
    (cond-> game
            (and (:action types)
                 actions-played) (update-in [:players player-no :actions-played] inc)
            (:attack types) (affect-other-players {:player-no player-no
                                                   :effects   [[:clear-unaffected {:works :once}]]})
            (:action types) (push-effect-stack {:player-no player-no
                                                :card-id   id
                                                :effects   effects})
            (:attack types) (affect-other-players {:player-no player-no
                                                   :effects   reaction-choice})
            duration (ut/update-in-vec [:players player-no :play-area] {:id id} update :at-start-turn concat [duration]))))

(effects/register {:card-effect card-effect})

(defn play [{:keys [effect-stack] :as game} player-no card-name]
  (let [{:keys [phase actions triggers]} (get-in game [:players player-no])
        {{:keys [types effects coin-value] :as card} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
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
        (move-card {:player-no player-no
                    :card-name card-name
                    :from      :hand
                    :to        :play-area})
        (card-effect {:player-no player-no
                      :card      card})
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

(defn calc-victory-points [{:keys [deck discard hand play-area island-mat]}]
  (let [cards (concat deck discard hand play-area island-mat)
        set-aside-cards (mapcat :set-aside cards)
        all-cards (concat cards set-aside-cards)]
    (->> all-cards
         (filter :victory-points)
         (map (partial get-victory-points all-cards))
         (apply + 0))))

(def calc-score (juxt calc-victory-points (comp - :number-of-turns)))

(defn end-game-for-player [best-score {:keys [deck discard play-area] :as player}]
  (let [victory-points (calc-victory-points player)]
    (-> player
        (update :hand concat deck discard play-area)
        (dissoc :deck :discard)
        (assoc :phase :end-of-game
               :victory-points victory-points
               :winner (= best-score (calc-score player))))))

(defn check-game-ended [{:keys [players] :as game}]
  (cond-> game
          (game-ended? game) (as-> game
                                   (let [best-score (->> players
                                                         (map calc-score)
                                                         sort
                                                         last)]
                                     (update game :players (partial mapv (partial end-game-for-player best-score)))))))

(defn clean-up [game {:keys [player-no number-of-cards extra-turn?]
                      :or   {number-of-cards 5}}]
  (let [clean-up-player (fn [{:keys [play-area hand number-of-turns] :as player}]
                          (let [used-cards (concat hand (remove stay-in-play play-area))]
                            (-> player
                                (cond-> (not-empty used-cards) (update :discard concat used-cards))
                                (dissoc :hand)
                                (update :play-area (partial filter stay-in-play))
                                (ut/dissoc-if-empty :play-area)
                                (assoc :actions 0
                                       :coins 0
                                       :buys 0
                                       :actions-played 0
                                       :phase :out-of-turn)
                                (dissoc :triggers)
                                (cond-> (and number-of-turns
                                             (not extra-turn?)) (update :number-of-turns inc))
                                (cond-> (not extra-turn?) (dissoc :previous-turn-was-yours?)))))]
    (-> game
        (update-in [:players player-no] clean-up-player)
        (set-approx-discard-size player-no)
        (draw {:player-no player-no :arg number-of-cards})
        (update :players (partial mapv (fn [player] (dissoc player :revealed-cards))))
        (dissoc :cost-reductions)
        check-game-ended
        check-stack)))

(effects/register {:clean-up clean-up})

(defn clear-at-end-turn-effects [game {:keys [player-no card-id]}]
  (-> game
      (ut/update-in-vec [:players player-no :play-area] {:id card-id} dissoc :at-end-turn)))

(effects/register {:clear-at-end-turn-effects clear-at-end-turn-effects})

(defn end-turn [{:keys [effect-stack players] :as game} player-no]
  (assert (empty? effect-stack) "You can't end your turn when you have a choice to make.")
  (let [at-end-turn-cards (->> (get-in game [:players player-no :play-area])
                               (filter (comp not-empty :at-end-turn)))]
    (if (not-empty at-end-turn-cards)
      (let [do-at-end-turn-effect (fn [game {:keys [id at-end-turn]}]
                                    (-> game
                                        (cond-> at-end-turn (push-effect-stack {:player-no player-no
                                                                                :card-id   id
                                                                                :effects   (concat at-end-turn
                                                                                                   [[:clear-at-end-turn-effects]])}))))]
        (-> (reduce do-at-end-turn-effect game (reverse at-end-turn-cards))
            check-stack))
      (let [next-player (mod (inc player-no) (count players))]
        (-> game
            (push-effect-stack {:player-no next-player
                                :effects   [[:start-turn]]})
            (push-effect-stack {:player-no player-no
                                :effects   [[:clean-up]]})
            check-stack)))))
