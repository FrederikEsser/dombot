(ns dombot.operations
  (:require [dombot.utils :as ut]
            [dombot.effects :as effects]
            [clojure.set]))

(defn game-ended? [game]
  (let [{province-pile-size :pile-size} (ut/get-pile-idx game :province)]
    (or (and province-pile-size (zero? province-pile-size))
        (>= (ut/empty-supply-piles game) 3))))

(defn push-effect-stack [game {:keys [player-no card-id effects choice args]}]
  (cond-> game
          (or (not-empty effects)
              choice) (update :effect-stack (partial concat (cond effects (->> effects
                                                                               (map (fn [effect]
                                                                                      (when effect
                                                                                        (merge {:player-no player-no
                                                                                                :effect    effect}
                                                                                               (when card-id
                                                                                                 {:card-id card-id})
                                                                                               (when args
                                                                                                 {:args args})))))
                                                                               (remove nil?))
                                                                  choice [(merge choice
                                                                                 {:player-no player-no}
                                                                                 (when card-id
                                                                                   {:card-id card-id}))])))))

(defn pop-effect-stack [{:keys [effect-stack] :as game}]
  (if (= 1 (count effect-stack))
    (dissoc game :effect-stack)
    (update game :effect-stack (partial drop 1))))

(defn do-effect [game {:keys              [player-no card-id args]
                       [name inline-args] :effect}]
  (let [effect-fn (effects/get-effect name)
        args (merge {:player-no player-no}
                    args
                    (when card-id
                      {:card-id card-id})
                    (cond (map? inline-args) inline-args
                          inline-args {:arg inline-args}))]
    (effect-fn game args)))

(defn check-stack [game]
  (let [[{:keys [player-no card-id effect args] :as top}] (get game :effect-stack)]
    (cond-> game
            effect (-> pop-effect-stack
                       (do-effect {:player-no player-no
                                   :card-id   card-id
                                   :effect    effect
                                   :args      args})
                       check-stack))))

(defn at-start-turn-effects [game player-no]
  (let [duration-cards (->> (get-in game [:players player-no :play-area])
                            (filter (comp not-empty :at-start-turn)))
        start-turn-effects (->> (get-in game [:players player-no :triggers])
                                (filter (comp #{:at-start-turn} :trigger))
                                (mapcat :effects))
        do-duration-effect (fn [game {:keys [id at-start-turn]}]
                             (-> game
                                 (ut/update-in-vec [:players player-no :play-area] {:id id} dissoc :at-start-turn)
                                 (cond-> at-start-turn (push-effect-stack {:player-no player-no
                                                                           :card-id   id
                                                                           :effects   (apply concat at-start-turn)}))))]
    (-> (reduce do-duration-effect game (reverse duration-cards))
        (push-effect-stack {:player-no player-no
                            :effects   start-turn-effects}))))

(defn start-turn
  ([player]
   (-> player
       (assoc :actions 1
              :coins 0
              :buys 1
              :phase :action)
       (dissoc :gained-cards)))
  ([game {:keys [player-no]}]
   (if (game-ended? game)
     game
     (-> game
         (assoc :current-player player-no)
         (update-in [:players player-no] start-turn)
         (at-start-turn-effects player-no)
         check-stack))))

(effects/register {:start-turn start-turn})

(defn spend-coffer [{:keys [effect-stack] :as game} player-no]
  (assert (empty? effect-stack) "You can't spend Coffers when you have a choice to make.")
  (let [{:keys [phase coffers]} (get-in game [:players player-no])]
    (when phase
      (assert (#{:action :pay} phase)
              (str "You can't spend Coffers when you're in the " (ut/format-name phase) " phase.")))
    (assert (and coffers (< 0 coffers)) "You have no Coffers to spend.")
    (-> game
        (cond-> phase (assoc-in [:players player-no :phase] :pay))
        (update-in [:players player-no :coffers] dec)
        (update-in [:players player-no :coins] inc))))

(defn spend-villager [{:keys [effect-stack] :as game} player-no]
  (assert (empty? effect-stack) "You can't spend Villagers when you have a choice to make.")
  (let [{:keys [phase villagers]} (get-in game [:players player-no])]
    (when phase
      (assert (#{:action} phase)
              (str "You can't spend Villagers when you're in the " (ut/format-name phase) " phase.")))
    (assert (and villagers (< 0 villagers)) "You have no Villagers to spend.")
    (-> game
        (update-in [:players player-no :villagers] dec)
        (update-in [:players player-no :actions] inc))))

(defn remove-trigger [game player-no trigger]
  (-> game
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:trigger trigger})
                                                                            (comp #{:once} :duration))))
      (update-in [:players player-no] ut/dissoc-if-empty :triggers)))

(defn- apply-triggers [game player-no trigger & [args]]
  (let [triggers (get-in game [:players player-no :triggers])
        apply-trigger (fn [game {:keys [card-id effects]}] (push-effect-stack game {:player-no player-no
                                                                                    :card-id   card-id
                                                                                    :effects   effects
                                                                                    :args      args}))
        matching-triggers (filter (comp #{trigger} :trigger) triggers)]
    (-> (reduce apply-trigger game (reverse matching-triggers))
        (remove-trigger player-no trigger))))

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

(defn- get-card [game {:keys [player-no card-name move-card-id from from-position] :as args}]
  (assert (or card-name move-card-id from-position) (str "Can't move unspecified card: " args))
  (if (= :supply from)
    (let [{:keys [card idx pile-size]} (ut/get-pile-idx game card-name)]
      (assert pile-size (str "Gain error: The supply doesn't have a " (ut/format-name card-name) " pile."))
      (when (< 0 pile-size)
        {:card      (ut/give-id! card)
         :from-path :supply
         :idx       idx}))
    (let [player (get-in game [:players player-no])
          from-path (case from
                      :trash [:trash]
                      [:players player-no from])]
      (merge {:from-path from-path}
             (case from-position
               :bottom {:idx (dec (count (get player from))) :card (last (get player from))}
               :top {:idx 0 :card (first (get player from))}
               (cond
                 move-card-id (ut/get-card-idx game from-path {:id move-card-id})
                 card-name (ut/get-card-idx game from-path {:name card-name})
                 :else {:idx 0 :card (first (get player from))}))))))

(defn- remove-card [game from-path idx]
  (if (= :supply from-path)
    (update-in game [:supply idx :pile-size] dec)
    (update-in game from-path ut/vec-remove idx)))

(defn- add-card [game to-path to-position {:keys [name] :as card}]
  (let [add-card-to-coll (fn [coll card]
                           (let [coll (vec coll)]
                             (if (empty? coll)
                               [card]
                               (cond
                                 (= :top to-position) (concat [card] coll)
                                 (integer? to-position) (concat (subvec coll 0 to-position)
                                                                [card]
                                                                (subvec coll to-position (count coll)))
                                 :else (concat coll [card])))))]
    (if (= :supply to-path)
      (let [{:keys [idx]} (ut/get-pile-idx game name)]
        (update-in game [:supply idx :pile-size] inc))
      (update-in game to-path add-card-to-coll card))))

(defn handle-on-gain [{:keys [track-gained-cards? current-player] :as game}
                      {:keys [player-no to to-position bought]
                       :or   {to          :discard
                              to-position :bottom}
                       :as   args}]
  (let [new-args (merge args {:from          to
                              :from-position to-position})
        {{:keys [on-gain id] :as card} :card} (get-card game new-args)]
    (cond-> game
            :always (apply-triggers player-no :on-gain (assoc new-args :gained-card-id id))
            on-gain (push-effect-stack (merge args {:effects on-gain}))
            (and track-gained-cards?
                 (= current-player player-no)) (update-in [:players player-no :gained-cards]
                                                          concat [(merge (select-keys card [:name :types :cost])
                                                                         (when bought {:bought true}))]))))

(defn do-gain [game {:keys [player-no card-name from to to-position]
                     :or   {from :supply
                            to   :discard}}]
  (assert card-name "No card-name specified for gain.")
  (let [{:keys [card from-path idx]} (get-card game {:player-no player-no
                                                     :card-name card-name
                                                     :from      from})]
    (cond-> game
            card (-> (remove-card from-path idx)
                     (add-card [:players player-no to] to-position card)
                     (state-maintenance player-no :supply to)))))

(defn gain [game args]
  (-> game
      (push-effect-stack (merge args {:effects [[:do-gain args]
                                                [:on-gain args]]}))
      check-stack))

(effects/register {:do-gain do-gain
                   :on-gain handle-on-gain
                   :gain    gain})

(defn buy-card [{:keys [effect-stack] :as game} player-no card-name]
  (let [{:keys [buys coins phase]} (get-in game [:players player-no])
        {:keys [card pile-size triggers] :as supply-pile} (ut/get-pile-idx game card-name)
        cost (ut/get-cost game card)
        trigger-effects (->> triggers
                             (filter (comp #{:on-buy} :trigger))
                             (mapcat :effects))]
    (assert (empty? effect-stack) "You can't buy cards when you have a choice to make.")
    (assert (and buys (> buys 0)) "Buy error: You have no more buys.")
    (assert supply-pile (str "Buy error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (assert (and coins cost (>= coins cost)) (str "Buy error: " (ut/format-name card-name) " costs " cost " and you only have " coins " coins."))
    (assert (and pile-size (< 0 pile-size)) (str "Buy error: " (ut/format-name card-name) " supply is empty."))
    (when phase
      (assert (#{:action :pay :buy} phase) (str "You can't buy cards when you're in the " (ut/format-name phase) " phase.")))
    (-> game
        (cond-> phase (assoc-in [:players player-no :phase] :buy))
        (push-effect-stack {:player-no player-no
                            :effects   (concat trigger-effects
                                               [[:gain {:card-name card-name
                                                        :bought    true}]])})
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

(defn do-move-card [game {:keys [player-no card-name move-card-id from to to-position to-player] :as args}]
  (let [{:keys [card from-path idx]} (get-card game args)
        to-path (case to
                  :trash [:trash]
                  :supply :supply
                  [:players (or to-player player-no) to])]
    (when card-name
      (assert card (str "Move error: There is no " (ut/format-name card-name) " in " from-path ".")))
    (cond-> game
            card (-> (remove-card from-path idx)
                     (add-card to-path to-position card)
                     (state-maintenance player-no from to)))))

(defn handle-on-trash [game {:keys [card-name] :as args}]
  (let [{{:keys [on-trash]} :card} (ut/get-card-idx game [:trash] {:name card-name})]
    (cond-> game
            on-trash (push-effect-stack (merge args {:effects on-trash})))))

(defn handle-on-reveal [game {:keys [player-no card-name] :as args}]
  (let [{{:keys [on-reveal]} :card} (ut/get-card-idx game [:players player-no :revealed] {:name card-name})]
    (cond-> game
            on-reveal (push-effect-stack (merge args {:effects on-reveal})))))

(defn move-card [game {:keys [player-no from to] :as args}]
  (let [{:keys [deck discard]} (get-in game [:players player-no])
        {{card-name :name} :card} (get-card game args)]
    (if (and (= :deck from) (empty? deck) (not-empty discard))
      (push-effect-stack game {:player-no player-no
                               :effects   [[:shuffle]
                                           [:move-card args]]})
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   [[:do-move-card args]
                                          (when (= to :trash)
                                            [:on-trash {:card-name card-name}])
                                          (when (= to :revealed)
                                            [:on-reveal {:card-name card-name}])]})
          check-stack))))

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

(effects/register {:do-move-card do-move-card
                   :on-trash     handle-on-trash
                   :on-reveal    handle-on-reveal
                   :move-card    move-card
                   :move-cards   move-cards})

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

(defn- get-choice-fn [data]
  (let [{:keys [choice] :as result} (if (vector? data)
                                      {:choice (first data)
                                       :args   (second data)}
                                      {:choice data})]
    (merge result {:choice-fn (effects/get-effect choice)})))

(defn- choose-single [game valid-choices selection]
  (if (coll? selection)
    (assert (<= (count selection) 1) "Choose error: You can only pick 1 option."))
  (let [[{:keys [player-no attacker card-id choice source min optional?]}] (get game :effect-stack)
        {:keys [choice-fn args]} (get-choice-fn choice)
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
        (choice-fn (merge args
                          {:player-no player-no
                           :card-id   card-id
                           arg-name   single-selection}
                          (when attacker
                            {:attacker attacker}))))))

(defn- choose-multi [game valid-choices selection]
  (let [[{:keys [player-no attacker card-id choice source min max optional?]}] (get game :effect-stack)
        {:keys [choice-fn args]} (get-choice-fn choice)
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
        (choice-fn (merge args
                          {:player-no player-no
                           :card-id   card-id
                           arg-name   multi-selection}
                          (when attacker
                            {:attacker attacker}))))))

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

(defn get-source [{[name arg & [{:keys [id last]}]] :options}]
  (if (= :player name)
    (merge {:source arg}
           (when (and (= :discard arg)
                      (not (or id last)))
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

(defn card-effect [game {:keys                                          [player-no]
                         {:keys [id types effects coin-value duration]} :card}]
  (let [{:keys [actions-played]} (get-in game [:players player-no])]
    (cond-> game
            (and (:action types)
                 actions-played) (update-in [:players player-no :actions-played] inc)
            (:attack types) (affect-other-players {:player-no player-no
                                                   :effects   [[:clear-unaffected {:works :once}]]})
            :always (push-effect-stack {:player-no player-no
                                        :card-id   id
                                        :effects   (concat effects
                                                           (when coin-value
                                                             [[:give-coins coin-value]]))})
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
          (not-empty triggers) (apply-triggers player-no [:play card-name]))
        check-stack)))

(defn play-treasures [game player-no]
  (let [{:keys [hand]} (get-in game [:players player-no])
        treasures (->> hand
                       (filter (comp :treasure :types))
                       (sort-by :auto-play-index)
                       (map :name))]
    (reduce (fn [game card-name] (play game player-no card-name)) game treasures))) ; TODO: Stack treasures separately

(defn- all-cards [{:keys [deck discard hand play-area island-mat native-village-mat]}]
  (let [cards (concat deck discard hand play-area island-mat native-village-mat)
        set-aside-cards (mapcat :set-aside cards)]
    (concat cards set-aside-cards)))

(defn- get-victory-points [cards {:keys [victory-points]}]
  (if (keyword? victory-points)
    (let [vp-fn (effects/get-effect victory-points)]
      (vp-fn cards))
    victory-points))

(defn calc-victory-points [player]
  (let [cards (all-cards player)]
    (->> cards
         (filter :victory-points)
         (map (partial get-victory-points cards))
         (apply + 0))))

(def calc-score (juxt calc-victory-points (comp - :number-of-turns)))

(defn end-game-for-player [best-score player]
  (let [victory-points (calc-victory-points player)]
    (-> player
        (assoc :hand (all-cards player))
        (dissoc :deck :discard :play-area :island-mat :native-village-mat)
        (assoc :phase :end-of-game
               :victory-points victory-points
               :winner (= best-score (calc-score player))))))

(defn check-game-ended [{:keys [players] :as game} args]
  (cond-> game
          (game-ended? game) (as-> game
                                   (let [best-score (->> players
                                                         (map calc-score)
                                                         sort
                                                         last)]
                                     (update game :players (partial mapv (partial end-game-for-player best-score)))))))

(effects/register {:check-game-ended check-game-ended})

(defn do-clean-up [game {:keys [player-no extra-turn?]}]
  (let [clean-up-player (fn [{:keys [play-area hand number-of-turns] :as player}]
                          (let [used-cards (concat hand (remove ut/stay-in-play play-area))]
                            (-> player
                                (cond-> (not-empty used-cards) (update :discard concat used-cards))
                                (dissoc :hand)
                                (update :play-area (partial filter ut/stay-in-play))
                                (ut/dissoc-if-empty :play-area)
                                (assoc :actions 0
                                       :coins 0
                                       :buys 0
                                       :actions-played 0
                                       :phase :out-of-turn)
                                (update :triggers (partial remove (comp #{:once :turn} :duration)))
                                (ut/dissoc-if-empty :triggers)
                                (cond-> (and number-of-turns
                                             (not extra-turn?)) (update :number-of-turns inc))
                                (cond-> (not extra-turn?) (dissoc :previous-turn-was-yours?)))))]
    (-> game
        (update-in [:players player-no] clean-up-player)
        (set-approx-discard-size player-no)
        (update :players (partial mapv (fn [player] (dissoc player :revealed-cards))))
        (dissoc :cost-reductions))))

(defn at-clean-up-choice [game {:keys [player-no card-name]}]
  (let [{{:keys [at-clean-up id]} :card} (ut/get-card-idx game [:players player-no :play-area] {:name card-name})]
    (if card-name
      (-> game
          (ut/update-in-vec [:players player-no :play-area] {:name card-name} dissoc :at-clean-up)
          (push-effect-stack {:player-no player-no
                              :card-id   id
                              :effects   (concat at-clean-up
                                                 [[:at-clean-up]])}))
      (update-in game [:players player-no :play-area] (partial map #(dissoc % :at-clean-up))))))

(defn at-clean-up [game {:keys [player-no]}]
  (let [check-clean-up-effects (fn [{:keys [clean-up-pred] :as card}]
                                 (if clean-up-pred
                                   (let [pred-fn (effects/get-effect clean-up-pred)]
                                     (cond-> card
                                             (not (pred-fn game player-no)) (dissoc :at-clean-up)))
                                   card))]
    (-> game
        (update-in [:players player-no :play-area] (partial map check-clean-up-effects))
        (as-> game
              (let [card-names (->> (get-in game [:players player-no :play-area])
                                    (filter :at-clean-up)
                                    (map :name)
                                    set)]

                (give-choice game {:player-no player-no
                                   :text      "You may activate cards, that do something when you discard them from play."
                                   :choice    :at-clean-up-choice
                                   :options   [:player :play-area {:names card-names}]
                                   :max       1}))))))

(effects/register {:at-clean-up-choice at-clean-up-choice
                   :at-clean-up        at-clean-up})

(defn clean-up [game {:keys [player-no number-of-cards]
                      :or   {number-of-cards 5}
                      :as   args}]
  (let [at-clean-up-triggers (->> (get-in game [:players player-no :triggers])
                                  (filter (comp #{:at-clean-up} :trigger))
                                  (mapcat :effects))
        at-draw-hand-triggers (->> (get-in game [:players player-no :triggers])
                                   (filter (comp #{:at-draw-hand} :trigger))
                                   (mapcat :effects))]
    (-> game
        (push-effect-stack (merge args
                                  {:effects (concat at-clean-up-triggers
                                                    [[:at-clean-up]
                                                     [:do-clean-up args]
                                                     [:draw number-of-cards]]
                                                    at-draw-hand-triggers
                                                    [[:check-game-ended]])}))
        check-stack)))

(effects/register {:do-clean-up do-clean-up
                   :clean-up    clean-up})

(defn clear-at-end-turn-effects [game {:keys [player-no card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})]
    (cond-> game
            card (ut/update-in-vec [:players player-no :play-area] {:id card-id} dissoc :at-end-turn))))

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
