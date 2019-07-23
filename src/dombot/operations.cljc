(ns dombot.operations
  (:require [dombot.utils :as ut]
            [dombot.effects :as effects]
            [clojure.set]))

(defn game-ended? [game]
  (let [{province-pile-size :pile-size} (ut/get-pile-idx game :province)
        {colony-pile-size :pile-size} (ut/get-pile-idx game :colony)]
    (or (and province-pile-size (zero? province-pile-size))
        (and colony-pile-size (zero? colony-pile-size))
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

(defn- get-trigger-effects [triggers]
  (if (< 1 (count triggers))
    [[:give-choice {:text    "Multiple things happen at the start of your turn. Select which one happens next."
                    :choice  [:simultaneous-effects-choice {:triggers triggers}]
                    :options (concat [:mixed]
                                     (map :name triggers))
                    :min     1
                    :max     1}]]
    (mapcat :effects triggers)))

(defn simultaneous-effects-choice [game {:keys [player-no triggers choice]}]
  (let [[{:keys [effects]} & more-triggers] (->> triggers
                                                 (sort-by (comp not #{choice} :name)))]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat effects
                                                (get-trigger-effects more-triggers))})))

(effects/register {:simultaneous-effects-choice simultaneous-effects-choice})

(defn at-start-turn-effects [game {:keys [player-no]}]
  (let [duration-cards (->> (get-in game [:players player-no :play-area])
                            (filter (comp not-empty :at-start-turn)))
        start-turn-triggers (->> (get-in game [:players player-no :triggers])
                                 (filter (comp #{:at-start-turn} :trigger)))
        auto-triggers (filter (comp #{:auto} :simultaneous-mode) start-turn-triggers)
        manual-triggers (filter (comp #{:manual} :simultaneous-mode) start-turn-triggers)
        do-duration-effect (fn [game {:keys [id at-start-turn]}]
                             (-> game
                                 (ut/update-in-vec [:players player-no :play-area] {:id id} dissoc :at-start-turn)
                                 (cond-> at-start-turn (push-effect-stack {:player-no player-no
                                                                           :card-id   id
                                                                           :effects   (apply concat at-start-turn)}))))]
    (assert (every? :simultaneous-mode start-turn-triggers) (str "Trigger error: Some triggers miss a simultaneous mode: "
                                                                 (->> start-turn-triggers (remove :simultaneous-mode) (map :name) (clojure.string/join ", "))))
    (-> (reduce do-duration-effect game (reverse duration-cards))
        (as-> game
              (push-effect-stack game {:player-no player-no
                                       :effects   (concat
                                                    (mapcat :effects auto-triggers)
                                                    (get-trigger-effects manual-triggers))})))))

(effects/register {:at-start-turn at-start-turn-effects})

(def phase-order [:out-of-turn
                  :action
                  :pay
                  :buy
                  :clean-up
                  :out-of-turn])

(defn next-phase [phase]
  (let [phase-index (->> phase-order
                         (keep-indexed (fn [idx p]
                                         (when (= p phase) idx)))
                         first)]
    (assert phase-index (str "Phase " phase " is not placed in the phase order."))
    (get phase-order (inc phase-index))))

(defn set-phase [game {:keys [player-no phase]}]
  (let [current-phase (get-in game [:players player-no :phase])]
    (if (and current-phase (not= current-phase phase))
      (let [next-phase (next-phase current-phase)
            phase-change (cond (#{:pay} next-phase) :at-start-buy
                               (#{:buy} current-phase) :at-end-buy)
            phase-change-effects (->> (get-in game [:players player-no :triggers])
                                      (filter (comp #{phase-change} :trigger))
                                      (mapcat :effects))]
        (-> game
            (assoc-in [:players player-no :phase] next-phase)
            (push-effect-stack {:player-no player-no
                                :effects   (concat phase-change-effects
                                                   (when (not= next-phase phase)
                                                     [[:set-phase {:phase phase}]]))})))
      game)))

(effects/register {:set-phase set-phase})

(defn start-turn
  ([player]
   (-> player
       (assoc :actions 1
              :coins 0
              :buys 1)
       (dissoc :gained-cards)))
  ([game {:keys [player-no]}]
   (if (game-ended? game)
     game
     (-> game
         (assoc :current-player player-no)
         (update-in [:players player-no] start-turn)
         (push-effect-stack {:player-no player-no
                             :effects   [[:set-phase {:phase :action}]
                                         [:at-start-turn]]})
         check-stack))))

(effects/register {:start-turn start-turn})

(defn spend-coffer [{:keys [effect-stack] :as game} player-no]
  (assert (empty? effect-stack) "You can't spend Coffers when you have a choice to make.")
  (let [{:keys [phase coffers]} (get-in game [:players player-no])]
    (when phase
      (assert (#{:action :pay} phase)
              (str "You can't spend Coffers when you're in the " (ut/format-name phase) " phase.")))
    (assert (and coffers (pos? coffers)) "You have no Coffers to spend.")
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   [[:set-phase {:phase :pay}]
                                        [:remove-coffers 1]
                                        [:give-coins 1]]})
        check-stack)))

(defn spend-villager [{:keys [effect-stack] :as game} player-no]
  (assert (empty? effect-stack) "You can't spend Villagers when you have a choice to make.")
  (let [{:keys [phase villagers]} (get-in game [:players player-no])]
    (when phase
      (assert (#{:action} phase)
              (str "You can't spend Villagers when you're in the " (ut/format-name phase) " phase.")))
    (assert (and villagers (pos? villagers)) "You have no Villagers to spend.")
    (-> game
        (update-in [:players player-no :villagers] dec)
        (update-in [:players player-no :actions] inc))))

(defn remove-trigger [game player-no trigger]
  (-> game
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:trigger trigger})
                                                                            (comp #{:once} :duration))))
      (update-in [:players player-no] ut/dissoc-if-empty :triggers)))

(defn- apply-triggers
  ([game {:keys [player-no trigger] :as args}]
   (apply-triggers game player-no trigger args))
  ([game player-no trigger & [args]]
   (let [triggers (get-in game [:players player-no :triggers])
         apply-trigger (fn [game {:keys [card-id effects]}] (push-effect-stack game {:player-no player-no
                                                                                     :card-id   card-id
                                                                                     :effects   effects
                                                                                     :args      args}))
         matching-triggers (filter (comp #{trigger} :trigger) triggers)]
     (-> (reduce apply-trigger game (reverse matching-triggers))
         (remove-trigger player-no trigger)))))

(effects/register {:apply-triggers apply-triggers})

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
            revealed-cards (update-in [:players player-no :revealed-cards] dissoc area)
            (= [area] (keys revealed-cards)) (update-in [:players player-no] dissoc :revealed-cards))))

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
  (if (#{:supply :extra-cards} from)
    (let [{:keys [card idx pile-size]} (ut/get-pile-idx game from card-name)]
      (when (and pile-size (pos? pile-size))
        {:card      (ut/give-id! card)
         :from-path from
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
  (if (#{:supply :extra-cards} from-path)
    (update-in game [from-path idx :pile-size] dec)
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

(defn- add-effect-args [new-args [effect args]]
  [effect (cond
            (map? args) (merge new-args args)
            args (merge new-args {:arg args})
            :else new-args)])

(defn handle-on-gain [{:keys [track-gained-cards? current-player] :as game}
                      {:keys [player-no gained-card-id from bought]
                       :or   {from :supply}
                       :as   args}]
  (let [{{:keys [name on-gain cost] :as card} :card} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})
        {:keys [hand play-area]} (get-in game [:players player-no])
        reaction-effects (->> hand
                              (mapcat (comp :on-gain :reaction))
                              (map (partial add-effect-args {:gained-card-id gained-card-id})))
        token-effects (when (= :supply from)
                        (->> (ut/get-pile-idx game name)
                             :tokens
                             (mapcat :on-gain)
                             (map (partial add-effect-args {:card-name name}))))
        while-in-play-effects (->> play-area
                                   (mapcat (comp :on-gain :while-in-play))
                                   (map (partial add-effect-args {:gained-card-id gained-card-id})))]
    (if card
      (cond-> game
              (or (not-empty reaction-effects)
                  on-gain
                  (not-empty token-effects)
                  (not-empty while-in-play-effects)) (push-effect-stack (merge args {:effects (concat reaction-effects
                                                                                                      (map (partial add-effect-args args) on-gain)
                                                                                                      token-effects
                                                                                                      while-in-play-effects)}))
              :always (apply-triggers player-no :on-gain args)
              (and track-gained-cards?
                   (= current-player player-no)) (update-in [:players player-no :gained-cards]
                                                            concat [(merge {:name  name
                                                                            :cost  cost
                                                                            :types (ut/get-types game card)}
                                                                           (when bought {:bought true}))]))
      game)))

(declare move-card)

(defn finalize-gain [game {:keys [player-no gained-card-id to to-position]
                           :or   {to :discard}}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})]
    (cond-> game
            card (-> (move-card {:player-no     player-no
                                 :move-card-id gained-card-id
                                 :from          :gaining
                                 :to            to
                                 :to-position   to-position})
                     (state-maintenance player-no :gaining to)))))

(defn gain [game {:keys [player-no card-name from to]
                  :or   {from :supply
                         to   :discard}
                  :as   args}]
  (if card-name
    (let [{:keys [card from-path idx]} (get-card game {:player-no player-no
                                                       :card-name card-name
                                                       :from      from})
          gain-args (merge args
                           {:gained-card-id (:id card)})]
      (cond-> game
              card (-> (remove-card from-path idx)
                       (add-card [:players player-no :gaining] :top card)
                       (push-effect-stack (merge args {:effects [[:on-gain gain-args]
                                                                 [:finalize-gain gain-args]]}))
                       check-stack)))
    game))

(defn overpay-choice [game {:keys [player-no amount effect]}]
  (if (pos? amount)
    (-> game
        (update-in [:players player-no :coins] - amount)
        (push-effect-stack {:player-no player-no
                            :effects   [[effect {:amount amount}]]}))
    game))

(effects/register {:on-gain        handle-on-gain
                   :finalize-gain  finalize-gain
                   :gain           gain
                   :overpay-choice overpay-choice})

(defn buy-card [{:keys [effect-stack] :as game} player-no card-name]
  (let [{:keys [buys coins phase play-area]} (get-in game [:players player-no])
        {:keys [card pile-size tokens] :as supply-pile} (ut/get-pile-idx game card-name)
        cost (ut/get-buy-cost game player-no card)
        {:keys [on-buy overpay]} card
        overpay-effects (when (and overpay (pos? (- coins cost)))
                          [[:give-choice {:text    (str "You may overpay for your " (ut/format-name card-name) ". Choose amount:")
                                          :choice  [:overpay-choice {:effect overpay}]
                                          :options [:overpay]
                                          :min     1
                                          :max     1}]])
        token-effects (->> tokens
                           (mapcat :on-buy))
        while-in-play-effects (->> play-area
                                   (mapcat (comp :on-buy :while-in-play))
                                   (map (partial add-effect-args {:card-name card-name})))]
    (assert (empty? effect-stack) "You can't buy cards when you have a choice to make.")
    (assert (and buys (> buys 0)) "Buy error: You have no more buys.")
    (assert supply-pile (str "Buy error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (assert (and coins cost (>= coins cost)) (str "Buy error: " (ut/format-name card-name) " costs " cost " and you only have " coins " coins."))
    (assert (and pile-size (pos? pile-size)) (str "Buy error: " (ut/format-name card-name) " supply is empty."))
    (assert (ut/card-buyable? game player-no card) (str (ut/format-name card-name) " can't be bought."))
    (when phase
      (assert (#{:action :pay :buy} phase) (str "You can't buy cards when you're in the " (ut/format-name phase) " phase.")))
    (-> game
        (update-in [:players player-no :coins] - cost)
        (update-in [:players player-no :buys] - 1)
        (push-effect-stack {:player-no player-no
                            :effects   (concat [[:set-phase {:phase :buy}]]
                                               overpay-effects
                                               on-buy
                                               token-effects
                                               while-in-play-effects
                                               [[:gain {:card-name card-name
                                                        :bought    true}]])})
        check-stack)))

(defn buy-project [{:keys [effect-stack] :as game} player-no project-name]
  (let [{:keys [buys coins phase]} (get-in game [:players player-no])
        {:keys [cost trigger on-buy participants] :as project} (get-in game [:projects project-name])]
    (assert (empty? effect-stack) "You can't buy cards when you have a choice to make.")
    (assert (and buys (> buys 0)) "Buy error: You have no more buys.")
    (assert project (str "Buy error: The project " (ut/format-name project-name) " isn't in the game."))
    (assert (and coins cost (>= coins cost)) (str "Buy error: " (ut/format-name project-name) " costs " cost " and you only have " coins " coins."))
    (assert (not-any? (comp #{player-no} :player-no) participants) (str "Buy error: You already participate in the project " (ut/format-name project-name) "."))
    (when phase
      (assert (#{:action :pay :buy} phase) (str "You can't buy projects when you're in the " (ut/format-name phase) " phase.")))
    (-> game
        (update-in [:players player-no :coins] - cost)
        (update-in [:players player-no :buys] - 1)
        (update-in [:projects project-name :participants] (comp vec conj) {:player-no player-no})
        (cond-> trigger (update-in [:players player-no :triggers] concat [(merge {:name     project-name
                                                                                  :duration :game}
                                                                                 trigger)])
                on-buy (push-effect-stack {:player-no player-no
                                           :effects   on-buy}))
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
                    (mapcat :before-shuffle))
        after (->> discard
                   (mapcat :after-shuffle))
        on-shuffle (->> (get-in game [:players player-no :triggers])
                        (filter (comp #{:on-shuffle} :trigger))
                        (mapcat :effects))]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat before
                                                [[:do-shuffle]]
                                                on-shuffle
                                                after)})))

(effects/register {:do-shuffle do-shuffle
                   :shuffle    shuffle-discard})

(defn peek-deck [game {:keys [player-no arg]}]
  (let [{:keys [deck discard]} (get-in game [:players player-no])]
    (cond-> game
            (and (< (count deck) arg) (not-empty discard)) (shuffle-discard {:player-no player-no}))))

(effects/register {:peek-deck peek-deck})

(defn do-move-card [game {:keys [player-no card-name from to to-position to-player] :as args}]
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

(defn handle-on-trash [game {:keys [player-no card-name] :as args}]
  (let [{{:keys [on-trash]} :card} (ut/get-card-idx game [:trash] {:name card-name})
        on-trash-triggers (->> (get-in game [:players player-no :triggers])
                               (filter (comp #{:on-trash} :trigger))
                               (mapcat :effects)
                               (map (partial add-effect-args args)))
        on-trash-effects (concat on-trash-triggers on-trash)]
    (cond-> game
            (not-empty on-trash-effects) (push-effect-stack (merge args {:effects on-trash-effects})))))

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
                                            [:on-trash args])
                                          (when (= to :revealed)
                                            [:on-reveal {:card-name card-name}])]})
          check-stack))))

(defn move-cards [game {:keys [player-no card-name card-names number-of-cards from-position] :as args}]
  (assert (or card-name
              card-names
              (and number-of-cards from-position)) (str "Can't move unspecified cards: " args))
  (if number-of-cards
    (cond-> game
            (pos? number-of-cards)
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
  (let [player-no (or player-no 0)
        player-nos (cond-> (->> (range 1 (count players))
                                (map (fn [n] (-> n (+ player-no) (mod (count players)))))
                                (remove (fn [n] (and attack (is-unaffected? game n)))))
                           (not at-once) reverse
                           all (concat [player-no]))
        effects (cond->> effects
                         attack (map (partial add-effect-args {:attacking-player-no player-no})))]
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
                   :overpay :amount
                   :special :choice
                   :mixed :choice
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
                   :overpay :amount
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
        valid-choices (->> options
                           (map (fn [{:keys [option] :as option-data}]
                                  (or option
                                      option-data)))
                           set)]
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
             {:reveal-discard? true}))
    {:source name}))

(defn- do-reveal-discard [game player-no reveal-discard?]
  (let [discard-count (-> (get-in game [:players player-no :discard]) count)]
    (cond-> game
            (and reveal-discard?
                 (< 1 discard-count)) (-> (set-approx-discard-size player-no discard-count)
                                          (assoc-in [:players player-no :revealed-cards :discard] discard-count)))))

(defn give-choice [{:keys [mode] :as game} {:keys                 [player-no card-id min max optional?]
                                            [opt-name & opt-args] :options
                                            :as                   choice}]
  (let [opt-fn (effects/get-option opt-name)
        options (apply opt-fn game player-no card-id opt-args)
        {:keys [source reveal-discard?]} (get-source choice)
        {:keys [min max] :as choice} (-> choice
                                         (assoc :options options
                                                :source source)
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
        (do-reveal-discard player-no reveal-discard?)
        check-stack)))

(effects/register {:give-choice give-choice})

(def reaction-choice [[:give-choice {:text    (str "You may reveal a Reaction to react to the Attack.")
                                     :choice  :reveal-reaction
                                     :options [:player :hand {:type      :reaction
                                                              :reacts-to :attack}]
                                     :max     1}]])

(defn reveal-reaction [game {:keys [player-no card-name]}]
  (if card-name
    (let [{{:keys [id reaction]} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})] ; TODO: Handle reactions that are not in hand
      (assert reaction (str "Revealed Reaction " card-name " has no reaction effects specified."))
      (-> game
          (push-effect-stack {:player-no player-no
                              :card-id   id
                              :effects   (concat reaction reaction-choice)})))
    game))

(effects/register {:reveal-reaction reveal-reaction})

(defn card-effect [{:keys [track-played-actions?] :as game} {:keys [player-no card]}]
  (let [{:keys [id effects coin-value duration]} card
        types (ut/get-types game card)]
    (cond-> game
            (and (:action types)
                 track-played-actions?) (update-in [:players player-no :actions-played] concat [id])
            (:attack types) (affect-other-players {:player-no player-no
                                                   :effects   [[:clear-unaffected {:works :once}]]})
            :always (push-effect-stack {:player-no player-no
                                        :card-id   id
                                        :effects   (concat (when coin-value
                                                             [[:give-coins coin-value]])
                                                           effects)})
            (:attack types) (affect-other-players {:player-no player-no
                                                   :effects   reaction-choice})
            duration (ut/update-in-vec [:players player-no :play-area] {:id id} update :at-start-turn concat [duration]))))

(effects/register {:card-effect card-effect})

(defn play
  ([game {:keys [player-no card-name]}]
   (play game player-no card-name))
  ([{:keys [effect-stack] :as game} player-no card-name]
   (let [{:keys [phase actions actions-played triggers]
          :or   {phase :action}} (get-in game [:players player-no])
         {{:keys [effects coin-value] :as card} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
         types (ut/get-types game card)
         play-type (cond
                     (and (#{:action} phase) (:action types) (pos? actions)) :action
                     (and (#{:action :pay} phase) (:treasure types)) :treasure)]
     (assert (-> effect-stack first :choice not) "You can't play cards when you have a choice to make.")
     (assert card (str "Play error: There is no " (ut/format-name card-name) " in your Hand."))
     (assert types (str "Play error: " (ut/format-name card-name) " has no types."))
     (case play-type
       :action (assert effects (str "Play error: " (ut/format-name card-name) " has no effect."))
       :treasure (assert (or coin-value effects) (str "Play error: " (ut/format-name card-name) " has no coin value or effects."))
       (assert false (str "Play error: You can't play " (ut/format-types types) " cards"
                          " when you're in the " (ut/format-name phase) " phase"
                          (when (and (#{:action} phase) (:action types))
                            (str " and have " actions " actions left"))
                          ".")))
     (-> game
         (cond-> (= :action play-type) (update-in [:players player-no :actions] - 1))
         (push-effect-stack {:player-no player-no
                             :effects   (concat [(when (= :treasure play-type)
                                                   [:set-phase {:phase :pay}])
                                                 [:move-card {:card-name card-name
                                                              :from      :hand
                                                              :to        :play-area}]
                                                 [:card-effect {:card card}]]
                                                (when (some (comp #{[:play card-name]} :trigger) triggers)
                                                  [[:apply-triggers {:trigger [:play card-name]}]])
                                                (when (and (:action types)
                                                           (empty? actions-played)
                                                           (some (comp #{:play-first-action} :trigger) triggers))
                                                  [[:apply-triggers {:trigger :play-first-action
                                                                     :card    card}]]))})
         check-stack))))

(effects/register {:play play})

(defn play-treasures [game {:keys [player-no]}]
  (let [{:keys [hand]} (get-in game [:players player-no])
        treasures (->> hand
                       (filter (comp :treasure (partial ut/get-types game)))
                       (sort-by (fn [{:keys [auto-play-index]}] (or auto-play-index 0)))
                       (map :name))]
    (if (not-empty treasures)
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   [[:set-phase {:phase :pay}]
                                          [:play {:card-name (first treasures)}]
                                          [:play-treasures]]})
          check-stack)
      game)))

(effects/register {:play-treasures play-treasures})

(defn- all-cards [{:keys [deck discard hand play-area island-mat native-village-mat]}]
  (let [cards (concat deck discard hand play-area island-mat native-village-mat)
        set-aside-cards (mapcat :set-aside cards)]
    (concat cards set-aside-cards)))

(defn- get-victory-points [cards {:keys [victory-points]}]
  (if (keyword? victory-points)
    (let [vp-fn (effects/get-effect victory-points)]
      (vp-fn cards))
    victory-points))

(defn calc-victory-points [{:keys [vp-tokens] :as player}]
  (let [cards (all-cards player)]
    (->> cards
         (filter :victory-points)
         (map (partial get-victory-points cards))
         (apply + (or vp-tokens 0)))))

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
                                (dissoc :hand
                                        :actions-played)
                                (update :play-area (partial filter ut/stay-in-play))
                                (ut/dissoc-if-empty :play-area)
                                (assoc :actions 0
                                       :coins 0
                                       :buys 0
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
        (dissoc :cost-reductions :unbuyable-cards))))

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
                                  {:effects (concat [[:set-phase {:phase :clean-up}]]
                                                    at-clean-up-triggers
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
