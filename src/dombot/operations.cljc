(ns dombot.operations
  (:require [dombot.utils :as ut]
            [dombot.effects :as effects]
            [clojure.set]
            [clojure.string :as string]))

(defn get-game-status [{:keys [players game-ending?] :as game}]
  (let [{province-pile-size :pile-size} (ut/get-pile-idx game :province)
        {colony-pile-size :pile-size} (ut/get-pile-idx game :colony)
        extra-turns? (->> players
                          (mapcat :triggers)
                          (some (comp #{:at-end-game} :event)))]
    (if (or (and province-pile-size (zero? province-pile-size))
            (and colony-pile-size (zero? colony-pile-size))
            (>= (ut/empty-supply-piles game) 3)
            game-ending?)
      (if extra-turns?
        :ending
        :finished)
      :active)))

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
        args      (merge {:player-no player-no}
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

(defn get-effects-from-trigger [{:keys [id effects card-id set-aside]}]
  (let [effect-args (merge {:trigger-id id
                            :card-id    card-id}
                           (when set-aside
                             {:set-aside set-aside}))]
    (concat (map (partial ut/add-effect-args effect-args) effects)
            [[:remove-trigger {:trigger-id id}]])))

(defn- get-trigger-effects [triggers]
  (let [complex?        (some (comp #{:complex} :mode) triggers)
        auto-triggers   (filter (fn [{:keys [mode]}]
                                  (or (nil? mode) (#{:auto (when-not complex? :semi)} mode))) triggers)
        manual-triggers (filter (comp #{(when complex? :semi) :manual :complex} :mode) triggers)
        trigger-names   (->> manual-triggers (map :name) set)]
    (concat (mapcat get-effects-from-trigger auto-triggers)
            (if (or (< 1 (count manual-triggers))
                    (some :optional? manual-triggers))
              (let [phase-change (->> triggers first :event)
                    text         (case phase-change
                                   :at-clean-up "You may activate cards, that do something when you discard them from play."
                                   :at-start-turn (str (-> (count manual-triggers)
                                                           ut/number->text
                                                           string/capitalize)
                                                       " things happen at the start of your turn. Select which one happens next.")
                                   "Do something!")]
                [[:give-choice (merge {:text    text
                                       :choice  [:simultaneous-effects-choice {:triggers manual-triggers}]
                                       :options [:mixed
                                                 [:player :play-area {:names trigger-names}]
                                                 [:player :tavern-mat {:names trigger-names}]
                                                 [:player :boons {:names trigger-names}]
                                                 [:artifacts {:names trigger-names}]
                                                 [:projects {:names trigger-names}]]
                                       :max     1}
                                      (when (not-every? :optional? manual-triggers)
                                        {:min 1}))]])
              (mapcat get-effects-from-trigger manual-triggers)))))

(defn simultaneous-effects-choice [game {:keys [player-no triggers choice]}]
  (if choice
    (let [[trigger & more-triggers] (->> triggers
                                         (sort-by (comp not #{(:card-name choice)} :name)))]
      (push-effect-stack game {:player-no player-no
                               :effects   (concat (get-effects-from-trigger trigger)
                                                  (get-trigger-effects more-triggers))}))
    game))

(effects/register {:simultaneous-effects-choice simultaneous-effects-choice})

(defn sync-repeated-play [game {:keys [player-no]}]
  (let [trigger-card-ids (->> (get-in game [:players player-no :triggers])
                              (keep :card-id)
                              set)]
    (-> game
        (update-in [:players player-no :repeated-play] (partial filter (comp trigger-card-ids :target)))
        (update-in [:players player-no] ut/dissoc-if-empty :repeated-play))))

(effects/register {:sync-repeated-play sync-repeated-play})

(defn get-call-trigger [{:keys [id name call]}]
  (merge call
         {:name      name
          :card-id   id
          :optional? true
          :effects   (concat [[:call-reserve {:card-id id}]]
                             (:effects call))}))

(defn- get-phase-change-effects [game {:keys [player-no phase-change]}]
  (let [card-triggers    (->> (get-in game [:players player-no :play-area])
                              (keep (fn [{:keys [id name trigger-condition] :as card}]
                                      (let [condition-fn         (if trigger-condition
                                                                   (effects/get-effect trigger-condition)
                                                                   (constantly true))
                                            phase-change-effects (get card phase-change)]
                                        (when (and phase-change-effects
                                                   (condition-fn game player-no))
                                          {:event     phase-change
                                           :name      name
                                           :card-id   id
                                           :mode      :manual
                                           :optional? true
                                           :effects   phase-change-effects})))))
        reserve-triggers (->> (get-in game [:players player-no :tavern-mat])
                              (filter (comp #{phase-change} :event :call))
                              (map get-call-trigger))
        triggers         (->> (get-in game [:players player-no :triggers])
                              (filter (comp #{phase-change} :event))
                              (filter (fn [{:keys [condition]}]
                                        (if condition
                                          (let [condition-fn (effects/get-effect condition)]
                                            (condition-fn game player-no))
                                          true)))
                              (concat card-triggers
                                      reserve-triggers))]
    (assert (every? :name triggers) (str "Trigger error. All triggers need a name. \n" (->> triggers
                                                                                            (remove :name)
                                                                                            (#?(:clj  clojure.pprint/pprint
                                                                                                :cljs cljs.pprint/pprint))
                                                                                            with-out-str)))
    (concat
      (get-trigger-effects triggers)
      [[:sync-repeated-play]])))

(def phase-order [:out-of-turn
                  :action
                  :pay
                  :buy
                  :night
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
      (let [next-phase   (next-phase current-phase)
            phase-change (cond (#{:action} next-phase) :at-start-turn
                               (#{:pay} next-phase) :at-start-buy
                               (#{:buy} current-phase) :at-end-buy
                               (#{:clean-up} next-phase) :at-clean-up
                               (#{:clean-up} current-phase) :at-draw-hand)]
        (-> game
            (assoc-in [:players player-no :phase] next-phase)
            (push-effect-stack {:player-no player-no
                                :effects   (concat (get-phase-change-effects game {:player-no    player-no
                                                                                   :phase-change phase-change})
                                                   (when (not= next-phase phase)
                                                     [[:set-phase {:phase phase}]]))})))
      game)))

(effects/register {:set-phase set-phase})

(declare end-turn)

(defn start-turn
  ([player]
   (-> player
       (assoc :actions 1
              :coins 0
              :buys 1)
       (dissoc :gained-cards)))
  ([game {:keys [player-no]}]
   (let [game-status (get-game-status game)
         extra-turn? (->> (get-in game [:players player-no :triggers])
                          (some (comp #{:at-end-game} :event)))]
     (cond (= :finished game-status) game
           (and (= :ending game-status)
                (not extra-turn?)) (end-turn game player-no)
           :else (-> game
                     (assoc :current-player player-no)
                     (update-in [:players player-no] start-turn)
                     (push-effect-stack {:player-no player-no
                                         :effects   (concat
                                                      (when (= :ending game-status)
                                                        [[:remove-triggers {:event :at-end-game}]])
                                                      [[:set-phase {:phase :action}]])})
                     check-stack)))))

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

(defn remove-trigger [game {:keys [player-no trigger-id card-id]}]
  (-> game
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:id trigger-id})
                                                                            (comp #{:once :once-turn} :duration))))
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:id trigger-id})
                                                                            (comp #{:until-empty} :duration)
                                                                            (comp empty? :set-aside))))
      (cond->
        card-id (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:card-id card-id})
                                                                                      (comp #{:attack} :duration)))))
      (update-in [:players player-no] ut/dissoc-if-empty :triggers)))

(defn remove-triggers [game {:keys [player-no event]}]
  (-> game
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:event event})
                                                                            (comp #{:once :once-turn} :duration))))
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:event event})
                                                                            (comp #{:until-empty} :duration)
                                                                            (comp empty? :set-aside))))
      (update-in [:players player-no] ut/dissoc-if-empty :triggers)))

(defn- apply-triggers
  ([game {:keys [player-no event] :as args}]
   (apply-triggers game player-no event args))
  ([game player-no event & [args]]
   (let [triggers          (get-in game [:players player-no :triggers])
         matching-triggers (cond->> (filter (comp #{event} :event) triggers)
                                    (= :instead-of-first-action event) (take-last 1)) ; only one effect should happen instead of "The first time you play an Action"
         apply-trigger     (fn [game {:keys [id card-id effects duration]}]
                             (push-effect-stack game {:player-no player-no
                                                      :card-id   card-id
                                                      :effects   (concat effects
                                                                         (when (#{:once :once-turn} duration)
                                                                           [[:remove-trigger {:trigger-id id}]]))
                                                      :args      args}))]
     (-> (reduce apply-trigger game (reverse matching-triggers))))))

(effects/register {:remove-trigger  remove-trigger
                   :remove-triggers remove-triggers
                   :apply-triggers  apply-triggers})

(defn set-approx-discard-size [game player-no & [n]]
  (let [{:keys [discard approx-discard-size]} (get-in game [:players player-no])
        size        (count discard)
        variance    (Math/round (/ size 4.0))
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
    (let [player    (get-in game [:players player-no])
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
    (update-in game [from-path idx] ut/remove-top-card)
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
    (if (#{:supply :extra-cards} to-path)
      (let [{:keys [idx]} (ut/get-pile-idx game to-path name #{:include-empty-split-piles})]
        (update-in game [to-path idx] ut/add-top-card card))
      (update-in game to-path add-card-to-coll card))))

(defn get-on-gain-effects [game player-no card-name]
  (let [{:keys [card tokens]} (ut/get-pile-idx game :supply card-name #{:include-empty-split-piles})
        {:keys [on-gain]} card
        types                 (ut/get-types game card)
        token-effects         (->> tokens
                                   vals
                                   (mapcat (fn [{:keys [number-of-tokens on-gain]}]
                                             (when on-gain
                                               (apply concat (repeat number-of-tokens on-gain))))))
        while-in-play-effects (->> (get-in game [:players player-no :play-area])
                                   (mapcat (comp :on-gain :while-in-play))
                                   (map (partial ut/add-effect-args {:card-name card-name})))
        trigger-effects       (->> (get-in game [:players player-no :triggers])
                                   (filter (fn [{:keys [event type]}]
                                             (and (= :on-gain event)
                                                  (or (not type)
                                                      (contains? types type)))))
                                   (mapcat (fn [{:keys [id card-id effects]}]
                                             (cond->> effects
                                                      card-id (map (partial ut/add-effect-args {:trigger-id id
                                                                                                :card-id    card-id}))))))]
    (concat on-gain while-in-play-effects trigger-effects token-effects)))

(defn handle-on-gain [{:keys [track-gained-cards? current-player] :as game}
                      {:keys [player-no gained-card-id from bought]
                       :or   {from :supply}
                       :as   args}]
  (let [{{:keys [name cost] :as card} :card} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})
        {:keys [hand]} (get-in game [:players player-no])
        reaction-effects (->> hand
                              (mapcat (comp :on-gain :reaction))
                              (map (partial ut/add-effect-args {:gained-card-id gained-card-id})))
        on-gain-effects  (->> (get-on-gain-effects game player-no name)
                              (map (partial ut/add-effect-args (merge args
                                                                      {:card-name      name
                                                                       :gained-card-id gained-card-id}))))]
    (if card
      (cond-> game
              (or (not-empty reaction-effects)
                  (not-empty on-gain-effects)) (push-effect-stack (merge args {:effects (concat reaction-effects
                                                                                                on-gain-effects
                                                                                                [[:remove-triggers {:event :on-gain}]])}))
              (and track-gained-cards?
                   (or (nil? current-player)
                       (= current-player player-no))) (update-in [:players player-no :gained-cards]
                                                                 concat [(merge {:name  name
                                                                                 :cost  cost
                                                                                 :types (ut/get-types game card)}
                                                                                (when bought {:bought true}))]))
      game)))

(declare move-card)

(defn finalize-gain [game {:keys [player-no gained-card-id to to-position]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})
        to (or to (:gain-to card) :discard)]
    (cond-> game
            card (-> (move-card {:player-no    player-no
                                 :move-card-id gained-card-id
                                 :from         :gaining
                                 :to           to
                                 :to-position  to-position})
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
                       (add-card [:players player-no :gaining] :top (dissoc card :face))
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

(defn get-on-buy-effects [game player-no card-name]
  (let [{:keys [card tokens]} (ut/get-pile-idx game card-name)
        {:keys [on-buy]} card
        token-effects         (->> tokens
                                   vals
                                   (mapcat (fn [{:keys [number-of-tokens on-buy]}]
                                             (when on-buy
                                               (apply concat (repeat number-of-tokens on-buy))))))
        while-in-play-effects (->> (get-in game [:players player-no :play-area])
                                   (mapcat (comp :on-buy :while-in-play))
                                   (map (partial ut/add-effect-args {:card-name card-name})))
        trigger-effects       (->> (get-in game [:players player-no :triggers])
                                   (filter (comp #{:on-buy} :event))
                                   (mapcat (fn [{:keys [id card-id effects]}]
                                             (->> effects
                                                  (map (partial ut/add-effect-args (merge {:trigger-id id
                                                                                           :card-name  card-name}
                                                                                          (when card-id
                                                                                            {:card-id card-id})))))))
                                   (map (partial ut/add-effect-args {})))]
    (concat on-buy token-effects while-in-play-effects trigger-effects)))

(defn buy-card
  ([game {:keys [player-no card-name]}]
   (buy-card game player-no card-name))
  ([{:keys [effect-stack] :as game} player-no card-name]
   (let [{:keys [buys coins debt phase]} (get-in game [:players player-no])
         coins-after-debt (if (and coins debt)
                            (- coins debt)
                            coins)
         {:keys [card pile-size] :as supply-pile} (ut/get-pile-idx game card-name)
         {:keys [coin-cost debt-cost]} (ut/get-buy-cost game player-no card)
         {:keys [overpay]} card
         on-buy-effects   (get-on-buy-effects game player-no card-name)
         overpay-effects  (when (and overpay (pos? (- coins-after-debt coin-cost)))
                            [[:give-choice {:text    (str "You may overpay for your " (ut/format-name card-name) ". Choose amount:")
                                            :choice  [:overpay-choice {:effect overpay}]
                                            :options [:overpay]
                                            :min     1
                                            :max     1}]])]
     (assert (empty? effect-stack) "You can't buy cards when you have a choice to make.")
     (assert (and buys (> buys 0)) "Buy error: You have no more buys.")
     (assert supply-pile (str "Buy error: The supply doesn't have a " (ut/format-name card-name) " pile."))
     (assert (and coins-after-debt coin-cost (>= coins-after-debt coin-cost)) (str "Buy error: " (ut/format-name card-name) " costs " coin-cost " and you only have " coins-after-debt " coins."))
     (assert (and pile-size (pos? pile-size)) (str "Buy error: " (ut/format-name card-name) " supply is empty."))
     (assert (ut/card-buyable? game player-no card) (str (ut/format-name card-name) " can't be bought."))
     (when phase
       (assert (#{:action :pay :buy} phase) (str "You can't buy cards when you're in the " (ut/format-name phase) " phase.")))
     (if (and phase (not= :buy phase))
       (-> game
           (push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :buy}]
                                           [:buy {:card-name card-name}]]})
           check-stack)
       (-> game
           (cond-> debt (-> (update-in [:players player-no :coins] - debt)
                            (update-in [:players player-no] dissoc :debt)))
           (update-in [:players player-no :coins] - coin-cost)
           (cond-> debt-cost (update-in [:players player-no :debt] ut/plus debt-cost))
           (update-in [:players player-no :buys] - 1)
           (push-effect-stack {:player-no player-no
                               :effects   (concat overpay-effects
                                                  on-buy-effects
                                                  [[:gain {:card-name card-name
                                                           :bought    true}]])})
           check-stack)))))

(effects/register {:buy buy-card})

(defn buy-project [{:keys [effect-stack] :as game} player-no project-name]
  (let [{:keys [buys coins debt phase]} (get-in game [:players player-no])
        coins-after-debt (if (and coins debt)
                           (- coins debt)
                           coins)
        {:keys [cost trigger on-buy participants] :as project} (get-in game [:projects project-name])]
    (assert (empty? effect-stack) "You can't buy projects when you have a choice to make.")
    (when phase
      (assert (#{:action :pay :buy} phase) (str "You can't buy projects when you're in the " (ut/format-name phase) " phase.")))
    (assert project (str "Buy error: The Project " (ut/format-name project-name) " isn't in the game."))
    (assert (and buys (> buys 0)) "Buy error: You have no more buys.")
    (assert (and coins-after-debt cost (>= coins-after-debt cost)) (str "Buy error: " (ut/format-name project-name) " costs " cost " and you only have " coins-after-debt " coins."))
    (assert (not-any? (comp #{player-no} :player-no) participants) (str "Buy error: You already participate in the project " (ut/format-name project-name) "."))
    (-> game
        (cond-> debt (-> (update-in [:players player-no :coins] - debt)
                         (update-in [:players player-no] dissoc :debt)))
        (update-in [:players player-no :coins] - cost)
        (update-in [:players player-no :buys] - 1)
        (update-in [:projects project-name :participants] (comp vec conj) {:player-no player-no})
        (push-effect-stack {:player-no player-no
                            :effects   (concat [[:set-phase {:phase :buy}]]
                                               (when trigger
                                                 [[:add-trigger {:trigger (merge {:name     project-name
                                                                                  :duration :game}
                                                                                 trigger)}]])
                                               on-buy)})
        check-stack)))

(defn buy-event [{:keys [effect-stack] :as game} player-no event-name]
  (let [{:keys [buys coins debt phase bought-events]} (get-in game [:players player-no])
        coins-after-debt (if (and coins debt)
                           (- coins debt)
                           coins)
        {:keys [cost on-buy once-per-turn] :as event} (get-in game [:events event-name])]
    (assert (empty? effect-stack) "You can't buy events when you have a choice to make.")
    (when phase
      (assert (#{:action :pay :buy} phase) (str "You can't buy events when you're in the " (ut/format-name phase) " phase.")))
    (assert event (str "Buy error: The Event " (ut/format-name event-name) " isn't in the game."))
    (assert (and buys (> buys 0)) "Buy error: You have no more buys.")
    (assert (and coins-after-debt cost (>= coins-after-debt cost)) (str "Buy error: " (ut/format-name event-name) " costs " cost " and you only have " coins-after-debt " coins."))
    (when once-per-turn
      (assert (not (contains? bought-events event-name)) (str "Buy error: Event " (ut/format-name event-name) " can only be bought once per turn.")))
    (-> game
        (cond-> debt (-> (update-in [:players player-no :coins] - debt)
                         (update-in [:players player-no] dissoc :debt))) (update-in [:players player-no :coins] - cost)
        (update-in [:players player-no :buys] - 1)
        (cond-> once-per-turn (update-in [:players player-no :bought-events] (comp set #(conj % event-name))))
        (push-effect-stack {:player-no player-no
                            :effects   (concat [[:set-phase {:phase :buy}]]
                                               on-buy)})
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
  (let [discard    (get-in game [:players player-no :discard])
        before     (->> discard
                        (mapcat :before-shuffle))
        after      (->> discard
                        (mapcat :after-shuffle))
        on-shuffle (->> (get-in game [:players player-no :triggers])
                        (filter (comp #{:on-shuffle} :event))
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
                  :extra-cards :extra-cards
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
                               (filter (comp #{:on-trash} :event))
                               (mapcat :effects)
                               (map (partial ut/add-effect-args args)))
        on-trash-effects  (concat on-trash-triggers on-trash)]
    (cond-> game
            (not-empty on-trash-effects) (push-effect-stack (merge args {:effects on-trash-effects})))))

(defn handle-on-reveal [game {:keys [player-no card-name] :as args}]
  (let [{{:keys [on-reveal]} :card} (ut/get-card-idx game [:players player-no :revealed] {:name card-name})]
    (cond-> game
            on-reveal (push-effect-stack (merge args {:effects on-reveal})))))

(defn handle-on-discard [game {:keys [player-no card-name] :as args}]
  (let [{{:keys [on-discard]} :card} (ut/get-card-idx game [:players player-no :discard] {:name card-name})]
    (cond-> game
            on-discard (push-effect-stack (merge args {:effects on-discard})))))

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
                                            [:on-trash (merge args {:card-name card-name})])
                                          (when (= to :revealed)
                                            [:on-reveal {:card-name card-name}])
                                          (when (and (= to :discard)
                                                     (not= from :gaining))
                                            [:on-discard {:card-name card-name}])]})
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
                   :on-discard   handle-on-discard
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

(defn affect-other-players [{:keys [players] :as game} {:keys [player-no card-id effects attack all at-once]}]
  (let [player-no  (or player-no 0)
        player-nos (cond-> (->> (range 1 (count players))
                                (map (fn [n] (-> n (+ player-no) (mod (count players)))))
                                (remove (fn [n] (and attack (is-unaffected? game n)))))
                           (not at-once) reverse
                           all (concat [player-no]))
        effects    (cond->> effects
                            attack (map (partial ut/add-effect-args {:attacking-player-no player-no})))]
    (reduce (fn [game other-player-no]
              (push-effect-stack game {:player-no other-player-no
                                       :effects   (cond->> effects
                                                           card-id (map (partial ut/add-effect-args {:card-id card-id})))}))
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
  (when (sequential? selection)
    (assert (<= (count selection) 1) "Choose error: You can only pick 1 option."))
  (let [[{:keys [player-no attacker card-id choice source min optional?]}] (get game :effect-stack)
        {:keys [choice-fn args]} (get-choice-fn choice)
        arg-name         (case source
                           :deck-position :position
                           :overpay :amount
                           :special :choice
                           :mixed :choice
                           :card-name)
        single-selection (if (sequential? selection)
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
  (let [[{:keys [player-no attacker card-id choice source min max optional? unique?]}] (get game :effect-stack)
        {:keys [choice-fn args]} (get-choice-fn choice)
        arg-name        (case source
                          :deck-position :position
                          :overpay :amount
                          :special :choices
                          :mixed :choices
                          :card-names)
        multi-selection (if (sequential? selection)
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
    (when unique?
      (assert (or (< (count multi-selection) 2)
                  (apply distinct? multi-selection)) (str "Choose error: All choices must be different: " (->> multi-selection
                                                                                                               (map ut/format-name)
                                                                                                               (string/join ", ")))))

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
        choose-fn     (if (= max 1) choose-single choose-multi)
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

(defn- do-reveal-discard [game player-no reveal-discard?]
  (let [discard-count (-> (get-in game [:players player-no :discard]) count)]
    (cond-> game
            (and reveal-discard?
                 (< 1 discard-count)) (-> (set-approx-discard-size player-no discard-count)
                                          (assoc-in [:players player-no :revealed-cards :discard] discard-count)))))

(defn give-choice [{:keys [mode] :as game} {:keys                            [player-no card-id min max optional?]
                                            [opt-name & opt-args :as option] :options
                                            :as                              choice}]
  (let [opt-fn    (effects/get-option opt-name)
        options   (apply opt-fn game player-no card-id opt-args)
        {:keys [source reveal-discard?]} (ut/get-source option)
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
                swiftable (choose (->> options
                                       (take min)
                                       (map (fn [o] (or (:option o) o))))))
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

(defn- get-play-triggers [event card triggers]
  (when (some (comp #{event} :event) triggers)
    [[:apply-triggers {:event event
                       :card  card}]]))

(defn card-effect [{:keys [current-player track-played-actions?] :as game} {:keys [player-no card]}]
  (let [{:keys [id name effects coin-value trigger]} card
        types                (ut/get-types game card)
        {:keys [actions-played triggers]} (get-in game [:players player-no])
        first-action-played? (and (= current-player player-no)
                                  (:action types)
                                  (empty? actions-played))
        special-card-effects (when first-action-played?
                               (get-play-triggers :instead-of-first-action card triggers))]
    (cond-> game
            (and (:action types)
                 track-played-actions?) (update-in [:players player-no :actions-played] concat [id])
            (:attack types) (affect-other-players {:player-no player-no
                                                   :effects   [[:clear-unaffected {:works :once}]]})
            :always (push-effect-stack {:player-no player-no
                                        :card-id   id
                                        :effects   (concat (or special-card-effects
                                                               (concat (when coin-value
                                                                         [[:give-coins coin-value]])
                                                                       effects))
                                                           (when first-action-played?
                                                             (get-play-triggers :play-first-action card triggers)))})
            (:attack types) (affect-other-players {:player-no player-no
                                                   :effects   reaction-choice})
            (and trigger
                 (not special-card-effects)) (update-in [:players player-no :triggers] concat [(assoc trigger :id (ut/next-id!)
                                                                                                              :card-id id
                                                                                                              :name name)]))))

(effects/register {:card-effect card-effect})

(defn play
  ([game {:keys [player-no card-name]}]
   (play game player-no card-name))
  ([{:keys [effect-stack] :as game} player-no card-name]
   (let [{:keys [phase actions actions-played triggers]
          :or   {phase :action}} (get-in game [:players player-no])
         {{:keys [effects coin-value trigger] :as card} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
         types      (ut/get-types game card)
         play-type  (cond
                      (and (#{:action} phase) (:action types) (pos? actions)) :action
                      (and (#{:action :pay} phase) (:treasure types)) :treasure
                      (and (#{:action :pay :buy :night} phase) (:night types)) :night)
         next-phase (case play-type
                      :treasure :pay
                      :night :night
                      phase)]
     (assert (-> effect-stack first :choice not) "You can't play cards when you have a choice to make.")
     (assert card (str "Play error: There is no " (ut/format-name card-name) " in your Hand."))
     (assert types (str "Play error: " (ut/format-name card-name) " has no types."))
     (case play-type
       :action (assert (or effects trigger) (str "Play error: " (ut/format-name card-name) " has no effect or trigger."))
       :treasure (assert (or coin-value effects) (str "Play error: " (ut/format-name card-name) " has no coin value or effects."))
       :night (assert (or effects trigger) (str "Play error: " (ut/format-name card-name) " has no effect or trigger."))
       (assert false (str "Play error: You can't play " (ut/format-types types) " cards"
                          " when you're in the " (ut/format-name phase) " phase"
                          (when (and (#{:action} phase) (:action types))
                            (str " and have " actions " actions left"))
                          ".")))
     (if (and (get-in game [:players player-no :phase]) (not= phase next-phase))
       (-> game
           (push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase next-phase}]
                                           [:play {:card-name card-name}]]})
           check-stack)
       (-> game
           (cond-> (= :action play-type) (update-in [:players player-no :actions] - 1))
           (push-effect-stack {:player-no player-no
                               :effects   (concat [[:move-card {:card-name card-name
                                                                :from      :hand
                                                                :to        :play-area}]
                                                   [:card-effect {:card card}]]
                                                  (when (:action types)
                                                    (get-play-triggers :play-action card triggers))
                                                  (get-play-triggers [:play card-name] card triggers))})
           check-stack)))))

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

(defn- all-cards [{:keys [deck discard hand play-area island-mat native-village-mat tavern-mat triggers]}]
  (let [cards           (concat deck discard hand play-area island-mat native-village-mat tavern-mat)
        set-aside-cards (mapcat :set-aside triggers)]
    (concat cards set-aside-cards)))

(defn- get-victory-points [cards {:keys [victory-points]}]
  (if (keyword? victory-points)
    (let [vp-fn (effects/get-effect victory-points)]
      (vp-fn cards))
    victory-points))

(defn put-all-cards-in-hands [game]
  (update game :players (partial mapv (fn [player]
                                        (-> player
                                            (assoc :hand (all-cards player))
                                            (dissoc :deck :discard :play-area :island-mat :native-village-mat)
                                            (assoc :phase :end-of-game))))))

(defn calculate-victory-points [{:keys [landmarks] :as game}]
  (let [update-fn (fn [{:keys [hand vp-tokens states] :as player}]
                    (let [vp-from-cards     (->> hand
                                                 (filter :victory-points)
                                                 (map (partial get-victory-points hand))
                                                 (apply + 0))
                          vp-from-tokens    (or vp-tokens 0)
                          vp-from-landmarks (->> landmarks
                                                 vals
                                                 (keep (fn [{:keys [when-scoring]}]
                                                         (when-let [scoring-fn (when when-scoring (effects/get-effect when-scoring))]
                                                           (scoring-fn hand game))))
                                                 (apply + 0))
                          vp-from-states    (->> states
                                                 (keep :victory-points)
                                                 (apply + 0))]
                      (assoc player :victory-points (+ vp-from-cards vp-from-tokens vp-from-landmarks vp-from-states))))]
    (update game :players (partial mapv update-fn))))

(defn- declare-winner [{:keys [players] :as game}]
  (let [score-fn   (juxt :victory-points (comp - :number-of-turns))
        best-score (->> players (map score-fn) sort last)]
    (update game :players (partial mapv (fn [player]
                                          (assoc player :winner (= best-score (score-fn player))))))))

(defn check-game-ended [game args]
  (case (get-game-status game)
    :active game
    :ending (assoc game :game-ending? true)
    :finished (-> game
                  put-all-cards-in-hands
                  calculate-victory-points
                  declare-winner)))

(effects/register {:check-game-ended check-game-ended})

(defn do-clean-up [game {:keys [player-no extra-turn?]}]
  (let [clean-up-player (fn [{:keys [play-area hand coins debt number-of-turns] :as player}]
                          (let [used-cards (concat hand (remove (partial ut/stay-in-play game player-no) play-area))]
                            (-> player
                                (cond->
                                  (and debt (< 0 coins debt)) (update :debt - coins)
                                  (and debt (<= debt coins)) (dissoc :debt)
                                  (not-empty used-cards) (update :discard concat used-cards))
                                (dissoc :hand
                                        :actions-played
                                        :bought-events
                                        :fortune-doubled?)
                                (update :play-area (partial filter (partial ut/stay-in-play game player-no)))
                                (ut/dissoc-if-empty :play-area)
                                (assoc :actions 0
                                       :coins 0
                                       :buys 0)
                                (update :triggers (partial remove (comp #{:once-turn :turn} :duration)))
                                (ut/dissoc-if-empty :triggers)
                                (cond-> (and number-of-turns
                                             (not extra-turn?)) (update :number-of-turns inc))
                                (cond-> (not extra-turn?) (dissoc :previous-turn-was-yours?)))))]
    (-> game
        (update-in [:players player-no] clean-up-player)
        (set-approx-discard-size player-no)
        (update :players (partial mapv (fn [player] (dissoc player :revealed-cards))))
        (dissoc :cost-reductions :unbuyable-cards :unbuyable-type)
        (ut/update-if-present :trash (partial map #(dissoc % :face))))))

(defn at-clean-up-choice [game {:keys [player-no card-name]}]
  (let [{{:keys [at-clean-up id]} :card} (ut/get-card-idx game [:players player-no :play-area] {:name card-name})]
    (cond-> game
            card-name (push-effect-stack {:player-no player-no
                                          :card-id   id
                                          :effects   (concat at-clean-up
                                                             [[:at-clean-up]])}))))

(effects/register {:at-clean-up-choice at-clean-up-choice})

(defn clean-up [game {:keys [player-no number-of-cards]
                      :or   {number-of-cards 5}
                      :as   args}]
  (-> game
      (push-effect-stack (merge args
                                {:effects [[:set-phase {:phase :clean-up}]
                                           [:do-clean-up args]
                                           [:draw number-of-cards]
                                           [:set-phase {:phase :out-of-turn}]
                                           [:check-game-ended]]}))
      check-stack))

(effects/register {:do-clean-up do-clean-up
                   :clean-up    clean-up})

(defn end-turn [{:keys [effect-stack players] :as game} player-no]
  (assert (empty? effect-stack) "You can't end your turn when you have a choice to make.")
  (let [at-end-turn-effects (->> (get-in game [:players player-no :triggers])
                                 (filter (comp #{:at-end-turn} :event))
                                 (mapcat :effects))]
    (if (not-empty at-end-turn-effects)
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   (concat at-end-turn-effects
                                                 [[:remove-triggers {:event :at-end-turn}]])})
          check-stack)
      (let [next-player (mod (inc player-no) (count players))]
        (-> game
            (push-effect-stack {:player-no next-player
                                :effects   [[:start-turn]]})
            (push-effect-stack {:player-no player-no
                                :effects   [[:clean-up]]})
            check-stack)))))
