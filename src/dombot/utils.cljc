(ns dombot.utils
  (:require [clojure.string :as s]
            [dombot.effects :as effects]))

(defonce id-state (atom 0))

(defn reset-ids! []
  (reset! id-state 0))

(defn next-id! []
  (swap! id-state inc))

(defn give-id! [{:keys [id] :as card}]
  (cond-> card
          (nil? id) (assoc :id (next-id!))))

(defn format-name [{:keys [card-name] :as kw}]
  (if card-name
    (format-name card-name)
    (-> kw
        name
        (s/split #"[- ]")
        (->> (map s/capitalize)
             (s/join " ")))))

(defn format-name-short [n]
  (-> n
      name
      (s/split #"[- ]")
      (->> (map (comp first s/capitalize))
           s/join)))

(defn format-token [{:keys [token-type]}]
  (str "("
       (-> token-type
           name
           (s/split #"[- ]")
           (->> (map (comp first s/capitalize))
                s/join))
       ")"))

(defn format-types [types]
  (->> types
       (map format-name)
       (s/join "/")))

(defn number->text [n]
  (case n
    1 "one"
    2 "two"
    3 "three"
    4 "four"
    :default n))

(defn redupeat [val n f & args]
  (loop [acc val n n]
    (if (> n 0)
      (recur (apply f acc args) (dec n))
      acc)))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (let [vcoll (vec coll)]
    (vec (concat (subvec vcoll 0 pos) (subvec vcoll (inc pos))))))

(defn frequencies-of [coll key]
  (->> coll
       (map key)
       frequencies
       (into (sorted-map))))

(defn dissoc-if-empty [map key]
  (cond-> map
          (empty? (get map key)) (dissoc map key)))

(defn match [data1]
  (fn [data2]
    (->> data1
         (every? (fn [[key val]]
                   (let [values (if (set? val) val #{val})]
                     (contains? values (get data2 key))))))))

(defn ensure-coll [data]
  (cond
    (coll? data) data
    data [data]
    :else []))

(defn count-as-coll [data]
  (-> data ensure-coll count))

(defn get-pile-idx
  ([game card-name]
   (get-pile-idx game :supply card-name))
  ([game from card-name]
   (->> game
        from
        (keep-indexed (fn [idx pile]
                        (when ((comp #{card-name} :name :card) pile) (merge pile {:idx idx}))))
        first)))

(defn get-card-idx [game path criteria]
  (->> (get-in game path)
       (keep-indexed (fn [idx card]
                       (when ((match criteria) card) {:idx idx :card card})))
       first))

(defn get-trigger-idx [game path criteria]
  (->> (get-in game path)
       (keep-indexed (fn [idx trigger]
                       (when ((match criteria) trigger) {:idx idx :trigger trigger})))
       first))

(defn update-in-vec [game path criteria f & args]
  (let [{:keys [idx]} (get-card-idx game path criteria)]
    (-> game
        (update-in path vec)
        (as-> game (apply update-in game (concat path [idx]) f args)))))

(defn update-if-present
  "Update if a value is already present, otherwise do nothing (don't insert nil)."
  [m k f & args]
  (cond->> m
           (get m k) (#(apply update % k f args))))

(defn update-in-if-present
  "Update if a value is already present, otherwise do nothing (don't insert nil)."
  [m ks f & args]
  (cond->> m
           (get-in m ks) (#(apply update-in % ks f args))))

(defn plus [n m]
  (if n (+ n m) m))

(defn- minus-cost [cost reduction]
  (if (< cost reduction) 0 (- cost reduction)))

(defn- capitalism-get-types [{:keys [name types effects] :as card}]
  (if (and
        (:action types)
        (or (some (fn [[effect {:keys [text options]}]]
                    (or (= :give-coins effect)
                        (and text (re-find #"\+\$" text))
                        (some (fn [{:keys [text]}]
                                (and text (re-find #"\+\$" text)))
                              options)))
                  effects)
            (contains? #{:merchant
                         :baron :ironworks :courtier
                         :pirate-ship :salvager
                         :trade-route :city
                         :harvest :tournament :trusty-steed} name)))
    (conj types :treasure)
    types))

(defn get-types [{:keys [current-player] :as game} {:keys [types] :as card}]
  (let [player-no (or current-player 0)]
    (if (->> (get-in game [:projects :capitalism :participants])
             (some (comp #{player-no} :player-no)))
      (capitalism-get-types card)
      types)))

(defn- reduction-matches-card-types [{reduction-type :type} card-types]
  (or (nil? reduction-type) (reduction-type card-types)))

(defn- get-cost-with-reduction [game player-no card]
  (let [cost-reductions (->> (get-in game [:players player-no :play-area])
                             (mapcat (comp :cost-reductions :while-in-play))
                             (concat (:cost-reductions game)
                                     (get-in game [:players player-no :cost-reductions])))
        card-types      (get-types game card)]
    (-> (reduce (fn [card {:keys [reduction] :as reduction-data}]
                  (cond-> card
                          (reduction-matches-card-types reduction-data card-types) (update :cost minus-cost reduction)))
                card cost-reductions)
        :cost)))

(defn get-buy-cost [game player-no {:keys [buy-cost] :as card}]
  (let [buy-cost-fn (when buy-cost (effects/get-effect buy-cost))]
    (get-cost-with-reduction game player-no (cond-> card
                                                    buy-cost-fn (assoc :cost (buy-cost-fn game {:player-no player-no}))))))

(defn get-cost [{:keys [current-player] :as game} card]
  (let [player-no (or current-player 0)
        {:keys [phase]} (get-in game [:players player-no])]
    (if (#{:pay :buy} phase)
      (get-buy-cost game player-no card)
      (get-cost-with-reduction game player-no card))))

(defn card-buyable? [{:keys [unbuyable-cards] :as game} player-no {:keys [name buyable?] :as card}]
  (cond
    (and unbuyable-cards (unbuyable-cards name)) false
    buyable? (let [buyable-fn (effects/get-effect buyable?)]
               (buyable-fn game {:player-no player-no}))
    :else true))

(defn stay-in-play [game player-no {:keys [id] :as card}]
  (let [{:keys [play-area triggers repeated-play]} (get-in game [:players player-no])
        card-ids-in-play      (->> play-area (keep :id) set)
        repeated-card-ids     (->> repeated-play
                                   (filter (comp #{id} :source))
                                   (filter (comp card-ids-in-play :target))
                                   (map :target)
                                   set)
        stay-in-play-triggers (filter (comp #{:at-start-turn :at-end-turn} :trigger) triggers)]
    (or (some (comp #{id} :card-id) stay-in-play-triggers)
        (some (comp repeated-card-ids :card-id) stay-in-play-triggers))))

(defn- can-react? [game player-no {:keys [react-pred]}]
  (if react-pred
    (let [can-react-fn (effects/get-effect react-pred)]
      (can-react-fn game player-no))
    true))

(defn options-from-player
  ([game player-no card-id area & [{:keys [last this id ids name names not-name type reacts-to min-cost leaves-play]}]]
   (when this
     (assert card-id (str "Card has no id, but is referring to :this in " area ".")))
   (cond->> (get-in game [:players player-no area])
            last (take-last 1)                              ; it's important that 'last' is evaluated first
            this (filter (comp #{card-id} :id))
            id (filter (comp #{id} :id))
            name (filter (comp #{name} :name))
            names (filter (comp names :name))
            not-name (remove (comp not-name :name))
            type (filter (comp type (partial get-types game)))
            reacts-to (filter (every-pred (comp #{reacts-to} :reacts-to)
                                          (partial can-react? game player-no)))
            min-cost (filter (comp (partial <= min-cost) (partial get-cost game)))
            leaves-play (remove (partial stay-in-play game player-no))
            ids (filter (comp ids :id))
            :always (map :name))))

(effects/register-options {:player options-from-player})

(defn options-from-supply [{:keys [supply] :as game} player-no card-id & [{:keys [max-cost cost type names all]}]]
  (cond->> supply
           max-cost (filter (comp (partial >= max-cost) (partial get-cost game) :card))
           cost (filter (comp #{cost} (partial get-cost game) :card))
           type (filter (comp type (partial get-types game) :card))
           names (filter (comp names :name :card))
           (not all) (filter (comp pos? :pile-size))
           :always (map (comp :name :card))))

(effects/register-options {:supply options-from-supply})

(defn options-from-extra-cards [{:keys [extra-cards] :as game} player-no card-id & [{:keys [max-cost cost type names all]}]]
  (cond->> extra-cards
           max-cost (filter (comp (partial >= max-cost) (partial get-cost game) :card))
           type (filter (comp type (partial get-types game) :card))
           :always (map (comp :name :card))))

(effects/register-options {:extra-cards options-from-extra-cards})

(defn options-from-deck-position [game player-no & args]
  (let [deck (get-in game [:players player-no :deck])]
    (-> deck count inc range)))

(effects/register-options {:deck-position options-from-deck-position})

(defn options-from-overbuy [game player-no & args]
  (let [coins (get-in game [:players player-no :coins])]
    (-> coins inc range)))

(effects/register-options {:overpay options-from-overbuy})

(defn options-from-trash [{:keys [trash] :as game} player-no card-id {:keys [type not-type face]}]
  (cond->> trash
           type (filter (comp type (partial get-types game)))
           not-type (remove (comp not-type (partial get-types game)))
           (= :up face) (remove (comp #{:down} :face))
           :always (map :name)))

(effects/register-options {:trash options-from-trash})

(defn special-options [game player-no card-id & options]
  options)

(effects/register-options {:special special-options
                           :mixed   special-options})

(defn get-source [[name arg & [{:keys [id last]}]]]
  (if (= :player name)
    (merge {:source arg}
           (when (and (= :discard arg)
                      (not (or id last)))
             {:reveal-discard? true}))
    {:source name}))

(defn multi-options [game player-no card-id & options]
  (->> options
       (mapcat (fn [[opt-name & opt-args :as option]]
                 (let [opt-fn (effects/get-option opt-name)
                       {:keys [source]} (get-source option)]
                   (->> (apply opt-fn game player-no card-id opt-args)
                        (map (fn [card-name]
                               {:area      source
                                :card-name card-name}))))))))

(effects/register-options {:multi multi-options})

(defn empty-supply-piles [{:keys [supply] :as game}]
  (->> supply
       (filter (comp zero? :pile-size))
       count))

(defn add-effect-args [new-args [effect args]]
  [effect (cond
            (map? args) (merge new-args args)
            args (merge new-args {:arg args})
            :else new-args)])
