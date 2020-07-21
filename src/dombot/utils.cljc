(ns dombot.utils
  (:require [clojure.string :as s]
            [clojure.set :refer [intersection]]
            [dombot.effects :as effects]))

(defonce id-state (atom 0))

(defn reset-ids! [& [last-id]]
  (reset! id-state (or last-id 0)))

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

(defn format-types [types]
  (->> types
       (map format-name)
       (s/join "/")))

(defn format-cost [{:keys [coin-cost debt-cost]
                    :or   {coin-cost 0
                           debt-cost 0}} & [{buy-cost :coin-cost}]]
  (str (when (or (pos? coin-cost)
                 (zero? debt-cost)) (str "$" coin-cost))
       (when (pos? debt-cost) (str "d" debt-cost))
       (when buy-cost (str "/" buy-cost))))

(defn number->text [n]
  (case n
    1 "one"
    2 "two"
    3 "three"
    4 "four"
    5 "five"
    6 "six"
    (str n)))

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

(defn coll-diff [coll1 coll2]
  (->>
    [coll1 coll2]
    (map frequencies)
    (apply merge-with -)
    (mapcat (fn [[x n]] (repeat n x)))))

(defn frequencies-of [coll key]
  (->> coll
       (map key)
       frequencies
       (into (sorted-map))))

(defn dissoc-if-empty [map key]
  (cond-> map
          (empty? (get map key)) (dissoc key)))

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

(defn access-top-card [{:keys [split-pile hidden?] :as pile}]
  (if split-pile
    (let [total-pile-size (->> split-pile
                               (map :pile-size)
                               (apply +))]
      (merge (or (->> split-pile
                      (filter (comp pos? :pile-size))
                      first)
                 (last split-pile))
             (if hidden?
               {:pile-size total-pile-size}
               {:total-pile-size total-pile-size})
             (select-keys pile [:tokens])))
    pile))

(defn access-card [card-name {:keys [split-pile] :as pile}]
  (if split-pile
    (merge (or (->> split-pile
                    (filter (comp #{card-name} :name :card))
                    first)
               (last split-pile))
           (select-keys pile [:tokens]))
    pile))

(defn remove-top-card [{:keys [split-pile] :as pile}]
  (if split-pile
    (let [{:keys [idx name pile-size]} (->> split-pile
                                            (keep-indexed (fn [idx {:keys [card pile-size]}]
                                                            (when (pos? pile-size)
                                                              {:idx       idx
                                                               :name      (:name card)
                                                               :pile-size pile-size})))
                                            first)
          delete-sub-pile? (and (= 1 pile-size)
                                (= 0 idx)
                                (->> split-pile
                                     (drop 1)
                                     (some (comp #{name} :name :card))))]
      (if delete-sub-pile?
        (update pile :split-pile (comp vec (partial drop 1)))
        (update-in pile [:split-pile idx :pile-size] dec)))
    (update pile :pile-size dec)))

(defn add-top-card [{:keys [split-pile] :as pile} {:keys [name] :as card}]
  (if split-pile
    (let [idx               (->> split-pile
                                 (keep-indexed (fn [idx sub-pile]
                                                 (when (= name (get-in sub-pile [:card :name]))
                                                   idx)))
                                 first)
          sub-pile-blocked? (->> split-pile
                                 (take idx)
                                 (some (comp pos? :pile-size)))]
      (if sub-pile-blocked?
        (update pile :split-pile (fn [split-pile]
                                   (vec (concat [{:card      (dissoc card :id)
                                                  :pile-size 1}]
                                                split-pile))))
        (update-in pile [:split-pile idx :pile-size] inc)))
    (update pile :pile-size inc)))

(defn get-pile-idx
  ([game card-name]
   (get-pile-idx game :supply card-name))
  ([game from card-name]
   (get-pile-idx game from card-name #{}))
  ([game from card-name flags]
   (let [handle-split-piles (if (flags :include-empty-split-piles)
                              (partial access-card card-name)
                              access-top-card)]
     (->> game
          from
          (map handle-split-piles)
          (keep-indexed (fn [idx pile]
                          (when ((comp #{card-name} :name :card) pile) (merge pile {:idx idx}))))
          first))))

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

(defn capitalism-get-types [{:keys [name types effects trigger] :as card}]
  (if (and
        (:action types)
        (or (some (fn [[effect {:keys [text options]}]]
                    (or (= :give-coins effect)
                        (and text (re-find #"\+\$" text))
                        (some (fn [{:keys [text]}]
                                (and text (re-find #"\+\$" text)))
                              options)))
                  (concat effects (:effects trigger)))
            (contains? #{:merchant
                         :baron :ironworks :courtier
                         :pirate-ship :salvager
                         :trade-route :city
                         :harvest :tournament :trusty-steed
                         :giant :miser :teacher
                         :chariot-race :farmers'-market :sacrifice} name)))
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

(defn- get-cost-with-reduction [game player-no {{:keys [coin-cost debt-cost] :as cost} :cost :as card}]
  (let [cost-reductions (->> (get-in game [:players player-no :play-area])
                             (mapcat (comp :cost-reductions :while-in-play))
                             (concat (:cost-reductions game)
                                     (get-in game [:players player-no :cost-reductions])))
        card-types      (get-types game card)
        base-cost       (if (int? cost)
                          {:coin-cost cost}
                          (merge {:coin-cost (or coin-cost 0)}
                                 (when debt-cost
                                   {:debt-cost debt-cost})))]
    (reduce (fn [card-cost {:keys [reduction] :as reduction-data}]
              (cond-> card-cost
                      (reduction-matches-card-types reduction-data card-types) (update :coin-cost minus-cost reduction)))
            base-cost
            cost-reductions)))

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

(defn card-buyable? [{:keys [unbuyable-cards unbuyable-type] :as game} player-no {:keys [name buyable?] :as card}]
  (cond
    (and unbuyable-cards (unbuyable-cards name)) false
    (and unbuyable-type (unbuyable-type (get-types game card))) false
    buyable? (let [buyable-fn (effects/get-effect buyable?)]
               (buyable-fn game {:player-no player-no}))
    :else true))

(defn stay-in-play [game player-no {:keys [id]}]
  (let [{:keys [play-area triggers repeated-play]} (get-in game [:players player-no])
        card-ids-in-play      (->> play-area (keep :id) set)
        repeated-card-ids     (->> repeated-play
                                   (filter (comp #{id} :source))
                                   (filter (comp card-ids-in-play :target))
                                   (map :target)
                                   set)
        stay-in-play-triggers (filter (comp #{:at-start-turn :at-end-turn :play-action} :event) triggers)]
    (or (some (comp #{id} :card-id) stay-in-play-triggers)
        (some (comp repeated-card-ids :card-id) stay-in-play-triggers))))

(defn- can-react? [game player-no {:keys [react-pred]}]
  (if react-pred
    (let [can-react-fn (effects/get-effect react-pred)]
      (can-react-fn game player-no))
    true))

(defn types-match [game types card]
  (->> card
       (get-types game)
       (intersection types)
       not-empty))

(defn normalize-cost [{:keys [coin-cost debt-cost] :as cost}]
  (if (int? cost)
    {:coin-cost cost
     :debt-cost 0}
    {:coin-cost (or coin-cost 0)
     :debt-cost (or debt-cost 0)}))

(defn costs-up-to [max-cost card-cost]
  (let [{max-coin-cost :coin-cost
         max-debt-cost :debt-cost} (normalize-cost max-cost)
        {card-coin-cost :coin-cost
         card-debt-cost :debt-cost} (normalize-cost card-cost)]
    (and (<= card-coin-cost max-coin-cost)
         (<= card-debt-cost max-debt-cost))))

(defn costs-at-least [min-cost card-cost]
  (costs-up-to card-cost min-cost))

(defn costs-between [min-cost max-cost card-cost]
  (and (costs-at-least min-cost card-cost)
       (costs-up-to max-cost card-cost)))

(defn costs-exactly [cost card-cost]
  (= (normalize-cost cost) (normalize-cost card-cost)))

(defn costs-less [max-cost+ card-cost]
  (and (costs-up-to max-cost+ card-cost)
       (not (costs-exactly max-cost+ card-cost))))

(defn costs-more [min-cost- card-cost]
  (costs-less card-cost min-cost-))

(defn add-to-cost [card-cost cost]
  (let [added-cost (if (int? cost)
                     {:coin-cost cost}
                     cost)]
    (merge-with + card-cost added-cost)))

(defn options-from-player
  ([game player-no card-id area & [{:keys [last this id ids name names not-names type types not-type reacts-to min-cost max-cost leaves-play]}]]
   (cond->> (get-in game [:players player-no area])
            last (take-last 1)                              ; it's important that 'last' is evaluated first
            this (filter (comp #{card-id} :id))
            id (filter (comp #{id} :id))
            name (filter (comp #{name} :name))
            names (filter (comp names :name))
            not-names (remove (comp not-names :name))
            type (filter (comp type (partial get-types game)))
            types (filter (partial types-match game types))
            not-type (remove (comp not-type (partial get-types game)))
            reacts-to (filter (every-pred (comp #{reacts-to} :reacts-to)
                                          (partial can-react? game player-no)))
            min-cost (filter (comp (partial costs-at-least min-cost) (partial get-cost game)))
            max-cost (filter (comp (partial costs-up-to max-cost) (partial get-cost game)))
            leaves-play (remove (partial stay-in-play game player-no))
            ids (filter (comp ids :id))
            :always (map :name))))

(effects/register-options {:player options-from-player})

(defn options-from-supply [{:keys [supply] :as game} player-no card-id & [{:keys [max-cost costs-less-than cost type types not-type names not-names all]}]]
  (cond->> (map access-top-card supply)
           max-cost (filter (comp (partial costs-up-to max-cost) (partial get-cost game) :card))
           costs-less-than (filter (comp (partial costs-less costs-less-than) (partial get-cost game) :card))
           cost (filter (comp (partial costs-exactly cost) (partial get-cost game) :card))
           type (filter (comp type (partial get-types game) :card))
           types (filter (comp (partial types-match game types) :card))
           not-type (remove (comp not-type (partial get-types game) :card))
           names (filter (comp names :name :card))
           not-names (remove (comp not-names :name :card))
           (not all) (filter (comp pos? :pile-size))
           :always (map (comp :name :card))))

(effects/register-options {:supply options-from-supply})

(defn options-from-extra-cards [{:keys [extra-cards] :as game} player-no card-id & [{:keys [max-cost costs-less-than type all]}]]
  (cond->> extra-cards
           max-cost (filter (comp (partial costs-up-to max-cost) (partial get-cost game) :card))
           costs-less-than (filter (comp (partial costs-less costs-less-than) (partial get-cost game) :card))
           type (filter (comp type (partial get-types game) :card))
           (not all) (filter (comp pos? :pile-size))
           :always (map (comp :name :card))))

(effects/register-options {:extra-cards options-from-extra-cards})

(defn options-from-projects [{:keys [projects] :as game} player-no card-id & [{:keys [names]}]]
  (cond->> (vals projects)
           names (filter (comp names :name))
           :always (map (comp :name))))

(effects/register-options {:projects options-from-projects})

(defn options-from-artifacts [{:keys [artifacts] :as game} player-no card-id & [{:keys [names]}]]
  (cond->> (vals artifacts)
           names (filter (comp names :name))
           :always (map (comp :name))))

(effects/register-options {:artifacts options-from-artifacts})

(defn options-from-deck-position [game player-no & args]
  (let [deck (get-in game [:players player-no :deck])]
    (-> deck count inc range)))

(effects/register-options {:deck-position options-from-deck-position})

(defn options-from-overbuy [game player-no & args]
  (let [coins (get-in game [:players player-no :coins])]
    (-> coins inc range)))

(effects/register-options {:overpay options-from-overbuy})

(defn options-from-trash [{:keys [trash] :as game} player-no card-id {:keys [type not-type face min-cost max-cost]}]
  (cond->> trash
           type (filter (comp type (partial get-types game)))
           not-type (remove (comp not-type (partial get-types game)))
           (= :up face) (remove (comp #{:down} :face))
           min-cost (filter (comp (partial costs-at-least min-cost) (partial get-cost game)))
           max-cost (filter (comp (partial costs-up-to max-cost) (partial get-cost game)))
           :always (map :name)))

(effects/register-options {:trash options-from-trash})

(defn options-from-druid [{:keys [druid-boons] :as game} player-no card-id]
  (map :name druid-boons))

(effects/register-options {:druid-boons options-from-druid})

(defn special-options [game player-no card-id & options]
  options)

(effects/register-options {:special special-options})

(defn get-source [[name & args]]
  (cond
    (= :player name) (let [[arg & [{:keys [id last]}]] args]
                       (merge {:source arg}
                              (when (and (= :discard arg)
                                         (not (or id last)))
                                {:reveal-discard? true})))
    (= :mixed name) (merge {:source :mixed}
                           (when (some (comp #{[:player :discard]} (partial take 2)) args)
                             {:reveal-discard? true}))
    :else {:source name}))

(defn mixed-options [game player-no card-id & options]
  (->> options
       (mapcat (fn [[opt-name & opt-args :as option]]
                 (let [opt-fn (effects/get-option opt-name)
                       {:keys [source]} (get-source option)]
                   (->> (apply opt-fn game player-no card-id opt-args)
                        (map (fn [card-name]
                               {:area      source
                                :card-name card-name}))))))))

(effects/register-options {:mixed mixed-options})

(defn empty-supply-piles [{:keys [supply] :as game}]
  (->> supply
       (map access-top-card)
       (filter (comp zero? :pile-size))
       count))

(defn add-effect-args [new-args [effect args]]
  [effect (cond
            (map? args) (merge new-args args)
            args (merge new-args {:arg args})
            :else new-args)])
