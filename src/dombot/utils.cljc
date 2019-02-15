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

(defn format-name [kw]
  (-> kw
      name
      (s/split #"[- ]")
      (->> (map s/capitalize)
           (s/join " "))))

(defn format-types [types]
  (->> types
       (map format-name)
       (s/join "/")))

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

(defn ensure-coll [data]
  (if (coll? data)
    data
    [data]))

(defn get-pile-idx [game card-name]
  (->> game
       :supply
       (keep-indexed (fn [idx pile]
                       (when ((comp #{card-name} :name :card) pile) (merge pile {:idx idx}))))
       first))

(defn get-card-idx [game path card-name]
  (->> (get-in game path)
       (keep-indexed (fn [idx {:keys [name] :as card}]
                       (when (= card-name name) {:idx idx :card card})))
       first))

(defn- minus-cost [cost reduction]
  (if (< cost reduction) 0 (- cost reduction)))

(defn- reduction-matches-card [{reduction-type :type}
                               {card-types :types}]
  (or (nil? reduction-type) (reduction-type card-types)))

(defn get-cost [{:keys [cost-reductions]} card]
  (-> (reduce (fn [card {:keys [reduction] :as reduction-data}]
                (cond-> card
                        (reduction-matches-card reduction-data card) (update :cost minus-cost reduction)))
              card cost-reductions)
      :cost))

(defn- can-react? [game player-no {:keys [react-pred]}]
  (if react-pred
    (let [can-react-fn (effects/get-option react-pred)]
      (can-react-fn game player-no))
    true))

(defn options-from-player
  ([game player-no card-id area & [{:keys [last this name not-name types reacts-to]}]]
   (when this
     (assert card-id (str "Card has no id, but is referring to :this in " area ".")))
   (cond->> (get-in game [:players player-no area])
            last (take-last 1)                              ; it's important that 'last' is evaluated first
            this (filter (comp #{card-id} :id))
            name (filter (comp #{name} :name))
            not-name (remove (comp #{not-name} :name))
            types (filter (comp types :types))
            reacts-to (filter (every-pred (comp #{reacts-to} :reacts-to)
                                          (partial can-react? game player-no)))
            :always (map :name))))

(effects/register-options {:player options-from-player})

(defn options-from-supply [{:keys [supply] :as game} player-no card-id & [{:keys [max-cost cost types]}]]
  (-> supply
      (cond->>
        max-cost (filter (comp (partial >= max-cost) (partial get-cost game) :card))
        cost (filter (comp #{cost} (partial get-cost game) :card))
        types (filter (comp types :types :card)))
      (->> (filter (comp pos? :pile-size))
           (map (comp :name :card)))))

(effects/register-options {:supply options-from-supply})

(defn options-from-trash [{:keys [trash]} player-no card-id {:keys [types]}]
  (cond->> trash
           types (filter (comp types :types))
           :always (map :name)))

(effects/register-options {:trash options-from-trash})

(defn special-options [game player-no card-id & options]
  options)

(effects/register-options {:special special-options})

(defn empty-supply-piles [{:keys [supply] :as game}]
  (->> supply
       (filter (comp zero? :pile-size))
       count))
