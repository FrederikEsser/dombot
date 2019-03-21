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
         (map (fn [[k v]]
                (#{v} (get data2 k))))
         (every? (comp not nil?)))))

(defn ensure-coll [data]
  (cond
    (coll? data) data
    data [data]
    :else []))

(defn count-as-coll [data]
  (-> data ensure-coll count))

(defn get-pile-idx [game card-name]
  (->> game
       :supply
       (keep-indexed (fn [idx pile]
                       (when ((comp #{card-name} :name :card) pile) (merge pile {:idx idx}))))
       first))

(defn get-card-idx [game path criteria]
  (let [result (->> (get-in game path)
                    (keep-indexed (fn [idx card]
                                    (when ((match criteria) card) {:idx idx :card card})))
                    first)]
    result))

(defn update-in-vec [game path criteria f & args]
  (let [{:keys [idx]} (get-card-idx game path criteria)]
    (-> game
        (update-in path vec)
        (as-> game (apply update-in game (concat path [idx]) f args)))))

(defn plus [n m]
  (if n (+ n m) m))

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

(defn stay-in-play [{:keys [at-start-turn at-end-turn]}]
  (or (not-empty at-start-turn)
      (not-empty at-end-turn)))

(defn- can-react? [game player-no {:keys [react-pred]}]
  (if react-pred
    (let [can-react-fn (effects/get-effect react-pred)]
      (can-react-fn game player-no))
    true))

(defn options-from-player
  ([game player-no card-id area & [{:keys [last this id name names not-name type reacts-to min-cost leaves-play]}]]
   (when this
     (assert card-id (str "Card has no id, but is referring to :this in " area ".")))
   (cond->> (get-in game [:players player-no area])
            last (take-last 1)                              ; it's important that 'last' is evaluated first
            this (filter (comp #{card-id} :id))
            id (filter (comp #{id} :id))
            name (filter (comp #{name} :name))
            names (filter (comp names :name))
            not-name (remove (comp #{not-name} :name))
            type (filter (comp type :types))
            reacts-to (filter (every-pred (comp #{reacts-to} :reacts-to)
                                          (partial can-react? game player-no)))
            min-cost (filter (comp (partial <= min-cost) (partial get-cost game)))
            leaves-play (remove stay-in-play)
            :always (map :name))))

(effects/register-options {:player options-from-player})

(defn options-from-supply [{:keys [supply] :as game} player-no card-id & [{:keys [max-cost cost type names all]}]]
  (cond->> supply
           max-cost (filter (comp (partial >= max-cost) (partial get-cost game) :card))
           cost (filter (comp #{cost} (partial get-cost game) :card))
           type (filter (comp type :types :card))
           names (filter (comp names :name :card))
           (not all) (filter (comp pos? :pile-size))
           :always (map (comp :name :card))))

(effects/register-options {:supply options-from-supply})

(defn options-from-deck-position [game player-no & args]
  (let [deck (get-in game [:players player-no :deck])]
    (-> deck count inc range)))

(effects/register-options {:deck-position options-from-deck-position})

(defn options-from-trash [{:keys [trash]} player-no card-id {:keys [type]}]
  (cond->> trash
           type (filter (comp type :types))
           :always (map :name)))

(effects/register-options {:trash options-from-trash})

(defn special-options [game player-no card-id & options]
  options)

(effects/register-options {:special special-options})

(defn empty-supply-piles [{:keys [supply] :as game}]
  (->> supply
       (filter (comp zero? :pile-size))
       count))
