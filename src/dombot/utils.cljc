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

(defn format-type [type]
  (->> type
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

(defn get-effect-idx [effect-stack effect-type]
  (->> effect-stack
       (keep-indexed (fn [idx {[effect-name :as effect] :effect}]
                       (when (= effect-type effect-name) {:idx idx :effect effect})))
       first))

(defn options-from-player
  ([game player-no card-id area & [{:keys [last this name not-name type reacts-to]}]]
   (when this
     (assert card-id (str "Card has no id, but is referring to :this in " area ".")))
   (cond->> (get-in game [:players player-no area])
            last (take-last 1)                              ; it's important that 'last' is evaluated first
            this (filter (comp #{card-id} :id))
            name (filter (comp #{name} :name))
            not-name (remove (comp #{not-name} :name))
            type (filter (comp type :type))
            reacts-to (filter (comp #{reacts-to} :reacts-to))
            :always (map :name))))

(effects/register-options {:player options-from-player})

(defn options-from-supply [{:keys [supply]} player-no card-id {:keys [max-cost cost type]}]
  (-> supply
      (cond->>
        max-cost (filter (comp (partial >= max-cost) :cost :card))
        cost (filter (comp #{cost} :cost :card))
        type (filter (comp type :type :card)))
      (->> (filter (comp pos? :pile-size))
           (map (comp :name :card)))))

(effects/register-options {:supply options-from-supply})

(defn options-from-trash [{:keys [trash]} player-no card-id {:keys [type]}]
  (cond->> trash
           type (filter (comp type :type))
           :always (map :name)))

(effects/register-options {:trash options-from-trash})

(defn special-options [game player-no card-id & options]
  options)

(effects/register-options {:special special-options})

(defn empty-supply-piles [{:keys [supply] :as game}]
  (->> supply
       (filter (comp zero? :pile-size))
       count))
