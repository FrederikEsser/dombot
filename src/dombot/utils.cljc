(ns dombot.utils
  (:require [clojure.string :as s]
            [dombot.effects :as effects]))

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

(defn get-card-idx [player area card-name]
  (->> player
       area
       (keep-indexed (fn [idx {:keys [name] :as card}]
                       (when (= card-name name) {:idx idx :card card})))
       first))

(defn get-effect-idx [effect-stack effect-type]
  (->> effect-stack
       (keep-indexed (fn [idx {[effect-name :as effect] :effect}]
                       (when (= effect-type effect-name) {:idx idx :effect effect})))
       first))

(defn player-area
  ([game player-no area & [{:keys [type reacts-to last name not-name]}]]
   (cond->> (get-in game [:players player-no area])
            last (take-last 1)                              ; it's important that this is evaluated first
            type (filter (comp type :type))
            name (filter (comp #{name} :name))
            not-name (remove (comp #{not-name} :name))
            reacts-to (filter (comp #{reacts-to} :reacts-to))
            :always (map :name))))

(defn supply-piles [{:keys [supply]} player-no {:keys [max-cost cost type]}]
  (-> supply
      (cond->>
        max-cost (filter (comp (partial >= max-cost) :cost :card))
        cost (filter (comp #{cost} :cost :card))
        type (filter (comp type :type :card)))
      (->> (filter (comp pos? :pile-size))
           (map (comp :name :card)))))

(defn empty-supply-piles [{:keys [supply] :as game}]
  (->> supply
       (filter (comp zero? :pile-size))
       count))

(effects/register {:player player-area
                   :supply supply-piles})
