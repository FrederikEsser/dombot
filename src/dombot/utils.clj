(ns dombot.utils
  (:require [clojure.string :as s]))

(defn format-name [kw]
  (-> kw
      name
      (s/split #"-")
      (->> (map s/capitalize)
           (s/join " "))))

(defn format-type [type]
  (->> type
       (map format-name)
       (s/join "/")))

(defn redupeat [val f n]
  (if (> n 0)
    (redupeat (f val) f (dec n))
    val))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (let [vcoll (vec coll)]
    (vec (concat (subvec vcoll 0 pos) (subvec vcoll (inc pos))))))

(defn frequencies-of [coll key]
  (->> coll
       (map key)
       frequencies))

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

(defn player-hand
  ([game player-no]
   (->> (get-in game [:players player-no :hand])
        (map :name)))
  ([filter-fn game player-no]
   (->> (get-in game [:players player-no :hand])
        (filter filter-fn)
        (map :name))))

(defn player-discard [game player-no]
  (->> (get-in game [:players player-no :discard])
       (map :name)))

(defn supply-piles [max-cost {:keys [supply]} player-no]
  (->> supply
       (keep (fn [{{:keys [name cost]} :card
                   pile-size           :pile-size}]
               (when (and (<= cost max-cost)
                          (< 0 pile-size))
                 name)))))

(defn empty-supply-piles [{:keys [supply] :as game}]
  (->> supply
       (filter (comp zero? :pile-size))
       count))

