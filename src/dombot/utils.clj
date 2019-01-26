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

(defn mapv-indexed [f coll]
  (->> coll
       (map-indexed f)
       vec))

(defn frequencies-of [coll key]
  (->> coll
       (map key)
       frequencies))

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

(defn player-hand [game player-no]
  (-> (get-in game [:players player-no :hand])
      (frequencies-of :name)))

(defn player-discard [game player-no]
  (-> (get-in game [:players player-no :discard])
      (frequencies-of :name)))

