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

(defn player-area
  ([area filter-fn]
   (fn [game player-no]
     (cond->> (get-in game [:players player-no area])
              filter-fn (filter filter-fn)
              :always (map :name))))
  ([area]
   (player-area area nil)))

(defn supply-piles [{:keys [max-cost type]}]
  (fn [{:keys [supply]} player-no]
    (cond->> supply
             max-cost (filter (fn [{{:keys [cost]} :card
                                    pile-size      :pile-size}]
                                (and (<= cost max-cost)
                                     (< 0 pile-size))))
             type (filter (comp type :type :card))
             :always (map (comp :name :card)))))

(defn empty-supply-piles [{:keys [supply] :as game}]
  (->> supply
       (filter (comp zero? :pile-size))
       count))

