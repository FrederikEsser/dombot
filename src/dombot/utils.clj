(ns dombot.utils)

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