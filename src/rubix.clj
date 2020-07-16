(ns rubix)

(def sides
  {:u [:f :l :b :r]
   :f [:u :r :d :l]
   :l [:u :f :d :b]
   :b [:u :l :d :r]
   :r [:u :b :d :f]
   :d [:f :r :b :l]})

(def solved-cube
  (->> sides
       (mapcat (fn [[s1 borders]]
                 (->> borders
                      (map-indexed (fn [i s2]
                                     (let [s3 (get borders (mod (inc i) (count borders)))]
                                       [{:piece [s1 s2] :pos #{s1 s2} :face s1}
                                        {:piece [s1 s2 s3] :pos #{s1 s2 s3} :face s1}])))
                      (apply concat))))
       (group-by :pos)
       (map (comp first second))
       (sort-by :piece)
       vec))

(def rotations
  (->> sides
       (mapcat (fn [[s1 borders]]
                 (->> (range 1 4)
                      (map (fn [n]
                             {[s1 n] (merge {s1 s1}
                                            (->> borders
                                                 (map-indexed (fn [i s2]
                                                                {s2 (get borders (mod (+ i n) (count borders)))}))
                                                 (apply merge)))})))))
       (apply merge)))

(defn rotate [[side :as rot] cube]
  (->> cube
       (mapv (fn [{:keys [pos] :as piece}]
               (cond-> piece
                       (pos side) (-> (update :pos (fn [pos]
                                                     (->> pos
                                                          (map (get rotations rot))
                                                          set)))
                                      (update :face (get rotations rot))))))))

(defn matches?
  [pred cube1 cube2]
  (= (filter pred cube1)
     (filter pred cube2)))

(defn step [{:keys [path cube]}]
  (let [illegal-rots (or (->> path
                              (take-last 2)
                              (map first)
                              set
                              #{#{:d :u} #{:f :b} #{:r :l}})
                         (some->> path
                                  last
                                  first
                                  (conj #{}))
                         #{:u :d :f :b :l})
        rots         (->> rotations
                          keys
                          (remove (comp illegal-rots first)))
        next-steps   (->> rots
                          (map (fn [rot]
                                 {:path (conj path rot)
                                  :cube (rotate rot cube)}))
                          (group-by (fn [{:keys [cube]}]
                                      (matches? (comp not :u :pos) solved-cube cube)))
                          doall)
        algs         (->> (get next-steps true)
                          (remove #(= solved-cube (:cube %)))
                          (map :path))]
    (when (not-empty algs)
      (prn algs))
    (get next-steps false)))

(comment
  (time (->> [{:path [] :cube solved-cube}]
             (mapcat step)
             (mapcat step)
             (mapcat step)
             (mapcat step)
             (mapcat step)
             (mapcat step)
             count))
  ([[:r 1] [:u 1] [:b 1] [:u 3] [:b 3] [:r 3]])
  ([[:r 1] [:b 1] [:u 1] [:b 3] [:u 3] [:r 3]])
  ([[:r 3] [:f 3] [:u 3] [:f 1] [:u 1] [:r 1]])
  ([[:r 3] [:u 3] [:f 3] [:u 1] [:f 1] [:r 1]])
  "Elapsed time: 48957.711621 msecs"
  => 1971212
  )