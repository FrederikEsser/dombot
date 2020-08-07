(ns dombot.test-utils
  (:require [clojure.test :refer :all]
            [dombot.operations :refer [calculate-score]]))

(defn rand-with-seed
  ([seed]
   (.nextFloat seed))
  ([seed n]
   (* n (.nextFloat seed))))

(defn shuffle-with-seed
  "Return a random permutation of coll"
  {:added  "1.2"
   :static true}
  [^java.util.Collection coll seed]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al seed)
    (clojure.lang.RT/vector (.toArray al))))

(defmacro with-rand-seed
  "Sets seed for calls to random in body. Beware of lazy seqs!"
  [seed & body]
  `(let [g# (java.util.Random. ~seed)]
     (with-redefs [rand     (partial rand-with-seed g#)
                   rand-int #(.nextInt g# %)
                   shuffle  #(shuffle-with-seed % g#)]
       ~@body)))

(deftest rand-test
  (with-rand-seed 123
                  (is (= 2 (rand-int 10)))
                  (is (= 0.2372438907623291 (rand)))
                  (is (= 9.90898847579956 (rand 10)))
                  (is (= [7 5 6 2 0 1 8 4 3 9] (shuffle (range 10))))))

(defn get-trigger [{:keys [id name trigger]} & [trigger-id]]
  (merge {:id (or trigger-id 1)}
         (when id {:card-id id})
         (when name {:name name})
         trigger))

(defn get-project-trigger [{:keys [name type trigger]}]
  (merge (if (= :project type)
           {:duration :game}
           {:duration name})
         {:id   1
          :name name}
         trigger))

(defn calc-victory-points [player]
  (-> {:players [player]}
      calculate-score
      (get-in [:players 0 :victory-points])))

(defn calc-score [player]
  (-> {:players [player]}
      calculate-score
      (get-in [:players 0 :score])))

