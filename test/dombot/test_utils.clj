(ns dombot.test-utils
  (:require [clojure.test :refer :all]))

(def ^:dynamic *rand* clojure.core/rand)
(def ^:dynamic *rand-int* clojure.core/rand-int)
(def ^:dynamic *shuffle* clojure.core/shuffle)

(defn rand'
  ([]
   (*rand* 1))
  ([n]
   (*rand* n)))

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
     (binding [*rand-int* #(.nextInt g# %)
               *rand* #(* % (.nextFloat g#))
               *shuffle* #(shuffle-with-seed % g#)]
       (with-redefs [rand rand'
                     rand-int *rand-int*
                     shuffle *shuffle*]
         ~@body))))

(deftest rand-test
  (with-rand-seed 123
                  (is (= 2 (rand-int 10)))
                  (is (= 0.2372438907623291 (rand)))
                  (is (= 9.90898847579956 (rand 10)))
                  (is (= [7 5 6 2 0 1 8 4 3 9] (shuffle (range 10))))))
