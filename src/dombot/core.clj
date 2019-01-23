(ns dombot.core
  (:require [dombot.cards :refer :all]
            [dombot.operations :refer :all])
  (:gen-class))

(defn get-vp [deck {:keys [:vp]}]
  (if (fn? vp)
    (vp deck)
    vp))

(defn calc-victory-points [cards]
  (->> cards
       (filter (comp :victory :type))
       (map (partial get-vp cards))
       (apply +)))

(comment
  (-> (game 2)
      (update-in [:players 0] start-round)
      (play-treasure 0 :copper)
      (play-treasure 0 :copper)
      (buy-card 0 :moat)
      (update-in [:players 0] clean-up)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
