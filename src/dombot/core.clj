(ns dombot.core
  (:require [dombot.cards :as cards]
            [dombot.operations :as op])
  (:gen-class))

; todo:
; phases
; undo
; unit tests for do-for-other-players, give-choice, chose, gardens, calc-victory-points
; handle multiple reaction cards
; game log

(defonce game-state (atom {}))

(defn start-game [player-names & [mode]]
  (let [{:keys [current-player] :as game} (cards/game player-names mode)]
    (swap! game-state assoc :game (-> game
                                      (op/start-round current-player)))
    (op/view-game (:game @game-state))))

(defn play-treasures []
  (let [{:keys [current-player] :as game} (:game @game-state)]
    (swap! game-state assoc :game (-> game
                                      (op/play-treasures current-player)))
    (op/view-game (:game @game-state))))

(defn play [card-name]
  (let [{:keys [current-player] :as game} (:game @game-state)]
    (swap! game-state assoc :game (-> game
                                      (op/play current-player card-name)))
    (op/view-game (:game @game-state))))

(defn chose [choice]
  (let [{:keys [current-player] :as game} (:game @game-state)]
    (swap! game-state assoc :game (-> game
                                      (op/chose choice)))
    (op/view-game (:game @game-state))))

(defn buy [card-name]
  (let [{:keys [current-player] :as game} (:game @game-state)]
    (swap! game-state assoc :game (-> game
                                      (op/buy-card current-player card-name)))
    (op/view-game (:game @game-state))))

(defn end-turn []
  (let [{:keys [players current-player] :as game} (:game @game-state)
        next-player (mod (inc current-player) (count players))]
    (swap! game-state assoc :game (-> game
                                      (op/clean-up current-player)
                                      (assoc :current-player next-player)
                                      (op/start-round next-player)))
    (op/view-game (:game @game-state))))

(defn view []
  (op/view-game (:game @game-state)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
