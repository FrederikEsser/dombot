(ns dombot.core
  (:require [dombot.cards :as cards]
            [dombot.operations :as op])
  (:gen-class))

; todo:
; play-treasures
; play instead of play-action and play-treasure
; witch
; view discard
; phases
; decisions
; reactions

(defonce game-state (atom {}))

(defn start-game [number-of-players]
  (let [{:keys [current-player] :as game} (cards/game number-of-players)]
    (swap! game-state assoc :game (-> game
                                      (update-in [:players current-player] op/start-round)))
    (op/view-game (:game @game-state))))

(defn play-treasure [card-name]
  (let [{:keys [current-player] :as game} (:game @game-state)]
    (swap! game-state assoc :game (-> game
                                      (op/play-treasure current-player card-name)))
    (op/view-game (:game @game-state))))

(defn play-action [card-name]
  (let [{:keys [current-player] :as game} (:game @game-state)]
    (swap! game-state assoc :game (-> game
                                      (op/play-action current-player card-name)))
    (op/view-game (:game @game-state))))

(defn buy [card-name]
  (let [{:keys [current-player] :as game} (:game @game-state)]
    (swap! game-state assoc :game (-> game
                                      (op/buy-card current-player card-name)))
    (op/view-game (:game @game-state))))

(defn end-round []
  (let [{:keys [players current-player] :as game} (:game @game-state)
        next-player (mod (inc current-player) (count players))]
    (swap! game-state assoc :game (-> game
                                      (update-in [:players current-player] op/clean-up)
                                      (assoc :current-player next-player)
                                      (update-in [:players next-player] op/start-round)))
    (op/view-game (:game @game-state))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
