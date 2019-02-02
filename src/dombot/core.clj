(ns dombot.core
  (:require [dombot.cards :as cards]
            [dombot.operations :as op])
  (:gen-class))

; todo:
; phases
; unit tests for do-for-other-players, give-choice, chose, gardens, calc-victory-points
; handle multiple reaction cards
; game log

(defonce game-state (atom {}))

(defn get-game []
  (-> @game-state
      :game
      first
      (assoc :can-undo? true)))

(defn view []
  (op/view-game (get-game)))

(defn undo []
  (let [{:keys [can-undo?]} (-> @game-state :game first)]
    (assert can-undo? "Unable to undo last move.")
    (swap! game-state update :game (partial drop 1))
    (view)))

(defn start-game [player-names & [mode]]
  (let [{:keys [current-player] :as game} (cards/game player-names (or mode :swift))]
    (swap! game-state assoc :game (-> game
                                      (op/start-round current-player)
                                      list))
    (view)))

(defn play-treasures []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/play-treasures current-player)))
    (view)))

(defn play [card-name]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/play current-player card-name)))
    (view)))

(defn chose [choice]
  (let [game (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/chose choice)))
    (view)))

(defn buy [card-name]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/buy-card current-player card-name)))
    (view)))

(defn end-turn []
  (let [{:keys [players current-player] :as game} (get-game)
        next-player (mod (inc current-player) (count players))]
    (swap! game-state update :game conj (-> game
                                            (op/clean-up current-player)
                                            (assoc :current-player next-player)
                                            (op/start-round next-player)))
    (view)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
