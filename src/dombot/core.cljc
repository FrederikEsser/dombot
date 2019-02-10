(ns dombot.core
  (:require [dombot.cards.kingdom :as kingdom]
            [dombot.operations :as op]
            [dombot.front-end-view])
  (:gen-class))

; todo:
;;; Intrigue
; refer to :this for choices (mining village)

; generalize checks for possible operations
; slow mode
; make frequencies optional
; unit tests for affect-other-players, give-choice, choose, gardens, calc-victory-points, ...
; handle multiple reaction cards
; game log / turn counter

(defonce game-state (atom {}))

(defn get-game []
  (-> @game-state
      :game
      first
      (assoc :can-undo? true)))

(defn view []
  (-> @game-state :game first dombot.front-end-view/view-game))

(defn undo []
  (let [{:keys [can-undo?]} (-> @game-state :game first)]
    (assert can-undo? "Unable to undo last move.")
    (swap! game-state update :game (partial drop 1))
    (view)))

(defn start-game [player-names & {:keys [mode sets]}]
  (let [{:keys [current-player] :as game} (kingdom/create-game player-names (or mode :swift) (or sets #{:intrigue}))]
    (swap! game-state assoc :game (-> game
                                      (op/start-turn current-player)
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

(defn choose [option]
  (let [game (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/choose option)))
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
                                            (op/start-turn next-player)))
    (view)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
