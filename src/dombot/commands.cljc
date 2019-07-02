(ns dombot.commands
  (:require [dombot.cards.kingdom :as kingdom]
            [dombot.operations :as op]
            [dombot.front-end-view]))

; todo:

; make extra-cards (Prizes) visible for Wishing Well
; view card images
; store commands in history
; save / load game
; fix Improve / double Border Guard bug
; fix 2x Cargo Ship / buy Experiment bug
; fix Improve / Cargo Ship bug
; fix Throne Room / Cargo Ship - both stay in play
; fix Torturer / Masquerade (empty hand)
; hide revealed cards on any movement (Border Guard)
; handle revealed-cards in hand as a list of card-names
; refac supply to a map of lists of cards by name
; change options into lists of maps with :name and :id (cards?) - or not??
; generalize checks for possible operations
; slow mode
; make frequencies optional
; unit tests for affect-other-players, give-choice, choose, calc-victory-points, ...
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

(defn restart []
  (swap! game-state update :game (partial take-last 1))
  (view))

(defn start-game [player-names & {:keys [mode sets]}]
  (let [{:keys [current-player] :as game} (kingdom/create-game player-names (or mode :swift) (or sets #{:intrigue}))]
    (swap! game-state assoc :game (-> game
                                      (op/start-turn {:player-no current-player})
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

(defn spend-coffer []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/spend-coffer current-player)))
    (view)))

(defn spend-villager []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/spend-villager current-player)))
    (view)))

(defn end-turn []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/end-turn current-player)))
    (view)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
