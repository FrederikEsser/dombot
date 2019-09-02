(ns dombot.commands
  (:require [dombot.cards.kingdom :as kingdom]
            [dombot.operations :as op]
            [dombot.front-end-view]
            [dombot.specs :as specs]
            [clojure.spec.alpha :as s]))

; TODOs:

; Trigger refac:
; at-end-buy: Exploration, Pageant
; at-clean-up: Boons (River, Field, Forest), Horn, Improve, Treasury
; handle modes :semi, :always-ask - Fx. Caravan & Captain/Ghost with Watch Tower - or Blessed Village, Sun's Gift and Caravan
; handle mode :optional - at-clean-up
; handle simultaneous effects for all reactions - at-clean-up
; fix Captain / Improve
; fix Throne Room / Improve
; fix Improve / double Border Guard bug
; show set-aside cards where their carrier has disappeared (Cargo Ship / Improve)
; show Throne Room'ed Durations with set-aside cards as separate cards

; Bugs & Testing:
; fix Ghost/Captain/Piazza/Innovation(with Cobbler?) with Citadel
; test swiftable for mixed choices
; fix Scepter / Caravan Guard reaction - not played this turn
; check Experiment with Innovation
; fix extra Outpost turn after Fleet turn
; fix Torturer / Masquerade (empty hand)
; unit tests for affect-other-players, give-choice, choose, calc-victory-points, ...

; Front End:
; show Actions as unbuyable when Deluded (and in action phase)
; make extra-cards (Prizes, Spirits, etc.) and unique cards (Zombies & Heirlooms) visible/choosable for Wishing Well / Doctor / Journeyman
; hide revealed cards on any movement (Border Guard)
; UI button for jumping to next phase
; show face-down cards in trash (Necromancer)
; make "any order" option for all topdecking (Sentry, Navigator, Rabble, Doctor, Seer, Night Watchman)
; view card images

; Refactoring:
; maybe refac gain to destination, but pass gained card as argument to on-gain effects
; consider refac buy and play separating set-phase and the rest
; refac supply to a map of lists of cards by name
; change options into lists of maps with :name and :id (cards?) - or not??
; handle revealed-cards in hand as a list of card-names
; generalize checks for possible operations

; Features:
; store commands in history
; save / load game
; slow mode
; make frequencies optional
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
                                            (op/play-treasures {:player-no current-player})))
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

(defn buy-project [project-name]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/buy-project current-player project-name)))
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
