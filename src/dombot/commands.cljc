(ns dombot.commands
  (:require [dombot.cards.kingdom :as kingdom]
            [dombot.operations :as op]
            [dombot.front-end-view]
            [dombot.specs :as specs]
            [clojure.spec.alpha :as s]))

; TODOs:

; Trigger refac:
; Check card effects at clean up at every iteration. Travellers need trigger-conditions
; fix Throne Room / Improve
; fix Captain / Improve
; show set-aside cards where their carrier has disappeared (Cargo Ship / Improve) & Save
; show Throne Room'ed Durations with set-aside cards as separate cards
; choose by trigger id when set aside cards - show different cards

; Bugs & Testing:
; Fix Vassal on Village Green
; Fix Labyrinth & buying Skulk
; fix Herald & Piazza with 1 card in deck - what about Vassal & ...?
; fix Academy gain Will'o'Wisp with Swamps gift (from Druid) or Imp with Devil's Workshop
; fix having Lost in the Woods when you haven't bought Fleet
; separate mixed choices from play area and tavern mat - 2 Ratcatchers called same turn
; receiving boons & hexes can't be undone
; give coin from Key when received at-start-turn
; fix Ghost/Captain/Piazza/Innovation(with Cobbler?) with Citadel/Enchantress
; test swiftable for mixed choices
; fix Scepter / Caravan Guard reaction - not played this turn
; check Experiment with Innovation
; fix extra Outpost turn after Fleet turn
; fix Torturer / Masquerade (empty hand)
; unit tests for affect-other-players, give-choice, choose, calc-victory-points, ...

; Front End:
; fix Fool choosing Moon's Gift last (after Earth's or Mountain's)
; show Actions as unbuyable when Deluded (and in action phase)
; make extra-cards (Prizes, Spirits, etc.) and unique cards (Zombies & Heirlooms) visible/choosable for Wishing Well / Doctor / Journeyman / Pursue
; hide revealed cards on any movement (Border Guard)
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

(defn goto-buy-phase []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/set-phase {:player-no current-player
                                                           :phase     :pay})
                                            op/check-stack))
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

(defn buy-event [event-name]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (op/buy-event current-player event-name)))
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
