(ns dombot.cards
  (:require [dombot.operations :refer [draw gain do-for-other-players move-card give-choice]]
            [dombot.utils :refer [mapv-indexed]]
            [dombot.utils :as ut]))

(def curse {:name :curse :type #{:curse} :cost 0 :victory-points -1})
(def estate {:name :estate :type #{:victory} :cost 2 :victory-points 1})
(def duchy {:name :duchy :type #{:victory} :cost 5 :victory-points 3})
(def province {:name :province :type #{:victory} :cost 8 :victory-points 6})
(def copper {:name :copper :type #{:treasure} :cost 0 :coin-value 1})
(def silver {:name :silver :type #{:treasure} :cost 3 :coin-value 2})
(def gold {:name :gold :type #{:treasure} :cost 6 :coin-value 3})

(defn trash [game player-no card-names]
  (reduce (fn [game card-name] (move-card game player-no card-name :hand :trash)) game card-names))

(def chapel {:name      :chapel :set :dominion :type #{:action} :cost 2
             :action-fn (fn chapel-action [game player-no]
                          (-> game
                              (give-choice player-no trash ut/player-hand {:max 4})))})

(defn cellar-sift [game player-no card-names]
  (-> (reduce (fn [game card-name] (move-card game player-no card-name :hand :discard)) game card-names)
      (update-in [:players player-no] draw (count card-names))))

(def cellar {:name      :cellar :set :dominion :type #{:action} :cost 2
             :action-fn (fn cellar-action [game player-no]
                          (-> game
                              (update-in [:players player-no :actions] + 1)
                              (give-choice player-no cellar-sift ut/player-hand)))})

(def council-room {:name      :council-room :set :dominion :type #{:action} :cost 5
                   :action-fn (fn council-room-action [game player-no]
                                (-> game
                                    (draw player-no 4)
                                    (update-in [:players player-no :buys] + 1)
                                    (do-for-other-players player-no draw 1)))})

(def festival {:name      :festival :set :dominion :type #{:action} :cost 5
               :action-fn (fn festival-action [game player-no]
                            (-> game
                                (update-in [:players player-no :actions] + 2)
                                (update-in [:players player-no :coins] + 2)
                                (update-in [:players player-no :buys] + 1)))})

(def gardens {:name           :gardens :set :dominion :type #{:victory} :cost 4
              :victory-points (fn [cards]
                                (Math/floorDiv (int (count cards)) (int 10)))})

(defn harbinger-topdeck [game player-no card-name]
  (cond-> game
          card-name (move-card player-no card-name :discard :deck :top)))

(def harbinger {:name      :harbinger :set :dominion :type #{:action} :cost 3
                :action-fn (fn harbinger-action [game player-no]
                             (-> game
                                 (draw player-no 1)
                                 (update-in [:players player-no :actions] + 1)
                                 (give-choice player-no harbinger-topdeck ut/player-discard {:max 1})))})

(def laboratory {:name      :laboratory :set :dominion :type #{:action} :cost 5
                 :action-fn (fn laboratory-action [game player-no]
                              (-> game
                                  (draw player-no 2)
                                  (update-in [:players player-no :actions] + 1)))})

(def market {:name      :market :set :dominion :type #{:action} :cost 5
             :action-fn (fn market-action [game player-no]
                          (-> game
                              (draw player-no 1)
                              (update-in [:players player-no :actions] + 1)
                              (update-in [:players player-no :coins] + 1)
                              (update-in [:players player-no :buys] + 1)))})

(def merchant-trigger {:trigger-id [:play :silver]
                       :trigger-fn (fn [game player-no]
                                     (-> game
                                         (update-in [:players player-no :coins] + 1)))})

(def merchant {:name      :merchant :set :dominion :type #{:action} :cost 3
               :action-fn (fn merchant-action [game player-no]
                            (-> game
                                (draw player-no 1)
                                (update-in [:players player-no :actions] + 1)
                                (update-in [:players player-no :triggers] concat [merchant-trigger])))})

(defn moneylender-trash [game player-no do-trash?]
  (cond-> game
          do-trash? (-> (move-card player-no :copper :hand :trash)
                        (update-in [:players player-no :coins] + 3))))

(def moneylender {:name      :moneylender :set :dominion :type #{:action} :cost 4
                  :action-fn (fn moneylender-action [game player-no]
                               (-> game
                                   (give-choice player-no moneylender-trash
                                                (comp (partial filter #{:copper}) ut/player-hand)
                                                {:max 1})))})

(def smithy {:name      :smithy :set :dominion :type #{:action} :cost 4
             :action-fn (fn smithy-action [game player-no]
                          (-> game
                              (draw player-no 3)))})

(defn play-action-twice [game player-no card-name]
  (if card-name
    (let [player (get-in game [:players player-no])
          {{:keys [type action-fn] :as card} :card} (ut/get-card-idx player :hand card-name)]
      (assert card (str "Play error: There is no " (ut/format-name card-name) " in your Hand."))
      (assert (:action type) (str "Play error: " (ut/format-name card-name) " is not an Action."))
      (assert action-fn (str "Play error: " (ut/format-name card-name) " has no action function."))
      (-> game
          (move-card player-no card-name :hand :play-area)
          (action-fn player-no)
          (action-fn player-no)))
    game))

(def throne-room {:name      :throne-room :set :dominion :type #{:action} :cost 4
                  :action-fn (fn throne-room-action [game player-no]
                               (-> game
                                   (give-choice player-no play-action-twice
                                                (partial ut/player-hand (comp :action :type))
                                                {:max 1})))})

(def village {:name      :village :set :dominion :type #{:action} :cost 3
              :action-fn (fn village-action [game player-no]
                           (-> game
                               (draw player-no 1)
                               (update-in [:players player-no :actions] + 2)))})

(def witch {:name      :witch :set :dominion :type #{:action} :cost 5
            :action-fn (fn witch-action [game player-no]
                         (-> game
                             (draw player-no 2)
                             (do-for-other-players player-no gain :curse)))})

(def woodcutter {:name      :woodcutter :set :dominion :type #{:action} :cost 3
                 :action-fn (fn woodcutter-action [game player-no]
                              (-> game
                                  (update-in [:players player-no :coins] + 2)
                                  (update-in [:players player-no :buys] + 1)))})

(def workshop {:name      :workshop :set :dominion :type #{:action} :cost 3
               :action-fn (fn workshop-action [game player-no]
                            (-> game
                                (give-choice player-no gain (partial ut/supply-piles 4) {:min 1 :max 1})))})

;; ONE CHOICE
(def vassal {:name :vassal :set :dominion :type #{:action} :cost 3})
(def poacher {:name :poacher :set :dominion :type #{:action} :cost 4})

;; ATTACK WITH CHOICE
(def bureaucrat {:name :bureaucrat :set :dominion :type #{:action :attack} :cost 4})
(def militia {:name :militia :set :dominion :type #{:action} :cost 4})
(def bandit {:name :bandit :set :dominion :type #{:action} :cost 5})

;; MULTI CHOICES
(def remodel {:name :remodel :set :dominion :type #{:action} :cost 4})
(def mine {:name :mine :set :dominion :type #{:action} :cost 5})
(def sentry {:name :sentry :set :dominion :type #{:action} :cost 5})
(def artisan {:name :artisan :set :dominion :type #{:action} :cost 6})
(def library {:name :library :set :dominion :type #{:action} :cost 5})

;; REACTION
(def moat {:name      :moat :set :dominion :type #{:action :reaction} :cost 2
           :action-fn (fn moat-action [game player-no]
                        (-> game
                            (draw player-no 2)))})

(def kingdom-cards [chapel
                    cellar
                    council-room
                    festival
                    gardens
                    harbinger
                    laboratory
                    market
                    merchant
                    moat
                    moneylender
                    smithy
                    village
                    witch
                    workshop])

(defn base-supply [number-of-players victory-pile-size]
  [{:card curse :pile-size (* 10 (dec number-of-players))}
   {:card estate :pile-size victory-pile-size}
   {:card duchy :pile-size victory-pile-size}
   {:card province :pile-size victory-pile-size}
   {:card copper :pile-size (- 60 (* 7 number-of-players))}
   {:card silver :pile-size 40}
   {:card gold :pile-size 30}])

(defn kingdom [sets victory-pile-size]
  (->> kingdom-cards
       (filter (comp sets :set))
       shuffle
       (take 10)
       (sort-by (juxt :cost :name))
       (map (fn [{:keys [:type] :as card}]
              {:card card :pile-size (if (:victory type) victory-pile-size 10)}))))

(defn player []
  (let [deck (->> (concat (repeat 7 copper) (repeat 3 estate))
                  shuffle)]
    {:hand (take 5 deck)
     :deck (drop 5 deck)}))

(defn game [number-of-players]
  (let [victory-pile-size (case number-of-players
                            2 8
                            3 12
                            4 12)]
    {:supply         (vec (concat (base-supply number-of-players victory-pile-size)
                                  (kingdom #{:dominion} victory-pile-size)))
     :players        (vec (repeatedly number-of-players player))
     :current-player 0}))

