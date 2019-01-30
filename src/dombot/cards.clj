(ns dombot.cards
  (:require [dombot.operations :refer [draw gain gain-to-hand do-for-other-players move-card move-cards give-choice push-effect-stack]]
            [dombot.utils :as ut]))

(def curse {:name :curse :type #{:curse} :cost 0 :victory-points -1})
(def estate {:name :estate :type #{:victory} :cost 2 :victory-points 1})
(def duchy {:name :duchy :type #{:victory} :cost 5 :victory-points 3})
(def province {:name :province :type #{:victory} :cost 8 :victory-points 6})
(def copper {:name :copper :type #{:treasure} :cost 0 :coin-value 1})
(def silver {:name :silver :type #{:treasure} :cost 3 :coin-value 2})
(def gold {:name :gold :type #{:treasure} :cost 6 :coin-value 3})

(defn topdeck-from-hand [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name   card-name
                                          :from        :hand
                                          :to          :deck
                                          :to-position :top})))

(defn artisan-topdeck-choice [game player-no]
  (-> game
      (give-choice player-no {:text       "Put a card from your hand onto your deck."
                              :choice-fn  topdeck-from-hand
                              :options-fn (ut/player-area :hand)
                              :min        1
                              :max        1})))

(def artisan {:name      :artisan :set :dominion :type #{:action} :cost 6
              :action-fn (fn artisan-action [game player-no]
                           (-> game
                               (push-effect-stack player-no {:action-fn artisan-topdeck-choice})
                               (give-choice player-no {:text       "Gain a card to your hand costing up to $5."
                                                       :choice-fn  gain-to-hand
                                                       :options-fn (ut/supply-piles {:max-cost 5})
                                                       :min        1
                                                       :max        1})))})

(defn cellar-sift [game player-no card-names]
  (-> game
      (move-cards player-no {:card-names card-names
                             :from       :hand
                             :to         :discard})
      (update-in [:players player-no] draw (count card-names))))

(def cellar {:name      :cellar :set :dominion :type #{:action} :cost 2
             :action-fn (fn cellar-action [game player-no]
                          (-> game
                              (update-in [:players player-no :actions] + 1)
                              (give-choice player-no {:text       "Discard any number of cards, then draw that many."
                                                      :choice-fn  cellar-sift
                                                      :options-fn (ut/player-area :hand)})))})

(defn trash [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :hand
                              :to         :trash}))

(def chapel {:name      :chapel :set :dominion :type #{:action} :cost 2
             :action-fn (fn chapel-action [game player-no]
                          (-> game
                              (give-choice player-no {:text       "Trash up to 4 cards from your hand."
                                                      :choice-fn  trash
                                                      :options-fn (ut/player-area :hand)
                                                      :max        4})))})

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

(defn topdeck-from-discard [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name   card-name
                                          :from        :discard
                                          :to          :deck
                                          :to-position :top})))

(def harbinger {:name      :harbinger :set :dominion :type #{:action} :cost 3
                :action-fn (fn harbinger-action [game player-no]
                             (-> game
                                 (draw player-no 1)
                                 (update-in [:players player-no :actions] + 1)
                                 (give-choice player-no {:text       "You may put a card from your discard pile onto your deck."
                                                         :choice-fn  topdeck-from-discard
                                                         :options-fn (ut/player-area :discard)
                                                         :max        1})))})

(def laboratory {:name      :laboratory :set :dominion :type #{:action} :cost 5
                 :action-fn (fn laboratory-action [game player-no]
                              (-> game
                                  (draw player-no 2)
                                  (update-in [:players player-no :actions] + 1)))})

(defn library-set-aside [game player-no card-name]
  (let [{:keys [hand]} (get-in game [:players player-no])
        {:keys [name]} (last hand)]
    (cond-> game
            (= name card-name) (move-card player-no {:card-name     card-name
                                                     :from          :hand
                                                     :from-position :bottom
                                                     :to            :set-aside}))))

(defn library-check-for-action [game player-no]
  (let [{:keys [hand]} (get-in game [:players player-no])
        {:keys [type name]} (last hand)]
    (cond-> game
            (:action type) (give-choice player-no {:text      (str "You may skip the " (ut/format-name name) "; set it aside, discarding it afterwards.")
                                                   :choice-fn library-set-aside
                                                   :options   [name]
                                                   :max       1}))))

(defn library-action [game player-no]
  (let [{:keys [hand deck discard set-aside]} (get-in game [:players player-no])]
    (if (or (>= (count hand) 7) (empty? (concat deck discard)))
      (cond-> game
              (not-empty set-aside) (-> (update-in [:players player-no :discard] concat set-aside)
                                        (update-in [:players player-no] dissoc :set-aside)))
      (-> game
          (push-effect-stack player-no {:action-fn library-action})
          (draw player-no 1)
          (library-check-for-action player-no)))))

(def library {:name      :library :set :dominion :type #{:action} :cost 5
              :action-fn library-action})

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

(defn mine-trash [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {{:keys [cost]} :card} (ut/get-card-idx player :hand card-name)
        max-cost (+ 3 cost)]
    (-> game
        (move-card player-no {:card-name card-name
                              :from      :hand
                              :to        :trash})
        (give-choice player-no {:text       (str "Gain a Treasure to your hand costing up to $" max-cost ".")
                                :choice-fn  gain-to-hand
                                :options-fn (ut/supply-piles {:max-cost max-cost :type :treasure})
                                :min        1
                                :max        1}))))

(def mine {:name      :mine :set :dominion :type #{:action} :cost 5
           :action-fn (fn mine-action [game player-no]
                        (-> game
                            (give-choice player-no {:text       "You may trash a Treasure from your hand."
                                                    :choice-fn  mine-trash
                                                    :options-fn (ut/player-area :hand (comp :treasure :type))
                                                    :max        1})))})

(defn moneylender-trash [game player-no do-trash?]
  (cond-> game
          do-trash? (-> (move-card player-no {:card-name :copper
                                              :from      :hand
                                              :to        :trash})
                        (update-in [:players player-no :coins] + 3))))

(def moneylender {:name      :moneylender :set :dominion :type #{:action} :cost 4
                  :action-fn (fn moneylender-action [game player-no]
                               (-> game
                                   (give-choice player-no {:text       "You may trash a Copper from your hand for +$3"
                                                           :choice-fn  moneylender-trash
                                                           :options-fn (ut/player-area :hand (comp #{:copper} :name))
                                                           :max        1})))})

(defn discard [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :hand
                              :to         :discard}))

(def poacher {:name      :poacher :set :dominion :type #{:action} :cost 4
              :action-fn (fn poacher-action [game player-no]
                           (let [empty-piles (ut/empty-supply-piles game)]
                             (-> game
                                 (draw player-no 1)
                                 (update-in [:players player-no :actions] + 1)
                                 (update-in [:players player-no :coins] + 1)
                                 (cond-> (< 0 empty-piles) (give-choice player-no {:text       (str "Discard a card per empty supply pile [" empty-piles "].")
                                                                                   :choice-fn  discard
                                                                                   :options-fn (ut/player-area :hand)
                                                                                   :min        empty-piles
                                                                                   :max        empty-piles})))))})

(defn remodel-trash [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {{:keys [cost]} :card} (ut/get-card-idx player :hand card-name)
        max-cost (+ 2 cost)]
    (-> game
        (move-card player-no {:card-name card-name
                              :from      :hand
                              :to        :trash})
        (give-choice player-no {:text       (str "Gain a card costing up to $" max-cost ".")
                                :choice-fn  gain
                                :options-fn (ut/supply-piles {:max-cost max-cost})
                                :min        1
                                :max        1}))))

(def remodel {:name      :remodel :set :dominion :type #{:action} :cost 4
              :action-fn (fn remodel-action [game player-no]
                           (-> game
                               (give-choice player-no {:text       "Trash a card from your hand."
                                                       :choice-fn  remodel-trash
                                                       :options-fn (ut/player-area :hand)
                                                       :min        1
                                                       :max        1})))})

(defn sentry-topdeck [game player-no card-names]
  (-> game
      (move-cards player-no {:card-names  card-names
                             :from        :look-at
                             :to          :deck
                             :to-position :top})))

(defn sentry-discard [game player-no card-names]
  (-> game
      (move-cards player-no {:card-names card-names
                             :from       :look-at
                             :to         :discard})
      (give-choice player-no {:text       "Put the rest back on top in any order."
                              :choice-fn  sentry-topdeck
                              :options-fn (ut/player-area :look-at)
                              :min        2})))

(defn sentry-trash [game player-no card-names]
  (-> game
      (move-cards player-no {:card-names card-names
                             :from       :look-at
                             :to         :trash})
      (give-choice player-no {:text       "Discard any number of the top 2 cards of your deck."
                              :choice-fn  sentry-discard
                              :options-fn (ut/player-area :look-at)})))

(def sentry {:name      :sentry :set :dominion :type #{:action} :cost 5
             :action-fn (fn sentry-action [game player-no]
                          (-> game
                              (draw player-no 1)
                              (update-in [:players player-no :actions] + 1)
                              (move-card player-no {:from          :deck
                                                    :from-position :top
                                                    :to            :look-at})
                              (move-card player-no {:from          :deck
                                                    :from-position :top
                                                    :to            :look-at})
                              (give-choice player-no {:text       "Trash any number of the top 2 cards of your deck."
                                                      :choice-fn  sentry-trash
                                                      :options-fn (ut/player-area :look-at)})))})

(def smithy {:name      :smithy :set :dominion :type #{:action} :cost 4
             :action-fn (fn smithy-action [game player-no]
                          (-> game
                              (draw player-no 3)))})

(defn play-action-twice [game player-no card-name]
  (if card-name
    (let [player (get-in game [:players player-no])
          {{:keys [action-fn] :as card} :card} (ut/get-card-idx player :hand card-name)]
      (-> game
          (push-effect-stack player-no card)
          (move-card player-no {:card-name card-name
                                :from      :hand
                                :to        :play-area})
          (action-fn player-no)))
    game))

(def throne-room {:name      :throne-room :set :dominion :type #{:action} :cost 4
                  :action-fn (fn throne-room-action [game player-no]
                               (-> game
                                   (give-choice player-no {:text       "You may play an Action card from your hand twice."
                                                           :choice-fn  play-action-twice
                                                           :options-fn (ut/player-area :hand (comp :action :type))
                                                           :max        1})))})

(defn play-discard-action [game player-no card-name]
  (let [{:keys [discard]} (get-in game [:players player-no])
        {:keys [name action-fn]} (last discard)]
    (cond-> game
            (= name card-name) (-> (move-card player-no {:card-name     card-name
                                                         :from          :discard
                                                         :from-position :bottom
                                                         :to            :play-area})
                                   (action-fn player-no)))))

(def vassal {:name      :vassal :set :dominion :type #{:action} :cost 3
             :action-fn (fn vassal-action [game player-no]
                          (-> game
                              (update-in [:players player-no :coins] + 2)
                              (move-card player-no {:from          :deck
                                                    :from-position :top
                                                    :to            :discard})
                              ((fn [game]
                                 (let [{:keys [discard]} (get-in game [:players player-no])
                                       {:keys [name action-fn]} (last discard)]
                                   (cond-> game
                                           action-fn (give-choice player-no {:text      (str "You may play the discarded " (ut/format-name name) ".")
                                                                             :choice-fn play-discard-action
                                                                             :options   [name]
                                                                             :max       1})))))))})

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
                                (give-choice player-no {:text       "Gain a card costing up to $4."
                                                        :choice-fn  gain
                                                        :options-fn (ut/supply-piles {:max-cost 4})
                                                        :min        1
                                                        :max        1})))})

;; ATTACK WITH CHOICE
(def bandit {:name :bandit :set :dominion :type #{:action} :cost 5})
(def militia {:name :militia :set :dominion :type #{:action} :cost 4})
(def bureaucrat {:name :bureaucrat :set :dominion :type #{:action :attack} :cost 4})

;; REACTION
(def moat {:name      :moat :set :dominion :type #{:action :reaction} :cost 2
           :action-fn (fn moat-action [game player-no]
                        (-> game
                            (draw player-no 2)))})

(def kingdom-cards [artisan
                    cellar
                    chapel
                    council-room
                    festival
                    gardens
                    harbinger
                    laboratory
                    library
                    market
                    merchant
                    mine
                    moat
                    moneylender
                    poacher
                    remodel
                    sentry
                    smithy
                    throne-room
                    vassal
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

(defn player [name]
  (let [deck (->> (concat (repeat 7 copper) (repeat 3 estate))
                  shuffle)]
    {:name name
     :hand (take 5 deck)
     :deck (drop 5 deck)}))

(defn game [player-names]
  (let [number-of-players (count player-names)
        victory-pile-size (case number-of-players
                            2 8
                            3 12
                            4 12)]
    {:supply         (vec (concat (base-supply number-of-players victory-pile-size)
                                  (kingdom #{:dominion} victory-pile-size)))
     :players        (vec (map player player-names))
     :current-player (rand-int number-of-players)}))

