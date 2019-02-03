(ns dombot.cards.dominion
  (:require [dombot.operations :refer [move-card move-cards draw gain
                                       do-for-other-players attack-other-players
                                       card-effect push-effect-stack give-choice]]
            [dombot.cards.common :refer :all]
            [dombot.utils :as ut]))

(def artisan {:name      :artisan :set :dominion :type #{:action} :cost 6
              :action-fn (fn artisan-action [game player-no]
                           (-> game
                               (push-effect-stack player-no {:action-fn give-topdeck-choice})
                               (give-choice player-no {:text       "Gain a card to your hand costing up to $5."
                                                       :choice-fn  gain-to-hand
                                                       :options-fn (ut/supply-piles {:max-cost 5})
                                                       :min        1
                                                       :max        1})))})

(defn bandit-attack [game player-no]
  (-> game
      (move-cards player-no {:number-of-cards 2
                             :from            :deck
                             :from-position   :top
                             :to              :reveal})
      (push-effect-stack player-no {:action-fn discard-all-revealed})
      (give-choice player-no {:text       "Trash a revealed Treasure other than Copper, and discards the rest."
                              :player-no  player-no
                              :choice-fn  trash-from-revealed
                              :options-fn (ut/player-area :reveal (fn [{:keys [name type]}]
                                                                    (and (:treasure type)
                                                                         (not= :copper name))))
                              :min        1
                              :max        1})))

(def bandit {:name      :bandit :set :dominion :type #{:action :attack} :cost 5
             :action-fn (fn bandit-action [game player-no unaffected]
                          (-> game
                              (gain player-no :gold)
                              (attack-other-players player-no unaffected bandit-attack)))})

(defn bureaucrat-attack [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (if (some (comp :victory :type) hand)
      (give-choice game player-no {:text       "Reveal a Victory card from your hand and put it onto your deck."
                                   :choice-fn  topdeck-from-hand
                                   :options-fn (ut/player-area :hand (comp :victory :type))
                                   :min        1
                                   :max        1})
      (-> game
          (assoc-in [:reveal player-no] hand)))))

(def bureaucrat {:name      :bureaucrat :set :dominion :type #{:action :attack} :cost 4
                 :action-fn (fn bureaucrat-action [game player-no unaffected]
                              (-> game
                                  (gain-to-topdeck player-no :silver)
                                  (attack-other-players player-no unaffected bureaucrat-attack)))})

(defn cellar-sift [game player-no card-names]
  (-> game
      (move-cards player-no {:card-names card-names
                             :from       :hand
                             :to         :discard})
      (draw player-no (count card-names))))

(def cellar {:name      :cellar :set :dominion :type #{:action} :cost 2
             :action-fn (fn cellar-action [game player-no]
                          (-> game
                              (update-in [:players player-no :actions] + 1)
                              (give-choice player-no {:text       "Discard any number of cards, then draw that many."
                                                      :choice-fn  cellar-sift
                                                      :options-fn (ut/player-area :hand)})))})

(def chapel {:name      :chapel :set :dominion :type #{:action} :cost 2
             :action-fn (fn chapel-action [game player-no]
                          (-> game
                              (give-choice player-no {:text       "Trash up to 4 cards from your hand."
                                                      :choice-fn  trash-from-hand
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
  (cond-> game
          card-name (move-card player-no {:card-name     card-name
                                          :from          :hand
                                          :from-position :bottom
                                          :to            :set-aside})))

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
      (-> game
          (move-cards player-no {:card-names (map :name set-aside)
                                 :from       :set-aside
                                 :to         :discard}))
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

(defn militia-attack [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (> (count hand) 3) (give-choice player-no {:text       "Discard down to 3 cards in hand."
                                                       :choice-fn  discard-from-hand
                                                       :options-fn (ut/player-area :hand)
                                                       :min        (- (count hand) 3)
                                                       :max        (- (count hand) 3)}))))

(def militia {:name      :militia :set :dominion :type #{:action :attack} :cost 4
              :action-fn (fn militia-action [game player-no unaffected]
                           (-> game
                               (update-in [:players player-no :coins] + 2)
                               (attack-other-players player-no unaffected militia-attack)))})

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

(defn moat-reaction [{:keys [effect-stack] :as game} player-no card-name]
  ; todo: make sure it's the correct attack effect
  (let [{:keys [idx]} (ut/get-effect-idx effect-stack :attack)]
    (assert idx)
    (cond-> game
            (= :moat card-name) (assoc :effect-stack (-> effect-stack
                                                         vec
                                                         (update-in [idx :unaffected] conj player-no))))))

(def moat {:name      :moat :set :dominion :type #{:action :reaction} :cost 2
           :action-fn (fn moat-action [game player-no]
                        (-> game
                            (draw player-no 2)))
           :reaction  {:attack {:text        "You may reveal a Moat from your hand, to be unaffected by"
                                :reaction-fn moat-reaction}}})

(defn moneylender-trash [game player-no card-name]
  (cond-> game
          (= :copper card-name) (-> (move-card player-no {:card-name :copper
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

(def poacher {:name      :poacher :set :dominion :type #{:action} :cost 4
              :action-fn (fn poacher-action [game player-no]
                           (let [empty-piles (ut/empty-supply-piles game)]
                             (-> game
                                 (draw player-no 1)
                                 (update-in [:players player-no :actions] + 1)
                                 (update-in [:players player-no :coins] + 1)
                                 (cond-> (< 0 empty-piles) (give-choice player-no {:text       (str "Discard a card per empty supply pile [" empty-piles "].")
                                                                                   :choice-fn  discard-from-hand
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
                              (move-cards player-no {:number-of-cards 2
                                                     :from            :deck
                                                     :from-position   :top
                                                     :to              :look-at})
                              (give-choice player-no {:text       "Trash any number of the top 2 cards of your deck."
                                                      :choice-fn  sentry-trash
                                                      :options-fn (ut/player-area :look-at)})))})

(def smithy {:name      :smithy :set :dominion :type #{:action} :cost 4
             :action-fn (fn smithy-action [game player-no]
                          (-> game
                              (draw player-no 3)))})

(def throne-room {:name      :throne-room :set :dominion :type #{:action} :cost 4
                  :action-fn (fn throne-room-action [game player-no]
                               (-> game
                                   (give-choice player-no {:text       "You may play an Action card from your hand twice."
                                                           :choice-fn  play-action-twice
                                                           :options-fn (ut/player-area :hand (comp :action :type))
                                                           :max        1})))})

(defn vassal-play-action [game player-no card-name]
  (let [{:keys [discard]} (get-in game [:players player-no])
        {:keys [name] :as card} (last discard)]
    (cond-> game
            (= name card-name) (-> (move-card player-no {:card-name     card-name
                                                         :from          :discard
                                                         :from-position :bottom
                                                         :to            :play-area})
                                   (card-effect player-no card)))))

(def vassal {:name      :vassal :set :dominion :type #{:action} :cost 3
             :action-fn (fn vassal-action [game player-no]
                          (-> game
                              (update-in [:players player-no :coins] + 2)
                              (move-card player-no {:from          :deck
                                                    :from-position :top
                                                    :to            :discard})
                              (as-> game
                                    (let [{:keys [discard]} (get-in game [:players player-no])
                                          {:keys [name type]} (last discard)]
                                      (cond-> game
                                              (:action type) (give-choice player-no {:text      (str "You may play the discarded " (ut/format-name name) ".")
                                                                                     :choice-fn vassal-play-action
                                                                                     :options   [name]
                                                                                     :max       1}))))))})

(def village {:name      :village :set :dominion :type #{:action} :cost 3
              :action-fn (fn village-action [game player-no]
                           (-> game
                               (draw player-no 1)
                               (update-in [:players player-no :actions] + 2)))})

(def witch {:name      :witch :set :dominion :type #{:action :attack} :cost 5
            :action-fn (fn witch-action [game player-no unaffected]
                         (-> game
                             (draw player-no 2)
                             (attack-other-players player-no unaffected gain :curse)))})

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

(def kingdom-cards [artisan
                    bandit
                    bureaucrat
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
                    militia
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
