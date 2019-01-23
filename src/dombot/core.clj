(ns dombot.core
  (:gen-class))

(defn get-vp [deck {:keys [:vp]}]
  (if (fn? vp)
    (vp deck)
    vp))

(defn calc-victory-points [cards]
  (->> cards
       (filter (comp :victory :type))
       (map (partial get-vp cards))
       (apply +)))

(defn start-round [player]
  (assoc player :actions 1
                :coins 0
                :buys 1))

(defn get-pile-idx [game card-name]
  (->> game
       :board
       :supply
       (keep-indexed (fn [idx pile]
                       (when ((comp #{card-name} :name :card) pile) {:idx idx :card (:card pile)})))
       first))

(defn gain [game player-no card-name]
  (let [{:keys [idx card]} (get-pile-idx game card-name)
        pile-size (get-in game [:board :supply idx :count])]
    (assert (and pile-size (< 0 pile-size)))
    (-> game
        (update-in [:board :supply idx :count] dec)
        (update-in [:players player-no :discard] concat [card]))))

(defn buy-card [game player-no card-name]
  (let [{:keys [buys coins]} (get-in game [:players player-no])
        {{:keys [cost]} :card} (get-pile-idx game card-name)]
    (assert (and buys (> buys 0)))
    (assert (and coins cost (>= coins cost)))
    (-> game
        (gain player-no card-name)
        (update-in [:players player-no :coins] - cost)
        (update-in [:players player-no :buys] - 1))))

(defn redupeat [val f n]
  (if (> n 0)
    (redupeat (f val) f (dec n))
    val))

(defn shuffle-discard [{:keys [deck discard] :as player}]
  (assert (empty? deck))
  (-> player
      (assoc :deck (shuffle discard))
      (assoc :discard [])))

(defn draw-one [{:keys [:deck :discard] :as player}]
  (if (empty? deck)
    (if (empty? discard)
      player
      (-> player
          shuffle-discard
          draw-one))
    (let [card (first deck)]
      (-> player
          (update :deck (partial drop 1))
          (update :hand conj card)))))

(defn draw [player n]
  (redupeat player draw-one n))

(defn clean-up [{:keys [play-area hand] :as player}]
  (-> player
      (update :discard concat play-area hand)
      (assoc :play-area []
             :hand [])
      (draw 5)))

(defn get-hand-idx [player card-name]
  (->> player
       :hand
       (keep-indexed (fn [idx {:keys [name] :as card}]
                       (when (= card-name name) {:idx idx :card card})))
       first))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn play [player card-name]
  (let [{:keys [idx card]} (get-hand-idx player card-name)]
    (assert card)
    (-> player
        (update :hand vec-remove idx)
        (update :play-area concat [card]))))

(defn play-treasure [game player-no card-name]
  (let [player (get-in game [:players player-no])
        {{:keys [coin-value]} :card} (get-hand-idx player card-name)]
    (assert coin-value)
    (-> game
        (update-in [:players player-no] play card-name)
        (update-in [:players player-no :coins] + coin-value))))

(defn play-action [game player-no card-name]
  (let [{:keys [actions] :as player} (get-in game [:players player-no])
        {{:keys [action-fn]} :card} (get-hand-idx player card-name)]
    (assert (and action-fn actions (< 0 actions)))
    (-> game
        (update-in [:players player-no] play card-name)
        (update-in [:players player-no :actions] - 1)
        (action-fn player-no))))

(def festival {:name      :festival :set :dominion :type #{:action} :cost 5
               :action-fn (fn festival-action [game player-no]
                            (-> game
                                (update-in [:players player-no :actions] + 2)
                                (update-in [:players player-no :coins] + 2)
                                (update-in [:players player-no :buys] + 1)))})

(def moat {:name      :moat :set :dominion :type #{:action :reaction} :cost 2
           :action-fn (fn moat-action [game player-no]
                        (-> game
                            (update-in [:players player-no] draw 2)))})

(def laboratory {:name      :laboratory :set :dominion :type #{:action} :cost 5
                 :action-fn (fn laboratory-action [game player-no]
                              (-> game
                                  (update-in [:players player-no] draw 2)
                                  (update-in [:players player-no :actions] + 1)))})

(def market {:name      :market :set :dominion :type #{:action} :cost 5
             :action-fn (fn market-action [game player-no]
                          (-> game
                              (update-in [:players player-no] draw 1)
                              (update-in [:players player-no :actions] + 1)
                              (update-in [:players player-no :coins] + 1)
                              (update-in [:players player-no :buys] + 1)))})

(def smithy {:name      :smithy :set :dominion :type #{:action} :cost 4
             :action-fn (fn smithy-action [game player-no]
                          (-> game
                              (update-in [:players player-no] draw 3)))})

(def village {:name      :village :set :dominion :type #{:action} :cost 3
              :action-fn (fn village-action [game player-no]
                           (-> game
                               (update-in [:players player-no] draw 1)
                               (update-in [:players player-no :actions] + 2)))})

(def woodcutter {:name      :woodcutter :set :dominion :type #{:action} :cost 3
                 :action-fn (fn woodcutter-action [game player-no]
                              (-> game
                                  (update-in [:players player-no :coins] + 2)
                                  (update-in [:players player-no :buys] + 1)))})

(def kingdom-cards [{:name :cellar :set :dominion :type #{:action} :cost 2}
                    {:name :chapel :set :dominion :type #{:action} :cost 2}
                    moat
                    {:name :harbinger :set :dominion :type #{:action} :cost 3}
                    {:name :merchant :set :dominion :type #{:action} :cost 3}
                    {:name :vassal :set :dominion :type #{:action} :cost 3}
                    village
                    {:name :workshop :set :dominion :type #{:action} :cost 3}
                    {:name :bureaucrat :set :dominion :type #{:action :attack} :cost 4}
                    {:name :gardens :set :dominion :type #{:victory} :cost 4
                     :vp   (fn [deck]
                             (Math/floorDiv (int (count deck)) (int 10)))}
                    {:name :militia :set :dominion :type #{:action} :cost 4}
                    {:name :moneylender :set :dominion :type #{:action} :cost 4}
                    {:name :poacher :set :dominion :type #{:action} :cost 4}
                    {:name :remodel :set :dominion :type #{:action} :cost 4}
                    smithy
                    {:name :throne-room :set :dominion :type #{:action} :cost 4}
                    {:name :bandit :set :dominion :type #{:action} :cost 5}
                    {:name :council-room :set :dominion :type #{:action} :cost 5}
                    festival
                    laboratory
                    {:name :library :set :dominion :type #{:action} :cost 5}
                    market
                    {:name :mine :set :dominion :type #{:action} :cost 5}
                    {:name :sentry :set :dominion :type #{:action} :cost 5}
                    {:name :witch :set :dominion :type #{:action} :cost 5}
                    {:name :artisan :set :dominion :type #{:action} :cost 6}])

(def curse {:name :curse :type #{:curse} :cost 0 :vp -1})
(def estate {:name :estate :type #{:victory} :cost 2 :vp 1})
(def duchy {:name :duchy :type #{:victory} :cost 5 :vp 3})
(def province {:name :province :type #{:victory} :cost 8 :vp 6})
(def copper {:name :copper :type #{:treasure} :cost 0 :coin-value 1})
(def silver {:name :silver :type #{:treasure} :cost 3 :coin-value 2})
(def gold {:name :gold :type #{:treasure} :cost 6 :coin-value 3})

(defn generate-player []
  (let [deck (->> (concat (repeat 7 copper) (repeat 3 estate))
                  shuffle)]
    {:hand    (take 5 deck)
     :deck    (drop 5 deck)
     :in-play []
     :discard []}))

(defn generate-game [num-players]
  (let [num-vic (case num-players
                  2 8
                  3 12
                  4 12)]
    {:board   {:supply (vec (concat [{:card curse :count (* 10 (dec num-players))}
                                     {:card estate :count num-vic}
                                     {:card duchy :count num-vic}
                                     {:card province :count num-vic}
                                     {:card copper :count (- 60 (* 7 num-players))}
                                     {:card silver :count 40}
                                     {:card gold :count 30}]
                                    (->> kingdom-cards
                                         (filter (comp #{:dominion} :set))
                                         shuffle
                                         (take 10)
                                         (sort-by (juxt :cost :name))
                                         (map (fn [{:keys [:type] :as card}]
                                                {:card card :count (if (:victory type) num-vic 10)})))))}
     :players (vec (repeatedly num-players generate-player))}))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
