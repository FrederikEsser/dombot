(ns dombot.cards
  (:require [dombot.operations :refer [draw]]
            [dombot.utils :refer [mapv-indexed]]))

(def curse {:name :curse :type #{:curse} :cost 0 :vp -1})
(def estate {:name :estate :type #{:victory} :cost 2 :vp 1})
(def duchy {:name :duchy :type #{:victory} :cost 5 :vp 3})
(def province {:name :province :type #{:victory} :cost 8 :vp 6})
(def copper {:name :copper :type #{:treasure} :cost 0 :coin-value 1})
(def silver {:name :silver :type #{:treasure} :cost 3 :coin-value 2})
(def gold {:name :gold :type #{:treasure} :cost 6 :coin-value 3})

(def council-room {:name      :council-room :set :dominion :type #{:action} :cost 5
                   :action-fn (fn council-room-action [game player-no]
                                (-> game
                                    (update-in [:players player-no] draw 4)
                                    (update :players (partial mapv-indexed (fn [idx player]
                                                                             (cond-> player
                                                                                     (not= idx player-no) (draw 1)))))
                                    (update-in [:players player-no :buys] + 1)))})

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

(def cellar {:name :cellar :set :dominion :type #{:action} :cost 2})
(def chapel {:name :chapel :set :dominion :type #{:action} :cost 2})
(def harbinger {:name :harbinger :set :dominion :type #{:action} :cost 3})
(def merchant {:name :merchant :set :dominion :type #{:action} :cost 3})
(def vassal {:name :vassal :set :dominion :type #{:action} :cost 3})
(def workshop {:name :workshop :set :dominion :type #{:action} :cost 3})
(def bureaucrat {:name :bureaucrat :set :dominion :type #{:action :attack} :cost 4})
(def gardens {:name :gardens :set :dominion :type #{:victory} :cost 4
              :vp   (fn [deck]
                      (Math/floorDiv (int (count deck)) (int 10)))})
(def militia {:name :militia :set :dominion :type #{:action} :cost 4})
(def moneylender {:name :moneylender :set :dominion :type #{:action} :cost 4})
(def poacher {:name :poacher :set :dominion :type #{:action} :cost 4})
(def remodel {:name :remodel :set :dominion :type #{:action} :cost 4})
(def throne-room {:name :throne-room :set :dominion :type #{:action} :cost 4})
(def bandit {:name :bandit :set :dominion :type #{:action} :cost 5})
(def library {:name :library :set :dominion :type #{:action} :cost 5})
(def mine {:name :mine :set :dominion :type #{:action} :cost 5})
(def sentry {:name :sentry :set :dominion :type #{:action} :cost 5})
(def witch {:name :witch :set :dominion :type #{:action} :cost 5})
(def artisan {:name :artisan :set :dominion :type #{:action} :cost 6})

(def kingdom-cards [moat
                    village
                    smithy
                    council-room
                    festival
                    laboratory
                    market])

(defn player []
  (let [deck (->> (concat (repeat 7 copper) (repeat 3 estate))
                  shuffle)]
    {:hand (take 5 deck)
     :deck (drop 5 deck)}))

(defn base-supply [number-of-players victory-pile-size]
  [{:card curse :count (* 10 (dec number-of-players))}
   {:card estate :count victory-pile-size}
   {:card duchy :count victory-pile-size}
   {:card province :count victory-pile-size}
   {:card copper :count (- 60 (* 7 number-of-players))}
   {:card silver :count 40}
   {:card gold :count 30}])

(defn kingdom [sets victory-pile-size]
  (->> kingdom-cards
       (filter (comp sets :set))
       shuffle
       (take 10)
       (sort-by (juxt :cost :name))
       (map (fn [{:keys [:type] :as card}]
              {:card card :count (if (:victory type) victory-pile-size 10)}))))

(defn game [number-of-players]
  (let [victory-pile-size (case number-of-players
                            2 8
                            3 12
                            4 12)]
    {:supply  (vec (concat (base-supply number-of-players victory-pile-size)
                           (kingdom #{:dominion} victory-pile-size)))
     :players (vec (repeatedly number-of-players player))}))

