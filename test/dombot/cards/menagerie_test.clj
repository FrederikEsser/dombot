(ns dombot.cards.menagerie-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.menagerie :as menagerie :refer :all]
            [dombot.cards.dominion :refer [throne-room workshop]]
            [dombot.cards.kingdom :refer [setup-game]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest cavalry-test
  (let [cavalry (assoc cavalry :id 0)
        horse   (assoc horse :id 1)]
    (testing "Cavalry"
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [cavalry]
                                 :actions 1}]}
                 (play 0 :cavalry))
             {:extra-cards [{:card horse :pile-size 28}]
              :players     [{:play-area [cavalry]
                             :discard   [horse horse]
                             :actions   0}]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 1}]
                  :players     [{:hand    [cavalry]
                                 :actions 1}]}
                 (play 0 :cavalry))
             {:extra-cards [{:card horse :pile-size 0}]
              :players     [{:play-area [cavalry]
                             :discard   [horse]
                             :actions   0}]}))
      (testing "on buy"
        (is (= (-> {:supply  [{:card cavalry :pile-size 10}]
                    :players [{:deck    [silver silver copper]
                               :actions 0
                               :coins   4
                               :buys    1
                               :phase   :pay}]}
                   (buy-card 0 :cavalry))
               {:supply  [{:card cavalry :pile-size 9}]
                :players [{:hand    [silver silver]
                           :deck    [copper]
                           :discard [cavalry]
                           :actions 0
                           :coins   0
                           :buys    1
                           :phase   :action}]}))
        (is (= (-> {:supply  [{:card cavalry :pile-size 10}]
                    :players [{:deck    [copper]
                               :actions 0
                               :coins   4
                               :buys    1
                               :phase   :pay}]}
                   (buy-card 0 :cavalry))
               {:supply  [{:card cavalry :pile-size 9}]
                :players [{:hand    [copper]
                           :discard [cavalry]
                           :actions 0
                           :coins   0
                           :buys    1
                           :phase   :action}]}))
        (is (= (-> {:supply  [{:card cavalry :pile-size 10}]
                    :players [{:hand    [workshop]
                               :deck    [horse silver silver]
                               :actions 1
                               :coins   0
                               :buys    1
                               :phase   :action}]}
                   (play 0 :workshop)
                   (choose :cavalry))
               {:supply  [{:card cavalry :pile-size 9}]
                :players [{:hand      [horse silver]
                           :play-area [workshop]
                           :deck      [silver]
                           :discard   [cavalry]
                           :actions   0
                           :coins     0
                           :buys      2
                           :phase     :action}]}))))))

(deftest horse-test
  (let [horse (assoc horse :id 1)]
    (testing "Horse"
      (is (= (-> {:extra-cards [{:card horse :pile-size 29}]
                  :players     [{:hand    [horse]
                                 :deck    [copper copper copper]
                                 :actions 1}]}
                 (play 0 :horse))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:hand    [copper copper]
                             :deck    [copper]
                             :actions 1}]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 29}]
                  :players     [{:hand    [throne-room horse]
                                 :deck    [copper copper copper copper copper]
                                 :actions 1}]}
                 (play 0 :throne-room)
                 (choose :horse))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:hand      [copper copper copper copper]
                             :play-area [throne-room]
                             :deck      [copper]
                             :actions   2}]})))))

(deftest livery-test
  (let [livery (assoc livery :id 0)
        horse  (assoc horse :id 1)]
    (testing "Livery"
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [livery]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :livery))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:play-area [livery]
                             :actions   0
                             :coins     3
                             :triggers  [(get-trigger livery)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 9}]
                  :players     [{:hand    [livery]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :livery)
                 (gain {:player-no 0 :card-name :livery}))
             {:extra-cards [{:card horse :pile-size 29}]
              :supply      [{:card livery :pile-size 8}]
              :players     [{:play-area [livery]
                             :discard   [horse livery]
                             :actions   0
                             :coins     3
                             :triggers  [(get-trigger livery)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 9}]
                  :players     [{:hand    [livery]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :livery)
                 (gain {:player-no 0 :card-name :livery})
                 (gain {:player-no 0 :card-name :livery}))
             {:extra-cards [{:card horse :pile-size 28}]
              :supply      [{:card livery :pile-size 7}]
              :players     [{:play-area [livery]
                             :discard   [horse livery horse livery]
                             :actions   0
                             :coins     3
                             :triggers  [(get-trigger livery)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 8}]
                  :players     [{:hand    [livery livery]
                                 :actions 2
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :livery)
                 (play 0 :livery)
                 (buy-card 0 :livery))
             {:extra-cards [{:card horse :pile-size 28}]
              :supply      [{:card livery :pile-size 7}]
              :players     [{:play-area [livery livery]
                             :discard   [horse horse livery]
                             :actions   0
                             :coins     1
                             :buys      0
                             :triggers  [(get-trigger livery)
                                         (get-trigger livery 2)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 9}]
                  :players     [{:hand    [throne-room livery]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :throne-room)
                 (choose :livery)
                 (gain {:player-no 0 :card-name :livery}))
             {:extra-cards [{:card horse :pile-size 28}]
              :supply      [{:card livery :pile-size 8}]
              :players     [{:play-area [throne-room livery]
                             :discard   [horse horse livery]
                             :actions   0
                             :coins     6
                             :triggers  [(get-trigger livery)
                                         (get-trigger livery 2)]}]})))))

(deftest scrap-test
  (let [scrap  (assoc scrap :id 0)
        horse  (assoc horse :id 1)
        silver (assoc silver :id 2)]
    (testing "Scrap"
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [scrap copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :scrap)
                 (choose :copper))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:play-area [scrap]
                             :actions   0
                             :coins     0
                             :buys      1}]
              :trash       [copper]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [scrap estate]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :scrap)
                 (choose :estate)
                 (choose [:card :action]))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:hand      [copper]
                             :play-area [scrap]
                             :deck      [copper]
                             :actions   1
                             :coins     0
                             :buys      1}]
              :trash       [estate]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [scrap silver]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :scrap)
                 (choose :silver)
                 (choose [:buy :coin :horse]))
             {:extra-cards [{:card horse :pile-size 29}]
              :players     [{:play-area [scrap]
                             :deck      [copper copper]
                             :discard   [horse]
                             :actions   0
                             :coins     1
                             :buys      2}]
              :trash       [silver]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card silver :pile-size 40}]
                  :players     [{:hand    [scrap gold]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :scrap)
                 (choose :gold)
                 (choose [:card :action :buy :coin :silver :horse]))
             {:extra-cards [{:card horse :pile-size 29}]
              :supply      [{:card silver :pile-size 39}]
              :players     [{:hand      [copper]
                             :play-area [scrap]
                             :deck      [copper]
                             :discard   [silver horse]
                             :actions   1
                             :coins     1
                             :buys      2}]
              :trash       [gold]})))))

(deftest supplies-test
  (let [supplies (assoc supplies :id 0)
        horse    (assoc horse :id 1)]
    (testing "Supplies"
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [supplies]
                                 :actions 0
                                 :coins   0}]}
                 (play 0 :supplies))
             {:extra-cards [{:card horse :pile-size 29}]
              :players     [{:play-area [supplies]
                             :deck      [horse]
                             :actions   0
                             :coins     1}]})))))

(deftest bargain-test
  (let [horse  (assoc horse :id 1)
        livery (assoc livery :id 2)]
    (testing "Bargain"
      (is (= (-> {:events      {:bargain bargain}
                  :extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 10}]
                  :players     [{:coins 4
                                 :buys  1}
                                {}
                                {}]}
                 (buy-event 0 :bargain)
                 (choose :livery))
             {:events      {:bargain bargain}
              :extra-cards [{:card horse :pile-size 28}]
              :supply      [{:card livery :pile-size 9}]
              :players     [{:discard [livery]
                             :coins   0
                             :buys    0}
                            {:discard [horse]}
                            {:discard [horse]}]}))
      (is (= (-> {:events      {:bargain bargain}
                  :extra-cards [{:card horse :pile-size 1}]
                  :supply      [{:card duchy :pile-size 8}]
                  :players     [{:coins 4
                                 :buys  1}
                                {}
                                {}]}
                 (buy-event 0 :bargain))
             {:events      {:bargain bargain}
              :extra-cards [{:card horse :pile-size 0}]
              :supply      [{:card duchy :pile-size 8}]
              :players     [{:coins 0
                             :buys  0}
                            {:discard [horse]}
                            {}]})))))

(deftest demand-test
  (let [horse    (assoc horse :id 1)
        cardinal (assoc cardinal :id 2)]
    (testing "Demand"
      (is (= (-> {:events      {:demand demand}
                  :extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card cardinal :pile-size 10}]
                  :players     [{:deck  [copper]
                                 :coins 5
                                 :buys  1}]}
                 (buy-event 0 :demand)
                 (choose :cardinal))
             {:events      {:demand demand}
              :extra-cards [{:card horse :pile-size 29}]
              :supply      [{:card cardinal :pile-size 9}]
              :players     [{:deck  [cardinal horse copper]
                             :coins 0
                             :buys  0}]})))))

(deftest ride-test
  (let [horse (assoc horse :id 1)]
    (testing "Ride"
      (is (= (-> {:events      {:ride ride}
                  :extra-cards [{:card horse :pile-size 30}]
                  :players     [{:coins 2
                                 :buys  1}]}
                 (buy-event 0 :ride))
             {:events      {:ride ride}
              :extra-cards [{:card horse :pile-size 29}]
              :players     [{:discard [horse]
                             :coins   0
                             :buys    0}]})))))

(deftest stampede-test
  (let [horse (assoc horse :id 1)]
    (testing "Stampede"
      (is (= (-> {:events      {:stampede stampede}
                  :extra-cards [{:card horse :pile-size 30}]
                  :players     [{:coins 5
                                 :buys  1}]}
                 (buy-event 0 :stampede))
             {:events      {:stampede stampede}
              :extra-cards [{:card horse :pile-size 25}]
              :players     [{:deck  (repeat 5 horse)
                             :coins 0
                             :buys  0}]}))
      (is (= (-> {:events      {:stampede stampede}
                  :extra-cards [{:card horse :pile-size 3}]
                  :players     [{:play-area (repeat 5 copper)
                                 :deck      [silver]
                                 :coins     5
                                 :buys      1}]}
                 (buy-event 0 :stampede))
             {:events      {:stampede stampede}
              :extra-cards [{:card horse :pile-size 0}]
              :players     [{:play-area (repeat 5 copper)
                             :deck      [horse horse horse silver]
                             :coins     0
                             :buys      0}]}))
      (is (= (-> {:events      {:stampede stampede}
                  :extra-cards [{:card horse :pile-size 30}]
                  :players     [{:play-area (repeat 6 copper)
                                 :coins     5
                                 :buys      1}]}
                 (buy-event 0 :stampede))
             {:events      {:stampede stampede}
              :extra-cards [{:card horse :pile-size 30}]
              :players     [{:play-area (repeat 6 copper)
                             :coins     0
                             :buys      0}]})))))