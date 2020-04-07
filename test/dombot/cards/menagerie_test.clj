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

(deftest exile-test
  (let [gold (assoc gold :id 1)]
    (testing "Camel Train"
      (is (= (-> {:supply  [{:card gold :pile-size 29}]
                  :players [{:exile    [gold]
                             :triggers [(assoc exile-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :gold})
                 (choose :gold))
             {:supply  [{:card gold :pile-size 28}]
              :players [{:discard  [gold gold]
                         :triggers [(assoc exile-trigger :id 1)]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 28}]
                  :players [{:exile    [gold gold]
                             :triggers [(assoc exile-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :gold})
                 (choose :gold))
             {:supply  [{:card gold :pile-size 27}]
              :players [{:discard  [gold gold gold]
                         :triggers [(assoc exile-trigger :id 1)]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 29}]
                  :players [{:exile    [gold]
                             :triggers [(assoc exile-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :gold})
                 (choose nil))
             {:supply  [{:card gold :pile-size 28}]
              :players [{:discard  [gold]
                         :exile    [gold]
                         :triggers [(assoc exile-trigger :id 1)]}]})))))

(deftest bounty-hunter-test
  (let [bounty-hunter (assoc bounty-hunter :id 0)]
    (testing "Bounty Huunter"
      (is (= (-> {:players [{:hand    [bounty-hunter copper]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :bounty-hunter)
                 (choose :copper))
             {:players [{:play-area [bounty-hunter]
                         :exile     [copper]
                         :actions   1
                         :coins     3
                         :phase     :action}]}))
      (is (= (-> {:players [{:hand    [bounty-hunter copper]
                             :exile   [copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :bounty-hunter)
                 (choose :copper))
             {:players [{:play-area [bounty-hunter]
                         :exile     [copper copper]
                         :actions   1
                         :coins     0}]}))
      (is (= (-> {:players [{:hand    [bounty-hunter estate]
                             :exile   [copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :bounty-hunter)
                 (choose :estate))
             {:players [{:play-area [bounty-hunter]
                         :exile     [copper estate]
                         :actions   1
                         :coins     3}]}))
      (is (= (-> {:players [{:hand    [bounty-hunter]
                             :actions 1
                             :coins   0}]}
                 (play 0 :bounty-hunter))
             {:players [{:play-area [bounty-hunter]
                         :actions   1
                         :coins     0}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:players [{:hand    [bounty-hunter gold]
                                            :actions 1
                                            :coins   0
                                            :buys    1}]}
                                (play 0 :bounty-hunter)
                                (choose nil)))))))

(deftest camel-train-test
  (let [camel-train (assoc camel-train :id 0)
        gold        (assoc gold :id 1)]
    (testing "Camel Train"
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [camel-train]
                             :actions 1}]}
                 (play 0 :camel-train)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [camel-train]
                         :exile     [gold]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [camel-train]
                             :actions 1}]}
                 (play 0 :camel-train))
             {:supply  [{:card duchy :pile-size 8}]
              :players [{:play-area [camel-train]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}
                            {:card camel-train :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0 :card-name :camel-train}))
             {:supply  [{:card gold :pile-size 29}
                        {:card camel-train :pile-size 9}]
              :players [{:discard [camel-train]
                         :exile   [gold]}]})))))

(deftest cardinal-test
  (let [cardinal (assoc cardinal :id 0)]
    (testing "Cardinal"
      (is (= (-> {:players [{:hand    [cardinal]
                             :actions 1
                             :coins   0}
                            {:deck [silver estate]}]}
                 (play 0 :cardinal)
                 (choose :silver))
             {:players [{:play-area [cardinal]
                         :actions   0
                         :coins     2}
                        {:exile          [silver]
                         :discard        [estate]
                         :revealed-cards {:discard 1
                                          :exile   1}}]}))
      (is (= (-> {:players [{:hand    [cardinal]
                             :actions 1
                             :coins   0}
                            {:deck [gold province]}]}
                 (play 0 :cardinal)
                 (choose :gold))
             {:players [{:play-area [cardinal]
                         :actions   0
                         :coins     2}
                        {:exile          [gold]
                         :discard        [province]
                         :revealed-cards {:discard 1
                                          :exile   1}}]}))
      (is (= (-> {:players [{:hand    [cardinal]
                             :actions 1
                             :coins   0}
                            {:deck [estate province]}]}
                 (play 0 :cardinal))
             {:players [{:play-area [cardinal]
                         :actions   0
                         :coins     2}
                        {:discard        [estate province]
                         :revealed-cards {:discard 2}}]}))
      (is (= (-> {:players [{:hand    [cardinal]
                             :actions 1
                             :coins   0}
                            {:deck [silver]}]}
                 (play 0 :cardinal)
                 (choose :silver))
             {:players [{:play-area [cardinal]
                         :actions   0
                         :coins     2}
                        {:exile          [silver]
                         :revealed-cards {:exile 1}}]})))))

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

(deftest coven-test
  (let [coven (assoc coven :id 0)
        curse (assoc curse :id 1)]
    (testing "Coven"
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [coven]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :coven))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:play-area [coven]
                         :actions   1
                         :coins     2}
                        {:exile [curse]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 1}]
                  :players [{:hand    [coven]
                             :actions 1
                             :coins   0}
                            {:exile (repeat 5 curse)}]}
                 (play 0 :coven))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:play-area [coven]
                         :actions   1
                         :coins     2}
                        {:exile (repeat 6 curse)}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 0}]
                  :players [{:hand    [coven]
                             :actions 1
                             :coins   0}
                            {:exile (repeat 6 curse)}]}
                 (play 0 :coven))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:play-area [coven]
                         :actions   1
                         :coins     2}
                        {:discard (repeat 6 curse)}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 1}]
                  :players [{:hand    [coven]
                             :actions 1
                             :coins   0}
                            {:exile (repeat 6 curse)}
                            {:exile (repeat 6 curse)}]}
                 (play 0 :coven))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:play-area [coven]
                         :actions   1
                         :coins     2}
                        {:exile (repeat 7 curse)}
                        {:discard (repeat 6 curse)}]})))))

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

(deftest sanctuary-test
  (let [sanctuary (assoc sanctuary :id 0)]
    (testing "Sanctuary"
      (is (= (-> {:players [{:hand    [sanctuary]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :sanctuary)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [sanctuary]
                         :deck      [copper]
                         :actions   1
                         :coins     0
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [sanctuary]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :sanctuary)
                 (choose :copper))
             {:players [{:play-area [sanctuary]
                         :deck      [copper]
                         :exile     [copper]
                         :actions   1
                         :coins     0
                         :buys      2}]}))
      (ut/reset-ids!)
      (is (= (-> {:supply  [{:card sanctuary :pile-size 10}]
                  :players [{}]}
                 setup-game)
             {:supply  [{:card sanctuary :pile-size 10}]
              :players [{:triggers [(assoc exile-trigger :id 1)]}]})))))

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

(deftest stockpile-test
  (let [stockpile (assoc stockpile :id 0)]
    (testing "Stockpile"
      (is (= (-> {:players [{:hand    [stockpile]
                             :actions 0
                             :coins   0
                             :buys    1
                             :phase   :action}]}
                 (play 0 :stockpile))
             {:players [{:exile   [stockpile]
                         :actions 0
                         :coins   3
                         :buys    2
                         :phase   :pay}]})))))

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