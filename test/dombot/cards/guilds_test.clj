(ns dombot.cards.guilds-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.guilds :as guilds :refer :all]
            [dombot.cards.dominion :refer [throne-room]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest baker-test
  (testing "Baker"
    (is (= (-> {:players [{:hand    [baker]
                           :deck    [copper copper]
                           :actions 1}]}
               (play 0 :baker))
           {:players [{:hand      [copper]
                       :play-area [baker]
                       :deck      [copper]
                       :actions   1
                       :coffers   1}]}))))

(deftest candlestick-maker-test
  (testing "Candlestick Maker"
    (is (= (-> {:players [{:hand    [candlestick-maker]
                           :actions 1
                           :buys    1}]}
               (play 0 :candlestick-maker))
           {:players [{:play-area [candlestick-maker]
                       :actions   1
                       :buys      2
                       :coffers   1}]}))))

(deftest merchant-guild-test
  (let [copper (assoc copper :id 1)]
    (testing "Merchant Guild"
      (is (= (-> {:players [{:hand    [merchant-guild]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :merchant-guild))
             {:players [{:play-area [merchant-guild]
                         :actions   0
                         :coins     1
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [merchant-guild]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :merchant-guild)
                 (buy-card 0 :copper))
             {:supply  [{:card copper :pile-size 45}]
              :players [{:play-area [merchant-guild]
                         :discard   [copper]
                         :actions   0
                         :coins     1
                         :buys      1
                         :coffers   1}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [merchant-guild]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :merchant-guild)
                 (buy-card 0 :copper)
                 (buy-card 0 :copper))
             {:supply  [{:card copper :pile-size 44}]
              :players [{:play-area [merchant-guild]
                         :discard   [copper copper]
                         :actions   0
                         :coins     1
                         :buys      0
                         :coffers   2}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [merchant-guild merchant-guild]
                             :actions 2
                             :coins   0
                             :buys    1}]}
                 (play 0 :merchant-guild)
                 (play 0 :merchant-guild)
                 (buy-card 0 :copper))
             {:supply  [{:card copper :pile-size 45}]
              :players [{:play-area [merchant-guild merchant-guild]
                         :discard   [copper]
                         :actions   0
                         :coins     2
                         :buys      2
                         :coffers   2}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [merchant-guild throne-room]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :throne-room)
                 (choose :merchant-guild)
                 (buy-card 0 :copper))
             {:supply  [{:card copper :pile-size 45}]
              :players [{:play-area [throne-room merchant-guild]
                         :discard   [copper]
                         :actions   0
                         :coins     2
                         :buys      2
                         :coffers   1}]})))))

(deftest plaza-test
  (testing "Plaza"
    (is (= (-> {:players [{:hand    [plaza]
                           :deck    [estate estate]
                           :actions 1}]}
               (play 0 :plaza))
           {:players [{:hand      [estate]
                       :play-area [plaza]
                       :deck      [estate]
                       :actions   2}]}))
    (is (= (-> {:players [{:hand    [plaza]
                           :deck    [copper copper]
                           :actions 1}]}
               (play 0 :plaza)
               (choose nil))
           {:players [{:hand      [copper]
                       :play-area [plaza]
                       :deck      [copper]
                       :actions   2}]}))
    (is (= (-> {:players [{:hand    [plaza]
                           :deck    [copper copper]
                           :actions 1}]}
               (play 0 :plaza)
               (choose :copper))
           {:players [{:play-area [plaza]
                       :deck      [copper]
                       :discard   [copper]
                       :actions   2
                       :coffers   1}]}))))
