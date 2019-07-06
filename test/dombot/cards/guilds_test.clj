(ns dombot.cards.guilds-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.guilds :as guilds :refer :all]
            [dombot.cards.dominion :refer [throne-room]]
            [dombot.cards.prosperity :refer [watchtower]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest advisor-test
  (testing "Advisor"
    (is (= (-> {:players [{:hand    [advisor]
                           :deck    [copper copper copper copper]
                           :actions 1}
                          {}]}
               (play 0 :advisor)
               (choose :copper))
           {:players [{:hand           [copper copper]
                       :play-area      [advisor]
                       :deck           [copper]
                       :discard        [copper]
                       :revealed-cards {:discard 1
                                        :hand    2}
                       :actions        1}
                      {}]}))
    (is (= (-> {:players [{:hand    [advisor]
                           :deck    [copper copper]
                           :actions 1}
                          {}]}
               (play 0 :advisor)
               (choose :copper))
           {:players [{:hand           [copper]
                       :play-area      [advisor]
                       :discard        [copper]
                       :revealed-cards {:discard 1
                                        :hand    1}
                       :actions        1}
                      {}]}))))

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

(deftest butcher-test
  (let [estate (assoc estate :id 0)
        silver (assoc silver :id 1)
        duchy (assoc duchy :id 2)]
    (testing "Butcher"
      (is (= (-> {:players [{:hand    [butcher]
                             :actions 1}]}
                 (play 0 :butcher))
             {:players [{:play-area [butcher]
                         :actions   0
                         :coffers   2}]}))
      (is (= (-> {:players [{:hand    [butcher estate]
                             :actions 1}]}
                 (play 0 :butcher)
                 (choose nil))
             {:players [{:hand      [estate]
                         :play-area [butcher]
                         :actions   0
                         :coffers   2}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [butcher estate]
                             :actions 1}]}
                 (play 0 :butcher)
                 (choose :estate)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:play-area [butcher]
                         :discard   [estate]
                         :actions   0
                         :coffers   2}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [butcher estate]
                             :actions 1}]}
                 (play 0 :butcher)
                 (choose :estate)
                 (choose :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [butcher]
                         :discard   [silver]
                         :actions   0
                         :coffers   1}]
              :trash   [estate]}))
      (is (thrown-with-msg? AssertionError #"Choose error. Duchy is not a valid option."
                            (-> {:supply  [{:card silver :pile-size 40}
                                           {:card duchy :pile-size 8}]
                                 :players [{:hand    [butcher estate]
                                            :actions 1}]}
                                (play 0 :butcher)
                                (choose :estate)
                                (choose :duchy))))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [butcher estate]
                             :actions 1
                             :coffers 1}]}
                 (play 0 :butcher)
                 (choose :estate)
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:play-area [butcher]
                         :discard   [duchy]
                         :actions   0
                         :coffers   0}]
              :trash   [estate]})))))

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

(deftest doctor-test
  (let [doctor (assoc doctor :id 0)]
    (testing "Doctor"
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [doctor]
                             :deck    [copper copper copper copper]
                             :actions 1}]}
                 (play 0 :doctor)
                 (choose :estate)
                 (choose [:copper :copper :copper]))
             {:supply  (base/supply 2 8)
              :players [{:play-area      [doctor]
                         :deck           [copper copper copper copper]
                         :revealed-cards {:deck 3}
                         :actions        0}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [doctor]
                             :deck    [copper copper copper copper]
                             :actions 1}]}
                 (play 0 :doctor)
                 (choose :copper))
             {:supply  (base/supply 2 8)
              :players [{:play-area [doctor]
                         :deck      [copper]
                         :actions   0}]
              :trash   [copper copper copper]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [doctor]
                             :deck    [silver estate copper copper]
                             :actions 1}]}
                 (play 0 :doctor)
                 (choose :estate)
                 (choose [:silver :copper]))
             {:supply  (base/supply 2 8)
              :players [{:play-area      [doctor]
                         :deck           [copper silver copper]
                         :revealed-cards {:deck 2}
                         :actions        0}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card doctor :pile-size 10}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-card 0 :doctor))
             {:supply  [{:card doctor :pile-size 9}]
              :players [{:discard [doctor]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card doctor :pile-size 10}]
                  :players [{:deck  [copper estate]
                             :coins 4
                             :buys  1}]}
                 (buy-card 0 :doctor)
                 (choose 1)
                 (choose :trash))
             {:supply  [{:card doctor :pile-size 9}]
              :players [{:deck    [estate]
                         :discard [doctor]
                         :coins   0
                         :buys    0}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card doctor :pile-size 10}]
                  :players [{:deck  [copper estate]
                             :coins 4
                             :buys  1}]}
                 (buy-card 0 :doctor)
                 (choose 1)
                 (choose :discard))
             {:supply  [{:card doctor :pile-size 9}]
              :players [{:deck    [estate]
                         :discard [copper doctor]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card doctor :pile-size 10}]
                  :players [{:deck  [copper estate]
                             :coins 4
                             :buys  1}]}
                 (buy-card 0 :doctor)
                 (choose 1)
                 (choose :deck))
             {:supply  [{:card doctor :pile-size 9}]
              :players [{:deck    [copper estate]
                         :discard [doctor]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card doctor :pile-size 10}]
                  :players [{:deck  [copper estate]
                             :coins 5
                             :buys  1}]}
                 (buy-card 0 :doctor)
                 (choose 2)
                 (choose :discard)
                 (choose :trash))
             {:supply  [{:card doctor :pile-size 9}]
              :players [{:discard [copper doctor]
                         :coins   0
                         :buys    0}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card doctor :pile-size 10}]
                  :players [{:deck  [copper estate]
                             :coins 5
                             :buys  1}]}
                 (buy-card 0 :doctor)
                 (choose 2)
                 (choose :deck)
                 (choose :trash))
             {:supply  [{:card doctor :pile-size 9}]
              :players [{:deck    [estate]
                         :discard [doctor]
                         :coins   0
                         :buys    0}]
              :trash   [copper]})))))

(deftest herald-test
  (let [herald (assoc herald :id 0)]
    (testing "Herald"
      (is (= (-> {:players [{:hand    [herald]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :herald))
             {:players [{:hand           [copper]
                         :play-area      [herald]
                         :deck           [copper]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [herald]
                             :deck    [copper candlestick-maker]
                             :actions 1
                             :buys    1}]}
                 (play 0 :herald))
             {:players [{:hand           [copper]
                         :play-area      [herald candlestick-maker]
                         :revealed-cards {:play-area 1}
                         :actions        2
                         :buys           2
                         :coffers        1}]}))
      (testing "overpay"
        (is (= (-> {:supply  [{:card herald :pile-size 10}]
                    :players [{:deck  [copper]
                               :coins 5
                               :buys  1}]}
                   (buy-card 0 :herald)
                   (choose 1))
               {:supply  [{:card herald :pile-size 9}]
                :players [{:deck    [copper]
                           :discard [herald]
                           :coins   0
                           :buys    0}]}))
        (is (= (-> {:supply  [{:card herald :pile-size 10}]
                    :players [{:discard [gold]
                               :coins   5
                               :buys    1}]}
                   (buy-card 0 :herald)
                   (choose 1)
                   (choose :gold))
               {:supply  [{:card herald :pile-size 9}]
                :players [{:deck    [gold]
                           :discard [herald]
                           :coins   0
                           :buys    0}]}))
        (is (= (-> {:supply  [{:card herald :pile-size 10}]
                    :players [{:discard [gold copper estate]
                               :coins   6
                               :buys    1}]}
                   (buy-card 0 :herald)
                   (choose 2)
                   (choose [:gold :copper]))
               {:supply  [{:card herald :pile-size 9}]
                :players [{:deck    [copper gold]
                           :discard [estate herald]
                           :coins   0
                           :buys    0}]}))))))

(deftest journeyman-test
  (testing "Journeyman"
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [journeyman]
                           :deck    [copper estate silver estate gold]
                           :actions 1}]}
               (play 0 :journeyman)
               (choose :province))
           {:supply  (base/supply 2 8)
            :players [{:hand           [copper estate silver]
                       :play-area      [journeyman]
                       :deck           [estate gold]
                       :revealed-cards {:hand 3}
                       :actions        0}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [journeyman]
                           :deck    [copper estate silver estate gold]
                           :actions 1}]}
               (play 0 :journeyman)
               (choose :copper))
           {:supply  (base/supply 2 8)
            :players [{:hand           [estate silver estate]
                       :play-area      [journeyman]
                       :deck           [gold]
                       :discard        [copper]
                       :revealed-cards {:hand    3
                                        :discard 1}
                       :actions        0}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [journeyman]
                           :deck    [copper estate silver estate gold copper]
                           :actions 1}]}
               (play 0 :journeyman)
               (choose :estate))
           {:supply  (base/supply 2 8)
            :players [{:hand           [copper silver gold]
                       :play-area      [journeyman]
                       :deck           [copper]
                       :discard        [estate estate]
                       :revealed-cards {:hand    3
                                        :discard 2}
                       :actions        0}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [journeyman]
                           :deck    [copper estate silver estate]
                           :actions 1}]}
               (play 0 :journeyman)
               (choose :estate))
           {:supply  (base/supply 2 8)
            :players [{:hand           [copper silver]
                       :play-area      [journeyman]
                       :discard        [estate estate]
                       :revealed-cards {:hand    2
                                        :discard 2}
                       :actions        0}]}))))

(deftest masterpiece-test
  (let [masterpiece (assoc masterpiece :id 0)
        silver (assoc silver :id 1)]
    (testing "Masterpiece"
      (is (= (-> {:players [{:hand  [masterpiece]
                             :coins 0}]}
                 (play 0 :masterpiece))
             {:players [{:play-area [masterpiece]
                         :coins     1}]}))
      (testing "overpay"
        (is (= (-> {:supply  [{:card silver :pile-size 40}
                              {:card masterpiece :pile-size 10}]
                    :players [{:coins 3
                               :buys  1}]}
                   (buy-card 0 :masterpiece))
               {:supply  [{:card silver :pile-size 40}
                          {:card masterpiece :pile-size 9}]
                :players [{:discard [masterpiece]
                           :coins   0
                           :buys    0}]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}
                              {:card masterpiece :pile-size 10}]
                    :players [{:coins 4
                               :buys  1}]}
                   (buy-card 0 :masterpiece))
               {:supply       [{:card silver :pile-size 40}
                               {:card masterpiece :pile-size 10}]
                :players      [{:coins 1
                                :buys  0}]
                :effect-stack [{:text      "You may overpay for your Masterpiece. Choose amount:"
                                :player-no 0
                                :choice    [:overpay-choice {:effect :dombot.cards.guilds/masterpiece-overpay}]
                                :source    :overpay
                                :options   [0 1]
                                :min       1
                                :max       1}
                               {:player-no 0
                                :effect    [:gain {:card-name :masterpiece
                                                   :bought    true}]}]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}
                              {:card masterpiece :pile-size 10}]
                    :players [{:coins 4
                               :buys  1}]}
                   (buy-card 0 :masterpiece)
                   (choose 0))
               {:supply  [{:card silver :pile-size 40}
                          {:card masterpiece :pile-size 9}]
                :players [{:coins   1
                           :buys    0
                           :discard [masterpiece]}]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}
                              {:card masterpiece :pile-size 10}]
                    :players [{:coins 4
                               :buys  1}]}
                   (buy-card 0 :masterpiece)
                   (choose 1))
               {:supply  [{:card silver :pile-size 39}
                          {:card masterpiece :pile-size 9}]
                :players [{:coins   0
                           :buys    0
                           :discard [silver masterpiece]}]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}
                              {:card masterpiece :pile-size 10}]
                    :players [{:coins 7
                               :buys  1}]}
                   (buy-card 0 :masterpiece)
                   (choose 4))
               {:supply  [{:card silver :pile-size 36}
                          {:card masterpiece :pile-size 9}]
                :players [{:coins   0
                           :buys    0
                           :discard [silver silver silver silver masterpiece]}]}))))))

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

(deftest soothsayer-test
  (let [gold (assoc gold :id 0)
        curse (assoc curse :id 1)]
    (testing "Soothsayer"
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [soothsayer]
                             :actions 1}
                            {:deck [copper copper]}]}
                 (play 0 :soothsayer))
             {:supply  [{:card curse :pile-size 9}
                        {:card gold :pile-size 29}]
              :players [{:play-area [soothsayer]
                         :discard   [gold]
                         :actions   0}
                        {:hand    [copper]
                         :deck    [copper]
                         :discard [curse]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 0}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [soothsayer]
                             :actions 1}
                            {:deck [copper copper]}]}
                 (play 0 :soothsayer))
             {:supply  [{:card curse :pile-size 0}
                        {:card gold :pile-size 29}]
              :players [{:play-area [soothsayer]
                         :discard   [gold]
                         :actions   0}
                        {:deck [copper copper]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [soothsayer]
                             :actions 1}
                            {:hand [watchtower]
                             :deck [copper copper]}]}
                 (play 0 :soothsayer)
                 (choose :trash))
             {:supply  [{:card curse :pile-size 9}
                        {:card gold :pile-size 29}]
              :players [{:play-area [soothsayer]
                         :discard   [gold]
                         :actions   0}
                        {:hand [watchtower copper]
                         :deck [copper]}]
              :trash   [curse]})))))

(deftest stonemason-test
  (let [stonemason (assoc stonemason :id 0)
        baker (assoc baker :id 1)]
    (testing "Stonemason"
      (is (= (-> {:players [{:hand    [stonemason copper]
                             :actions 1}]}
                 (play 0 :stonemason)
                 (choose :copper))
             {:players [{:play-area [stonemason]
                         :actions   0}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card baker :pile-size 10}]
                  :players [{:hand    [stonemason gold]
                             :actions 1}]}
                 (play 0 :stonemason)
                 (choose :gold)
                 (choose :baker)
                 (choose :baker))
             {:supply  [{:card baker :pile-size 8}]
              :players [{:play-area [stonemason]
                         :discard   [baker baker]
                         :actions   0}]
              :trash   [gold]}))
      (is (= (-> {:supply  [{:card stonemason :pile-size 10}
                            {:card baker :pile-size 10}]
                  :players [{:coins 7
                             :buys  1}]}
                 (buy-card 0 :stonemason)
                 (choose 0))
             {:supply  [{:card stonemason :pile-size 9}
                        {:card baker :pile-size 10}]
              :players [{:discard [stonemason]
                         :coins   5
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card stonemason :pile-size 10}
                            {:card silver :pile-size 40}
                            {:card baker :pile-size 10}]
                  :players [{:coins 7
                             :buys  1}]}
                 (buy-card 0 :stonemason)
                 (choose 3))
             {:supply  [{:card stonemason :pile-size 9}
                        {:card silver :pile-size 40}
                        {:card baker :pile-size 10}]
              :players [{:discard [stonemason]
                         :coins   2
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card stonemason :pile-size 10}
                            {:card baker :pile-size 10}]
                  :players [{:coins 7
                             :buys  1}]}
                 (buy-card 0 :stonemason)
                 (choose 5)
                 (choose :baker)
                 (choose :baker))
             {:supply  [{:card stonemason :pile-size 9}
                        {:card baker :pile-size 8}]
              :players [{:discard [baker baker stonemason]
                         :coins   0
                         :buys    0}]})))))

(deftest taxman-test
  (let [gold (assoc gold :id 0)]
    (testing "Taxman"
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [taxman silver]
                             :actions 1}
                            {:hand [copper copper copper copper silver]}]}
                 (play 0 :taxman)
                 (choose nil))
             {:supply  [{:card gold :pile-size 30}]
              :players [{:hand      [silver]
                         :play-area [taxman]
                         :actions   0}
                        {:hand [copper copper copper copper silver]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [taxman silver]
                             :actions 1}
                            {:hand [copper copper copper copper silver]}]}
                 (play 0 :taxman)
                 (choose :silver)                           ; player 0 trash
                 (choose :silver)                           ; player 1 discard
                 (choose :gold))                            ; player 0 gain onto deck
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [taxman]
                         :deck      [gold]
                         :actions   0}
                        {:hand    [copper copper copper copper]
                         :discard [silver]}]
              :trash   [silver]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [taxman silver]
                             :actions 1}
                            {:hand [copper copper copper silver]}]}
                 (play 0 :taxman)
                 (choose :silver)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [taxman]
                         :deck      [gold]
                         :actions   0}
                        {:hand [copper copper copper silver]}]
              :trash   [silver]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [taxman silver]
                             :actions 1}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :taxman)
                 (choose :silver)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [taxman]
                         :deck      [gold]
                         :actions   0}
                        {:hand           [copper copper copper copper copper]
                         :revealed-cards {:hand 5}}]
              :trash   [silver]})))))
