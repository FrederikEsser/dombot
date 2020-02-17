(ns dombot.cards.empires-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.empires :as empires :refer :all]
            [dombot.cards.dominion :refer [market]]
            [dombot.cards.intrigue :refer [mill]]
            [dombot.cards.renaissance :refer [patron]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest chariot-race-test
  (testing "Chariot Race"
    (is (= (-> {:players [{:hand    [chariot-race]
                           :deck    [silver]
                           :actions 1
                           :coins   0}
                          {:deck [estate]}]}
               (play 0 :chariot-race))
           {:players [{:hand           [silver]
                       :play-area      [chariot-race]
                       :revealed-cards {:hand 1}
                       :actions        1
                       :coins          1
                       :vp-tokens      1}
                      {:deck           [estate]
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:players [{:hand    [chariot-race]
                           :deck    [estate]
                           :actions 1
                           :coins   0}
                          {:deck [estate]}]}
               (play 0 :chariot-race))
           {:players [{:hand           [estate]
                       :play-area      [chariot-race]
                       :revealed-cards {:hand 1}
                       :actions        1
                       :coins          0}
                      {:deck           [estate]
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:players [{:hand    [chariot-race]
                           :deck    [estate]
                           :actions 1
                           :coins   0}
                          {}]}
               (play 0 :chariot-race))
           {:players [{:hand           [estate]
                       :play-area      [chariot-race]
                       :revealed-cards {:hand 1}
                       :actions        1
                       :coins          0}
                      {}]}))
    (is (= (-> {:players [{:hand    [chariot-race]
                           :actions 1
                           :coins   0}
                          {:deck [estate]}]}
               (play 0 :chariot-race))
           {:players [{:play-area [chariot-race]
                       :actions   1
                       :coins     0}
                      {:deck           [estate]
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:players [{:hand    [chariot-race]
                           :deck    [patron]
                           :actions 1
                           :coins   0}
                          {:discard [patron]}]}
               (play 0 :chariot-race))
           {:players [{:hand           [patron]
                       :play-area      [chariot-race]
                       :revealed-cards {:hand 1}
                       :actions        1
                       :coins          0
                       :coffers        1}
                      {:deck           [patron]
                       :revealed-cards {:deck 1}
                       :coffers        1}]}))))

(deftest charm-test
  (let [charm        (assoc charm :id 0)
        silver       (assoc silver :id 1)
        chariot-race (assoc chariot-race :id 2)]
    (testing "Charm"
      (is (= (-> {:players [{:hand  [charm]
                             :coins 0
                             :buys  1}]}
                 (play 0 :charm)
                 (choose :coins))
             {:players [{:play-area [charm]
                         :coins     2
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card chariot-race :pile-size 10}]
                  :players [{:hand  [charm]
                             :coins 3
                             :buys  1}]}
                 (play 0 :charm)
                 (choose :gain)
                 (buy-card 0 :silver)
                 (choose :chariot-race))
             {:supply  [{:card silver :pile-size 39}
                        {:card chariot-race :pile-size 9}]
              :players [{:play-area [charm]
                         :discard   [chariot-race silver]
                         :coins     0
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card chariot-race :pile-size 10}]
                  :players [{:hand  [charm]
                             :coins 3
                             :buys  1}]}
                 (play 0 :charm)
                 (choose :gain)
                 (buy-card 0 :silver)
                 (choose nil))
             {:supply  [{:card silver :pile-size 39}
                        {:card chariot-race :pile-size 10}]
              :players [{:play-area [charm]
                         :discard   [silver]
                         :coins     0
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand  [charm]
                             :coins 3
                             :buys  1}]}
                 (play 0 :charm)
                 (choose :gain)
                 (buy-card 0 :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [charm]
                         :discard   [silver]
                         :coins     0
                         :buys      0}]})))))

(deftest farmers-market-test
  (let [farmers-market (assoc farmers-market :id 0)]
    (testing "farmers-market"
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}]}]
              :players [{:play-area [farmers-market]
                         :actions   0
                         :coins     1
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}]}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}
                                                                    {:token-type :victory-point}]}]
              :players [{:play-area [farmers-market]
                         :actions   0
                         :coins     2
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}
                                                                        {:token-type :victory-point}
                                                                        {:token-type :victory-point}]}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}
                                                                    {:token-type :victory-point}
                                                                    {:token-type :victory-point}
                                                                    {:token-type :victory-point}]}]
              :players [{:play-area [farmers-market]
                         :actions   0
                         :coins     4
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}
                                                                        {:token-type :victory-point}
                                                                        {:token-type :victory-point}
                                                                        {:token-type :victory-point}]}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9}]
              :players [{:actions   0
                         :coins     0
                         :buys      2
                         :vp-tokens 4}]
              :trash   [farmers-market]})))))

(deftest forum-test
  (let [forum (assoc forum :id 0)]
    (testing "Forum"
      (is (= (-> {:players [{:hand    [forum]
                             :deck    [copper silver estate estate]
                             :actions 1}]}
                 (play 0 :forum)
                 (choose [:copper :estate]))
             {:players [{:hand      [silver]
                         :play-area [forum]
                         :deck      [estate]
                         :discard   [copper estate]
                         :actions   1}]}))
      (is (= (-> {:supply  [{:card forum :pile-size 10}]
                  :players [{:coins 7
                             :buys  1}]}
                 (buy-card 0 :forum))
             {:supply  [{:card forum :pile-size 9}]
              :players [{:discard [forum]
                         :coins   2
                         :buys    1}]})))))

(deftest legionary-test
  (let [legionary (assoc legionary :id 0)]
    (testing "Legionary"
      (is (= (-> {:players [{:hand    [legionary]
                             :actions 1
                             :coins   0}]}
                 (play 0 :legionary))
             {:players [{:play-area [legionary]
                         :actions   0
                         :coins     3}]}))
      (is (= (-> {:players [{:hand    [legionary silver gold]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper estate estate]
                             :deck [estate copper]}]}
                 (play 0 :legionary)
                 (choose :gold)
                 (choose [:estate :estate :copper]))
             {:players [{:hand      [silver gold]
                         :play-area [legionary]
                         :actions   0
                         :coins     3}
                        {:hand    [copper copper estate]
                         :deck    [copper]
                         :discard [estate estate copper]}]}))
      (is (= (-> {:players [{:hand    [legionary silver gold]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper estate estate]
                             :deck [estate copper]}]}
                 (play 0 :legionary)
                 (choose nil))
             {:players [{:hand      [silver gold]
                         :play-area [legionary]
                         :actions   0
                         :coins     3}
                        {:hand [copper copper copper estate estate]
                         :deck [estate copper]}]})))))

(deftest sacrifice-test
  (let [sacrifice (assoc sacrifice :id 0)]
    (testing "Sacrifice"
      (is (= (-> {:players [{:hand    [sacrifice copper estate chariot-race]
                             :deck    [copper silver estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :sacrifice)
                 (choose :copper))
             {:players [{:hand      [estate chariot-race]
                         :play-area [sacrifice]
                         :deck      [copper silver estate]
                         :actions   0
                         :coins     2}]
              :trash   [copper]}))
      (is (= (-> {:players [{:hand    [sacrifice copper estate chariot-race]
                             :deck    [copper silver estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :sacrifice)
                 (choose :estate))
             {:players [{:hand      [copper chariot-race]
                         :play-area [sacrifice]
                         :deck      [copper silver estate]
                         :actions   0
                         :coins     0
                         :vp-tokens 2}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand    [sacrifice copper estate chariot-race]
                             :deck    [copper silver estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :sacrifice)
                 (choose :chariot-race))
             {:players [{:hand      [copper estate copper silver]
                         :play-area [sacrifice]
                         :deck      [estate]
                         :actions   2
                         :coins     0}]
              :trash   [chariot-race]}))
      (is (= (-> {:players [{:hand    [sacrifice mill]
                             :deck    [copper silver estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :sacrifice)
                 (choose :mill))
             {:players [{:hand      [copper silver]
                         :play-area [sacrifice]
                         :deck      [estate]
                         :actions   2
                         :coins     0
                         :vp-tokens 2}]
              :trash   [mill]})))))

(deftest temple-test
  (let [temple (assoc temple :id 0)]
    (testing "Temple"
      (is (= (-> {:supply  [{:card temple :pile-size 9}]
                  :players [{:hand    [temple copper copper estate]
                             :actions 1}]}
                 (play 0 :temple)
                 (choose [:copper :estate]))
             {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}]}]
              :players [{:hand      [copper]
                         :play-area [temple]
                         :actions   0
                         :vp-tokens 1}]
              :trash   [copper estate]}))
      (is (= (-> {:supply  [{:card temple :pile-size 9}]
                  :players [{:hand    [temple copper copper estate]
                             :actions 1}]}
                 (play 0 :temple)
                 (choose [:estate]))
             {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}]}]
              :players [{:hand      [copper copper]
                         :play-area [temple]
                         :actions   0
                         :vp-tokens 1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card temple :pile-size 9}]
                  :players [{:hand    [temple copper]
                             :actions 1}]}
                 (play 0 :temple)
                 (choose :copper))
             {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}]}]
              :players [{:play-area [temple]
                         :actions   0
                         :vp-tokens 1}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card temple :pile-size 9}]
                  :players [{:hand    [temple]
                             :actions 1}]}
                 (play 0 :temple))
             {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}]}]
              :players [{:play-area [temple]
                         :actions   0
                         :vp-tokens 1}]}))
      (is (thrown-with-msg? AssertionError #"Choose error: All choices must be different: Copper, Copper, Estate"
                            (-> {:supply  [{:card temple :pile-size 9}]
                                 :players [{:hand    [temple copper copper estate]
                                            :actions 1}]}
                                (play 0 :temple)
                                (choose [:copper :copper :estate]))))
      (is (= (-> {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}
                                                                {:token-type :victory-point}
                                                                {:token-type :victory-point}]}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-card 0 :temple))
             {:supply  [{:card temple :pile-size 8}]
              :players [{:discard   [temple]
                         :coins     0
                         :buys      0
                         :vp-tokens 3}]})))))

;; EVENTS

(deftest advance-test
  (let [legionary (assoc legionary :id 0)]
    (testing "Advance"
      (is (= (-> {:events  {:advance advance}
                  :supply  [{:card legionary :pile-size 10}]
                  :players [{:hand  [chariot-race copper estate]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :advance))
             {:events       {:advance advance}
              :supply       [{:card legionary :pile-size 10}]
              :players      [{:hand  [chariot-race copper estate]
                              :coins 0
                              :buys  0}]
              :effect-stack [{:text      "You may trash an Action card from your hand."
                              :player-no 0
                              :choice    ::empires/advance-trash
                              :source    :hand
                              :options   [:chariot-race]
                              :max       1}]}))
      (is (= (-> {:events  {:advance advance}
                  :supply  [{:card legionary :pile-size 10}]
                  :players [{:hand  [chariot-race copper estate]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :advance)
                 (choose :chariot-race)
                 (choose :legionary))
             {:events  {:advance advance}
              :supply  [{:card legionary :pile-size 9}]
              :players [{:hand    [copper estate]
                         :discard [legionary]
                         :coins   0
                         :buys    0}]
              :trash   [chariot-race]}))
      (is (= (-> {:events  {:advance advance}
                  :supply  [{:card legionary :pile-size 10}]
                  :players [{:hand  [chariot-race copper estate]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :advance)
                 (choose nil))
             {:events  {:advance advance}
              :supply  [{:card legionary :pile-size 10}]
              :players [{:hand  [chariot-race copper estate]
                         :coins 0
                         :buys  0}]})))))

(deftest banquet-test
  (let [copper    (assoc copper :id 0)
        legionary (assoc legionary :id 1)]
    (testing "Banquet"
      (is (= (-> {:events  {:banquet banquet}
                  :supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}
                            {:card gold :pile-size 30}
                            {:card duchy :pile-size 8}
                            {:card legionary :pile-size 10}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-event 0 :banquet))
             {:events       {:banquet banquet}
              :supply       [{:card copper :pile-size 44}
                             {:card silver :pile-size 40}
                             {:card gold :pile-size 30}
                             {:card duchy :pile-size 8}
                             {:card legionary :pile-size 10}]
              :players      [{:discard [copper copper]
                              :coins   0
                              :buys    0}]
              :effect-stack [{:text      "Gain a non-Victory card costing up to $5."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:copper :silver :legionary]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:events  {:banquet banquet}
                  :supply  [{:card copper :pile-size 46}
                            {:card legionary :pile-size 10}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-event 0 :banquet)
                 (choose :legionary))
             {:events  {:banquet banquet}
              :supply  [{:card copper :pile-size 44}
                        {:card legionary :pile-size 9}]
              :players [{:discard [copper copper legionary]
                         :coins   0
                         :buys    0}]})))))

(deftest conquest-test
  (let [silver (assoc silver :id 0)]
    (testing "Conquest"
      (is (= (-> {:track-gained-cards? true
                  :events              {:conquest conquest}
                  :supply              [{:card silver :pile-size 40}]
                  :players             [{:coins 6
                                         :buys  1}]}
                 (buy-event 0 :conquest))
             {:track-gained-cards? true
              :events              {:conquest conquest}
              :supply              [{:card silver :pile-size 38}]
              :players             [{:discard      [silver silver]
                                     :coins        0
                                     :buys         0
                                     :vp-tokens    2
                                     :gained-cards [{:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}
                                                    {:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}]}]}))
      (is (= (-> {:track-gained-cards? true
                  :events              {:conquest conquest}
                  :supply              [{:card silver :pile-size 39}]
                  :players             [{:coins        6
                                         :buys         1
                                         :gained-cards [{:cost  3
                                                         :name  :silver
                                                         :types #{:treasure}}
                                                        {:cost  6
                                                         :name  :gold
                                                         :types #{:treasure}}]}]}
                 (buy-event 0 :conquest))
             {:track-gained-cards? true
              :events              {:conquest conquest}
              :supply              [{:card silver :pile-size 37}]
              :players             [{:discard      [silver silver]
                                     :coins        0
                                     :buys         0
                                     :vp-tokens    3
                                     :gained-cards [{:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}
                                                    {:cost  6
                                                     :name  :gold
                                                     :types #{:treasure}}
                                                    {:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}
                                                    {:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}]}]})))))

(deftest delve-test
  (let [silver (assoc silver :id 0)]
    (testing "Delve"
      (is (= (-> {:events  {:delve delve}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :delve))
             {:events  {:delve delve}
              :supply  [{:card silver :pile-size 39}]
              :players [{:discard [silver]
                         :coins   2
                         :buys    1}]}))
      (is (= (-> {:events  {:delve delve}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :delve)
                 (buy-event 0 :delve))
             {:events  {:delve delve}
              :supply  [{:card silver :pile-size 38}]
              :players [{:discard [silver silver]
                         :coins   0
                         :buys    1}]})))))

(deftest salt-the-earth-test
  (testing "Salt the Earth"
    (is (= (-> {:events  {:salt-the-earth salt-the-earth}
                :supply  (base/supply 2 8)
                :players [{:coins 4
                           :buys  1}]}
               (buy-event 0 :salt-the-earth))
           {:events       {:salt-the-earth salt-the-earth}
            :supply       (base/supply 2 8)
            :players      [{:coins     0
                            :buys      0
                            :vp-tokens 1}]
            :effect-stack [{:text      "Trash a Victory card from the Supply."
                            :player-no 0
                            :choice    :trash-from-supply
                            :source    :supply
                            :options   [:estate :duchy :province]
                            :min       1
                            :max       1}]}))
    (let [province (assoc province :id 0)]
      (is (= (-> {:events  {:salt-the-earth salt-the-earth}
                  :supply  [{:card province :pile-size 8}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :salt-the-earth)
                 (choose :province))
             {:events  {:salt-the-earth salt-the-earth}
              :supply  [{:card province :pile-size 7}]
              :players [{:coins     0
                         :buys      0
                         :vp-tokens 1}]
              :trash   [province]})))))

(deftest dominate-test
  (let [province (assoc province :id 0)]
    (testing "Dominate"
      (is (= (-> {:events  {:dominate dominate}
                  :supply  [{:card province :pile-size 8}]
                  :players [{:coins 14
                             :buys  1}]}
                 (buy-event 0 :dominate))
             {:events  {:dominate dominate}
              :supply  [{:card province :pile-size 7}]
              :players [{:discard   [province]
                         :coins     0
                         :buys      0
                         :vp-tokens 9}]}))
      (is (= (-> {:events  {:dominate dominate}
                  :supply  [{:card province :pile-size 0}]
                  :players [{:coins 14
                             :buys  1}]}
                 (buy-event 0 :dominate))
             {:events  {:dominate dominate}
              :supply  [{:card province :pile-size 0}]
              :players [{:coins 0
                         :buys  0}]})))))

(deftest ritual-test
  (let [curse (assoc curse :id 0)]
    (testing "Ritual"
      (is (= (-> {:events  {:ritual ritual}
                  :supply  [{:card curse :pile-size 10}]
                  :players [{:hand  [gold]
                             :coins 4
                             :buys  1}]}
                 (buy-event 0 :ritual)
                 (choose :gold))
             {:events  {:ritual ritual}
              :supply  [{:card curse :pile-size 9}]
              :players [{:discard   [curse]
                         :coins     0
                         :buys      0
                         :vp-tokens 6}]
              :trash   [gold]}))
      (is (= (-> {:events  {:ritual ritual}
                  :supply  [{:card curse :pile-size 10}]
                  :players [{:hand  [copper]
                             :coins 4
                             :buys  1}]}
                 (buy-event 0 :ritual)
                 (choose :copper))
             {:events  {:ritual ritual}
              :supply  [{:card curse :pile-size 9}]
              :players [{:discard [curse]
                         :coins   0
                         :buys    0}]
              :trash   [copper]}))
      (is (= (-> {:events  {:ritual ritual}
                  :supply  [{:card curse :pile-size 10}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :ritual))
             {:events  {:ritual ritual}
              :supply  [{:card curse :pile-size 9}]
              :players [{:discard [curse]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:events  {:ritual ritual}
                  :supply  [{:card curse :pile-size 0}]
                  :players [{:hand  [gold]
                             :coins 4
                             :buys  1}]}
                 (buy-event 0 :ritual))
             {:events  {:ritual ritual}
              :supply  [{:card curse :pile-size 0}]
              :players [{:hand  [gold]
                         :coins 0
                         :buys  0}]})))))

(deftest windfall-test
  (let [gold (assoc gold :id 0)]
    (testing "windfall"
      (is (= (-> {:events  {:windfall windfall}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:hand      [estate]
                             :play-area [silver silver copper]
                             :coins     5
                             :buys      1}]}
                 (buy-event 0 :windfall))
             {:events  {:windfall windfall}
              :supply  [{:card gold :pile-size 27}]
              :players [{:hand      [estate]
                         :play-area [silver silver copper]
                         :discard   [gold gold gold]
                         :coins     0
                         :buys      0}]}))
      (is (= (-> {:events  {:windfall windfall}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:deck  [estate]
                             :coins 5
                             :buys  1}]}
                 (buy-event 0 :windfall))
             {:events  {:windfall windfall}
              :supply  [{:card gold :pile-size 30}]
              :players [{:deck  [estate]
                         :coins 0
                         :buys  0}]}))
      (is (= (-> {:events  {:windfall windfall}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:discard [estate]
                             :coins   5
                             :buys    1}]}
                 (buy-event 0 :windfall))
             {:events  {:windfall windfall}
              :supply  [{:card gold :pile-size 30}]
              :players [{:discard [estate]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:events  {:windfall windfall}
                  :supply  [{:card gold :pile-size 2}]
                  :players [{:coins 5
                             :buys  1}]}
                 (buy-event 0 :windfall))
             {:events  {:windfall windfall}
              :supply  [{:card gold :pile-size 0}]
              :players [{:discard [gold gold]
                         :coins   0
                         :buys    0}]})))))
