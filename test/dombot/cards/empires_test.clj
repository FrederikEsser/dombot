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


(deftest banquet-test
  (let [copper (assoc copper :id 0)
        market (assoc market :id 1)]
    (testing "Banquet"
      (is (= (-> {:events  {:banquet banquet}
                  :supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}
                            {:card gold :pile-size 30}
                            {:card duchy :pile-size 8}
                            {:card market :pile-size 10}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-event 0 :banquet))
             {:events       {:banquet banquet}
              :supply       [{:card copper :pile-size 44}
                             {:card silver :pile-size 40}
                             {:card gold :pile-size 30}
                             {:card duchy :pile-size 8}
                             {:card market :pile-size 10}]
              :players      [{:discard [copper copper]
                              :coins   0
                              :buys    0}]
              :effect-stack [{:text      "Gain a non-Victory card costing up to $5."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:copper :silver :market]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:events  {:banquet banquet}
                  :supply  [{:card copper :pile-size 46}
                            {:card market :pile-size 10}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-event 0 :banquet)
                 (choose :market))
             {:events  {:banquet banquet}
              :supply  [{:card copper :pile-size 44}
                        {:card market :pile-size 9}]
              :players [{:discard [copper copper market]
                         :coins   0
                         :buys    0}]})))))

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
