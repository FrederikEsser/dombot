(ns dombot.cards.seaside-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.seaside :as intrigue :refer :all]
            [dombot.utils :as ut]))

(deftest caravan-test
  (testing "Caravan"
    (is (= (-> {:players [{:hand    [caravan estate estate estate copper]
                           :deck    [copper copper copper copper copper copper silver]
                           :actions 1}]}
               (play 0 :caravan))
           {:players [{:hand               [estate estate estate copper copper]
                       :play-area-duration [caravan]
                       :deck               [copper copper copper copper copper silver]
                       :actions            1}]}))
    (is (= (-> {:players [{:hand            [caravan estate estate estate copper]
                           :deck            [copper copper copper copper copper copper silver]
                           :actions         1
                           :number-of-turns 2}]}
               (play 0 :caravan)
               (clean-up 0))
           {:players [{:hand               [copper copper copper copper copper]
                       :play-area-duration [caravan]
                       :deck               [silver]
                       :discard            [estate estate estate copper copper]
                       :actions            0
                       :coins              0
                       :buys               0
                       :actions-played     0
                       :phase              :out-of-turn
                       :number-of-turns    3}]}))
    (is (= (-> {:players [{:hand            [caravan estate estate estate copper]
                           :deck            [copper copper copper copper copper copper silver]
                           :actions         1
                           :number-of-turns 2}]}
               (play 0 :caravan)
               (end-turn 0))
           {:current-player 0
            :players        [{:hand            [copper copper copper copper copper silver]
                              :play-area       [caravan]
                              :discard         [estate estate estate copper copper]
                              :actions         1
                              :coins           0
                              :buys            1
                              :actions-played  0
                              :phase           :action
                              :number-of-turns 3}]}))
    (is (= (-> {:players [{:hand            [caravan caravan copper copper copper]
                           :deck            [copper copper estate estate copper copper silver]
                           :actions         1
                           :number-of-turns 2}]}
               (play 0 :caravan)
               (play 0 :caravan)
               (end-turn 0))
           {:current-player 0
            :players        [{:hand            [estate estate copper copper silver copper copper]
                              :play-area       [caravan caravan]
                              :deck            [copper copper copper]
                              :actions         1
                              :coins           0
                              :buys            1
                              :actions-played  0
                              :phase           :action
                              :number-of-turns 3}]}))))

(deftest fishing-village-test
  (testing "Fishing Village"
    (is (= (-> {:players [{:hand    [fishing-village]
                           :actions 1
                           :coins   0}]}
               (play 0 :fishing-village))
           {:players [{:play-area-duration [fishing-village]
                       :actions            2
                       :coins              1}]}))
    (is (= (-> {:players [{:hand            [fishing-village]
                           :actions         1
                           :coins           0
                           :number-of-turns 2}]}
               (play 0 :fishing-village)
               (end-turn 0))
           {:current-player 0
            :players        [{:play-area       [fishing-village]
                              :actions         2
                              :coins           1
                              :buys            1
                              :actions-played  0
                              :phase           :action
                              :number-of-turns 3}]}))))

(deftest wharf-test
  (testing "Wharf"
    (is (= (-> {:players [{:hand    [wharf estate estate estate copper]
                           :deck    [copper copper copper copper copper copper silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :wharf))
           {:players [{:hand               [estate estate estate copper copper copper]
                       :play-area-duration [wharf]
                       :deck               [copper copper copper copper silver]
                       :actions            0
                       :buys               2}]}))
    (is (= (-> {:players [{:hand            [wharf estate estate estate copper]
                           :deck            [copper copper copper copper copper copper silver silver silver]
                           :actions         1
                           :buys 1
                           :number-of-turns 2}]}
               (play 0 :wharf)
               (end-turn 0))
           {:current-player 0
            :players        [{:hand            [copper copper copper copper silver silver silver]
                              :play-area       [wharf]
                              :discard         [estate estate estate copper copper copper]
                              :actions         1
                              :coins           0
                              :buys            2
                              :actions-played  0
                              :phase           :action
                              :number-of-turns 3}]}))))
