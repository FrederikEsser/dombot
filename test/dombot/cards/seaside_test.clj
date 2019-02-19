(ns dombot.cards.seaside-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :as dominion :refer [moat witch]]
            [dombot.cards.seaside :as seaside :refer :all]
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
    (is (= (-> {:players [{:hand    [caravan estate estate estate copper]
                           :deck    [copper copper copper copper copper copper silver]
                           :actions 1}]}
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
                       :phase              :out-of-turn}]}))
    (is (= (-> {:players [{:hand    [caravan estate estate estate copper]
                           :deck    [copper copper copper copper copper copper silver]
                           :actions 1}]}
               (play 0 :caravan)
               (end-turn 0))
           {:current-player 0
            :players        [{:hand           [copper copper copper copper copper silver]
                              :play-area      [caravan]
                              :discard        [estate estate estate copper copper]
                              :actions        1
                              :coins          0
                              :buys           1
                              :actions-played 0
                              :phase          :action}]}))
    (is (= (-> {:players [{:hand    [caravan caravan copper copper copper]
                           :deck    [copper copper estate estate copper copper silver]
                           :actions 1}]}
               (play 0 :caravan)
               (play 0 :caravan)
               (end-turn 0))
           {:current-player 0
            :players        [{:hand           [estate estate copper copper silver copper copper]
                              :play-area      [caravan caravan]
                              :deck           [copper copper copper]
                              :actions        1
                              :coins          0
                              :buys           1
                              :actions-played 0
                              :phase          :action}]}))))

(deftest fishing-village-test
  (testing "Fishing Village"
    (is (= (-> {:players [{:hand    [fishing-village]
                           :actions 1
                           :coins   0}]}
               (play 0 :fishing-village))
           {:players [{:play-area-duration [fishing-village]
                       :actions            2
                       :coins              1}]}))
    (is (= (-> {:players [{:hand    [fishing-village]
                           :actions 1
                           :coins   0}]}
               (play 0 :fishing-village)
               (end-turn 0))
           {:current-player 0
            :players        [{:play-area      [fishing-village]
                              :actions        2
                              :coins          1
                              :buys           1
                              :actions-played 0
                              :phase          :action}]}))))

(deftest haven-test
  (let [haven (assoc haven :id 1)]
    (testing "Haven"
      (is (= (-> {:players [{:hand    [haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :haven))
             {:players      [{:hand               [estate copper]
                              :play-area-duration [haven]
                              :deck               [copper]
                              :actions            1}]
              :effect-stack [{:text      "Set aside a card from your hand."
                              :player-no 0
                              :card-id   1
                              :choice    ::seaside/haven-set-aside
                              :source    :hand
                              :options   [:estate :copper]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :haven)
                 (choose :copper))
             {:players [{:hand               [estate]
                         :play-area-duration [(assoc haven :set-aside [copper])]
                         :deck               [copper]
                         :actions            1}]}))
      (is (= (-> {:players [{:hand    [haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :haven)
                 (choose :copper)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           [copper estate copper]
                                :play-area      [haven]
                                :actions        1
                                :coins          0
                                :buys           1
                                :actions-played 0
                                :phase          :action}]})))))

(deftest lighthouse-test
  (let [lighthouse-1 (assoc lighthouse :id 1)
        lighthouse-2 (assoc lighthouse :id 2)]
    (testing "Lighthouse"
      (is (= (-> {:players [{:hand    [lighthouse-1]
                             :actions 1
                             :coins   0}]}
                 (play 0 :lighthouse))
             {:players [{:play-area-duration [lighthouse-1]
                         :actions            1
                         :coins              1
                         :unaffected         [{:card-id 1}]}]}))
      (is (= (-> {:players [{:hand    [lighthouse-1]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :lighthouse)
                 (end-turn 0))
             {:current-player 1
              :players        [{:play-area-duration [lighthouse-1]
                                :actions            0
                                :coins              0
                                :buys               0
                                :actions-played     0
                                :phase              :out-of-turn
                                :unaffected         [{:card-id 1}]}
                               {:actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}]}))
      (let [curse (assoc curse :id 1)]
        (is (= (-> {:supply  [{:card curse :pile-size 20}]
                    :players [{:hand    [lighthouse-1]
                               :deck    [moat]
                               :actions 1
                               :coins   0}
                              {:hand [witch]}
                              {:hand [witch]}]}
                   (play 0 :lighthouse)
                   (end-turn 0)
                   (play 1 :witch)
                   (end-turn 1)
                   (play 2 :witch))
               {:current-player 2
                :supply         [{:card curse :pile-size 18}]
                :players        [{:hand               [moat]
                                  :play-area-duration [lighthouse-1]
                                  :actions            0
                                  :coins              0
                                  :buys               0
                                  :actions-played     0
                                  :phase              :out-of-turn
                                  :unaffected         [{:card-id 1}]}
                                 {:hand           [witch]
                                  :discard        [curse]
                                  :actions        0
                                  :coins          0
                                  :buys           0
                                  :actions-played 0
                                  :phase          :out-of-turn}
                                 {:hand      [curse]
                                  :play-area [witch]
                                  :actions   0
                                  :coins     0
                                  :buys      1
                                  :phase     :action}]})))
      (is (= (-> {:players [{:hand    [lighthouse-1]
                             :actions 1
                             :coins   0}]}
                 (play 0 :lighthouse)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area      [lighthouse-1]
                                :actions        1
                                :coins          1
                                :buys           1
                                :actions-played 0
                                :phase          :action}]}))
      (is (= (-> {:players [{:hand    [lighthouse-1 lighthouse-2]
                             :actions 1
                             :coins   0}]}
                 (play 0 :lighthouse)
                 (play 0 :lighthouse)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area      [lighthouse-1 lighthouse-2]
                                :actions        1
                                :coins          2
                                :buys           1
                                :actions-played 0
                                :phase          :action}]})))))

(deftest merchant-ship-test
  (testing "Merchant Ship"
    (is (= (-> {:players [{:hand    [merchant-ship]
                           :actions 1
                           :coins   0}]}
               (play 0 :merchant-ship))
           {:players [{:play-area-duration [merchant-ship]
                       :actions            0
                       :coins              2}]}))
    (is (= (-> {:players [{:hand    [merchant-ship]
                           :actions 1
                           :coins   0}]}
               (play 0 :merchant-ship)
               (end-turn 0))
           {:current-player 0
            :players        [{:play-area      [merchant-ship]
                              :actions        1
                              :coins          2
                              :buys           1
                              :actions-played 0
                              :phase          :action}]}))))

(deftest tactician-test
  (testing "Tactician"
    (is (= (-> {:players [{:hand    [tactician estate]
                           :actions 1}]}
               (play 0 :tactician))
           {:players [{:play-area-duration [tactician]
                       :discard            [estate]
                       :actions            0}]}))
    (is (= (-> {:players [{:hand    [tactician]
                           :actions 1}]}
               (play 0 :tactician))
           {:players [{:play-area [tactician]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [tactician estate]
                           :deck    (repeat 10 copper)
                           :actions 1
                           :coins   0}]}
               (play 0 :tactician)
               (end-turn 0))
           {:current-player 0
            :players        [{:hand           (repeat 10 copper)
                              :play-area      [tactician]
                              :discard        [estate]
                              :actions        2
                              :coins          0
                              :buys           2
                              :actions-played 0
                              :phase          :action}]}))
    (is (= (-> {:players [{:hand    [tactician]
                           :deck    (repeat 10 copper)
                           :actions 1
                           :coins   0}]}
               (play 0 :tactician)
               (end-turn 0))
           {:current-player 0
            :players        [{:hand           (repeat 5 copper)
                              :deck           (repeat 5 copper)
                              :discard        [tactician]
                              :actions        1
                              :coins          0
                              :buys           1
                              :actions-played 0
                              :phase          :action}]}))))

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
    (is (= (-> {:players [{:hand    [wharf estate estate estate copper]
                           :deck    [copper copper copper copper copper copper silver silver silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :wharf)
               (end-turn 0))
           {:current-player 0
            :players        [{:hand           [copper copper copper copper silver silver silver]
                              :play-area      [wharf]
                              :discard        [estate estate estate copper copper copper]
                              :actions        1
                              :coins          0
                              :buys           2
                              :actions-played 0
                              :phase          :action}]}))))
