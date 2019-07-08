(ns dombot.operations-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.utils :as ut]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :refer :all]
            [dombot.cards.seaside :refer :all]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest start-turn-test
  (testing "Start turn"
    (is (= (start-turn {})
           {:actions 1
            :coins   0
            :buys    1}))))

(deftest gain-test
  (testing "Gain"
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 8}]
                :players [{}]}
               (gain {:player-no 0
                      :card-name :province}))
           {:supply  [{:card {:name :province} :pile-size 7}]
            :players [{:discard [{:name :province :id 1}]}]}))
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 1}]
                :players [{}]}
               (gain {:player-no 0
                      :card-name :province}))
           {:supply  [{:card {:name :province} :pile-size 0}]
            :players [{:discard [{:name :province :id 2}]}]}))
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 0}]
                :players [{}]}
               (gain {:player-no 0
                      :card-name :province}))
           {:supply  [{:card {:name :province} :pile-size 0}]
            :players [{}]}))
    (is (= (-> {:players [{}]}
               (gain {:player-no 0 :card-name :province}))
           {:players [{}]}))
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 8}]
                :players [{:discard             [{:name :copper} {:name :copper}]
                           :approx-discard-size 1}]}
               (gain {:player-no 0
                      :card-name :province}))
           {:supply  [{:card {:name :province} :pile-size 7}]
            :players [{:discard             [{:name :copper} {:name :copper} {:name :province :id 3}]
                       :approx-discard-size 2}]}))))

(deftest buy-test
  (testing "Buying a card"
    (testing "is impossible because"
      (testing "player has no buys left"
        (is (thrown-with-msg? AssertionError #"Buy error: You have no more buys."
                              (buy-card {:supply  [{:card {:name :copper} :pile-size 40}]
                                         :players [{:coins 0
                                                    :buys  0}]}
                                        0 :copper))))
      (testing "player has not enough coins"
        (is (thrown-with-msg? AssertionError #"Buy error: Silver costs 3 and you only have 2 coins."
                              (buy-card {:supply  [{:card {:name :silver :cost 3} :pile-size 40}]
                                         :players [{:coins 2
                                                    :buys  1}]}
                                        0 :silver))))
      (testing "supply is empty"
        (is (thrown-with-msg? AssertionError #"Buy error: Copper supply is empty."
                              (buy-card {:supply  [{:card {:name :copper :cost 0} :pile-size 0}]
                                         :players [{:coins 0
                                                    :buys  1}]}
                                        0 :copper))))
      (testing "supply does not contain card-name"
        (is (thrown-with-msg? AssertionError #"Buy error: The supply doesn't have a Copper pile."
                              (buy-card {:players [{:coins 0
                                                    :buys  1}]}
                                        0 :copper)))))
    (is (= (buy-card {:supply  [{:card {:name :copper :cost 0} :pile-size 3}]
                      :players [{:coins 0
                                 :buys  1}]}
                     0 :copper)
           {:supply  [{:card {:name :copper :cost 0} :pile-size 2}]
            :players [{:discard [{:name :copper :cost 0 :id 1}]
                       :coins   0
                       :buys    0}]}))
    (is (= (-> {:supply  [{:card {:name :copper :cost 0} :pile-size 3}]
                :players [{:coins 0
                           :buys  2}]}
               (buy-card 0 :copper)
               (buy-card 0 :copper))
           {:supply  [{:card {:name :copper :cost 0} :pile-size 1}]
            :players [{:discard [{:name :copper :cost 0 :id 2} {:name :copper :cost 0 :id 3}]
                       :coins   0
                       :buys    0}]}))
    (is (= (buy-card {:supply  [{:card {:name :silver :cost 3} :pile-size 3}]
                      :players [{:coins 4
                                 :buys  2}]}
                     0 :silver)
           {:supply  [{:card {:name :silver :cost 3} :pile-size 2}]
            :players [{:discard [{:name :silver :cost 3 :id 4}]
                       :coins   1
                       :buys    1}]}))
    (testing "with cost reduction"
      (is (= (-> {:supply          [{:card {:name :silver :cost 3} :pile-size 3}]
                  :cost-reductions [{:reduction 1}]
                  :players         [{:coins 4
                                     :buys  2}]}
                 (buy-card 0 :silver))
             {:supply          [{:card {:name :silver :cost 3} :pile-size 2}]
              :cost-reductions [{:reduction 1}]
              :players         [{:discard [{:name :silver :cost 3 :id 5}]
                                 :coins   2
                                 :buys    1}]}))
      (is (= (-> {:supply          [{:card {:name :silver :cost 3} :pile-size 3}]
                  :cost-reductions [{:reduction 1}]
                  :players         [{:coins 4
                                     :buys  2}]}
                 (buy-card 0 :silver)
                 (buy-card 0 :silver))
             {:supply          [{:card {:name :silver :cost 3} :pile-size 1}]
              :cost-reductions [{:reduction 1}]
              :players         [{:discard [{:name :silver :cost 3 :id 6}
                                           {:name :silver :cost 3 :id 7}]
                                 :coins   0
                                 :buys    0}]})))))

(deftest shuffle-test
  (testing "Shuffle discard"
    (is (= (-> {:players [{:discard [1]}]}
               (shuffle-discard {:player-no 0})
               check-stack)
           {:players [{:deck [1]}]}))
    (is (= (-> {:players [{:deck [1] :discard [2]}]}
               (shuffle-discard {:player-no 0})
               check-stack)
           {:players [{:deck [1 2]}]}))))

(deftest move-card-test
  (testing "Playing a card from hand to play-area"
    (is (= (move-card {:players [{:hand [{:name :smithy}]}]}
                      {:player-no 0
                       :card-name :smithy
                       :from      :hand
                       :to        :play-area})
           {:players [{:play-area [{:name :smithy}]}]}))
    (is (thrown? AssertionError
                 (move-card {:players [{:hand [{:name :smithy}]}]}
                            {:player-no 0
                             :card-name :copper
                             :from      :hand
                             :to        :play-area})))
    (is (= (move-card {:players [{:hand [{:name :copper} {:name :smithy}]}]}
                      {:player-no 0
                       :card-name :smithy
                       :from      :hand
                       :to        :play-area})
           {:players [{:hand [{:name :copper}] :play-area [{:name :smithy}]}]}))
    (is (= (move-card {:players [{:hand [{:name :smithy} {:name :smithy}]}]}
                      {:player-no 0
                       :card-name :smithy
                       :from      :hand
                       :to        :play-area})
           {:players [{:hand [{:name :smithy}] :play-area [{:name :smithy}]}]}))
    (is (= (move-card {:players [{:hand [{:name :smithy}]
                                  :deck [{:name :copper}]}]}
                      {:player-no   0
                       :card-name   :smithy
                       :from        :hand
                       :to          :deck
                       :to-position :top})
           {:players [{:deck [{:name :smithy} {:name :copper}]}]}))
    (is (= (move-card {:players [{:hand [{:name :smithy}]}]}
                      {:player-no 0
                       :card-name :smithy
                       :from      :hand
                       :to        :trash})
           {:players [{}]
            :trash   [{:name :smithy}]}))
    (is (= (move-card {:players [{:deck [{:name :copper} {:name :smithy}]}]}
                      {:player-no     0
                       :from          :deck
                       :from-position :top
                       :to            :discard})
           {:players [{:deck    [{:name :smithy}]
                       :discard [{:name :copper}]}]}))
    (is (= (move-card {:players [{}]}
                      {:player-no     0
                       :from          :deck
                       :from-position :top
                       :to            :discard})
           {:players [{}]}))
    (is (= (-> {:players [{:discard [{:name :copper} {:name :copper}]}]}
               (move-card {:player-no     0
                           :from          :deck
                           :from-position :top
                           :to            :discard})
               check-stack)
           {:players [{:deck    [{:name :copper}]
                       :discard [{:name :copper}]}]}))
    (is (= (move-card {:players [{:deck                [{:name :copper} {:name :smithy}]
                                  :approx-discard-size 0}]}
                      {:player-no     0
                       :from          :deck
                       :from-position :top
                       :to            :discard})
           {:players [{:deck                [{:name :smithy}]
                       :discard             [{:name :copper}]
                       :approx-discard-size 1}]}))
    (is (= (move-card {:players [{:hand                [{:name :copper} {:name :smithy}]
                                  :discard             [{:name :copper} {:name :estate}]
                                  :approx-discard-size 2}]}
                      {:player-no     0
                       :from          :discard
                       :from-position :bottom
                       :to            :hand})
           {:players [{:hand                [{:name :copper} {:name :smithy} {:name :estate}]
                       :discard             [{:name :copper}]
                       :approx-discard-size 1}]}))))

(deftest draw-test
  (testing "Draw"
    (let [game {:players [{:hand [1 2 3] :deck [4 5] :discard [6 7]}]}]
      (is (= (-> game
                 (draw {:player-no 0 :arg 1})
                 check-stack)
             {:players [{:hand [1 2 3 4] :deck [5] :discard [6 7]}]}))
      (is (= (-> game
                 (draw {:player-no 0 :arg 2})
                 check-stack)
             {:players [{:hand [1 2 3 4 5] :discard [6 7]}]}))
      (is (= (-> game
                 (draw {:player-no 0 :arg 3})
                 check-stack)
             {:players [{:hand [1 2 3 4 5 6] :deck [7]}]}))
      (is (= (-> game
                 (draw {:player-no 0 :arg 4})
                 check-stack)
             {:players [{:hand [1 2 3 4 5 7 6]}]}))
      (is (= (-> game
                 (draw {:player-no 0 :arg 5})
                 check-stack)
             {:players [{:hand [1 2 3 4 5 6 7]}]})))))

#_(deftest choose-test
    (testing "No/invalid choice"
      (is (thrown-with-msg? AssertionError #"Choose error: You don't have a choice to make."
                            (choose {} :copper)))
      (is (thrown-with-msg? AssertionError #"Choose error: Choice has no options"
                            (choose {:effect-stack [{:player-no 0 :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)}]} :copper))))
    (testing "Optional single choice"
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]
                                      :max       1}]}
                     nil)
             {:players [{:chosen nil}]}))
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]
                                      :max       1}]}
                     :copper)
             {:players [{:chosen :copper}]}))
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]
                                      :max       1}]}
                     [])
             {:players [{:chosen nil}]}))
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]
                                      :max       1}]}
                     [:copper])
             {:players [{:chosen :copper}]}))
      (is (thrown-with-msg? AssertionError #"Choose error: You can only pick 1 option."
                            (choose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                     :options   [:copper :copper]
                                                     :max       1}]}
                                    [:copper :copper])))
      (is (thrown-with-msg? AssertionError #"Choose error: Estate is not a valid choice."
                            (choose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                     :options   [:copper]
                                                     :max       1}]}
                                    :estate))))
    (testing "Mandatory single choice"
      (is (thrown-with-msg? AssertionError #"Choose error: You must pick an option"
                            (choose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                     :options   [:copper]
                                                     :min       1
                                                     :max       1}]}
                                    nil)))
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]
                                      :min       1
                                      :max       1}]}
                     :copper)
             {:players [{:chosen :copper}]}))
      (is (thrown-with-msg? AssertionError #"Choose error: You must pick an option"
                            (choose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                     :options   [:copper]
                                                     :min       1
                                                     :max       1}]}
                                    [])))
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]
                                      :min       1
                                      :max       1}]}
                     [:copper])
             {:players [{:chosen :copper}]}))
      (is (thrown-with-msg? AssertionError #"Choose error: You can only pick 1 option."
                            (choose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                     :options   [:copper :copper]
                                                     :min       1
                                                     :max       1}]}
                                    [:copper :copper]))))
    (testing "Multi choice"
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]}]}
                     nil)
             {:players [{}]}))
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]}]}
                     :copper)
             {:players [{:chosen [:copper]}]}))
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]}]}
                     [])
             {:players [{}]}))
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper]}]}
                     [:copper])
             {:players [{:chosen [:copper]}]}))
      (is (= (choose {:effect-stack [{:player-no 0
                                      :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                      :options   [:copper :copper]}]}
                     [:copper :copper])
             {:players [{:chosen [:copper :copper]}]}))
      (is (thrown-with-msg? AssertionError #"Choose error: Estate is not a valid choice."
                            (choose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                     :options   [:copper]}]}
                                    [:copper :estate :silver])))))

(deftest play-test
  (testing "Playing a card is impossible because"
    (testing "it has no/wrong types"
      (is (thrown-with-msg? AssertionError #"Play error: No Card has no types"
                            (play {:players [{:hand [{:name :no-card}]}]}
                                  0 :no-card)))
      (is (thrown-with-msg? AssertionError #"Play error: Victory cards cannot be played."
                            (play {:players [{:hand [{:name :estate :types #{:victory}}]}]}
                                  0 :estate))))
    (testing "player has no cards in hand"
      (is (thrown? AssertionError
                   (play {:players [{}]}
                         0 :copper)))))
  (testing "Playing treasure"
    (testing "is impossible because"
      (testing "card has no coin-value"
        (is (thrown-with-msg? AssertionError #"Play error: Copper has no coin value"
                              (play {:players [{:hand [{:name :copper :types #{:treasure}}]}]}
                                    0 :copper))))))
  (testing "Playing action"
    (testing "is impossible because"
      (testing "player has no more actions"
        (is (thrown-with-msg? AssertionError #"Play error: You have no more actions."
                              (play {:players [{:hand    [{:name :village :types #{:action} :effects []}]
                                                :actions 0}]}
                                    0 :village))))
      (testing "card has no effects"
        (is (thrown-with-msg? AssertionError #"Play error: Village has no effect."
                              (play {:players [{:hand    [{:name :village :types #{:action}}]
                                                :actions 1}]}
                                    0 :village)))))))

(deftest cost-reduction-test
  (testing "Cost reduction"
    (is (= (ut/get-cost {:cost-reductions [{:reduction 1}]} 0 {:cost 3})
           2))
    (is (= (ut/get-cost {:cost-reductions [{:reduction 2}]} 0 {:cost 3})
           1))
    (is (= (ut/get-cost {:cost-reductions [{:reduction 1} {:reduction 1}]} 0 {:cost 3})
           1))
    (is (= (ut/get-cost {:cost-reductions [{:reduction 1}]} 0 {:cost 0})
           0))
    (is (= (ut/get-cost {:cost-reductions [{:type      :action
                                            :reduction 2}]}
                        0
                        {:types #{:action}
                         :cost  5})
           3))
    (is (= (ut/get-cost {:cost-reductions [{:type      :action
                                            :reduction 2}]}
                        0
                        {:types #{:victory}
                         :cost  5})
           5))))

(deftest clean-up-test
  (testing "Clean up"
    (is (= (-> {:players [{:hand            [{:name :estate}]
                           :play-area       [{:name :silver}]
                           :deck            (repeat 5 {:name :copper})
                           :discard         [{:name :cellar}]
                           :number-of-turns 8}]}
               (clean-up {:player-no 0})
               check-stack)
           {:players [{:hand            (repeat 5 {:name :copper})
                       :discard         [{:name :cellar} {:name :estate} {:name :silver}]
                       :actions         0
                       :coins           0
                       :buys            0
                       :number-of-turns 9
                       :phase           :out-of-turn}]}))
    (is (= (-> {:players [{:hand            [{:name :copper}]
                           :play-area       [{:name :copper}]
                           :deck            [{:name :copper}]
                           :discard         [{:name :copper}]
                           :number-of-turns 8}]}
               (clean-up {:player-no 0})
               check-stack)
           {:players [{:hand            (repeat 4 {:name :copper})
                       :actions         0
                       :coins           0
                       :buys            0
                       :number-of-turns 9
                       :phase           :out-of-turn}]}))
    (is (= (-> {:players [{:hand            [{:name :copper}]
                           :play-area       [{:name :copper}]
                           :deck            (repeat 3 {:name :silver})
                           :discard         [{:name :copper}]
                           :number-of-turns 8}]}
               (clean-up {:player-no 0})
               check-stack)
           {:players [{:hand            (concat (repeat 3 {:name :silver}) (repeat 2 {:name :copper}))
                       :deck            [{:name :copper}]
                       :actions         0
                       :coins           0
                       :buys            0
                       :number-of-turns 9
                       :phase           :out-of-turn}]}))
    (is (= (-> {:players [{:number-of-turns 8
                           :triggers        [{:duration :turn}]}]}
               (clean-up {:player-no 0})
               check-stack)
           {:players [{:actions         0
                       :coins           0
                       :buys            0
                       :number-of-turns 9
                       :phase           :out-of-turn}]}))
    (is (= (-> {:cost-reductions [{:reduction 1}]
                :players         [{:number-of-turns 8}]}
               (clean-up {:player-no 0})
               check-stack)
           {:players [{:actions         0
                       :coins           0
                       :buys            0
                       :number-of-turns 9
                       :phase           :out-of-turn}]}))
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 0}]
                :players [{:hand            [{:name :estate :victory-points 1}]
                           :play-area       [{:name :silver}]
                           :deck            [{:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate :victory-points 1}]
                           :discard         [{:name :cellar} {:name :estate :victory-points 1} {:name :duchy :victory-points 3}]
                           :number-of-turns 8}
                          {:hand            [{:name :duchy :victory-points 3}]
                           :number-of-turns 9}]}
               (clean-up {:player-no 0})
               check-stack)
           {:supply  [{:card {:name :province} :pile-size 0}]
            :players [{:hand            [{:name :cellar} {:name :estate :victory-points 1} {:name :duchy :victory-points 3} {:name :estate :victory-points 1} {:name :silver}
                                         {:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate :victory-points 1}]
                       :actions         0
                       :coins           0
                       :buys            0
                       :number-of-turns 9
                       :phase           :end-of-game
                       :victory-points  6
                       :winner          true}
                      {:hand            [{:name :duchy :victory-points 3}]
                       :number-of-turns 9
                       :phase           :end-of-game
                       :victory-points  3
                       :winner          false}]}))
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 0}]
                :players [{:hand            [{:name :estate :victory-points 1}]
                           :play-area       [{:name :silver}]
                           :deck            [{:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate :victory-points 1}]
                           :discard         [{:name :cellar} {:name :estate :victory-points 1} {:name :duchy :victory-points 3}]
                           :number-of-turns 9}
                          {:hand            [{:name :duchy :victory-points 3}]
                           :number-of-turns 9}]}
               (clean-up {:player-no 0})
               check-stack)
           {:supply  [{:card {:name :province} :pile-size 0}]
            :players [{:hand            [{:name :cellar} {:name :estate :victory-points 1} {:name :duchy :victory-points 3} {:name :estate :victory-points 1} {:name :silver}
                                         {:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate :victory-points 1}]
                       :actions         0
                       :coins           0
                       :buys            0
                       :number-of-turns 10
                       :phase           :end-of-game
                       :victory-points  6
                       :winner          true}
                      {:hand            [{:name :duchy :victory-points 3}]
                       :number-of-turns 9
                       :phase           :end-of-game
                       :victory-points  3
                       :winner          false}]}))
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 0}]
                :players [{:hand            [{:name :estate :victory-points 1}]
                           :play-area       [{:name :silver}]
                           :deck            [{:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate :victory-points 1}]
                           :discard         [{:name :cellar} {:name :estate :victory-points 1} {:name :duchy :victory-points 3}]
                           :number-of-turns 8}
                          {:hand            [{:name :province :victory-points 6}]
                           :number-of-turns 9}]}
               (clean-up {:player-no 0})
               check-stack)
           {:supply  [{:card {:name :province} :pile-size 0}]
            :players [{:hand            [{:name :cellar} {:name :estate :victory-points 1} {:name :duchy :victory-points 3} {:name :estate :victory-points 1} {:name :silver}
                                         {:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate :victory-points 1}]
                       :actions         0
                       :coins           0
                       :buys            0
                       :number-of-turns 9
                       :phase           :end-of-game
                       :victory-points  6
                       :winner          true}
                      {:hand            [{:name :province :victory-points 6}]
                       :number-of-turns 9
                       :phase           :end-of-game
                       :victory-points  6
                       :winner          true}]}))
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 0}]
                :players [{:hand            [{:name :estate :victory-points 1}]
                           :play-area       [{:name :silver}]
                           :deck            [{:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate :victory-points 1}]
                           :discard         [{:name :cellar} {:name :estate :victory-points 1} {:name :duchy :victory-points 3}]
                           :number-of-turns 9}
                          {:hand            [{:name :province :victory-points 6}]
                           :number-of-turns 9}]}
               (clean-up {:player-no 0})
               check-stack)
           {:supply  [{:card {:name :province} :pile-size 0}]
            :players [{:hand            [{:name :cellar} {:name :estate :victory-points 1} {:name :duchy :victory-points 3} {:name :estate :victory-points 1} {:name :silver}
                                         {:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate :victory-points 1}]
                       :actions         0
                       :coins           0
                       :buys            0
                       :number-of-turns 10
                       :phase           :end-of-game
                       :victory-points  6
                       :winner          false}
                      {:hand            [{:name :province :victory-points 6}]
                       :number-of-turns 9
                       :phase           :end-of-game
                       :victory-points  6
                       :winner          true}]}))))

(deftest game-end-test
  (testing "Game ending conditions"
    (is (not (game-ended? {:supply [{:card {:name :province} :pile-size 1}]})))
    (is (game-ended? {:supply [{:card {:name :province} :pile-size 0}]}))
    (is (not (game-ended? {:supply (concat [{:card {:name :province} :pile-size 1}] (repeat 1 {:pile-size 0}))})))
    (is (not (game-ended? {:supply (concat [{:card {:name :province} :pile-size 1}] (repeat 2 {:pile-size 0}))})))
    (is (game-ended? {:supply (concat [{:card {:name :province} :pile-size 1}] (repeat 3 {:pile-size 0}))}))
    (is (game-ended? {:supply (concat [{:card {:name :province} :pile-size 1}] (repeat 4 {:pile-size 0}))}))))

(deftest calc-victory-points-test
  (testing "Calculate Victory Points"
    (is (= (calc-victory-points {:hand       (repeat 1 estate)
                                 :play-area  (repeat 2 estate)
                                 :deck       (repeat 4 estate)
                                 :discard    (repeat 8 estate)
                                 :island-mat (repeat 16 estate)
                                 :vp-tokens  32})
           63))
    (is (= (calc-victory-points {:play-area [(assoc haven :set-aside [estate])]})
           1))))
