(ns dombot.core-test
  (:require [clojure.test :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards :refer :all]))

(deftest start-round-test
  (testing "Start round"
    (is (= (start-round {})
           {:actions 1
            :coins   0
            :buys    1}))))

(deftest shuffle-test
  (testing "Shuffle discard"
    (is (= (shuffle-discard {:deck [] :discard [1]})
           {:deck [1] :discard []}))
    (is (thrown? AssertionError (shuffle-discard {:deck [1] :discard [2]})))))

(deftest draw-test
  (testing "Draw"
    (let [player {:hand [1 2 3] :deck [4 5] :discard [6 7]}]
      (is (= (draw player 1)
             {:hand [1 2 3 4] :deck [5] :discard [6 7]}))
      (is (= (draw player 2)
             {:hand [1 2 3 4 5] :deck [] :discard [6 7]}))
      (let [result (draw player 3)]
        (is (or (= result {:hand [1 2 3 4 5 6] :deck [7] :discard []})
                (= result {:hand [1 2 3 4 5 7] :deck [6] :discard []}))))
      (let [result (draw player 4)]
        (is (or (= result {:hand [1 2 3 4 5 6 7] :deck [] :discard []})
                (= result {:hand [1 2 3 4 5 7 6] :deck [] :discard []}))))
      (let [result (draw player 5)]
        (is (or (= result {:hand [1 2 3 4 5 6 7] :deck [] :discard []})
                (= result {:hand [1 2 3 4 5 7 6] :deck [] :discard []})))))))

(deftest gain-test
  (testing "Gain"
    (is (= (-> {:supply  [{:card {:name :province} :count 8}]
                :players [{:discard []}]}
               (gain 0 :province))
           {:supply  [{:card {:name :province} :count 7}]
            :players [{:discard [{:name :province}]}]}))
    (is (= (-> {:supply  [{:card {:name :province} :count 1}]
                :players [{:discard []}]}
               (gain 0 :province))
           {:supply  [{:card {:name :province} :count 0}]
            :players [{:discard [{:name :province}]}]}))
    (is (thrown? AssertionError (-> {:supply  [{:card {:name :province} :count 0}]
                                     :players [{:discard []}]}
                                    (gain 0 :province))))
    (is (thrown? AssertionError (-> {:supply  []
                                     :players [{:discard []}]}
                                    (gain 0 :province))))))

(deftest put-in-play-test
  (testing "Playing a card from hand to play-area"
    (is (= (put-in-play {:hand [{:name :smithy}] :play-area []} :smithy)
           {:hand [] :play-area [{:name :smithy}]}))
    (is (thrown? AssertionError (put-in-play {:hand [{:name :smithy}] :play-area []} :copper)))
    (is (= (put-in-play {:hand [{:name :copper} {:name :smithy}] :play-area []} :smithy)
           {:hand [{:name :copper}] :play-area [{:name :smithy}]}))
    (is (= (put-in-play {:hand [{:name :smithy} {:name :smithy}] :play-area []} :smithy)
           {:hand [{:name :smithy}] :play-area [{:name :smithy}]}))))

(deftest play-test
  (testing "Playing card is impossible because it has no/wrong type"
    (is (thrown? AssertionError (play {:players [{:hand [{:name :estate}]}]}
                                      0 :estate)))
    (is (thrown? AssertionError (play {:players [{:hand [{:name :estate :type #{:victory}}]}]}
                                      0 :estate))))
  (testing "Playing treasure"
    (testing "is impossible because"
      (testing "player has no cards in hand"
        (is (thrown? AssertionError (play {:players [{:hand []}]}
                                          0 :any-card))))
      (testing "card has no value"
        (is (thrown? AssertionError (play {:players [{:hand [{:name :any-card :type #{:treasure}}]}]}
                                          0 :any-card)))))
    (testing "Copper"
      (is (= (play {:players [{:hand      [copper]
                               :play-area []
                               :coins     0}]}
                   0 :copper)
             {:players [{:hand      []
                         :play-area [copper]
                         :coins     1}]})))
    (testing "Silver"
      (is (= (play {:players [{:hand      [silver]
                               :play-area []
                               :coins     0}]}
                   0 :silver)
             {:players [{:hand      []
                         :play-area [silver]
                         :coins     2}]})))
    (testing "Gold"
      (is (= (play {:players [{:hand      [gold]
                               :play-area []
                               :coins     0}]}
                   0 :gold)
             {:players [{:hand      []
                         :play-area [gold]
                         :coins     3}]})))
    (testing "All treasures"
      (is (= (play-treasures {:players [{:hand      [gold silver copper {:name :smithy} copper]
                                         :play-area []
                                         :coins     0}]}
                             0)
             {:players [{:hand      [{:name :smithy}]
                         :play-area [gold silver copper copper]
                         :coins     7}]}))))
  (testing "Playing action"
    (testing "is impossible because"
      (testing "player has no cards in hand"
        (is (thrown? AssertionError (play {:players [{:hand    []
                                                      :actions 1}]}
                                          0 :any-card))))
      (testing "player has no more actions"
        (is (thrown? AssertionError (play {:hand    [{:name :any-card :type #{:action} :action-fn (fn [game _] game)}]
                                           :actions 0}
                                          0 :any-card))))
      (testing "card has no action-fn"
        (is (thrown? AssertionError (play {:players [{:hand    [{:name :any-card :type #{:action}}]
                                                      :actions 1}]}
                                          0 :any-card)))))
    (testing "Council Room"
      (is (= (play {:players [{:deck      (repeat 5 {:name :copper})
                               :hand      [council-room]
                               :play-area []
                               :actions   1
                               :coins     0
                               :buys      1}
                              {:deck [{:name :copper} {:name :copper}]
                               :hand []}]}
                   0 :council-room)
             {:players [{:deck      [{:name :copper}]
                         :hand      (repeat 4 {:name :copper})
                         :play-area [council-room]
                         :actions   0
                         :coins     0
                         :buys      2}
                        {:deck [{:name :copper}]
                         :hand [{:name :copper}]}]})))
    (testing "Festival"
      (is (= (play {:players [{:deck      [{:name :copper}]
                               :hand      [festival]
                               :play-area []
                               :actions   1
                               :coins     0
                               :buys      1}]}
                   0 :festival)
             {:players [{:deck      [{:name :copper}]
                         :hand      []
                         :play-area [festival]
                         :actions   2
                         :coins     2
                         :buys      2}]})))
    (testing "Moat"
      (is (= (play {:players [{:deck      [{:name :copper} {:name :copper} {:name :copper}]
                               :hand      [moat]
                               :play-area []
                               :actions   1}]}
                   0 :moat)
             {:players [{:deck      [{:name :copper}]
                         :hand      [{:name :copper} {:name :copper}]
                         :play-area [moat]
                         :actions   0}]}))
      (is (= (play {:players [{:deck      [{:name :copper}]
                               :hand      [moat]
                               :play-area []
                               :actions   1}]}
                   0 :moat)
             {:players [{:deck      []
                         :hand      [{:name :copper}]
                         :play-area [moat]
                         :actions   0}]})))
    (testing "Laboratory"
      (is (= (play {:players [{:deck      [{:name :copper} {:name :copper} {:name :copper}]
                               :hand      [laboratory]
                               :play-area []
                               :actions   1}]}
                   0 :laboratory)
             {:players [{:deck      [{:name :copper}]
                         :hand      [{:name :copper} {:name :copper}]
                         :play-area [laboratory]
                         :actions   1}]})))
    (testing "Market"
      (is (= (play {:players [{:deck      [{:name :copper} {:name :copper} {:name :copper}]
                               :hand      [market]
                               :play-area []
                               :actions   1
                               :coins     0
                               :buys      1}]}
                   0 :market)
             {:players [{:deck      [{:name :copper} {:name :copper}]
                         :hand      [{:name :copper}]
                         :play-area [market]
                         :actions   1
                         :coins     1
                         :buys      2}]})))
    (testing "Smithy"
      (is (= (play {:players [{:deck      [{:name :copper} {:name :copper} {:name :copper}]
                               :hand      [smithy]
                               :play-area []
                               :actions   1}]}
                   0 :smithy)
             {:players [{:deck      []
                         :hand      [{:name :copper} {:name :copper} {:name :copper}]
                         :play-area [smithy]
                         :actions   0}]})))
    (testing "Village"
      (is (= (play {:players [{:deck      [{:name :copper} {:name :copper}]
                               :hand      [village]
                               :play-area []
                               :actions   1}]}
                   0 :village)
             {:players [{:deck      [{:name :copper}]
                         :hand      [{:name :copper}]
                         :play-area [village]
                         :actions   2}]})))
    (testing "Woodcutter"
      (is (= (play {:players [{:deck      [{:name :copper}]
                               :hand      [woodcutter]
                               :play-area []
                               :actions   1
                               :coins     0
                               :buys      1}]}
                   0 :woodcutter)
             {:players [{:deck      [{:name :copper}]
                         :hand      []
                         :play-area [woodcutter]
                         :actions   0
                         :coins     2
                         :buys      2}]})))))

(deftest buy-test
  (testing "Buying a card"
    (testing "is impossible because"
      (testing "player has no buys left"
        (is (thrown? AssertionError (buy-card {:supply  [{:card {:name :copper :cost 0} :count 40}]
                                               :players [{:coins 0
                                                          :buys  0}]}
                                              0 :copper))))
      (testing "player has not enough coins"
        (is (thrown? AssertionError (buy-card {:supply  [{:card {:name :silver :cost 3} :count 40}]
                                               :players [{:coins 2
                                                          :buys  1}]}
                                              0 :silver))))
      (testing "supply is empty"
        (is (thrown? AssertionError (buy-card {:supply  [{:card {:name :copper :cost 0} :count 0}]
                                               :players [{:coins 0
                                                          :buys  1}]}
                                              0 :copper))))
      (testing "supply does not contain card-name"
        (is (thrown? AssertionError (buy-card {:supply  []
                                               :players [{:coins 0
                                                          :buys  1}]}
                                              0 :copper)))))
    (is (= (buy-card {:supply  [{:card {:name :copper :cost 0} :count 40}]
                      :players [{:discard []
                                 :coins   0
                                 :buys    1}]}
                     0 :copper)
           {:supply  [{:card {:name :copper :cost 0} :count 39}]
            :players [{:discard [{:name :copper :cost 0}]
                       :coins   0
                       :buys    0}]}))
    (is (= (buy-card {:supply  [{:card {:name :silver :cost 3} :count 40}]
                      :players [{:discard []
                                 :coins   6
                                 :buys    2}]}
                     0 :silver)
           {:supply  [{:card {:name :silver :cost 3} :count 39}]
            :players [{:discard [{:name :silver :cost 3}]
                       :coins   3
                       :buys    1}]}))))

(deftest clean-up-test
  (testing "Clean up"
    (is (= (clean-up {:hand      [{:name :estate}]
                      :play-area [{:name :silver}]
                      :deck      (repeat 5 {:name :copper})
                      :discard   [{:name :cellar}]})
           {:hand      (repeat 5 {:name :copper})
            :play-area []
            :deck      []
            :discard   [{:name :cellar} {:name :silver} {:name :estate}]}))
    (is (= (clean-up {:hand      [{:name :copper}]
                      :play-area [{:name :copper}]
                      :deck      [{:name :copper}]
                      :discard   [{:name :copper}]})
           {:hand      (repeat 4 {:name :copper})
            :play-area []
            :deck      []
            :discard   []}))
    (is (= (clean-up {:hand      [{:name :copper}]
                      :play-area [{:name :copper}]
                      :deck      (repeat 3 {:name :silver})
                      :discard   [{:name :copper}]})
           {:hand      (concat (repeat 3 {:name :silver}) (repeat 2 {:name :copper}))
            :play-area []
            :deck      [{:name :copper}]
            :discard   []}))))

(deftest view-test
  (testing "View player"
    (is (= (view-player {:hand      [{:name :copper} {:name :copper} {:name :copper} {:name :estate} {:name :estate}]
                         :play-area []
                         :deck      [{:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate}]
                         :discard   []})
           {:hand      {:copper 3 :estate 2}
            :play-area {}
            :deck      5
            :discard   0})))
  (testing "View supply"
    (is (= (view-supply (base-supply 2 8))
           {:copper 46 :silver 40 :gold 30 :estate 8 :duchy 8 :province 8 :curse 10})))
  (testing "View game"
    (is (= (view-game {:supply         (base-supply 2 8)
                       :players        [{:hand      [{:name :copper} {:name :copper} {:name :copper} {:name :estate} {:name :estate}]
                                         :play-area []
                                         :deck      [{:name :copper} {:name :copper} {:name :copper} {:name :copper} {:name :estate}]
                                         :discard   []}]
                       :current-player 0})
           {:supply         {:copper 46 :silver 40 :gold 30 :estate 8 :duchy 8 :province 8 :curse 10}
            :player         {:hand      {:copper 3 :estate 2}
                             :play-area {}
                             :deck      5
                             :discard   0}
            :current-player 0}))))
