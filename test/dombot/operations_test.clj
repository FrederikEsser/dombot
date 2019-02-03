(ns dombot.operations-test
  (:require [clojure.test :refer :all]
            [dombot.operations :refer :all]))

(deftest start-turn-test
  (testing "Start turn"
    (is (= (start-turn {})
           {:actions 1
            :coins   0
            :buys    1
            :phase   :action}))))

(deftest gain-test
  (testing "Gain"
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 8}]
                :players [{:discard []}]}
               (gain 0 :province))
           {:supply  [{:card {:name :province} :pile-size 7}]
            :players [{:discard [{:name :province}]}]}))
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 1}]
                :players [{:discard []}]}
               (gain 0 :province))
           {:supply  [{:card {:name :province} :pile-size 0}]
            :players [{:discard [{:name :province}]}]}))
    (is (= (-> {:supply  [{:card {:name :province} :pile-size 0}]
                :players [{:discard []}]}
               (gain 0 :province))
           {:supply  [{:card {:name :province} :pile-size 0}]
            :players [{:discard []}]}))
    (is (thrown-with-msg? AssertionError #"Gain error: The supply doesn't have a Province pile"
                          (-> {:supply  []
                               :players [{:discard []}]}
                              (gain 0 :province))))))

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
                              (buy-card {:supply  []
                                         :players [{:coins 0
                                                    :buys  1}]}
                                        0 :copper)))))
    (is (= (buy-card {:supply  [{:card {:name :copper :cost 0} :pile-size 40}]
                      :players [{:discard []
                                 :coins   0
                                 :buys    1}]}
                     0 :copper)
           {:supply  [{:card {:name :copper :cost 0} :pile-size 39}]
            :players [{:discard [{:name :copper :cost 0}]
                       :coins   0
                       :buys    0}]}))
    (is (= (buy-card {:supply  [{:card {:name :silver :cost 3} :pile-size 40}]
                      :players [{:discard []
                                 :coins   6
                                 :buys    2}]}
                     0 :silver)
           {:supply  [{:card {:name :silver :cost 3} :pile-size 39}]
            :players [{:discard [{:name :silver :cost 3}]
                       :coins   3
                       :buys    1}]}))))

(deftest shuffle-test
  (testing "Shuffle discard"
    (is (= (shuffle-discard {:deck [] :discard [1]})
           {:deck [1] :discard []}))
    (is (thrown-with-msg? AssertionError #"Shuffle error: Your deck is not empty."
                          (shuffle-discard {:deck [1] :discard [2]})))))

(deftest move-card-test
  (testing "Playing a card from hand to play-area"
    (is (= (move-card {:players [{:hand [{:name :smithy}] :play-area []}]} 0
                      {:card-name :smithy
                       :from      :hand
                       :to        :play-area})
           {:players [{:hand [] :play-area [{:name :smithy}]}]}))
    (is (thrown-with-msg? AssertionError #"Move error: There is no Copper in your Hand"
                          (move-card {:players [{:hand [{:name :smithy}] :play-area []}]} 0
                                     {:card-name :copper
                                      :from      :hand
                                      :to        :play-area})))
    (is (= (move-card {:players [{:hand [{:name :copper} {:name :smithy}] :play-area []}]} 0
                      {:card-name :smithy
                       :from      :hand
                       :to        :play-area})
           {:players [{:hand [{:name :copper}] :play-area [{:name :smithy}]}]}))
    (is (= (move-card {:players [{:hand [{:name :smithy} {:name :smithy}] :play-area []}]} 0
                      {:card-name :smithy
                       :from      :hand
                       :to        :play-area})
           {:players [{:hand [{:name :smithy}] :play-area [{:name :smithy}]}]}))
    (is (= (move-card {:players [{:hand [{:name :smithy}]
                                  :deck [{:name :copper}]}]} 0
                      {:card-name   :smithy
                       :from        :hand
                       :to          :deck
                       :to-position :top})
           {:players [{:hand []
                       :deck [{:name :smithy} {:name :copper}]}]}))
    (is (= (move-card {:players [{:hand [{:name :smithy}]}]} 0
                      {:card-name :smithy
                       :from      :hand
                       :to        :trash})
           {:players [{:hand []}]
            :trash   [{:name :smithy}]}))
    (is (= (move-card {:players [{:deck [{:name :copper} {:name :smithy}]}]} 0
                      {:from          :deck
                       :from-position :top
                       :to            :discard})
           {:players [{:deck    [{:name :smithy}]
                       :discard [{:name :copper}]}]}))
    (is (= (move-card {:players [{:deck []}]} 0
                      {:from          :deck
                       :from-position :top
                       :to            :discard})
           {:players [{:deck []}]}))
    (is (= (move-card {:players [{:deck    []
                                  :discard [{:name :copper} {:name :copper}]}]} 0
                      {:from          :deck
                       :from-position :top
                       :to            :discard})
           {:players [{:deck    [{:name :copper}]
                       :discard [{:name :copper}]}]}))))

(deftest draw-test
  (testing "Draw"
    (let [game {:players [{:hand [1 2 3] :deck [4 5] :discard [6 7]}]}]
      (is (= (draw game 0 1)
             {:players [{:hand [1 2 3 4] :deck [5] :discard [6 7]}]}))
      (is (= (draw game 0 2)
             {:players [{:hand [1 2 3 4 5] :deck [] :discard [6 7]}]}))
      (let [result (draw game 0 3)]
        (is (or (= result {:players [{:hand [1 2 3 4 5 6] :deck [7] :discard []}]})
                (= result {:players [{:hand [1 2 3 4 5 7] :deck [6] :discard []}]}))))
      (let [result (draw game 0 4)]
        (is (or (= result {:players [{:hand [1 2 3 4 5 6 7] :deck [] :discard []}]})
                (= result {:players [{:hand [1 2 3 4 5 7 6] :deck [] :discard []}]}))))
      (let [result (draw game 0 5)]
        (is (or (= result {:players [{:hand [1 2 3 4 5 6 7] :deck [] :discard []}]})
                (= result {:players [{:hand [1 2 3 4 5 7 6] :deck [] :discard []}]})))))))

(deftest chose-test
  (testing "No/invalid choice"
    (is (thrown-with-msg? AssertionError #"Chose error: You don't have a choice to make."
                          (chose {:effect-stack []} :copper)))
    (is (thrown-with-msg? AssertionError #"Chose error: Choice has no options"
                          (chose {:effect-stack [{:player-no 0 :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)}]} :copper))))
  (testing "Optional single choice"
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]
                                   :max       1}]}
                  nil)
           {:players [{:chosen nil}]}))
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]
                                   :max       1}]}
                  :copper)
           {:players [{:chosen :copper}]}))
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]
                                   :max       1}]}
                  [])
           {:players [{:chosen nil}]}))
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]
                                   :max       1}]}
                  [:copper])
           {:players [{:chosen :copper}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: You can only pick 1 option."
                          (chose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                  :options   [:copper :copper]
                                                  :max       1}]}
                                 [:copper :copper])))
    (is (thrown-with-msg? AssertionError #"Chose error: Estate is not a valid choice."
                          (chose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                  :options   [:copper]
                                                  :max       1}]}
                                 :estate))))
  (testing "Mandatory single choice"
    (is (thrown-with-msg? AssertionError #"Chose error: You must pick an option"
                          (chose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                  :options   [:copper]
                                                  :min       1
                                                  :max       1}]}
                                 nil)))
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]
                                   :min       1
                                   :max       1}]}
                  :copper)
           {:players [{:chosen :copper}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: You must pick an option"
                          (chose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                  :options   [:copper]
                                                  :min       1
                                                  :max       1}]}
                                 [])))
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]
                                   :min       1
                                   :max       1}]}
                  [:copper])
           {:players [{:chosen :copper}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: You can only pick 1 option."
                          (chose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                  :options   [:copper :copper]
                                                  :min       1
                                                  :max       1}]}
                                 [:copper :copper]))))
  (testing "Multi choice"
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]}]}
                  nil)
           {:players [{:chosen []}]}))
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]}]}
                  :copper)
           {:players [{:chosen [:copper]}]}))
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]}]}
                  [])
           {:players [{:chosen []}]}))
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper]}]}
                  [:copper])
           {:players [{:chosen [:copper]}]}))
    (is (= (chose {:players      []
                   :effect-stack [{:player-no 0
                                   :choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                   :options   [:copper :copper]}]}
                  [:copper :copper])
           {:players [{:chosen [:copper :copper]}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: Estate is not a valid choice."
                          (chose {:effect-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                  :options   [:copper]}]}
                                 [:copper :estate :silver])))))

(deftest play-test
  (testing "Playing a card is impossible because"
    (testing "it has no/wrong type"
      (is (thrown-with-msg? AssertionError #"Play error: No Card has no type"
                            (play {:players [{:hand [{:name :no-card}]}]}
                                  0 :no-card)))
      (is (thrown-with-msg? AssertionError #"Play error: Victory cards cannot be played."
                            (play {:players [{:hand [{:name :estate :type #{:victory}}]}]}
                                  0 :estate))))
    (testing "player has no cards in hand"
      (is (thrown-with-msg? AssertionError #"Play error: There is no Copper in your Hand."
                            (play {:players [{:hand []}]}
                                  0 :copper)))))
  (testing "Playing treasure"
    (testing "is impossible because"
      (testing "card has no coin-value"
        (is (thrown-with-msg? AssertionError #"Play error: Copper has no coin value"
                              (play {:players [{:hand [{:name :copper :type #{:treasure}}]}]}
                                    0 :copper))))))
  (testing "Playing action"
    (testing "is impossible because"
      (testing "player has no more actions"
        (is (thrown-with-msg? AssertionError #"Play error: You have no more actions."
                              (play {:players [{:hand    [{:name :village :type #{:action} :action-fn #(%1)}]
                                                :actions 0}]}
                                    0 :village))))
      (testing "card has no action-fn"
        (is (thrown-with-msg? AssertionError #"Play error: Village has no action function."
                              (play {:players [{:hand    [{:name :village :type #{:action}}]
                                                :actions 1}]}
                                    0 :village)))))))

(deftest clean-up-test
  (testing "Clean up"
    (is (= (clean-up {:players [{:hand      [{:name :estate}]
                                 :play-area [{:name :silver}]
                                 :deck      (repeat 5 {:name :copper})
                                 :discard   [{:name :cellar}]}]} 0)
           {:players [{:hand      (repeat 5 {:name :copper})
                       :play-area []
                       :deck      []
                       :discard   [{:name :cellar} {:name :silver} {:name :estate}]}]}))
    (is (= (clean-up {:players [{:hand      [{:name :copper}]
                                 :play-area [{:name :copper}]
                                 :deck      [{:name :copper}]
                                 :discard   [{:name :copper}]}]} 0)
           {:players [{:hand      (repeat 4 {:name :copper})
                       :play-area []
                       :deck      []
                       :discard   []}]}))
    (is (= (clean-up {:players [{:hand      [{:name :copper}]
                                 :play-area [{:name :copper}]
                                 :deck      (repeat 3 {:name :silver})
                                 :discard   [{:name :copper}]}]} 0)
           {:players [{:hand      (concat (repeat 3 {:name :silver}) (repeat 2 {:name :copper}))
                       :play-area []
                       :deck      [{:name :copper}]
                       :discard   []}]}))
    (is (= (clean-up {:players [{:hand      []
                                 :play-area []
                                 :deck      []
                                 :discard   []
                                 :triggers  [{:some :trigger}]}]} 0)
           {:players [{:hand      []
                       :play-area []
                       :deck      []
                       :discard   []}]}))))

(deftest game-end-test
  (testing "Game ending conditions"
    (is (not (game-ended? {:supply [{:card {:name :province} :pile-size 1}]})))
    (is (game-ended? {:supply [{:card {:name :province} :pile-size 0}]}))
    (is (not (game-ended? {:supply (concat [{:card {:name :province} :pile-size 1}] (repeat 1 {:pile-size 0}))})))
    (is (not (game-ended? {:supply (concat [{:card {:name :province} :pile-size 1}] (repeat 2 {:pile-size 0}))})))
    (is (game-ended? {:supply (concat [{:card {:name :province} :pile-size 1}] (repeat 3 {:pile-size 0}))}))
    (is (game-ended? {:supply (concat [{:card {:name :province} :pile-size 1}] (repeat 4 {:pile-size 0}))}))))
