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
    (is (thrown-with-msg? AssertionError #"Shuffle error: Your deck is not empty."
                          (shuffle-discard {:deck [1] :discard [2]})))))

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

(deftest play-test
  (testing "Playing card is impossible because"
    (testing "it has no/wrong type"
      (is (thrown-with-msg? AssertionError #"Play error: Estate has no type"
                            (play {:players [{:hand [{:name :estate}]}]}
                                  0 :estate)))
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
                              (play {:players [{:hand    [village]
                                                :actions 0}]}
                                    0 :village))))
      (testing "card has no action-fn"
        (is (thrown-with-msg? AssertionError #"Play error: Village has no action function."
                              (play {:players [{:hand    [{:name :village :type #{:action}}]
                                                :actions 1}]}
                                    0 :village)))))))

(deftest treasure-test
  (testing "Copper"
    (is (= (play {:players [{:hand  [copper]
                             :coins 0}]}
                 0 :copper)
           {:players [{:hand      []
                       :play-area [copper]
                       :coins     1}]})))
  (testing "Silver"
    (is (= (play {:players [{:hand  [silver]
                             :coins 0}]}
                 0 :silver)
           {:players [{:hand      []
                       :play-area [silver]
                       :coins     2}]})))
  (testing "Gold"
    (is (= (play {:players [{:hand  [gold]
                             :coins 0}]}
                 0 :gold)
           {:players [{:hand      []
                       :play-area [gold]
                       :coins     3}]}))))

(deftest play-treasures-test
  (testing "All treasures"
    (is (= (play-treasures {:players [{:hand  [gold silver copper {:name :smithy} copper]
                                       :coins 0}]}
                           0)
           {:players [{:hand      [{:name :smithy}]
                       :play-area [gold silver copper copper]
                       :coins     7}]}))))

(deftest cellar-test
  (testing "Cellar"
    (is (= (play {:players [{:hand    [cellar {:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                             :deck    (repeat 5 {:name :copper})
                             :actions 1}]}
                 0 :cellar)
           {:players [{:hand       [{:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                       :play-area  [cellar]
                       :deck       (repeat 5 {:name :copper})
                       :actions    1
                       :play-stack [{:text      "Discard any number of cards, then draw that many."
                                     :choice-fn cellar-sift
                                     :options   [:copper :estate :estate :estate]}]}]}))
    (is (= (-> {:players [{:hand    [cellar {:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                           :deck    (repeat 5 {:name :copper})
                           :actions 1}]}
               (play 0 :cellar)
               (chose 0 [:estate :estate :estate]))
           {:players [{:hand       [{:name :copper} {:name :copper} {:name :copper} {:name :copper}]
                       :play-area  [cellar]
                       :deck       [{:name :copper} {:name :copper}]
                       :discard    [{:name :estate} {:name :estate} {:name :estate}]
                       :actions    1
                       :play-stack []}]}))
    (is (= (-> {:players [{:hand    [cellar {:name :copper}]
                           :actions 1}]}
               (play 0 :cellar)
               (chose 0 :copper))
           {:players [{:hand       [{:name :copper}]
                       :play-area  [cellar]
                       :deck       []
                       :discard    []
                       :actions    1
                       :play-stack []}]}))
    (is (= (-> {:players [{:hand    [cellar {:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                           :deck    (repeat 5 {:name :copper})
                           :actions 1}]}
               (play 0 :cellar)
               (chose 0 []))
           {:players [{:hand       [{:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                       :play-area  [cellar]
                       :deck       (repeat 5 {:name :copper})
                       :actions    1
                       :play-stack []}]}))
    (is (= (-> {:players [{:hand    [cellar]
                           :deck    (repeat 5 {:name :copper})
                           :actions 1}]}
               (play 0 :cellar))
           {:players [{:hand      []
                       :play-area [cellar]
                       :deck      (repeat 5 {:name :copper})
                       :actions   1}]}))
    (is (thrown-with-msg? AssertionError #"Move error: There is no Estate in your Hand"
                          (-> {:players [{:hand    [cellar {:name :copper} {:name :estate}]
                                          :deck    (repeat 5 {:name :copper})
                                          :actions 1}]}
                              (play 0 :cellar)
                              (chose 0 [:estate :estate]))))))

(deftest chapel-test
  (testing "Chapel"
    (is (= (play {:players [{:hand    [chapel {:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                             :actions 1}]}
                 0 :chapel)
           {:players [{:hand       [{:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                       :play-area  [chapel]
                       :actions    0
                       :play-stack [{:text      "Trash up to 4 cards from your hand."
                                     :choice-fn trash
                                     :options   [:copper :estate :estate :estate]
                                     :max       4}]}]}))
    (is (= (-> {:players [{:hand    [chapel {:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                           :actions 1}]}
               (play 0 :chapel)
               (chose 0 [:estate :estate :estate]))
           {:players [{:hand       [{:name :copper}]
                       :play-area  [chapel]
                       :actions    0
                       :play-stack []}]
            :trash   [{:name :estate} {:name :estate} {:name :estate}]}))
    (is (= (-> {:players [{:hand    [chapel {:name :copper}]
                           :actions 1}]}
               (play 0 :chapel)
               (chose 0 :copper))
           {:players [{:hand       []
                       :play-area  [chapel]
                       :actions    0
                       :play-stack []}]
            :trash   [{:name :copper}]}))
    (is (= (-> {:players [{:hand    [chapel {:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                           :actions 1}]}
               (play 0 :chapel)
               (chose 0 []))
           {:players [{:hand       [{:name :copper} {:name :estate} {:name :estate} {:name :estate}]
                       :play-area  [chapel]
                       :actions    0
                       :play-stack []}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: You can only pick 4 options."
                          (-> {:players [{:hand    (concat [chapel] (repeat 5 {:name :copper}))
                                          :actions 1}]}
                              (play 0 :chapel)
                              (chose 0 (repeat 5 :copper)))))))

(deftest council-room-test
  (testing "Council Room"
    (is (= (play {:players [{:deck    (repeat 5 {:name :copper})
                             :hand    [council-room]
                             :actions 1
                             :buys    1}
                            {:deck [{:name :copper} {:name :copper}]
                             :hand []}]}
                 0 :council-room)
           {:players [{:deck      [{:name :copper}]
                       :hand      (repeat 4 {:name :copper})
                       :play-area [council-room]
                       :actions   0
                       :buys      2}
                      {:deck [{:name :copper}]
                       :hand [{:name :copper}]}]}))))

(deftest festival-test
  (testing "Festival"
    (is (= (play {:players [{:deck    [{:name :copper}]
                             :hand    [festival]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 0 :festival)
           {:players [{:deck      [{:name :copper}]
                       :hand      []
                       :play-area [festival]
                       :actions   2
                       :coins     2
                       :buys      2}]}))))

(deftest harbinger-test
  (testing "Harbinger"
    (is (= (play {:players [{:hand    [harbinger]
                             :deck    [{:name :copper} {:name :copper} {:name :copper}]
                             :discard [{:name :gold}]
                             :actions 1}]}
                 0 :harbinger)
           {:players [{:hand       [{:name :copper}]
                       :deck       [{:name :copper} {:name :copper}]
                       :discard    [{:name :gold}]
                       :play-area  [harbinger]
                       :actions    1
                       :play-stack [{:text      "You may put a card from it onto your deck."
                                     :choice-fn harbinger-topdeck
                                     :options   [:gold]
                                     :max       1}]}]}))
    (is (= (-> {:players [{:hand    [harbinger]
                           :deck    [{:name :copper} {:name :copper} {:name :copper}]
                           :discard [{:name :gold}]
                           :actions 1}]}
               (play 0 :harbinger)
               (chose 0 :gold))
           {:players [{:hand       [{:name :copper}]
                       :deck       [{:name :gold} {:name :copper} {:name :copper}]
                       :discard    []
                       :play-area  [harbinger]
                       :actions    1
                       :play-stack []}]}))
    (is (= (-> {:players [{:hand    [harbinger]
                           :deck    [{:name :copper} {:name :copper} {:name :copper}]
                           :discard [{:name :estate}]
                           :actions 1}]}
               (play 0 :harbinger)
               (chose 0 nil))
           {:players [{:hand       [{:name :copper}]
                       :deck       [{:name :copper} {:name :copper}]
                       :discard    [{:name :estate}]
                       :play-area  [harbinger]
                       :actions    1
                       :play-stack []}]}))
    (is (= (-> {:players [{:hand    [harbinger]
                           :deck    [{:name :copper} {:name :copper} {:name :copper}]
                           :actions 1}]}
               (play 0 :harbinger))
           {:players [{:hand      [{:name :copper}]
                       :deck      [{:name :copper} {:name :copper}]
                       :play-area [harbinger]
                       :actions   1}]}))))

(deftest laboratory-test
  (testing "Laboratory"
    (is (= (play {:players [{:deck    [{:name :copper} {:name :copper} {:name :copper}]
                             :hand    [laboratory]
                             :actions 1}]}
                 0 :laboratory)
           {:players [{:deck      [{:name :copper}]
                       :hand      [{:name :copper} {:name :copper}]
                       :play-area [laboratory]
                       :actions   1}]}))))

(deftest market-test
  (testing "Market"
    (is (= (play {:players [{:deck    [{:name :copper} {:name :copper} {:name :copper}]
                             :hand    [market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 0 :market)
           {:players [{:deck      [{:name :copper} {:name :copper}]
                       :hand      [{:name :copper}]
                       :play-area [market]
                       :actions   1
                       :coins     1
                       :buys      2}]}))))

(deftest merchant-test
  (testing "Merchant"
    (is (= (-> {:players [{:deck    [copper copper]
                           :hand    [merchant]
                           :actions 1
                           :coins   0}]}
               (play 0 :merchant)
               (play 0 :copper))
           {:players [{:deck      [copper]
                       :hand      []
                       :play-area [merchant copper]
                       :triggers  [merchant-trigger]
                       :actions   1
                       :coins     1}]}))
    (is (= (-> {:players [{:deck    [silver copper]
                           :hand    [merchant]
                           :actions 1
                           :coins   0}]}
               (play 0 :merchant)
               (play 0 :silver))
           {:players [{:deck      [copper]
                       :hand      []
                       :play-area [merchant silver]
                       :triggers  []
                       :actions   1
                       :coins     3}]}))
    (is (= (-> {:players [{:deck    [silver copper]
                           :hand    [silver merchant]
                           :actions 1
                           :coins   0}]}
               (play 0 :merchant)
               (play 0 :silver)
               (play 0 :silver))
           {:players [{:deck      [copper]
                       :hand      []
                       :play-area [merchant silver silver]
                       :triggers  []
                       :actions   1
                       :coins     5}]}))
    (is (= (-> {:players [{:deck    [silver copper]
                           :hand    [merchant merchant]
                           :actions 1
                           :coins   0}]}
               (play 0 :merchant)
               (play 0 :merchant)
               (play 0 :silver))
           {:players [{:deck      []
                       :hand      [copper]
                       :play-area [merchant merchant silver]
                       :triggers  []
                       :actions   1
                       :coins     4}]}))))

(deftest mine-test
  (testing "Mine"
    (is (= (play {:players [{:hand    [mine copper estate]
                             :actions 1}]}
                 0 :mine)
           {:players [{:hand       [copper estate]
                       :play-area  [mine]
                       :actions    0
                       :play-stack [{:text      "You may trash a Treasure from your hand."
                                     :choice-fn mine-trash
                                     :options   [:copper]
                                     :min       1
                                     :max       1}]}]}))
    (is (= (play {:players [{:hand    [mine estate]
                             :actions 1}]}
                 0 :mine)
           {:players [{:hand      [estate]
                       :play-area [mine]
                       :actions   0}]}))
    (is (= (-> {:supply  (base-supply 2 8)
                :players [{:hand    [mine copper estate]
                           :actions 1}]}
               (play 0 :mine)
               (chose 0 :copper))
           {:supply  (base-supply 2 8)
            :players [{:hand       [estate]
                       :play-area  [mine]
                       :actions    0
                       :play-stack [{:text      "Gain a Treasure to your hand costing up to $3."
                                     :choice-fn gain-to-hand
                                     :options   [:copper :silver]
                                     :min       1
                                     :max       1}]}]
            :trash   [copper]}))
    (is (= (-> {:supply  [{:card copper :pile-size 0} {:card gold :pile-size 30}]
                :players [{:hand    [mine copper estate]
                           :actions 1}]}
               (play 0 :mine)
               (chose 0 :copper))
           {:supply  [{:card copper :pile-size 0} {:card gold :pile-size 30}]
            :players [{:hand       [estate]
                       :play-area  [mine]
                       :actions    0
                       :play-stack []}]
            :trash   [copper]}))
    (is (= (-> {:supply  [{:card gold :pile-size 30}]
                :players [{:hand    [mine silver estate]
                           :actions 1}]}
               (play 0 :mine)
               (chose 0 :silver)
               (chose 0 :gold))
           {:supply  [{:card gold :pile-size 29}]
            :players [{:hand       [estate gold]
                       :play-area  [mine]
                       :actions    0
                       :play-stack []}]
            :trash   [silver]}))))

(deftest moat-test
  (testing "Moat"
    (is (= (play {:players [{:deck    [{:name :copper} {:name :copper} {:name :copper}]
                             :hand    [moat]
                             :actions 1}]}
                 0 :moat)
           {:players [{:deck      [{:name :copper}]
                       :hand      [{:name :copper} {:name :copper}]
                       :play-area [moat]
                       :actions   0}]}))
    (is (= (play {:players [{:deck    [{:name :copper}]
                             :hand    [moat]
                             :actions 1}]}
                 0 :moat)
           {:players [{:deck      []
                       :hand      [{:name :copper}]
                       :play-area [moat]
                       :actions   0}]}))))

(deftest moneylender-test
  (testing "Moneylender"
    (is (= (play {:players [{:hand    [moneylender {:name :copper} {:name :copper} {:name :estate}]
                             :actions 1}]}
                 0 :moneylender)
           {:players [{:hand       [{:name :copper} {:name :copper} {:name :estate}]
                       :play-area  [moneylender]
                       :actions    0
                       :play-stack [{:text      "You may trash a Copper from your hand for +$3"
                                     :choice-fn moneylender-trash
                                     :options   [:copper :copper]
                                     :max       1}]}]}))
    (is (= (-> {:players [{:hand    [moneylender {:name :copper} {:name :copper} {:name :estate}]
                           :actions 1
                           :coins   0}]}
               (play 0 :moneylender)
               (chose 0 :copper))
           {:players [{:hand       [{:name :copper} {:name :estate}]
                       :play-area  [moneylender]
                       :actions    0
                       :coins      3
                       :play-stack []}]
            :trash   [{:name :copper}]}))
    (is (= (-> {:players [{:hand    [moneylender {:name :copper} {:name :copper} {:name :estate}]
                           :actions 1}]}
               (play 0 :moneylender)
               (chose 0 nil))
           {:players [{:hand       [{:name :copper} {:name :copper} {:name :estate}]
                       :play-area  [moneylender]
                       :actions    0
                       :play-stack []}]}))
    (is (= (-> {:players [{:hand    [moneylender {:name :estate}]
                           :actions 1}]}
               (play 0 :moneylender))
           {:players [{:hand      [{:name :estate}]
                       :play-area [moneylender]
                       :actions   0}]}))))

(deftest smithy-test
  (testing "Smithy"
    (is (= (play {:players [{:deck    [{:name :copper} {:name :copper} {:name :copper}]
                             :hand    [smithy]
                             :actions 1}]}
                 0 :smithy)
           {:players [{:deck      []
                       :hand      [{:name :copper} {:name :copper} {:name :copper}]
                       :play-area [smithy]
                       :actions   0}]}))))

(deftest poacher-test
  (testing "Poacher"
    (is (= (play {:players [{:deck    [copper copper]
                             :hand    [poacher estate]
                             :actions 1}]}
                 0 :poacher)
           {:players [{:deck      [copper]
                       :hand      [estate copper]
                       :play-area [poacher]
                       :actions   1}]}))
    (is (= (play {:supply  [{:pile-size 0} {:pile-size 1}]
                  :players [{:deck    [copper copper]
                             :hand    [poacher estate]
                             :actions 1}]}
                 0 :poacher)
           {:supply  [{:pile-size 0} {:pile-size 1}]
            :players [{:deck       [copper]
                       :hand       [estate copper]
                       :play-area  [poacher]
                       :actions    1
                       :play-stack [{:text      "Discard a card per empty supply pile [1]."
                                     :choice-fn discard-cards
                                     :options   [:estate :copper]
                                     :min       1
                                     :max       1}]}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 1}]
                :players [{:deck    [copper copper]
                           :hand    [poacher estate]
                           :actions 1}]}
               (play 0 :poacher)
               (chose 0 [:estate]))
           {:supply  [{:pile-size 0} {:pile-size 1}]
            :players [{:deck       [copper]
                       :discard    [estate]
                       :hand       [copper]
                       :play-area  [poacher]
                       :actions    1
                       :play-stack []}]}))
    (is (= (play {:supply  [{:pile-size 0} {:pile-size 0}]
                  :players [{:deck    [copper copper]
                             :hand    [poacher estate silver]
                             :actions 1}]}
                 0 :poacher)
           {:supply  [{:pile-size 0} {:pile-size 0}]
            :players [{:deck       [copper]
                       :hand       [estate silver copper]
                       :play-area  [poacher]
                       :actions    1
                       :play-stack [{:text      "Discard a card per empty supply pile [2]."
                                     :choice-fn discard-cards
                                     :options   [:estate :silver :copper]
                                     :min       2
                                     :max       2}]}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 0}]
                :players [{:deck    [copper copper]
                           :hand    [poacher estate silver]
                           :actions 1}]}
               (play 0 :poacher)
               (chose 0 [:copper :estate]))
           {:supply  [{:pile-size 0} {:pile-size 0}]
            :players [{:hand       [silver]
                       :deck       [copper]
                       :discard    [copper estate]
                       :play-area  [poacher]
                       :actions    1
                       :play-stack []}]}))
    (is (= (play {:supply  [{:pile-size 0}]
                  :players [{:deck    []
                             :hand    [poacher]
                             :actions 1}]}
                 0 :poacher)
           {:supply  [{:pile-size 0}]
            :players [{:deck      []
                       :hand      []
                       :play-area [poacher]
                       :actions   1}]}))
    (is (= (play {:supply  [{:pile-size 0} {:pile-size 0}]
                  :players [{:deck    [copper copper]
                             :hand    [poacher]
                             :actions 1}]}
                 0 :poacher)
           {:supply  [{:pile-size 0} {:pile-size 0}]
            :players [{:deck       [copper]
                       :hand       [copper]
                       :play-area  [poacher]
                       :actions    1
                       :play-stack [{:text      "Discard a card per empty supply pile [2]."
                                     :choice-fn discard-cards
                                     :options   [:copper]
                                     :min       1
                                     :max       1}]}]}))))

(deftest remodel-test
  (testing "Remodel"
    (is (= (play {:players [{:hand    [remodel copper estate]
                             :actions 1}]}
                 0 :remodel)
           {:players [{:hand       [copper estate]
                       :play-area  [remodel]
                       :actions    0
                       :play-stack [{:text      "Trash a card from your hand."
                                     :choice-fn remodel-trash
                                     :options   [:copper :estate]
                                     :min       1
                                     :max       1}]}]}))
    (is (= (play {:players [{:hand    [remodel]
                             :actions 1}]}
                 0 :remodel)
           {:players [{:hand      []
                       :play-area [remodel]
                       :actions   0}]}))
    (is (= (-> {:supply  (base-supply 2 8)
                :players [{:hand    [remodel copper estate]
                           :actions 1}]}
               (play 0 :remodel)
               (chose 0 :estate))
           {:supply  (base-supply 2 8)
            :players [{:hand       [copper]
                       :play-area  [remodel]
                       :actions    0
                       :play-stack [{:text      "Gain a card costing up to $4."
                                     :choice-fn gain
                                     :options   [:curse :estate :copper :silver]
                                     :min       1
                                     :max       1}]}]
            :trash   [estate]}))
    (is (= (-> {:supply  [{:card estate :pile-size 0} {:card silver :pile-size 40}]
                :players [{:hand    [remodel copper estate]
                           :actions 1}]}
               (play 0 :remodel)
               (chose 0 :copper))
           {:supply  [{:card estate :pile-size 0} {:card silver :pile-size 40}]
            :players [{:hand       [estate]
                       :play-area  [remodel]
                       :actions    0
                       :play-stack []}]
            :trash   [copper]}))
    (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                :players [{:hand    [remodel silver estate]
                           :actions 1}]}
               (play 0 :remodel)
               (chose 0 :silver)
               (chose 0 :duchy))
           {:supply  [{:card duchy :pile-size 7}]
            :players [{:hand       [estate]
                       :play-area  [remodel]
                       :discard    [duchy]
                       :actions    0
                       :play-stack []}]
            :trash   [silver]}))))

(deftest throne-room-test
  (testing "Throne Room"
    (is (= (play {:players [{:deck    [{:name :copper} {:name :copper} {:name :copper}]
                             :hand    [throne-room market {:name :copper}]
                             :actions 1}]}
                 0 :throne-room)
           {:players [{:deck       [{:name :copper} {:name :copper} {:name :copper}]
                       :hand       [market {:name :copper}]
                       :play-area  [throne-room]
                       :actions    0
                       :play-stack [{:text      "You may play an Action card from your hand twice."
                                     :choice-fn play-action-twice
                                     :options   [:market]
                                     :max       1}]}]}))
    (is (= (-> {:players [{:deck    [{:name :copper} {:name :copper} {:name :copper}]
                           :hand    [throne-room market {:name :copper}]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :throne-room)
               (chose 0 :market))
           {:players [{:deck       [{:name :copper}]
                       :hand       [{:name :copper} {:name :copper} {:name :copper}]
                       :play-area  [throne-room market]
                       :actions    2
                       :coins      2
                       :buys       3
                       :play-stack []}]}))
    (is (= (-> {:players [{:deck    [{:name :copper} {:name :copper} {:name :copper}]
                           :hand    [throne-room market {:name :copper}]
                           :actions 1}]}
               (play 0 :throne-room)
               (chose 0 nil))
           {:players [{:deck       [{:name :copper} {:name :copper} {:name :copper}]
                       :hand       [market {:name :copper}]
                       :play-area  [throne-room]
                       :actions    0
                       :play-stack []}]}))
    (is (= (-> {:players [{:deck    [witch copper copper silver]
                           :hand    [throne-room throne-room merchant]
                           :actions 1}]}
               (play 0 :throne-room)
               (chose 0 :throne-room))
           {:players [{:deck       [witch copper copper silver]
                       :hand       [merchant]
                       :play-area  [throne-room throne-room]
                       :actions    0
                       :play-stack [{:text      "You may play an Action card from your hand twice."
                                     :choice-fn play-action-twice
                                     :options   [:merchant]
                                     :max       1}
                                    throne-room]}]}))
    (is (= (-> {:players [{:deck    [witch copper copper silver]
                           :hand    [throne-room throne-room merchant]
                           :actions 1}]}
               (play 0 :throne-room)
               (chose 0 :throne-room)
               (chose 0 :merchant))
           {:players [{:deck       [copper silver]
                       :hand       [witch copper]
                       :play-area  [throne-room throne-room merchant]
                       :triggers   [merchant-trigger merchant-trigger]
                       :play-stack [{:text      "You may play an Action card from your hand twice."
                                     :choice-fn play-action-twice
                                     :options   [:witch]
                                     :max       1}]
                       :actions    2}]}))
    (is (= (-> {:supply  [{:card curse :pile-size 10}]
                :players [{:deck    [witch copper copper silver]
                           :hand    [throne-room throne-room merchant]
                           :actions 1
                           :coins   0}
                          {}]}
               (play 0 :throne-room)
               (chose 0 :throne-room)
               (chose 0 :merchant)
               (chose 0 :witch)
               (play-treasures 0))
           {:supply  [{:card curse :pile-size 8}]
            :players [{:deck       []
                       :hand       []
                       :play-area  [throne-room throne-room merchant witch copper copper silver]
                       :play-stack []
                       :triggers   []
                       :actions    2
                       :coins      6}
                      {:discard [curse curse]}]}))))

(deftest vassal-test
  (testing "Vassal"
    (is (= (play {:players [{:hand    [vassal]
                             :deck    [copper]
                             :actions 1
                             :coins   0}]}
                 0 :vassal)
           {:players [{:hand      []
                       :play-area [vassal]
                       :deck      []
                       :discard   [copper]
                       :actions   0
                       :coins     2}]}))
    (is (= (play {:players [{:hand    [vassal]
                             :deck    []
                             :actions 1
                             :coins   0}]}
                 0 :vassal)
           {:players [{:hand      []
                       :play-area [vassal]
                       :deck      []
                       :actions   0
                       :coins     2}]}))
    (is (= (play {:players [{:hand    [vassal]
                             :deck    [market copper]
                             :discard [market copper]
                             :actions 1
                             :coins   0}]}
                 0 :vassal)
           {:players [{:hand       []
                       :play-area  [vassal]
                       :play-stack [{:text      "Discard the top card of your deck. If it is an Action card, you may play it."
                                     :choice-fn play-discard-action
                                     :options   [:market]
                                     :max       1}]
                       :deck       [copper]
                       :discard    [market copper market]
                       :actions    0
                       :coins      2}]}))
    (is (= (-> {:players [{:hand    [vassal]
                           :deck    [market copper]
                           :discard [market copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :vassal)
               (chose 0 :market))
           {:players [{:hand       [copper]
                       :play-area  [vassal market]
                       :play-stack []
                       :deck       []
                       :discard    [market copper]
                       :actions    1
                       :coins      3
                       :buys       2}]}))
    (is (= (-> {:players [{:hand    [vassal]
                           :deck    [market copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :vassal)
               (chose 0 nil))
           {:players [{:hand       []
                       :play-area  [vassal]
                       :play-stack []
                       :deck       [copper]
                       :discard    [market]
                       :actions    0
                       :coins      2}]}))))

(deftest village-test
  (testing "Village"
    (is (= (play {:players [{:deck    [{:name :copper} {:name :copper}]
                             :hand    [village]
                             :actions 1}]}
                 0 :village)
           {:players [{:deck      [{:name :copper}]
                       :hand      [{:name :copper}]
                       :play-area [village]
                       :actions   2}]}))))
(deftest witch-test
  (testing "Witch"
    (is (= (play {:supply  [{:card curse :pile-size 10}]
                  :players [{:deck    (repeat 3 {:name :copper})
                             :hand    [witch]
                             :actions 1}
                            {:discard [{:name :copper} {:name :copper}]}]}
                 0 :witch)
           {:supply  [{:card curse :pile-size 9}]
            :players [{:deck      [{:name :copper}]
                       :hand      [{:name :copper} {:name :copper}]
                       :play-area [witch]
                       :actions   0}
                      {:discard [{:name :copper} {:name :copper} curse]}]}))))

(deftest woodcutter-test
  (testing "Woodcutter"
    (is (= (play {:players [{:deck    [{:name :copper}]
                             :hand    [woodcutter]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 0 :woodcutter)
           {:players [{:deck      [{:name :copper}]
                       :hand      []
                       :play-area [woodcutter]
                       :actions   0
                       :coins     2
                       :buys      2}]}))))
(deftest workshop-test
  (testing "Workshop"
    (is (= (play {:supply  (base-supply 2 8)
                  :players [{:hand    [workshop copper]
                             :actions 1}]}
                 0 :workshop)
           {:supply  (base-supply 2 8)
            :players [{:hand       [copper]
                       :play-area  [workshop]
                       :actions    0
                       :play-stack [{:text "Gain a card costing up to $4."
                                     :choice-fn gain
                                     :options   [:curse :estate :copper :silver]
                                     :min       1
                                     :max       1}]}]}))
    (is (= (-> {:supply  [{:card silver :pile-size 40}]
                :players [{:hand    [workshop copper]
                           :actions 1}]}
               (play 0 :workshop)
               (chose 0 :silver))
           {:supply  [{:card silver :pile-size 39}]
            :players [{:hand       [copper]
                       :discard    [silver]
                       :play-area  [workshop]
                       :actions    0
                       :play-stack []}]}))
    (is (= (-> {:supply  [{:card province :pile-size 8}]
                :players [{:hand    [workshop copper]
                           :actions 1}]}
               (play 0 :workshop))
           {:supply  [{:card province :pile-size 8}]
            :players [{:hand      [copper]
                       :play-area [workshop]
                       :actions   0}]}))))

(deftest chose-test
  (testing "No/invalid choice"
    (is (thrown-with-msg? AssertionError #"Chose error: You don't have a choice to make."
                          (chose {:players [{}]} 0 :copper)))
    (is (thrown-with-msg? AssertionError #"Chose error: Choice has no options"
                          (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)}]}]} 0 :copper))))
  (testing "Optional single choice"
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]
                                            :max       1}]}]}
                  0 nil)
           {:players [{:chosen nil :play-stack []}]}))
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]
                                            :max       1}]}]}
                  0 :copper)
           {:players [{:chosen :copper :play-stack []}]}))
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]
                                            :max       1}]}]}
                  0 [])
           {:players [{:chosen nil :play-stack []}]}))
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]
                                            :max       1}]}]}
                  0 [:copper])
           {:players [{:chosen :copper :play-stack []}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: You can only pick 1 option."
                          (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                           :options   [:copper :copper]
                                                           :max       1}]}]}
                                 0 [:copper :copper])))
    (is (thrown-with-msg? AssertionError #"Chose error: Estate is not a valid choice."
                          (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                           :options   [:copper]
                                                           :max       1}]}]}
                                 0 :estate))))
  (testing "Mandatory single choice"
    (is (thrown-with-msg? AssertionError #"Chose error: You must pick an option"
                          (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                           :options   [:copper]
                                                           :min       1
                                                           :max       1}]}]}
                                 0 nil)))
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]
                                            :min       1
                                            :max       1}]}]}
                  0 :copper)
           {:players [{:chosen :copper :play-stack []}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: You must pick an option"
                          (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                           :options   [:copper]
                                                           :min       1
                                                           :max       1}]}]}
                                 0 [])))
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]
                                            :min       1
                                            :max       1}]}]}
                  0 [:copper])
           {:players [{:chosen :copper :play-stack []}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: You can only pick 1 option."
                          (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                           :options   [:copper :copper]
                                                           :min       1
                                                           :max       1}]}]}
                                 0 [:copper :copper]))))
  (testing "Multi choice"
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]}]}]}
                  0 nil)
           {:players [{:chosen [] :play-stack []}]}))
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]}]}]}
                  0 :copper)
           {:players [{:chosen [:copper] :play-stack []}]}))
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]}]}]}
                  0 [])
           {:players [{:chosen [] :play-stack []}]}))
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper]}]}]}
                  0 [:copper])
           {:players [{:chosen [:copper] :play-stack []}]}))
    (is (= (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                            :options   [:copper :copper]}]}]}
                  0 [:copper :copper])
           {:players [{:chosen [:copper :copper] :play-stack []}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: Estate is not a valid choice."
                          (chose {:players [{:play-stack [{:choice-fn #(assoc-in %1 [:players %2 :chosen] %3)
                                                           :options   [:copper]}]}]}
                                 0 [:copper :estate :silver])))))

(deftest buy-test
  (testing "Buying a card"
    (testing "is impossible because"
      (testing "player has no buys left"
        (is (thrown-with-msg? AssertionError #"Buy error: You have no more buys."
                              (buy-card {:supply  [{:card {:name :copper :cost 0} :pile-size 40}]
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
            :discard   []}))
    (is (= (clean-up {:hand      []
                      :play-area []
                      :deck      []
                      :discard   []
                      :triggers  [merchant-trigger]})
           {:hand      []
            :play-area []
            :deck      []
            :discard   []}))))

(deftest game-end-test
  (testing "Game ending conditions"
    (is (not (game-ended? {:supply [{:card province :pile-size 1}]})))
    (is (game-ended? {:supply [{:card province :pile-size 0}]}))
    (is (not (game-ended? {:supply (concat [{:card province :pile-size 1}] (repeat 1 {:pile-size 0}))})))
    (is (not (game-ended? {:supply (concat [{:card province :pile-size 1}] (repeat 2 {:pile-size 0}))})))
    (is (game-ended? {:supply (concat [{:card province :pile-size 1}] (repeat 3 {:pile-size 0}))}))
    (is (game-ended? {:supply (concat [{:card province :pile-size 1}] (repeat 4 {:pile-size 0}))}))))

(deftest view-test
  (testing "View game"
    (is (= (view-game {:supply         (base-supply 2 8)
                       :players        [{:name      :dombot
                                         :hand      [{:name :copper} {:name :copper} {:name :copper} estate estate]
                                         :play-area []
                                         :deck      [{:name :copper} {:name :copper} {:name :copper} {:name :copper} estate]
                                         :discard   []}]
                       :trash          [{:name :estate} {:name :estate} {:name :copper}]
                       :current-player 0})
           {:supply         [{:card :curse :price 0 :count 10}
                             {:card :copper :price 0 :count 46}
                             {:card :silver :price 3 :count 40}
                             {:card :gold :price 6 :count 30}
                             {:card :estate :price 2 :count 8}
                             {:card :duchy :price 5 :count 8}
                             {:card :province :price 8 :count 8}]
            :player         {:name           :dombot
                             :hand           {:copper 3 :estate 2}
                             :play-area      {}
                             :deck           5
                             :discard        :empty
                             :victory-points 3}
            :trash          {:copper 1 :estate 2}
            :current-player :dombot})))
  (testing "View game end"
    (is (= (view-game {:supply         [{:card province :pile-size 0}]
                       :players        [{:name      :dombot
                                         :hand      [{:name :copper} {:name :copper} {:name :copper} estate estate]
                                         :play-area []
                                         :deck      [{:name :copper} {:name :copper} {:name :copper} {:name :copper} estate]
                                         :discard   []}]
                       :current-player 0})
           {:players [{:name           :dombot
                       :cards          {:copper 7 :estate 3}
                       :victory-points 3}]}))))
