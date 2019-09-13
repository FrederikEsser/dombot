(ns dombot.cards.prosperity-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.prosperity :as prosperity :refer :all]
            [dombot.cards.dominion :refer [market]]
            [dombot.cards.seaside :refer [sea-hag]]
            [dombot.cards.renaissance :as renaissance :refer [improve]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest bank-test
  (testing "Bank"
    (is (= (-> {:players [{:hand  [bank]
                           :coins 0}]}
               (play 0 :bank))
           {:players [{:play-area [bank]
                       :coins     1}]}))
    (is (= (-> {:players [{:hand      [bank]
                           :play-area [copper]
                           :coins     1}]}
               (play 0 :bank))
           {:players [{:play-area [copper bank]
                       :coins     3}]}))))

(deftest bishop-test
  (testing "Bishop"
    (is (= (-> {:players [{:hand    [bishop estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :bishop))
           {:players      [{:hand      [estate]
                            :play-area [bishop]
                            :actions   0
                            :coins     1
                            :vp-tokens 1}]
            :effect-stack [{:text      "Trash a card from your hand."
                            :player-no 0
                            :choice    ::prosperity/bishop-trash
                            :source    :hand
                            :options   [:estate]
                            :min       1
                            :max       1}
                           {:player-no 0
                            :effect    [:other-players {:effects [[:give-choice {:text    "You may trash a card from your hand."
                                                                                 :choice  :trash-from-hand
                                                                                 :options [:player :hand]
                                                                                 :max     1}]]}]}]}))
    (is (= (-> {:players [{:hand    [bishop estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :bishop)
               (choose :estate))
           {:players [{:play-area [bishop]
                       :actions   0
                       :coins     1
                       :vp-tokens 2}]
            :trash   [estate]}))
    (is (= (-> {:players [{:hand    [bishop]
                           :actions 1
                           :coins   0}]}
               (play 0 :bishop))
           {:players [{:play-area [bishop]
                       :actions   0
                       :coins     1
                       :vp-tokens 1}]}))
    (is (= (-> {:players [{:hand    [bishop copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :bishop)
               (choose :copper))
           {:players [{:play-area [bishop]
                       :actions   0
                       :coins     1
                       :vp-tokens 1}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [bishop silver]
                           :actions 1
                           :coins   0}]}
               (play 0 :bishop)
               (choose :silver))
           {:players [{:play-area [bishop]
                       :actions   0
                       :coins     1
                       :vp-tokens 2}]
            :trash   [silver]}))
    (is (= (-> {:players [{:hand    [bishop]
                           :actions 1
                           :coins   0}
                          {:hand [estate]}]}
               (play 0 :bishop))
           {:players      [{:play-area [bishop]
                            :actions   0
                            :coins     1
                            :vp-tokens 1}
                           {:hand [estate]}]
            :effect-stack [{:text      "You may trash a card from your hand."
                            :player-no 1
                            :choice    :trash-from-hand
                            :source    :hand
                            :options   [:estate]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [bishop]
                           :actions 1
                           :coins   0}
                          {:hand [estate]}]}
               (play 0 :bishop)
               (choose :estate))
           {:players [{:play-area [bishop]
                       :actions   0
                       :coins     1
                       :vp-tokens 1}
                      {}]
            :trash   [estate]}))))

(deftest city-test
  (testing "City"
    (is (= (-> {:players [{:hand    [city]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :city))
           {:players [{:hand      [copper]
                       :play-area [city]
                       :deck      [copper copper]
                       :actions   2}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 1}]
                :players [{:hand    [city]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :city))
           {:supply  [{:pile-size 0} {:pile-size 1}]
            :players [{:hand      [copper copper]
                       :play-area [city]
                       :deck      [copper]
                       :actions   2}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 0} {:pile-size 1}]
                :players [{:hand    [city]
                           :deck    [copper copper copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :city))
           {:supply  [{:pile-size 0} {:pile-size 0} {:pile-size 1}]
            :players [{:hand      [copper copper]
                       :play-area [city]
                       :deck      [copper]
                       :actions   2
                       :coins     1
                       :buys      2}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 0} {:pile-size 0}]
                :players [{:hand    [city]
                           :deck    [copper copper copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :city))
           {:supply  [{:pile-size 0} {:pile-size 0} {:pile-size 0}]
            :players [{:hand      [copper copper]
                       :play-area [city]
                       :deck      [copper]
                       :actions   2
                       :coins     1
                       :buys      2}]}))))

(deftest contraband-test
  (testing "Contraband"
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand  [contraband]
                           :coins 0
                           :buys  1}
                          {}
                          {}]}
               (play 0 :contraband))
           {:supply       (base/supply 2 8)
            :players      [{:play-area [contraband]
                            :coins     3
                            :buys      2}
                           {}
                           {}]
            :effect-stack [{:text      "Name a card that can't be bought this turn."
                            :player-no 1
                            :choice    :mark-unbuyable
                            :source    :supply
                            :options   [:curse :estate :duchy :province :copper :silver :gold]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand  [contraband]
                           :coins 0
                           :buys  1}
                          {}
                          {}]}
               (play 0 :contraband)
               (choose :gold))
           {:supply          (base/supply 2 8)
            :players         [{:play-area [contraband]
                               :coins     3
                               :buys      2}
                              {}
                              {}]
            :unbuyable-cards #{:gold}}))
    (is (thrown-with-msg? AssertionError #"Gold can't be bought."
                          (-> {:supply          (base/supply 2 8)
                               :players         [{:play-area [contraband gold]
                                                  :coins     6
                                                  :buys      2}]
                               :unbuyable-cards #{:gold}}
                              (buy-card 0 :gold))))
    (let [silver (assoc silver :id 0)]
      (is (= (-> {:supply          [{:card silver :pile-size 40}
                                    {:card gold :pile-size 30}]
                  :players         [{:play-area [contraband gold]
                                     :coins     6
                                     :buys      2}]
                  :unbuyable-cards #{:gold}}
                 (buy-card 0 :silver))
             {:supply          [{:card silver :pile-size 39}
                                {:card gold :pile-size 30}]
              :players         [{:play-area [contraband gold]
                                 :discard   [silver]
                                 :coins     3
                                 :buys      1}]
              :unbuyable-cards #{:gold}})))
    (is (= (-> {:players         [{:play-area [contraband gold]
                                   :deck      (repeat 5 copper)
                                   :phase     :action}]
                :unbuyable-cards #{:gold}}
               (clean-up {:player-no 0}))
           {:players [{:hand    (repeat 5 copper)
                       :discard [contraband gold]
                       :actions 0
                       :coins   0
                       :buys    0
                       :phase   :out-of-turn}]}))))

(deftest counting-house-test
  (testing "Counting House"
    (is (= (-> {:players [{:hand    [counting-house]
                           :discard [estate copper copper]
                           :actions 1}]}
               (play 0 :counting-house))
           {:players      [{:play-area      [counting-house]
                            :discard        [estate copper copper]
                            :revealed-cards {:discard 3}
                            :actions        0}]
            :effect-stack [{:text      "Put any number of Coppers from your discard pile into your hand."
                            :player-no 0
                            :choice    :take-from-discard
                            :source    :discard
                            :options   [:copper :copper]}]}))
    (is (= (-> {:players [{:hand    [counting-house]
                           :discard [estate copper copper]
                           :actions 1}]}
               (play 0 :counting-house)
               (choose nil))
           {:players [{:play-area      [counting-house]
                       :discard        [estate copper copper]
                       :revealed-cards {:discard 3}
                       :actions        0}]}))
    (is (= (-> {:players [{:hand    [counting-house]
                           :discard [estate copper copper]
                           :actions 1}]}
               (play 0 :counting-house)
               (choose :copper))
           {:players [{:hand      [copper]
                       :play-area [counting-house]
                       :discard   [estate copper]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [counting-house]
                           :discard [estate copper copper]
                           :actions 1}]}
               (play 0 :counting-house)
               (choose [:copper :copper]))
           {:players [{:hand      [copper copper]
                       :play-area [counting-house]
                       :discard   [estate]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [counting-house]
                           :discard [estate estate estate]
                           :actions 1}]}
               (play 0 :counting-house))
           {:players [{:play-area      [counting-house]
                       :discard        [estate estate estate]
                       :revealed-cards {:discard 3}
                       :actions        0}]}))))

(deftest expand-test
  (let [duchy (assoc duchy :id 1)]
    (testing "Expand"
      (is (= (-> {:players [{:hand    [expand copper estate]
                             :actions 1}]}
                 (play 0 :expand))
             {:players      [{:hand      [copper estate]
                              :play-area [expand]
                              :actions   0}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :choice    [:trash-and-gain {:extra-cost 3}]
                              :source    :hand
                              :options   [:copper :estate]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [expand]
                             :actions 1}]}
                 (play 0 :expand))
             {:players [{:play-area [expand]
                         :actions   0}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [expand copper estate]
                             :actions 1}]}
                 (play 0 :expand)
                 (choose :estate))
             {:supply       (base/supply 2 8)
              :players      [{:hand      [copper]
                              :play-area [expand]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing up to $5."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:curse :estate :duchy :copper :silver]
                              :min       1
                              :max       1}]
              :trash        [estate]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [expand silver estate]
                             :actions 1}]}
                 (play 0 :expand)
                 (choose :estate)
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:hand      [silver]
                         :play-area [expand]
                         :discard   [duchy]
                         :actions   0}]
              :trash   [estate]})))))

(deftest forge-test
  (testing "Forge"
    (is (= (-> {:players [{:hand    [forge copper estate estate estate copper]
                           :actions 1}]}
               (play 0 :forge))
           {:players      [{:hand      [copper estate estate estate copper]
                            :play-area [forge]
                            :actions   0}]
            :effect-stack [{:text      "Trash any number of cards from your hand."
                            :player-no 0
                            :choice    ::prosperity/forge-trash
                            :source    :hand
                            :options   [:copper :estate :estate :estate :copper]}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [forge copper estate estate estate copper]
                           :actions 1}]}
               (play 0 :forge)
               (choose nil))
           {:supply       (base/supply 2 8)
            :players      [{:hand      [copper estate estate estate copper]
                            :play-area [forge]
                            :actions   0}]
            :effect-stack [{:text      "Gain a card costing exactly 0."
                            :player-no 0
                            :choice    :gain
                            :source    :supply
                            :options   [:curse :copper]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [forge copper estate estate estate copper]
                           :actions 1}]}
               (play 0 :forge)
               (choose [:copper :copper]))
           {:supply       (base/supply 2 8)
            :players      [{:hand      [estate estate estate]
                            :play-area [forge]
                            :actions   0}]
            :effect-stack [{:text      "Gain a card costing exactly 0."
                            :player-no 0
                            :choice    :gain
                            :source    :supply
                            :options   [:curse :copper]
                            :min       1
                            :max       1}]
            :trash        [copper copper]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [forge copper estate estate estate copper]
                           :actions 1}]}
               (play 0 :forge)
               (choose [:copper :copper :estate]))
           {:supply       (base/supply 2 8)
            :players      [{:hand      [estate estate]
                            :play-area [forge]
                            :actions   0}]
            :effect-stack [{:text      "Gain a card costing exactly 2."
                            :player-no 0
                            :choice    :gain
                            :source    :supply
                            :options   [:estate]
                            :min       1
                            :max       1}]
            :trash        [copper copper estate]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [forge copper estate estate estate copper]
                           :actions 1}]}
               (play 0 :forge)
               (choose [:copper :copper :estate :estate]))
           {:supply  (base/supply 2 8)
            :players [{:hand      [estate]
                       :play-area [forge]
                       :actions   0}]
            :trash   [copper copper estate estate]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [forge copper estate estate estate copper]
                           :actions 1}]}
               (play 0 :forge)
               (choose [:copper :copper :estate :estate :estate]))
           {:supply       (base/supply 2 8)
            :players      [{:play-area [forge]
                            :actions   0}]
            :effect-stack [{:text      "Gain a card costing exactly 6."
                            :player-no 0
                            :choice    :gain
                            :source    :supply
                            :options   [:gold]
                            :min       1
                            :max       1}]
            :trash        [copper copper estate estate estate]}))))

(deftest goons-test
  (let [estate (assoc estate :id 0)
        copper (assoc copper :id 1)]
    (testing "Goons"
      (is (= (-> {:players [{:hand    [goons]
                             :actions 1
                             :coins   0
                             :buys    1}
                            {:hand (repeat 5 copper)}]}
                 (play 0 :goons))
             {:players      [{:play-area [goons]
                              :actions   0
                              :coins     2
                              :buys      2}
                             {:hand (repeat 5 copper)}]
              :effect-stack [{:text      "Discard down to 3 cards in hand."
                              :player-no 1
                              :choice    :discard-from-hand
                              :source    :hand
                              :options   (repeat 5 :copper)
                              :min       2
                              :max       2}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card copper :pile-size 46}]
                  :players [{:play-area [goons]
                             :coins     2
                             :buys      2}]}
                 (buy-card 0 :estate))
             {:supply  [{:card estate :pile-size 7}
                        {:card copper :pile-size 46}]
              :players [{:play-area [goons]
                         :discard   [estate]
                         :coins     0
                         :buys      1
                         :vp-tokens 1}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card copper :pile-size 46}]
                  :players [{:play-area [goons]
                             :coins     2
                             :buys      2}]}
                 (buy-card 0 :estate)
                 (buy-card 0 :copper))
             {:supply  [{:card estate :pile-size 7}
                        {:card copper :pile-size 45}]
              :players [{:play-area [goons]
                         :discard   [estate copper]
                         :coins     0
                         :buys      0
                         :vp-tokens 2}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card copper :pile-size 46}]
                  :players [{:play-area [goons goons]
                             :coins     4
                             :buys      3}]}
                 (buy-card 0 :estate))
             {:supply  [{:card estate :pile-size 7}
                        {:card copper :pile-size 46}]
              :players [{:play-area [goons goons]
                         :discard   [estate]
                         :coins     2
                         :buys      2
                         :vp-tokens 2}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card copper :pile-size 46}]
                  :players [{:play-area [goons goons]
                             :coins     4
                             :buys      3}]}
                 (buy-card 0 :estate)
                 (buy-card 0 :estate)
                 (buy-card 0 :copper))
             {:supply  [{:card estate :pile-size 6}
                        {:card copper :pile-size 45}]
              :players [{:play-area [goons goons]
                         :discard   [estate estate copper]
                         :coins     0
                         :buys      0
                         :vp-tokens 6}]})))))

(deftest grand-market-test
  (let [grand-market (assoc grand-market :id 0)]
    (testing "Grand Market"
      (is (= (-> {:players [{:hand    [grand-market]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :grand-market))
             {:players [{:hand      [copper]
                         :play-area [grand-market]
                         :deck      [copper]
                         :actions   1
                         :coins     2
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card grand-market :pile-size 10}]
                  :players [{:coins 6
                             :buys  1}]}
                 (buy-card 0 :grand-market))
             {:supply  [{:card grand-market :pile-size 9}]
              :players [{:discard [grand-market]
                         :coins   0
                         :buys    0}]}))
      (is (thrown-with-msg? AssertionError #"Grand Market can't be bought."
                            (-> {:supply  [{:card grand-market :pile-size 10}]
                                 :players [{:play-area [copper]
                                            :coins     6
                                            :buys      1}]}
                                (buy-card 0 :grand-market)))))))

(deftest hoard-test
  (let [duchy (assoc duchy :id 0)
        gold  (assoc gold :id 1)]
    (testing "Hoard"
      (is (= (-> {:players [{:hand  [hoard]
                             :coins 0}]}
                 (play 0 :hoard))
             {:players [{:play-area [hoard]
                         :coins     2}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}
                            {:card gold :pile-size 30}]
                  :players [{:play-area [hoard]
                             :coins     6
                             :buys      1}]}
                 (buy-card 0 :duchy))
             {:supply  [{:card duchy :pile-size 7}
                        {:card gold :pile-size 29}]
              :players [{:play-area [hoard]
                         :discard   [gold duchy]
                         :coins     1
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}
                            {:card gold :pile-size 30}]
                  :players [{:play-area [hoard]
                             :coins     6
                             :buys      1}]}
                 (buy-card 0 :gold))
             {:supply  [{:card duchy :pile-size 8}
                        {:card gold :pile-size 29}]
              :players [{:play-area [hoard]
                         :discard   [gold]
                         :coins     0
                         :buys      0}]})))))

(deftest kings-court-test
  (let [kings-court (assoc kings-court :id 0)]
    (testing "King's Court"
      (is (= (-> {:players [{:hand    [kings-court market estate]
                             :deck    [copper copper copper copper]
                             :actions 1}]}
                 (play 0 :king's-court))
             {:players      [{:deck      [copper copper copper copper]
                              :hand      [market estate]
                              :play-area [kings-court]
                              :actions   0}]
              :effect-stack [{:text      "You may play an Action card from your hand three times."
                              :player-no 0
                              :card-id   0
                              :choice    [:repeat-action {:times 3}]
                              :source    :hand
                              :options   [:market]
                              :max       1}]}))
      (is (= (-> {:players [{:deck    [copper copper copper copper]
                             :hand    [kings-court market estate]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :king's-court)
                 (choose :market))
             {:players [{:deck      [copper]
                         :hand      [estate copper copper copper]
                         :play-area [kings-court market]
                         :actions   3
                         :coins     3
                         :buys      4}]}))
      (is (= (-> {:players [{:deck    [copper copper copper]
                             :hand    [kings-court market copper]
                             :actions 1}]}
                 (play 0 :king's-court)
                 (choose nil))
             {:players [{:deck      [copper copper copper]
                         :hand      [market copper]
                         :play-area [kings-court]
                         :actions   0}]})))))

(deftest loan-test
  (testing "Loan"
    (is (= (-> {:players [{:hand  [loan]
                           :deck  [copper estate]
                           :coins 0}]}
               (play 0 :loan))
           {:players      [{:play-area [loan]
                            :deck      [estate]
                            :revealed  [copper]
                            :coins     1}]
            :effect-stack [{:text      "You may trash the revealed Copper."
                            :player-no 0
                            :choice    :trash-from-revealed
                            :source    :revealed
                            :options   [:copper]
                            :max       1}
                           {:player-no 0
                            :effect    [:discard-all-revealed]}]}))
    (is (= (-> {:players [{:hand  [loan]
                           :deck  [copper estate]
                           :coins 0}]}
               (play 0 :loan)
               (choose nil))
           {:players [{:play-area      [loan]
                       :deck           [estate]
                       :discard        [copper]
                       :revealed-cards {:discard 1}
                       :coins          1}]}))
    (is (= (-> {:players [{:hand  [loan]
                           :deck  [copper estate]
                           :coins 0}]}
               (play 0 :loan)
               (choose :copper))
           {:players [{:play-area [loan]
                       :deck      [estate]
                       :coins     1}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand  [loan]
                           :deck  [estate copper estate]
                           :coins 0}]}
               (play 0 :loan)
               (choose :copper))
           {:players [{:play-area      [loan]
                       :deck           [estate]
                       :discard        [estate]
                       :revealed-cards {:discard 1}
                       :coins          1}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [loan]
                           :deck    [estate]
                           :discard [duchy]
                           :coins   0}]}
               (play 0 :loan))
           {:players [{:play-area      [loan]
                       :discard        [estate duchy]
                       :revealed-cards {:discard 2}
                       :coins          1}]}))
    (is (= (-> {:players [{:hand  [loan copper]
                           :deck  [copper estate]
                           :coins 0}]}
               (play-treasures {:player-no 0})
               (choose :copper))
           {:players [{:play-area [loan copper]
                       :deck      [estate]
                       :coins     2}]
            :trash   [copper]}))))

(deftest mint-test
  (let [gold (assoc gold :id 0)]
    (testing "Mint"
      (is (= (-> {:players [{:hand    [mint copper gold]
                             :actions 1}]}
                 (play 0 :mint))
             {:players      [{:hand      [copper gold]
                              :play-area [mint]
                              :actions   0}]
              :effect-stack [{:text      "You may reveal a Treasure card from your hand to gain a copy of it."
                              :player-no 0
                              :choice    :gain
                              :source    :hand
                              :options   [:copper :gold]
                              :max       1}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [mint copper gold]
                             :actions 1}]}
                 (play 0 :mint)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:hand      [copper gold]
                         :play-area [mint]
                         :discard   [gold]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [mint copper]
                             :actions 1}]}
                 (play 0 :mint)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [mint]
                         :actions   0}]}))
      (testing "on buy"
        (let [mint (assoc mint :id 1)]
          (is (= (-> {:supply  [{:card mint :pile-size 10}]
                      :players [{:play-area [mountebank copper copper copper]
                                 :coins     5
                                 :buys      1}]}
                     (buy-card 0 :mint))
                 {:supply  [{:card mint :pile-size 9}]
                  :players [{:play-area [mountebank]
                             :discard   [mint]
                             :coins     0
                             :buys      0}]
                  :trash   [copper copper copper]})))))))

(deftest monument-test
  (testing "Monument"
    (is (= (-> {:players [{:hand    [monument]
                           :actions 1
                           :coins   0}]}
               (play 0 :monument))
           {:players [{:play-area [monument]
                       :actions   0
                       :coins     2
                       :vp-tokens 1}]}))))

(deftest mountebank-test
  (let [curse  (assoc curse :id 0)
        copper (assoc copper :id 1)]
    (testing "Mountebank"
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card copper :pile-size 46}]
                  :players [{:hand    [mountebank]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :mountebank))
             {:supply  [{:card curse :pile-size 9}
                        {:card copper :pile-size 45}]
              :players [{:play-area [mountebank]
                         :actions   0
                         :coins     2}
                        {:discard [curse copper]}]}))
      (is (= (-> {:players [{:hand    [mountebank]
                             :actions 1
                             :coins   0}
                            {:hand [curse]}]}
                 (play 0 :mountebank))
             {:players      [{:play-area [mountebank]
                              :actions   0
                              :coins     2}
                             {:hand [curse]}]
              :effect-stack [{:text      "You may discard a Curse."
                              :player-no 1
                              :choice    ::prosperity/mountebank-discard-curse
                              :source    :hand
                              :options   [:curse]
                              :max       1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand    [mountebank]
                             :actions 1
                             :coins   0}
                            {:hand [curse]}]}
                 (play 0 :mountebank)
                 (choose :curse))
             {:players [{:play-area [mountebank]
                         :actions   0
                         :coins     2}
                        {:discard [curse]}]})))
    (is (= (-> {:supply  [{:card curse :pile-size 10}
                          {:card copper :pile-size 46}]
                :players [{:hand    [mountebank]
                           :actions 1
                           :coins   0}
                          {:hand [curse]}]}
               (play 0 :mountebank)
               (choose nil))
           {:supply  [{:card curse :pile-size 9}
                      {:card copper :pile-size 45}]
            :players [{:play-area [mountebank]
                       :actions   0
                       :coins     2}
                      {:hand    [curse]
                       :discard [curse copper]}]}))))

(deftest peddler-test
  (let [peddler (assoc peddler :id 0)]
    (testing "Peddler"
      (is (= (-> {:players [{:hand    [peddler]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :peddler))
             {:players [{:hand      [copper]
                         :play-area [peddler]
                         :deck      [copper]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card peddler :pile-size 10}]
                  :players [{:coins 8
                             :buys  1
                             :phase :buy}]}
                 (buy-card 0 :peddler))
             {:supply  [{:card peddler :pile-size 9}]
              :players [{:discard [peddler]
                         :coins   0
                         :buys    0
                         :phase   :buy}]}))
      (is (= (-> {:supply  [{:card peddler :pile-size 9}]
                  :players [{:play-area [peddler gold silver]
                             :coins     6
                             :buys      1
                             :phase     :buy}]}
                 (buy-card 0 :peddler))
             {:supply  [{:card peddler :pile-size 8}]
              :players [{:play-area [peddler gold silver]
                         :discard   [peddler]
                         :coins     0
                         :buys      0
                         :phase     :buy}]}))
      (is (= (-> {:supply          [{:card peddler :pile-size 9}]
                  :cost-reductions [{:reduction 2 :type :action}]
                  :players         [{:play-area [peddler peddler]
                                     :coins     2
                                     :buys      1
                                     :phase     :action}]}
                 (buy-card 0 :peddler))
             {:supply          [{:card peddler :pile-size 8}]
              :cost-reductions [{:reduction 2 :type :action}]
              :players         [{:play-area [peddler peddler]
                                 :discard   [peddler]
                                 :coins     0
                                 :buys      0
                                 :phase     :buy}]}))
      (is (= (-> {:players [{:hand      [bishop peddler]
                             :play-area [peddler peddler]
                             :actions   1
                             :coins     2
                             :phase     :action}]}
                 (play 0 :bishop)
                 (choose :peddler))
             {:players [{:play-area [peddler peddler bishop]
                         :actions   0
                         :coins     3
                         :vp-tokens 5
                         :phase     :action}]
              :trash   [peddler]}))
      (let [improve (assoc improve :id 0)]
        (is (= (-> {:supply  [{:card peddler :pile-size 9}]
                    :players [{:hand      [improve]
                               :play-area [expand]
                               :actions   1
                               :coins     0
                               :phase     :action}]}
                   (play 0 :improve)
                   (clean-up {:player-no 0})
                   (choose {:area :play-area :card-name :improve})
                   (choose :expand)
                   (choose :peddler))
               {:supply  [{:card peddler :pile-size 8}]
                :players [{:hand    [peddler improve]
                           :actions 0
                           :coins   0
                           :buys    0
                           :phase   :out-of-turn}]
                :trash   [expand]}))))))

(deftest rabble-test
  (testing "Rabble"
    (is (= (-> {:players [{:hand    [rabble]
                           :deck    [copper copper copper copper]
                           :actions 1}
                          {:deck [copper rabble copper estate]}]}
               (play 0 :rabble))
           {:players [{:hand      [copper copper copper]
                       :play-area [rabble]
                       :deck      [copper]
                       :actions   0}
                      {:deck           [estate]
                       :discard        [copper rabble copper]
                       :revealed-cards {:discard 3}}]}))
    (is (= (-> {:players [{:hand    [rabble]
                           :deck    [copper copper copper copper]
                           :actions 1}
                          {:deck [estate copper rabble copper]}]}
               (play 0 :rabble))
           {:players      [{:hand      [copper copper copper]
                            :play-area [rabble]
                            :deck      [copper]
                            :actions   0}
                           {:deck           [copper]
                            :discard        [copper rabble]
                            :revealed       [estate]
                            :revealed-cards {:discard 2}}]
            :effect-stack [{:text                "Put the revealed cards back onto your deck in any order."
                            :player-no           1
                            :attacking-player-no 0
                            :choice              :topdeck-from-revealed
                            :source              :revealed
                            :options             [:estate]
                            :min                 1
                            :max                 1}
                           {:player-no 1
                            :effect    [:clear-unaffected {:works :once}]}]}))
    (is (= (-> {:players [{:hand    [rabble]
                           :deck    [copper copper copper copper]
                           :actions 1}
                          {:deck [estate copper rabble copper]}]}
               (play 0 :rabble)
               (choose :estate))
           {:players [{:hand      [copper copper copper]
                       :play-area [rabble]
                       :deck      [copper]
                       :actions   0}
                      {:deck           [estate copper]
                       :discard        [copper rabble]
                       :revealed-cards {:discard 2
                                        :deck    1}}]}))))

(deftest quarry-test
  (let [monument (assoc monument :id 0)
        quarry   (assoc quarry :id 1)]
    (testing "Quarry"
      (is (= (-> {:players [{:hand  [quarry]
                             :coins 0}]}
                 (play 0 :quarry))
             {:players [{:play-area [quarry]
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card monument :pile-size 10}]
                  :players [{:play-area [quarry gold]
                             :coins     4
                             :buys      1}]}
                 (buy-card 0 :monument))
             {:supply  [{:card monument :pile-size 9}]
              :players [{:play-area [quarry gold]
                         :discard   [monument]
                         :coins     2
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card monument :pile-size 10}]
                  :players [{:play-area [quarry quarry]
                             :coins     2
                             :buys      1}]}
                 (buy-card 0 :monument))
             {:supply  [{:card monument :pile-size 9}]
              :players [{:play-area [quarry quarry]
                         :discard   [monument]
                         :coins     2
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card quarry :pile-size 10}]
                  :players [{:play-area [quarry gold]
                             :coins     4
                             :buys      1}]}
                 (buy-card 0 :quarry))
             {:supply  [{:card quarry :pile-size 9}]
              :players [{:play-area [quarry gold]
                         :discard   [quarry]
                         :coins     0
                         :buys      0}]})))))

(deftest royal-seal-test
  (let [gold (assoc gold :id 0)]
    (testing "Royal Seal"
      (is (= (-> {:players [{:hand  [royal-seal]
                             :coins 0}]}
                 (play 0 :royal-seal))
             {:players [{:play-area [royal-seal]
                         :coins     2}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:play-area [royal-seal]
                             :coins     6
                             :buys      1}]}
                 (buy-card 0 :gold))
             {:supply       [{:card gold :pile-size 29}]
              :players      [{:play-area [royal-seal]
                              :gaining   [gold]
                              :coins     0
                              :buys      0}]
              :effect-stack [{:text      "You may put the gained Gold onto your deck."
                              :player-no 0
                              :choice    [:topdeck-from-gained {:gained-card-id 0}]
                              :source    :gaining
                              :options   [:gold]
                              :max       1}
                             {:player-no 0
                              :effect    [:remove-triggers {:event :on-gain}]}
                             {:player-no 0
                              :effect    [:finalize-gain {:player-no      0
                                                          :card-name      :gold
                                                          :gained-card-id 0
                                                          :bought         true}]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:play-area [royal-seal]
                             :deck      [copper]
                             :coins     6
                             :buys      1}]}
                 (buy-card 0 :gold)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [royal-seal]
                         :deck      [gold copper]
                         :coins     0
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:play-area [royal-seal royal-seal]
                             :deck      [copper]
                             :coins     6
                             :buys      1}]}
                 (buy-card 0 :gold)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [royal-seal royal-seal]
                         :deck      [gold copper]
                         :coins     0
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:play-area [royal-seal]
                             :deck      [copper]
                             :coins     6
                             :buys      1}]}
                 (buy-card 0 :gold)
                 (choose nil))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [royal-seal]
                         :deck      [copper]
                         :discard   [gold]
                         :coins     0
                         :buys      0}]})))))

(deftest talisman-test
  (let [talisman (assoc talisman :id 0)
        estate   (assoc estate :id 1)
        venture  (assoc venture :id 2)]
    (testing "Talisman"
      (is (= (-> {:players [{:hand  [talisman]
                             :coins 0}]}
                 (play 0 :talisman))
             {:players [{:play-area [talisman]
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card talisman :pile-size 9}]
                  :players [{:play-area [talisman]
                             :coins     5
                             :buys      1}]}
                 (buy-card 0 :talisman))
             {:supply  [{:card talisman :pile-size 7}]
              :players [{:play-area [talisman]
                         :discard   [talisman talisman]
                         :coins     1
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card talisman :pile-size 1}]
                  :players [{:play-area [talisman]
                             :coins     5
                             :buys      1}]}
                 (buy-card 0 :talisman))
             {:supply  [{:card talisman :pile-size 0}]
              :players [{:play-area [talisman]
                         :discard   [talisman]
                         :coins     1
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card talisman :pile-size 8}]
                  :players [{:play-area [talisman talisman]
                             :coins     5
                             :buys      1}]}
                 (buy-card 0 :talisman))
             {:supply  [{:card talisman :pile-size 5}]
              :players [{:play-area [talisman talisman]
                         :discard   [talisman talisman talisman]
                         :coins     1
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:play-area [talisman]
                             :coins     5
                             :buys      1}]}
                 (buy-card 0 :estate))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:play-area [talisman]
                         :discard   [estate]
                         :coins     3
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card venture :pile-size 10}]
                  :players [{:play-area [talisman]
                             :coins     5
                             :buys      1}]}
                 (buy-card 0 :venture))
             {:supply  [{:card venture :pile-size 9}]
              :players [{:play-area [talisman]
                         :discard   [venture]
                         :coins     0
                         :buys      0}]})))))

(deftest trade-route-test
  (let [duchy (assoc duchy :id 0)]
    (testing "Trade Route"
      (is (= (-> {:players [{:hand    [trade-route copper estate]
                             :actions 1
                             :buys    1}]}
                 (play 0 :trade-route))
             {:players      [{:hand      [copper estate]
                              :play-area [trade-route]
                              :actions   0
                              :buys      2}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :choice    :trash-from-hand
                              :source    :hand
                              :options   [:copper :estate]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:trade-route-mat 1
                  :players         [{:hand    [trade-route copper estate]
                                     :actions 1
                                     :coins   0
                                     :buys    1}]}
                 (play 0 :trade-route))
             {:trade-route-mat 1
              :players         [{:hand      [copper estate]
                                 :play-area [trade-route]
                                 :actions   0
                                 :coins     1
                                 :buys      2}]
              :effect-stack    [{:text      "Trash a card from your hand."
                                 :player-no 0
                                 :choice    :trash-from-hand
                                 :source    :hand
                                 :options   [:copper :estate]
                                 :min       1
                                 :max       1}]}))
      (is (= (-> {:supply  [{:card      duchy
                             :pile-size 8
                             :tokens    [{:token-type :trade-route
                                          :on-gain    [[::prosperity/trade-route-move-token]]}]}]
                  :players [{:coins 5
                             :buys  1}]}
                 (buy-card 0 :duchy))
             {:trade-route-mat 1
              :supply          [{:card duchy :pile-size 7}]
              :players         [{:discard [duchy]
                                 :coins   0
                                 :buys    0}]})))))

(deftest vault-test
  (testing "Vault"
    (is (= (-> {:players [{:hand    [vault copper copper estate estate]
                           :deck    [copper estate silver]
                           :actions 1}]}
               (play 0 :vault))
           {:players      [{:hand      [copper copper estate estate copper estate]
                            :play-area [vault]
                            :deck      [silver]
                            :actions   0}]
            :effect-stack [{:text      "Discard any number of cards for +$1 each."
                            :player-no 0
                            :choice    ::prosperity/vault-discard
                            :source    :hand
                            :options   [:copper :copper :estate :estate :copper :estate]}
                           {:player-no 0
                            :effect    [:other-players {:effects [[:give-choice {:text      "You may discard 2 cards, to draw a card."
                                                                                 :choice    ::prosperity/vault-discard-2
                                                                                 :options   [:player :hand]
                                                                                 :min       2
                                                                                 :max       2
                                                                                 :optional? true}]]}]}]}))
    (is (= (-> {:players [{:hand    [vault copper copper estate estate]
                           :deck    [copper estate silver]
                           :actions 1
                           :coins   0}]}
               (play 0 :vault)
               (choose [:estate :estate :estate]))
           {:players [{:hand      [copper copper copper]
                       :play-area [vault]
                       :deck      [silver]
                       :discard   [estate estate estate]
                       :actions   0
                       :coins     3}]})))
  (is (= (-> {:players [{:hand    [vault]
                         :deck    [silver silver]
                         :actions 1
                         :coins   0}
                        {:hand [estate estate copper]
                         :deck [copper copper]}]}
             (play 0 :vault)
             (choose nil))
         {:players      [{:hand      [silver silver]
                          :play-area [vault]
                          :actions   0
                          :coins     0}
                         {:hand [estate estate copper]
                          :deck [copper copper]}]
          :effect-stack [{:text      "You may discard 2 cards, to draw a card."
                          :player-no 1
                          :choice    ::prosperity/vault-discard-2
                          :source    :hand
                          :options   [:estate :estate :copper]
                          :min       2
                          :max       2
                          :optional? true}]}))
  (is (= (-> {:players [{:hand    [vault]
                         :deck    [silver silver]
                         :actions 1
                         :coins   0}
                        {:hand [estate estate copper]
                         :deck [copper copper]}]}
             (play 0 :vault)
             (choose nil)
             (choose nil))
         {:players [{:hand      [silver silver]
                     :play-area [vault]
                     :actions   0
                     :coins     0}
                    {:hand [estate estate copper]
                     :deck [copper copper]}]}))
  (is (= (-> {:players [{:hand    [vault]
                         :deck    [silver silver]
                         :actions 1
                         :coins   0}
                        {:hand [estate estate copper]
                         :deck [copper copper]}]}
             (play 0 :vault)
             (choose nil)
             (choose [:estate :estate]))
         {:players [{:hand      [silver silver]
                     :play-area [vault]
                     :actions   0
                     :coins     0}
                    {:hand    [copper copper]
                     :deck    [copper]
                     :discard [estate estate]}]}))
  (is (= (-> {:players [{:hand    [vault]
                         :deck    [silver silver]
                         :actions 1
                         :coins   0}
                        {:hand [estate]
                         :deck [copper copper]}]}
             (play 0 :vault)
             (choose nil)
             (choose [:estate]))
         {:players [{:hand      [silver silver]
                     :play-area [vault]
                     :actions   0
                     :coins     0}
                    {:deck    [copper copper]
                     :discard [estate]}]})))

(deftest venture-test
  (testing "Venture"
    (is (= (-> {:players [{:hand  [venture]
                           :deck  [copper estate]
                           :coins 0}]}
               (play 0 :venture))
           {:players [{:play-area      [venture copper]
                       :deck           [estate]
                       :revealed-cards {:play-area 1}
                       :coins          2}]}))
    (is (= (-> {:players [{:hand  [venture]
                           :deck  [estate copper estate]
                           :coins 0}]}
               (play 0 :venture))
           {:players [{:play-area      [venture copper]
                       :deck           [estate]
                       :discard        [estate]
                       :revealed-cards {:play-area 1
                                        :discard   1}
                       :coins          2}]}))
    (is (= (-> {:players [{:hand    [venture]
                           :deck    [estate]
                           :discard [duchy]
                           :coins   0}]}
               (play 0 :venture))
           {:players [{:play-area      [venture]
                       :discard        [estate duchy]
                       :revealed-cards {:discard 2}
                       :coins          1}]}))
    (is (= (-> {:players [{:hand  [venture copper]
                           :deck  [copper estate]
                           :coins 0}]}
               (play-treasures {:player-no 0}))
           {:players [{:play-area      [copper venture copper]
                       :deck           [estate]
                       :revealed-cards {:play-area 1}
                       :coins          3}]}))
    (is (= (-> {:players [{:hand    [venture]
                           :deck    [estate loan duchy]
                           :discard (repeat 7 copper)
                           :coins   0}]}
               (play 0 :venture)
               (choose :copper))
           {:players [{:play-area      [venture loan]
                       :deck           (repeat 6 copper)
                       :discard        [estate duchy]
                       :revealed-cards {:play-area 1
                                        :discard   2}
                       :coins          2}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand  [venture loan]
                           :deck  [copper gold]
                           :coins 0}]}
               (play-treasures {:player-no 0})
               (choose :copper))
           {:players [{:play-area      [loan venture gold]
                       :revealed-cards {:play-area 1}
                       :coins          5}]
            :trash   [copper]}))))

(deftest watchtower-test
  (let [gold  (assoc gold :id 0)
        curse (assoc curse :id 1)]
    (testing "Watchtower"
      (is (= (-> {:players [{:hand    [watchtower estate estate]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 (play 0 :watchtower))
             {:players [{:hand      [estate estate copper copper copper copper]
                         :play-area [watchtower]
                         :deck      [copper]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand  [watchtower]
                             :coins 6
                             :buys  1}]}
                 (buy-card 0 :gold))
             {:supply       [{:card gold :pile-size 29}]
              :players      [{:hand    [watchtower]
                              :gaining [gold]
                              :coins   0
                              :buys    0}]
              :effect-stack [{:text      "You may reveal a Watchtower from your hand, to either trash the gained Gold or put it onto your deck."
                              :player-no 0
                              :choice    [::prosperity/watchtower-choice {:gained-card-id 0}]
                              :source    :special
                              :options   [{:option :trash :text "Trash Gold."}
                                          {:option :topdeck :text "Put Gold onto your deck."}
                                          {:option :nothing :text "Don't reveal Watchtower."}]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:remove-triggers {:event :on-gain}]}
                             {:player-no 0
                              :effect    [:finalize-gain {:player-no      0
                                                          :card-name      :gold
                                                          :gained-card-id 0
                                                          :bought         true}]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand  [watchtower]
                             :coins 6
                             :buys  1}]}
                 (buy-card 0 :gold)
                 (choose :trash))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:hand  [watchtower]
                         :coins 0
                         :buys  0}]
              :trash   [gold]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand  [watchtower]
                             :deck  [copper]
                             :coins 6
                             :buys  1}]}
                 (buy-card 0 :gold)
                 (choose :topdeck))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:hand  [watchtower]
                         :deck  [gold copper]
                         :coins 0
                         :buys  0}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand  [watchtower]
                             :coins 6
                             :buys  1}]}
                 (buy-card 0 :gold)
                 (choose :nothing))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:hand    [watchtower]
                         :discard [gold]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [sea-hag]
                             :actions 1}
                            {:hand [watchtower]
                             :deck [copper copper]}]}
                 (play 0 :sea-hag))
             {:supply       [{:card curse :pile-size 9}]
              :players      [{:play-area [sea-hag]
                              :actions   0}
                             {:hand    [watchtower]
                              :gaining [curse]
                              :deck    [copper]
                              :discard [copper]}]
              :effect-stack [{:text      "You may reveal a Watchtower from your hand, to either trash the gained Curse or put it onto your deck."
                              :player-no 1
                              :choice    [::prosperity/watchtower-choice {:gained-card-id 1}]
                              :source    :special
                              :options   [{:option :trash :text "Trash Curse."}
                                          {:option :topdeck :text "Put Curse onto your deck."}
                                          {:option :nothing :text "Don't reveal Watchtower."}]
                              :min       1
                              :max       1}
                             {:player-no 1
                              :effect    [:remove-triggers {:event :on-gain}]}
                             {:player-no 1
                              :effect    [:finalize-gain {:player-no           1
                                                          :card-name           :curse
                                                          :gained-card-id      1
                                                          :to                  :deck
                                                          :to-position         :top
                                                          :attacking-player-no 0}]}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [sea-hag]
                             :actions 1}
                            {:hand [watchtower]
                             :deck [copper copper]}]}
                 (play 0 :sea-hag)
                 (choose :trash))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:play-area [sea-hag]
                         :actions   0}
                        {:hand    [watchtower]
                         :deck    [copper]
                         :discard [copper]}]
              :trash   [curse]}))
      (is (= (-> {:supply  [{:card curse :pile-size 0}]
                  :players [{:hand    [sea-hag]
                             :actions 1}
                            {:hand [watchtower]
                             :deck [copper gold]}]}
                 (play 0 :sea-hag))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:play-area [sea-hag]
                         :actions   0}
                        {:hand    [watchtower]
                         :deck    [gold]
                         :discard [copper]}]})))))

(deftest workers-village-test
  (testing "Worker's Village"
    (is (= (-> {:players [{:hand    [workers-village]
                           :deck    [estate estate]
                           :actions 1
                           :buys    1}]}
               (play 0 :worker's-village))
           {:players [{:hand      [estate]
                       :play-area [workers-village]
                       :deck      [estate]
                       :actions   2
                       :buys      2}]}))))