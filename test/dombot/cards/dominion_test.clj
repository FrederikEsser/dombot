(ns dombot.cards.dominion-test
  (:require [clojure.test :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :refer :all]))

(deftest artisan-test
  (testing "Artisan"
    (is (= (play {:supply  (base/supply 2 8)
                  :players [{:hand    [artisan silver]
                             :actions 1}]}
                 0 :artisan)
           {:supply       (base/supply 2 8)
            :players      [{:hand      [silver]
                            :play-area [artisan]
                            :actions   0}]
            :effect-stack [{:text      "Gain a card to your hand costing up to $5."
                            :player-no 0
                            :choice-fn gain-to-hand
                            :options   [:curse :estate :duchy :copper :silver]
                            :min       1
                            :max       1}
                           {:player-no 0
                            :action-fn give-topdeck-choice}]}))
    (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                :players [{:hand    [artisan silver]
                           :actions 1}]}
               (play 0 :artisan)
               (chose :duchy))
           {:supply       [{:card duchy :pile-size 7}]
            :players      [{:hand      [silver duchy]
                            :play-area [artisan]
                            :actions   0}]
            :effect-stack [{:text      "Put a card from your hand onto your deck."
                            :player-no 0
                            :choice-fn topdeck-from-hand
                            :options   [:silver :duchy]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                :players [{:hand    [artisan silver]
                           :deck    [gold]
                           :actions 1}]}
               (play 0 :artisan)
               (chose :duchy)
               (chose :silver))
           {:supply  [{:card duchy :pile-size 7}]
            :players [{:hand      [duchy]
                       :play-area [artisan]
                       :deck      [silver gold]
                       :actions   0}]}))
    (is (= (play {:supply  []                               ; totally hypothetical supply with no cards costing 5 or less
                  :players [{:hand    [artisan silver]
                             :actions 1}]}
                 0 :artisan)
           {:supply       []
            :players      [{:hand      [silver]
                            :play-area [artisan]
                            :actions   0}]
            :effect-stack [{:text      "Put a card from your hand onto your deck."
                            :player-no 0
                            :choice-fn topdeck-from-hand
                            :options   [:silver]
                            :min       1
                            :max       1}]}))))

(deftest bandit-test
  (testing "Bandit"
    (is (= (play {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [bandit]
                             :actions 1}
                            {:deck [estate estate estate]}]}
                 0 :bandit)
           {:supply  [{:card gold :pile-size 29}]
            :players [{:hand      []
                       :play-area [bandit]
                       :discard   [gold]
                       :actions   0}
                      {:deck    [estate]
                       :discard [estate estate]
                       :reveal  []}]}))
    (is (= (play {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [bandit]
                             :actions 1}
                            {:deck [estate silver gold]}]}
                 0 :bandit)
           {:supply       [{:card gold :pile-size 29}]
            :players      [{:hand      []
                            :play-area [bandit]
                            :discard   [gold]
                            :actions   0}
                           {:deck   [gold]
                            :reveal [estate silver]}]
            :effect-stack [{:text      "Trash a revealed Treasure other than Copper, and discards the rest."
                            :player-no 1
                            :choice-fn trash-from-revealed
                            :options   [:silver]
                            :min       1
                            :max       1}
                           {:player-no 1
                            :action-fn discard-all-revealed}]}))
    (is (= (-> {:supply  [{:card gold :pile-size 30}]
                :players [{:hand    [bandit]
                           :actions 1}
                          {:deck [estate silver gold]}]}
               (play 0 :bandit)
               (chose :silver))
           {:supply  [{:card gold :pile-size 29}]
            :players [{:hand      []
                       :play-area [bandit]
                       :discard   [gold]
                       :actions   0}
                      {:deck    [gold]
                       :reveal  []
                       :discard [estate]}]
            :trash   [silver]}))
    (is (= (-> {:mode    :swift
                :supply  [{:card gold :pile-size 30}]
                :players [{:hand    [bandit]
                           :actions 1}
                          {:deck [estate silver gold]}]}
               (play 0 :bandit))
           {:mode    :swift
            :supply  [{:card gold :pile-size 29}]
            :players [{:hand      []
                       :play-area [bandit]
                       :discard   [gold]
                       :actions   0}
                      {:deck    [gold]
                       :reveal  []
                       :discard [estate]}]
            :trash   [silver]}))
    (is (= (play {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [bandit]
                             :actions 1}
                            {:deck [silver gold estate]}
                            {:deck [copper gold estate]}]}
                 0 :bandit)
           {:supply       [{:card gold :pile-size 29}]
            :players      [{:hand      []
                            :play-area [bandit]
                            :discard   [gold]
                            :actions   0}
                           {:deck   [estate]
                            :reveal [silver gold]}
                           {:deck [copper gold estate]}]
            :effect-stack [{:text      "Trash a revealed Treasure other than Copper, and discards the rest."
                            :player-no 1
                            :choice-fn trash-from-revealed
                            :options   [:silver :gold]
                            :min       1
                            :max       1}
                           {:player-no 1
                            :action-fn discard-all-revealed}
                           {:player-no 2
                            :action-fn bandit-attack}]}))
    (is (= (-> {:supply  [{:card gold :pile-size 30}]
                :players [{:hand    [bandit]
                           :actions 1}
                          {:deck [silver gold estate]}
                          {:deck [copper gold estate]}]}
               (play 0 :bandit)
               (chose :silver))
           {:supply       [{:card gold :pile-size 29}]
            :players      [{:hand      []
                            :play-area [bandit]
                            :discard   [gold]
                            :actions   0}
                           {:deck    [estate]
                            :discard [gold]
                            :reveal  []}
                           {:deck   [estate]
                            :reveal [copper gold]}]
            :effect-stack [{:text      "Trash a revealed Treasure other than Copper, and discards the rest."
                            :player-no 2
                            :choice-fn trash-from-revealed
                            :options   [:gold]
                            :min       1
                            :max       1}
                           {:player-no 2
                            :action-fn discard-all-revealed}]
            :trash        [silver]}))
    (is (= (-> {:supply  [{:card gold :pile-size 30}]
                :players [{:hand    [bandit]
                           :actions 1}
                          {:deck [silver]}]}
               (play 0 :bandit)
               (chose :silver))
           {:supply  [{:card gold :pile-size 29}]
            :players [{:hand      []
                       :play-area [bandit]
                       :discard   [gold]
                       :actions   0}
                      {:deck   []
                       :reveal []}]
            :trash   [silver]}))))

(deftest bureaucrat-test
  (testing "Bureaucrat"
    (is (= (play {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [bureaucrat]
                             :deck    [copper]
                             :actions 1}
                            {:hand (repeat 5 copper)}]}
                 0 :bureaucrat)
           {:supply  [{:card silver :pile-size 39}]
            :players [{:hand      []
                       :play-area [bureaucrat]
                       :deck      [silver copper]
                       :actions   0}
                      {:hand (repeat 5 copper)}]
            :reveal  {1 (repeat 5 copper)}}))
    (is (= (play {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [bureaucrat]
                             :deck    [copper]
                             :actions 1}
                            {:hand [copper copper copper estate estate]}]}
                 0 :bureaucrat)
           {:supply       [{:card silver :pile-size 39}]
            :players      [{:hand      []
                            :play-area [bureaucrat]
                            :deck      [silver copper]
                            :actions   0}
                           {:hand [copper copper copper estate estate]}]
            :effect-stack [{:text      "Reveal a Victory card from your hand and put it onto your deck."
                            :player-no 1
                            :choice-fn topdeck-from-hand
                            :options   [:estate :estate]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  [{:card silver :pile-size 40}]
                :players [{:hand    [bureaucrat]
                           :deck    [copper]
                           :actions 1}
                          {:hand [copper copper copper estate estate]
                           :deck [gold]}]}
               (play 0 :bureaucrat)
               (chose :estate))
           {:supply  [{:card silver :pile-size 39}]
            :players [{:hand      []
                       :play-area [bureaucrat]
                       :deck      [silver copper]
                       :actions   0}
                      {:hand [copper copper copper estate]
                       :deck [estate gold]}]}))
    (is (= (-> {:mode    :swift
                :supply  [{:card silver :pile-size 40}]
                :players [{:hand    [bureaucrat]
                           :deck    [copper]
                           :actions 1}
                          {:hand [copper copper copper estate estate]
                           :deck [gold]}]}
               (play 0 :bureaucrat))
           {:mode    :swift
            :supply  [{:card silver :pile-size 39}]
            :players [{:hand      []
                       :play-area [bureaucrat]
                       :deck      [silver copper]
                       :actions   0}
                      {:hand [copper copper copper estate]
                       :deck [estate gold]}]}))))

(deftest cellar-test
  (testing "Cellar"
    (is (= (play {:players [{:hand    [cellar copper estate estate estate]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 0 :cellar)
           {:players      [{:hand      [copper estate estate estate]
                            :play-area [cellar]
                            :deck      (repeat 5 copper)
                            :actions   1}]
            :effect-stack [{:text      "Discard any number of cards, then draw that many."
                            :player-no 0
                            :choice-fn cellar-sift
                            :options   [:copper :estate :estate :estate]}]}))
    (is (= (-> {:players [{:hand    [cellar copper estate estate estate]
                           :deck    (repeat 5 copper)
                           :actions 1}]}
               (play 0 :cellar)
               (chose [:estate :estate :estate]))
           {:players [{:hand      [copper copper copper copper]
                       :play-area [cellar]
                       :deck      [copper copper]
                       :discard   [estate estate estate]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [cellar copper]
                           :actions 1}]}
               (play 0 :cellar)
               (chose :copper))
           {:players [{:hand      [copper]
                       :play-area [cellar]
                       :deck      []
                       :discard   []
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [cellar copper estate estate estate]
                           :deck    (repeat 5 copper)
                           :actions 1}]}
               (play 0 :cellar)
               (chose []))
           {:players [{:hand      [copper estate estate estate]
                       :play-area [cellar]
                       :deck      (repeat 5 copper)
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [cellar]
                           :deck    (repeat 5 copper)
                           :actions 1}]}
               (play 0 :cellar))
           {:players [{:hand      []
                       :play-area [cellar]
                       :deck      (repeat 5 copper)
                       :actions   1}]}))
    (is (thrown-with-msg? AssertionError #"Move error: There is no Estate in your Hand"
                          (-> {:players [{:hand    [cellar copper estate]
                                          :deck    (repeat 5 copper)
                                          :actions 1}]}
                              (play 0 :cellar)
                              (chose [:estate :estate]))))))

(deftest chapel-test
  (testing "Chapel"
    (is (= (play {:players [{:hand    [chapel copper estate estate estate]
                             :actions 1}]}
                 0 :chapel)
           {:players      [{:hand      [copper estate estate estate]
                            :play-area [chapel]
                            :actions   0}]
            :effect-stack [{:text      "Trash up to 4 cards from your hand."
                            :player-no 0
                            :choice-fn trash-from-hand
                            :options   [:copper :estate :estate :estate]
                            :max       4}]}))
    (is (= (-> {:players [{:hand    [chapel copper estate estate estate]
                           :actions 1}]}
               (play 0 :chapel)
               (chose [:estate :estate :estate]))
           {:players [{:hand      [copper]
                       :play-area [chapel]
                       :actions   0}]
            :trash   [estate estate estate]}))
    (is (= (-> {:players [{:hand    [chapel copper]
                           :actions 1}]}
               (play 0 :chapel)
               (chose :copper))
           {:players [{:hand      []
                       :play-area [chapel]
                       :actions   0}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [chapel copper estate estate estate]
                           :actions 1}]}
               (play 0 :chapel)
               (chose []))
           {:players [{:hand      [copper estate estate estate]
                       :play-area [chapel]
                       :actions   0}]}))
    (is (thrown-with-msg? AssertionError #"Chose error: You can only pick 4 options."
                          (-> {:players [{:hand    (concat [chapel] (repeat 5 copper))
                                          :actions 1}]}
                              (play 0 :chapel)
                              (chose (repeat 5 :copper)))))))

(deftest council-room-test
  (testing "Council Room"
    (is (= (play {:players [{:deck    (repeat 5 copper)
                             :hand    [council-room]
                             :actions 1
                             :buys    1}
                            {:deck [copper copper]
                             :hand []}]}
                 0 :council-room)
           {:players [{:deck      [copper]
                       :hand      (repeat 4 copper)
                       :play-area [council-room]
                       :actions   0
                       :buys      2}
                      {:deck [copper]
                       :hand [copper]}]}))))

(deftest festival-test
  (testing "Festival"
    (is (= (play {:players [{:deck    [copper]
                             :hand    [festival]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 0 :festival)
           {:players [{:deck      [copper]
                       :hand      []
                       :play-area [festival]
                       :actions   2
                       :coins     2
                       :buys      2}]}))))

(deftest harbinger-test
  (testing "Harbinger"
    (is (= (play {:players [{:hand    [harbinger]
                             :deck    [copper copper copper]
                             :discard [gold]
                             :actions 1}]}
                 0 :harbinger)
           {:players      [{:hand      [copper]
                            :deck      [copper copper]
                            :discard   [gold]
                            :play-area [harbinger]
                            :actions   1}]
            :effect-stack [{:text      "You may put a card from your discard pile onto your deck."
                            :player-no 0
                            :choice-fn topdeck-from-discard
                            :options   [:gold]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [harbinger]
                           :deck    [copper copper copper]
                           :discard [gold]
                           :actions 1}]}
               (play 0 :harbinger)
               (chose :gold))
           {:players [{:hand      [copper]
                       :deck      [gold copper copper]
                       :discard   []
                       :play-area [harbinger]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [harbinger]
                           :deck    [copper copper copper]
                           :discard [estate]
                           :actions 1}]}
               (play 0 :harbinger)
               (chose nil))
           {:players [{:hand      [copper]
                       :deck      [copper copper]
                       :discard   [estate]
                       :play-area [harbinger]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [harbinger]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :harbinger))
           {:players [{:hand      [copper]
                       :deck      [copper copper]
                       :play-area [harbinger]
                       :actions   1}]}))))

(deftest laboratory-test
  (testing "Laboratory"
    (is (= (play {:players [{:deck    [copper copper copper]
                             :hand    [laboratory]
                             :actions 1}]}
                 0 :laboratory)
           {:players [{:deck      [copper]
                       :hand      [copper copper]
                       :play-area [laboratory]
                       :actions   1}]}))))

(deftest library-test
  (testing "Library"
    (is (= (play {:players [{:hand    [library]
                             :deck    (repeat 8 copper)
                             :actions 1}]}
                 0 :library)
           {:players [{:hand      (repeat 7 copper)
                       :play-area [library]
                       :deck      [copper]
                       :actions   0}]}))
    (is (= (play {:players [{:hand    (concat [library] (repeat 7 copper))
                             :deck    [estate]
                             :actions 1}]}
                 0 :library)
           {:players [{:hand      (repeat 7 copper)
                       :play-area [library]
                       :deck      [estate]
                       :actions   0}]}))
    (is (= (play {:players [{:hand    [library]
                             :deck    (repeat 6 copper)
                             :actions 1}]}
                 0 :library)
           {:players [{:hand      (repeat 6 copper)
                       :play-area [library]
                       :deck      []
                       :actions   0}]}))
    (is (= (play {:players [{:hand    [smithy library copper]
                             :deck    [silver gold village estate duchy smithy province]
                             :actions 1}]}
                 0 :library)
           {:players      [{:hand      [smithy copper silver gold village]
                            :play-area [library]
                            :deck      [estate duchy smithy province]
                            :actions   0}]
            :effect-stack [{:text      "You may skip the Village; set it aside, discarding it afterwards."
                            :player-no 0
                            :choice-fn library-set-aside
                            :options   [:village]
                            :max       1}
                           {:player-no 0
                            :action-fn library-action}]}))
    (is (= (-> {:players [{:hand    [smithy library copper]
                           :deck    [silver gold village estate duchy smithy province]
                           :actions 1}]}
               (play 0 :library)
               (chose nil))
           {:players [{:hand      [smithy copper silver gold village estate duchy]
                       :play-area [library]
                       :deck      [smithy province]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [smithy library copper]
                           :deck    [silver gold village estate duchy smithy province]
                           :actions 1}]}
               (play 0 :library)
               (chose :village))
           {:players      [{:hand      [smithy copper silver gold estate duchy smithy]
                            :play-area [library]
                            :set-aside [village]
                            :deck      [province]
                            :actions   0}]
            :effect-stack [{:text      "You may skip the Smithy; set it aside, discarding it afterwards."
                            :player-no 0
                            :choice-fn library-set-aside
                            :options   [:smithy]
                            :max       1}
                           {:player-no 0
                            :action-fn library-action}]}))
    (is (= (-> {:players [{:hand    [smithy library copper]
                           :deck    [silver gold village estate duchy smithy province]
                           :actions 1}]}
               (play 0 :library)
               (chose :village)
               (chose :smithy))
           {:players [{:hand      [smithy copper silver gold estate duchy province]
                       :play-area [library]
                       :deck      []
                       :set-aside []
                       :discard   [village smithy]
                       :actions   0}]}))))

(deftest market-test
  (testing "Market"
    (is (= (play {:players [{:deck    [copper copper copper]
                             :hand    [market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 0 :market)
           {:players [{:deck      [copper copper]
                       :hand      [copper]
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

(deftest militia-test
  (testing "Militia"
    (is (= (play {:players [{:hand    [militia]
                             :actions 1
                             :coins   0}
                            {:hand (repeat 5 copper)}]}
                 0 :militia)
           {:players      [{:hand      []
                            :play-area [militia]
                            :actions   0
                            :coins     2}
                           {:hand (repeat 5 copper)}]
            :effect-stack [{:text      "Discard down to 3 cards in hand."
                            :player-no 1
                            :choice-fn discard-from-hand
                            :options   (repeat 5 :copper)
                            :min       2
                            :max       2}]}))
    (is (= (-> {:players [{:hand    [militia]
                           :actions 1
                           :coins   0}
                          {:hand (repeat 5 copper)}]}
               (play 0 :militia)
               (chose [:copper :copper]))
           {:players [{:hand      []
                       :play-area [militia]
                       :actions   0
                       :coins     2}
                      {:hand    (repeat 3 copper)
                       :discard [copper copper]}]}))
    (is (= (-> {:mode    :swift
                :players [{:hand    [militia]
                           :actions 1
                           :coins   0}
                          {:hand (repeat 5 copper)}]}
               (play 0 :militia))
           {:mode    :swift
            :players [{:hand      []
                       :play-area [militia]
                       :actions   0
                       :coins     2}
                      {:hand    (repeat 3 copper)
                       :discard [copper copper]}]}))
    (is (= (-> {:players [{:hand    [militia]
                           :actions 1
                           :coins   0}
                          {:hand (repeat 6 copper)}]}
               (play 0 :militia)
               (chose [:copper :copper :copper]))
           {:players [{:hand      []
                       :play-area [militia]
                       :actions   0
                       :coins     2}
                      {:hand    (repeat 3 copper)
                       :discard (repeat 3 copper)}]}))))

(deftest mine-test
  (testing "Mine"
    (is (= (play {:players [{:hand    [mine copper estate]
                             :actions 1}]}
                 0 :mine)
           {:players      [{:hand      [copper estate]
                            :play-area [mine]
                            :actions   0}]
            :effect-stack [{:text      "You may trash a Treasure from your hand."
                            :player-no 0
                            :choice-fn mine-trash
                            :options   [:copper]
                            :max       1}]}))
    (is (= (play {:players [{:hand    [mine estate]
                             :actions 1}]}
                 0 :mine)
           {:players [{:hand      [estate]
                       :play-area [mine]
                       :actions   0}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [mine copper estate]
                           :actions 1}]}
               (play 0 :mine)
               (chose :copper))
           {:supply       (base/supply 2 8)
            :players      [{:hand      [estate]
                            :play-area [mine]
                            :actions   0}]
            :effect-stack [{:text      "Gain a Treasure to your hand costing up to $3."
                            :player-no 0
                            :choice-fn gain-to-hand
                            :options   [:copper :silver]
                            :min       1
                            :max       1}]
            :trash        [copper]}))
    (is (= (-> {:supply  [{:card copper :pile-size 0} {:card gold :pile-size 30}]
                :players [{:hand    [mine copper estate]
                           :actions 1}]}
               (play 0 :mine)
               (chose :copper))
           {:supply  [{:card copper :pile-size 0} {:card gold :pile-size 30}]
            :players [{:hand      [estate]
                       :play-area [mine]
                       :actions   0}]
            :trash   [copper]}))
    (is (= (-> {:supply  [{:card gold :pile-size 30}]
                :players [{:hand    [mine silver estate]
                           :actions 1}]}
               (play 0 :mine)
               (chose :silver)
               (chose :gold))
           {:supply  [{:card gold :pile-size 29}]
            :players [{:hand      [estate gold]
                       :play-area [mine]
                       :actions   0}]
            :trash   [silver]}))))

(deftest moat-test
  (testing "Moat"
    (is (= (play {:players [{:deck    [copper copper copper]
                             :hand    [moat]
                             :actions 1}]}
                 0 :moat)
           {:players [{:hand      [copper copper]
                       :deck      [copper]
                       :play-area [moat]
                       :actions   0}]}))
    (is (= (play {:players [{:hand    [moat]
                             :deck    [copper]
                             :discard [estate estate]
                             :actions 1}]}
                 0 :moat)
           {:players [{:hand      [copper estate]
                       :deck      [estate]
                       :discard   []
                       :play-area [moat]
                       :actions   0}]}))
    (is (= (play {:players [{:hand    [militia]
                             :actions 1
                             :coins   0}
                            {:hand [moat]}]}
                 0 :militia)
           {:players      [{:hand      []
                            :play-area [militia]
                            :actions   0
                            :coins     0}
                           {:hand [moat]}]
            :effect-stack [{:text      "You may reveal a Moat from your hand, to be unaffected by a Militia attack."
                            :player-no 1
                            :choice-fn moat-reaction
                            :options   [:moat]
                            :max       1}
                           (assoc militia :player-no 0)]}))
    (is (= (-> {:players [{:hand    [militia]
                           :actions 1
                           :coins   0}
                          {:hand [moat copper copper copper copper]}]}
               (play 0 :militia)
               (chose :moat))
           {:players [{:hand      []
                       :play-area [militia]
                       :actions   0
                       :coins     2}
                      {:hand [moat copper copper copper copper]}]}))
    (is (= (-> {:players [{:hand    [militia]
                           :actions 1
                           :coins   0}
                          {:hand [moat copper copper copper copper]}]}
               (play 0 :militia)
               (chose nil))
           {:players      [{:hand      []
                            :play-area [militia]
                            :actions   0
                            :coins     2}
                           {:hand [moat copper copper copper copper]}]
            :effect-stack [{:text      "Discard down to 3 cards in hand."
                            :player-no 1
                            :choice-fn discard-from-hand
                            :options   [:moat :copper :copper :copper :copper]
                            :min       2
                            :max       2}]}))
    (is (= (-> {:players [{:hand    [vassal]
                           :deck    [militia]
                           :actions 1
                           :coins   0}
                          {:hand [moat]}]}
               (play 0 :vassal)
               (chose :militia))
           {:players      [{:hand      []
                            :play-area [vassal militia]
                            :deck      []
                            :discard   []
                            :actions   0
                            :coins     2}
                           {:hand [moat]}]
            :effect-stack [{:text      "You may reveal a Moat from your hand, to be unaffected by a Militia attack."
                            :player-no 1
                            :choice-fn moat-reaction
                            :options   [:moat]
                            :max       1}
                           (assoc militia :player-no 0)]}))))

(deftest moneylender-test
  (testing "Moneylender"
    (is (= (play {:players [{:hand    [moneylender copper copper estate]
                             :actions 1}]}
                 0 :moneylender)
           {:players      [{:hand      [copper copper estate]
                            :play-area [moneylender]
                            :actions   0}]
            :effect-stack [{:text      "You may trash a Copper from your hand for +$3"
                            :player-no 0
                            :choice-fn moneylender-trash
                            :options   [:copper :copper]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [moneylender copper copper estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :moneylender)
               (chose :copper))
           {:players [{:hand      [copper estate]
                       :play-area [moneylender]
                       :actions   0
                       :coins     3}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [moneylender copper copper estate]
                           :actions 1}]}
               (play 0 :moneylender)
               (chose nil))
           {:players [{:hand      [copper copper estate]
                       :play-area [moneylender]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [moneylender estate]
                           :actions 1}]}
               (play 0 :moneylender))
           {:players [{:hand      [estate]
                       :play-area [moneylender]
                       :actions   0}]}))))

(deftest poacher-test
  (testing "Poacher"
    (is (= (play {:players [{:deck    [copper copper]
                             :hand    [poacher estate]
                             :actions 1
                             :coins   0}]}
                 0 :poacher)
           {:players [{:deck      [copper]
                       :hand      [estate copper]
                       :play-area [poacher]
                       :actions   1
                       :coins     1}]}))
    (is (= (play {:supply  [{:pile-size 0} {:pile-size 1}]
                  :players [{:deck    [copper copper]
                             :hand    [poacher estate]
                             :actions 1
                             :coins   0}]}
                 0 :poacher)
           {:supply       [{:pile-size 0} {:pile-size 1}]
            :players      [{:deck      [copper]
                            :hand      [estate copper]
                            :play-area [poacher]
                            :actions   1
                            :coins     1}]
            :effect-stack [{:text      "Discard a card per empty supply pile [1]."
                            :player-no 0
                            :choice-fn discard-from-hand
                            :options   [:estate :copper]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 1}]
                :players [{:deck    [copper copper]
                           :hand    [poacher estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :poacher)
               (chose [:estate]))
           {:supply  [{:pile-size 0} {:pile-size 1}]
            :players [{:deck      [copper]
                       :discard   [estate]
                       :hand      [copper]
                       :play-area [poacher]
                       :actions   1
                       :coins     1}]}))
    (is (= (play {:supply  [{:pile-size 0} {:pile-size 0}]
                  :players [{:deck    [copper copper]
                             :hand    [poacher estate silver]
                             :actions 1
                             :coins   0}]}
                 0 :poacher)
           {:supply       [{:pile-size 0} {:pile-size 0}]
            :players      [{:deck      [copper]
                            :hand      [estate silver copper]
                            :play-area [poacher]
                            :actions   1
                            :coins     1}]
            :effect-stack [{:text      "Discard a card per empty supply pile [2]."
                            :player-no 0
                            :choice-fn discard-from-hand
                            :options   [:estate :silver :copper]
                            :min       2
                            :max       2}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 0}]
                :players [{:deck    [copper copper]
                           :hand    [poacher estate silver]
                           :actions 1
                           :coins   0}]}
               (play 0 :poacher)
               (chose [:copper :estate]))
           {:supply  [{:pile-size 0} {:pile-size 0}]
            :players [{:hand      [silver]
                       :deck      [copper]
                       :discard   [copper estate]
                       :play-area [poacher]
                       :actions   1
                       :coins     1}]}))
    (is (= (play {:supply  [{:pile-size 0}]
                  :players [{:deck    []
                             :hand    [poacher]
                             :actions 1
                             :coins   0}]}
                 0 :poacher)
           {:supply  [{:pile-size 0}]
            :players [{:deck      []
                       :hand      []
                       :play-area [poacher]
                       :actions   1
                       :coins     1}]}))
    (is (= (play {:supply  [{:pile-size 0} {:pile-size 0}]
                  :players [{:deck    [copper copper]
                             :hand    [poacher]
                             :actions 1
                             :coins   0}]}
                 0 :poacher)
           {:supply       [{:pile-size 0} {:pile-size 0}]
            :players      [{:deck      [copper]
                            :hand      [copper]
                            :play-area [poacher]
                            :actions   1
                            :coins     1}]
            :effect-stack [{:text      "Discard a card per empty supply pile [2]."
                            :player-no 0
                            :choice-fn discard-from-hand
                            :options   [:copper]
                            :min       1
                            :max       1}]}))
    (is (= (play {:mode    :swift
                  :supply  [{:pile-size 0} {:pile-size 0}]
                  :players [{:deck    [copper copper]
                             :hand    [poacher]
                             :actions 1
                             :coins   0}]}
                 0 :poacher)
           {:mode    :swift
            :supply  [{:pile-size 0} {:pile-size 0}]
            :players [{:hand      []
                       :deck      [copper]
                       :play-area [poacher]
                       :discard   [copper]
                       :actions   1
                       :coins     1}]}))))

(deftest remodel-test
  (testing "Remodel"
    (is (= (play {:players [{:hand    [remodel copper estate]
                             :actions 1}]}
                 0 :remodel)
           {:players      [{:hand      [copper estate]
                            :play-area [remodel]
                            :actions   0}]
            :effect-stack [{:text      "Trash a card from your hand."
                            :player-no 0
                            :choice-fn remodel-trash
                            :options   [:copper :estate]
                            :min       1
                            :max       1}]}))
    (is (= (play {:players [{:hand    [remodel]
                             :actions 1}]}
                 0 :remodel)
           {:players [{:hand      []
                       :play-area [remodel]
                       :actions   0}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [remodel copper estate]
                           :actions 1}]}
               (play 0 :remodel)
               (chose :estate))
           {:supply       (base/supply 2 8)
            :players      [{:hand      [copper]
                            :play-area [remodel]
                            :actions   0}]
            :effect-stack [{:text      "Gain a card costing up to $4."
                            :player-no 0
                            :choice-fn gain
                            :options   [:curse :estate :copper :silver]
                            :min       1
                            :max       1}]
            :trash        [estate]}))
    (is (= (-> {:supply  [{:card estate :pile-size 0} {:card silver :pile-size 40}]
                :players [{:hand    [remodel copper estate]
                           :actions 1}]}
               (play 0 :remodel)
               (chose :copper))
           {:supply  [{:card estate :pile-size 0} {:card silver :pile-size 40}]
            :players [{:hand      [estate]
                       :play-area [remodel]
                       :actions   0}]
            :trash   [copper]}))
    (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                :players [{:hand    [remodel silver estate]
                           :actions 1}]}
               (play 0 :remodel)
               (chose :silver)
               (chose :duchy))
           {:supply  [{:card duchy :pile-size 7}]
            :players [{:hand      [estate]
                       :play-area [remodel]
                       :discard   [duchy]
                       :actions   0}]
            :trash   [silver]}))))

(deftest sentry-test
  (testing "Sentry"
    (is (= (play {:players [{:deck    [copper silver estate gold]
                             :hand    [sentry]
                             :actions 1}]}
                 0 :sentry)
           {:players      [{:hand      [copper]
                            :play-area [sentry]
                            :look-at   [silver estate]
                            :deck      [gold]
                            :actions   1}]
            :effect-stack [{:text      "Trash any number of the top 2 cards of your deck."
                            :player-no 0
                            :choice-fn sentry-trash
                            :options   [:silver :estate]}]}))
    (is (= (-> {:players [{:deck    [copper silver estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (chose :estate))
           {:players      [{:hand      [copper]
                            :play-area [sentry]
                            :look-at   [silver]
                            :deck      [gold]
                            :actions   1}]
            :effect-stack [{:text      "Discard any number of the top 2 cards of your deck."
                            :player-no 0
                            :choice-fn sentry-discard
                            :options   [:silver]}]
            :trash        [estate]}))
    (is (= (-> {:players [{:deck    [copper silver estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (chose :estate)
               (chose nil))
           {:players      [{:hand      [copper]
                            :play-area [sentry]
                            :look-at   [silver]
                            :deck      [gold]
                            :actions   1}]
            :effect-stack [{:text      "Put the rest back on top in any order."
                            :player-no 0
                            :choice-fn sentry-topdeck
                            :options   [:silver]
                            :min       1}]
            :trash        [estate]}))
    (is (= (-> {:players [{:deck    [copper silver estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (chose :estate)
               (chose nil)
               (chose :silver))
           {:players [{:hand      [copper]
                       :play-area [sentry]
                       :look-at   []
                       :deck      [silver gold]
                       :actions   1}]
            :trash   [estate]}))
    (is (= (-> {:mode    :swift
                :players [{:deck    [copper silver estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (chose :estate)
               (chose nil))
           {:mode    :swift
            :players [{:hand      [copper]
                       :play-area [sentry]
                       :look-at   []
                       :deck      [silver gold]
                       :actions   1}]
            :trash   [estate]}))
    (is (= (-> {:players [{:deck    [silver copper estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (chose [:estate :copper]))
           {:players [{:hand      [silver]
                       :play-area [sentry]
                       :look-at   []
                       :deck      [gold]
                       :actions   1}]
            :trash   [estate copper]}))
    (is (= (-> {:players [{:deck    [silver province province gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (chose [])
               (chose [:province :province]))
           {:players [{:hand      [silver]
                       :play-area [sentry]
                       :look-at   []
                       :deck      [gold]
                       :discard   [province province]
                       :actions   1}]}))
    (is (= (-> {:players [{:deck    [market silver market gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (chose [])
               (chose [])
               (chose [:silver :market]))
           {:players [{:hand      [market]
                       :play-area [sentry]
                       :look-at   []
                       :deck      [market silver gold]
                       :actions   1}]}))
    (is (= (-> {:players [{:deck    [market copper]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry))
           {:players      [{:hand      [market]
                            :play-area [sentry]
                            :look-at   [copper]
                            :deck      []
                            :actions   1}]
            :effect-stack [{:text      "Trash any number of the top 2 cards of your deck."
                            :player-no 0
                            :choice-fn sentry-trash
                            :options   [:copper]}]}))))

(deftest smithy-test
  (testing "Smithy"
    (is (= (play {:players [{:deck    [copper copper copper]
                             :hand    [smithy]
                             :actions 1}]}
                 0 :smithy)
           {:players [{:deck      []
                       :hand      [copper copper copper]
                       :play-area [smithy]
                       :actions   0}]}))))

(deftest throne-room-test
  (testing "Throne Room"
    (is (= (play {:players [{:deck    [copper copper copper]
                             :hand    [throne-room market copper]
                             :actions 1}]}
                 0 :throne-room)
           {:players      [{:deck      [copper copper copper]
                            :hand      [market copper]
                            :play-area [throne-room]
                            :actions   0}]
            :effect-stack [{:text      "You may play an Action card from your hand twice."
                            :player-no 0
                            :choice-fn play-action-twice
                            :options   [:market]
                            :max       1}]}))
    (is (= (-> {:players [{:deck    [copper copper copper]
                           :hand    [throne-room market copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :throne-room)
               (chose :market))
           {:players [{:deck      [copper]
                       :hand      [copper copper copper]
                       :play-area [throne-room market]
                       :actions   2
                       :coins     2
                       :buys      3}]}))
    (is (= (-> {:players [{:deck    [copper copper copper]
                           :hand    [throne-room market copper]
                           :actions 1}]}
               (play 0 :throne-room)
               (chose nil))
           {:players [{:deck      [copper copper copper]
                       :hand      [market copper]
                       :play-area [throne-room]
                       :actions   0}]}))
    (is (= (-> {:players [{:deck    [witch copper copper silver]
                           :hand    [throne-room throne-room merchant]
                           :actions 1}]}
               (play 0 :throne-room)
               (chose :throne-room))
           {:players      [{:deck      [witch copper copper silver]
                            :hand      [merchant]
                            :play-area [throne-room throne-room]
                            :actions   0}]
            :effect-stack [{:text      "You may play an Action card from your hand twice."
                            :player-no 0
                            :choice-fn play-action-twice
                            :options   [:merchant]
                            :max       1}
                           (assoc throne-room :player-no 0)]}))
    (is (= (-> {:players [{:deck    [witch copper copper silver]
                           :hand    [throne-room throne-room merchant]
                           :actions 1}]}
               (play 0 :throne-room)
               (chose :throne-room)
               (chose :merchant))
           {:players      [{:deck      [copper silver]
                            :hand      [witch copper]
                            :play-area [throne-room throne-room merchant]
                            :triggers  [merchant-trigger merchant-trigger]
                            :actions   2}]
            :effect-stack [{:text      "You may play an Action card from your hand twice."
                            :player-no 0
                            :choice-fn play-action-twice
                            :options   [:witch]
                            :max       1}]}))
    (is (= (-> {:supply  [{:card curse :pile-size 10}]
                :players [{:deck    [witch copper copper silver]
                           :hand    [throne-room throne-room merchant]
                           :actions 1
                           :coins   0}
                          {}]}
               (play 0 :throne-room)
               (chose :throne-room)
               (chose :merchant)
               (chose :witch)
               (play-treasures 0))
           {:supply  [{:card curse :pile-size 8}]
            :players [{:deck      []
                       :hand      []
                       :play-area [throne-room throne-room merchant witch copper copper silver]
                       :triggers  []
                       :actions   2
                       :coins     6}
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
           {:players      [{:hand      []
                            :play-area [vassal]
                            :deck      [copper]
                            :discard   [market copper market]
                            :actions   0
                            :coins     2}]
            :effect-stack [{:text      "You may play the discarded Market."
                            :player-no 0
                            :choice-fn vassal-play-action
                            :options   [:market]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [vassal]
                           :deck    [market copper]
                           :discard [market copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :vassal)
               (chose :market))
           {:players [{:hand      [copper]
                       :play-area [vassal market]
                       :deck      []
                       :discard   [market copper]
                       :actions   1
                       :coins     3
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [vassal]
                           :deck    [market copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :vassal)
               (chose nil))
           {:players [{:hand      []
                       :play-area [vassal]
                       :deck      [copper]
                       :discard   [market]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:supply  [{:card curse :pile-size 10}]
                :players [{:hand    [vassal]
                           :deck    [witch copper copper copper]
                           :actions 1
                           :coins   0}
                          {}]}
               (play 0 :vassal)
               (chose :witch))
           {:supply  [{:card curse :pile-size 9}]
            :players [{:hand      [copper copper]
                       :deck      [copper]
                       :discard   []
                       :play-area [vassal witch]
                       :actions   0
                       :coins     2}
                      {:discard [curse]}]}))))

(deftest village-test
  (testing "Village"
    (is (= (play {:players [{:deck    [copper copper]
                             :hand    [village]
                             :actions 1}]}
                 0 :village)
           {:players [{:deck      [copper]
                       :hand      [copper]
                       :play-area [village]
                       :actions   2}]}))))
(deftest witch-test
  (testing "Witch"
    (is (= (play {:supply  [{:card curse :pile-size 20}]
                  :players [{:deck    (repeat 3 copper)
                             :hand    [witch]
                             :actions 1}
                            {:discard [copper copper]}
                            {:discard []}]}
                 0 :witch)
           {:supply  [{:card curse :pile-size 18}]
            :players [{:deck      [copper]
                       :hand      [copper copper]
                       :play-area [witch]
                       :actions   0}
                      {:discard [copper copper curse]}
                      {:discard [curse]}]}))
    (is (= (play {:supply  [{:card curse :pile-size 1}]
                  :players [{:deck    (repeat 3 copper)
                             :hand    [witch]
                             :actions 1}
                            {:discard [copper copper]}
                            {:discard []}]}
                 0 :witch)
           {:supply  [{:card curse :pile-size 0}]
            :players [{:deck      [copper]
                       :hand      [copper copper]
                       :play-area [witch]
                       :actions   0}
                      {:discard [copper copper curse]}
                      {:discard []}]}))
    (is (= (play {:supply  [{:card curse :pile-size 1}]
                  :players [{:discard [copper copper]}
                            {:deck    (repeat 3 copper)
                             :hand    [witch]
                             :actions 1}
                            {:discard []}]}
                 1 :witch)
           {:supply  [{:card curse :pile-size 0}]
            :players [{:discard [copper copper]}
                      {:deck      [copper]
                       :hand      [copper copper]
                       :play-area [witch]
                       :actions   0}
                      {:discard [curse]}]}))))

(deftest woodcutter-test
  (testing "Woodcutter"
    (is (= (play {:players [{:deck    [copper]
                             :hand    [woodcutter]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 0 :woodcutter)
           {:players [{:deck      [copper]
                       :hand      []
                       :play-area [woodcutter]
                       :actions   0
                       :coins     2
                       :buys      2}]}))))
(deftest workshop-test
  (testing "Workshop"
    (is (= (play {:supply  (base/supply 2 8)
                  :players [{:hand    [workshop copper]
                             :actions 1}]}
                 0 :workshop)
           {:supply       (base/supply 2 8)
            :players      [{:hand      [copper]
                            :play-area [workshop]
                            :actions   0}]
            :effect-stack [{:text      "Gain a card costing up to $4."
                            :player-no 0
                            :choice-fn gain
                            :options   [:curse :estate :copper :silver]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  [{:card silver :pile-size 40}]
                :players [{:hand    [workshop copper]
                           :actions 1}]}
               (play 0 :workshop)
               (chose :silver))
           {:supply  [{:card silver :pile-size 39}]
            :players [{:hand      [copper]
                       :discard   [silver]
                       :play-area [workshop]
                       :actions   0}]}))
    (is (= (-> {:supply  [{:card province :pile-size 8}]
                :players [{:hand    [workshop copper]
                           :actions 1}]}
               (play 0 :workshop))
           {:supply  [{:card province :pile-size 8}]
            :players [{:hand      [copper]
                       :play-area [workshop]
                       :actions   0}]}))))
