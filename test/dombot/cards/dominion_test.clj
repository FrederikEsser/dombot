(ns dombot.cards.dominion-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :as dominion :refer :all]
            [dombot.cards.intrigue :as intrigue :refer [minion]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest artisan-test
  (let [duchy (assoc duchy :id 1)]
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
                              :choice    :gain-to-hand
                              :source    :supply
                              :options   [:curse :estate :duchy :copper :silver]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:give-choice {:text    "Put a card from your hand onto your deck."
                                                        :choice  :topdeck-from-hand
                                                        :options [:player :hand]
                                                        :min     1
                                                        :max     1}]}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [artisan silver]
                             :actions 1}]}
                 (play 0 :artisan)
                 (choose :duchy))
             {:supply       [{:card duchy :pile-size 7}]
              :players      [{:hand      [silver duchy]
                              :play-area [artisan]
                              :actions   0}]
              :effect-stack [{:text      "Put a card from your hand onto your deck."
                              :player-no 0
                              :choice    :topdeck-from-hand
                              :source    :hand
                              :options   [:silver :duchy]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [artisan silver]
                             :deck    [gold]
                             :actions 1}]}
                 (play 0 :artisan)
                 (choose :duchy)
                 (choose :silver))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:hand      [duchy]
                         :play-area [artisan]
                         :deck      [silver gold]
                         :actions   0}]}))
      (is (= (play {:supply  []                             ; totally hypothetical supply with no cards costing 5 or less
                    :players [{:hand    [artisan silver]
                               :actions 1}]}
                   0 :artisan)
             {:supply       []
              :players      [{:hand      [silver]
                              :play-area [artisan]
                              :actions   0}]
              :effect-stack [{:text      "Put a card from your hand onto your deck."
                              :player-no 0
                              :choice    :topdeck-from-hand
                              :source    :hand
                              :options   [:silver]
                              :min       1
                              :max       1}]})))))

(deftest bandit-test
  (let [gold (assoc gold :id 1)]
    (testing "Bandit"
      (is (= (play {:supply  [{:card gold :pile-size 30}]
                    :players [{:hand    [bandit]
                               :actions 1}
                              {:deck [estate estate estate]}]}
                   0 :bandit)
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [bandit]
                         :discard   [gold]
                         :actions   0}
                        {:deck           [estate]
                         :discard        [estate estate]
                         :revealed-cards {:discard 2}}]}))
      (is (= (play {:supply  [{:card gold :pile-size 30}]
                    :players [{:hand    [bandit]
                               :actions 1}
                              {:deck [estate silver gold]}]}
                   0 :bandit)
             {:supply       [{:card gold :pile-size 29}]
              :players      [{:play-area [bandit]
                              :discard   [gold]
                              :actions   0}
                             {:deck     [gold]
                              :revealed [estate silver]}]
              :effect-stack [{:text                "Trash a revealed Treasure other than Copper, and discards the rest."
                              :player-no           1
                              :attacking-player-no 0
                              :choice              :trash-from-revealed
                              :source              :revealed
                              :options             [:silver]
                              :min                 1
                              :max                 1}
                             {:player-no 1
                              :effect    [:discard-all-revealed {:attacking-player-no 0}]}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [bandit]
                             :actions 1}
                            {:deck [estate silver gold]}]}
                 (play 0 :bandit)
                 (choose :silver))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [bandit]
                         :discard   [gold]
                         :actions   0}
                        {:deck           [gold]
                         :revealed-cards {:discard 1}
                         :discard        [estate]}]
              :trash   [silver]}))
      (is (= (-> {:mode    :swift
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [bandit]
                             :actions 1}
                            {:deck [estate silver gold]}]}
                 (play 0 :bandit))
             {:mode    :swift
              :supply  [{:card gold :pile-size 29}]
              :players [{:play-area [bandit]
                         :discard   [gold]
                         :actions   0}
                        {:deck           [gold]
                         :revealed-cards {:discard 1}
                         :discard        [estate]}]
              :trash   [silver]}))
      (is (= (play {:supply  [{:card gold :pile-size 30}]
                    :players [{:hand    [bandit]
                               :actions 1}
                              {:deck [silver gold estate]}
                              {:deck [copper gold estate]}]}
                   0 :bandit)
             {:supply       [{:card gold :pile-size 29}]
              :players      [{:play-area [bandit]
                              :discard   [gold]
                              :actions   0}
                             {:deck     [estate]
                              :revealed [silver gold]}
                             {:deck [copper gold estate]}]
              :effect-stack [{:text                "Trash a revealed Treasure other than Copper, and discards the rest."
                              :player-no           1
                              :attacking-player-no 0
                              :choice              :trash-from-revealed
                              :source              :revealed
                              :options             [:silver :gold]
                              :min                 1
                              :max                 1}
                             {:player-no 1
                              :effect    [:discard-all-revealed {:attacking-player-no 0}]}
                             {:player-no 2
                              :effect    [:reveal-from-deck {:arg                 2
                                                             :attacking-player-no 0}]}
                             {:player-no 2
                              :effect    [:give-choice
                                          {:attacking-player-no 0
                                           :text                "Trash a revealed Treasure other than Copper, and discards the rest."
                                           :choice              :trash-from-revealed
                                           :options             [:player :revealed {:not-names #{:copper} :type :treasure}]
                                           :max                 1
                                           :min                 1}]}
                             {:player-no 2
                              :effect    [:discard-all-revealed {:attacking-player-no 0}]}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}
                             {:player-no 2
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [bandit]
                             :actions 1}
                            {:deck [silver gold estate]}
                            {:deck [copper gold estate]}]}
                 (play 0 :bandit)
                 (choose :silver))
             {:supply       [{:card gold :pile-size 29}]
              :players      [{:play-area [bandit]
                              :discard   [gold]
                              :actions   0}
                             {:deck           [estate]
                              :discard        [gold]
                              :revealed-cards {:discard 1}}
                             {:deck     [estate]
                              :revealed [copper gold]}]
              :effect-stack [{:text                "Trash a revealed Treasure other than Copper, and discards the rest."
                              :player-no           2
                              :attacking-player-no 0
                              :choice              :trash-from-revealed
                              :source              :revealed
                              :options             [:gold]
                              :min                 1
                              :max                 1}
                             {:player-no 2
                              :effect    [:discard-all-revealed {:attacking-player-no 0}]}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}
                             {:player-no 2
                              :effect    [:clear-unaffected {:works :once}]}]
              :trash        [silver]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [bandit]
                             :actions 1}
                            {:deck [silver]}]}
                 (play 0 :bandit)
                 (choose :silver))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [bandit]
                         :discard   [gold]
                         :actions   0}
                        {}]
              :trash   [silver]})))))

(deftest bureaucrat-test
  (let [silver (assoc silver :id 1)]
    (testing "Bureaucrat"
      (is (= (play {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [bureaucrat]
                               :deck    [copper]
                               :actions 1}
                              {:hand (repeat 5 copper)}]}
                   0 :bureaucrat)
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [bureaucrat]
                         :deck      [silver copper]
                         :actions   0}
                        {:hand           (repeat 5 copper)
                         :revealed-cards {:hand 5}}]}))
      (is (= (play {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [bureaucrat]
                               :deck    [copper]
                               :actions 1}
                              {:hand [copper copper copper estate estate]}]}
                   0 :bureaucrat)
             {:supply       [{:card silver :pile-size 39}]
              :players      [{:play-area [bureaucrat]
                              :deck      [silver copper]
                              :actions   0}
                             {:hand [copper copper copper estate estate]}]
              :effect-stack [{:text      "Reveal a Victory card from your hand and put it onto your deck."
                              :player-no 1
                              :choice    ::dominion/bureaucrat-topdeck-victory
                              :source    :hand
                              :options   [:estate :estate]
                              :min       1
                              :max       1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [bureaucrat]
                             :deck    [copper]
                             :actions 1}
                            {:hand [copper copper copper estate estate]
                             :deck [gold]}]}
                 (play 0 :bureaucrat)
                 (choose :estate))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [bureaucrat]
                         :deck      [silver copper]
                         :actions   0}
                        {:hand           [copper copper copper estate]
                         :deck           [estate gold]
                         :revealed-cards {:deck 1}}]}))
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
              :players [{:play-area [bureaucrat]
                         :deck      [silver copper]
                         :actions   0}
                        {:hand           [copper copper copper estate]
                         :deck           [estate gold]
                         :revealed-cards {:deck 1}}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [bureaucrat militia]
                             :deck    [copper]
                             :actions 2
                             :coins   0}
                            {:hand (repeat 5 copper)}]}
                 (play 0 :bureaucrat)
                 (play 0 :militia)
                 (choose [:copper :copper]))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [bureaucrat militia]
                         :deck      [silver copper]
                         :actions   0
                         :coins     2}
                        {:hand    [copper copper copper]
                         :discard [copper copper]}]})))))

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
                            :choice    ::dominion/cellar-sift
                            :source    :hand
                            :options   [:copper :estate :estate :estate]}]}))
    (is (= (-> {:players [{:hand    [cellar copper estate estate estate]
                           :deck    (repeat 5 copper)
                           :actions 1}]}
               (play 0 :cellar)
               (choose [:estate :estate :estate]))
           {:players [{:hand      [copper copper copper copper]
                       :play-area [cellar]
                       :deck      [copper copper]
                       :discard   [estate estate estate]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [cellar copper]
                           :actions 1}]}
               (play 0 :cellar)
               (choose :copper))
           {:players [{:hand      [copper]
                       :play-area [cellar]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [cellar copper estate estate estate]
                           :deck    (repeat 5 copper)
                           :actions 1}]}
               (play 0 :cellar)
               (choose []))
           {:players [{:hand      [copper estate estate estate]
                       :play-area [cellar]
                       :deck      (repeat 5 copper)
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [cellar]
                           :deck    (repeat 5 copper)
                           :actions 1}]}
               (play 0 :cellar))
           {:players [{:play-area [cellar]
                       :deck      (repeat 5 copper)
                       :actions   1}]}))
    (is (thrown? AssertionError
                 (-> {:players [{:hand    [cellar copper estate]
                                 :deck    (repeat 5 copper)
                                 :actions 1}]}
                     (play 0 :cellar)
                     (choose [:estate :estate]))))
    (is (= (-> {:players [{:hand    [cellar estate estate estate]
                           :discard [copper copper copper]
                           :actions 1}]}
               (play 0 :cellar)
               (choose [:estate :estate :estate]))
           {:players [{:hand      [estate copper estate]
                       :play-area [cellar]
                       :deck      [estate copper copper]
                       :actions   1}]}))))

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
                            :choice    :trash-from-hand
                            :source    :hand
                            :options   [:copper :estate :estate :estate]
                            :max       4}]}))
    (is (= (-> {:players [{:hand    [chapel copper estate estate estate]
                           :actions 1}]}
               (play 0 :chapel)
               (choose [:estate :estate :estate]))
           {:players [{:hand      [copper]
                       :play-area [chapel]
                       :actions   0}]
            :trash   [estate estate estate]}))
    (is (= (-> {:players [{:hand    [chapel copper]
                           :actions 1}]}
               (play 0 :chapel)
               (choose :copper))
           {:players [{:play-area [chapel]
                       :actions   0}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [chapel copper estate estate estate]
                           :actions 1}]}
               (play 0 :chapel)
               (choose []))
           {:players [{:hand      [copper estate estate estate]
                       :play-area [chapel]
                       :actions   0}]}))
    (is (thrown-with-msg? AssertionError #"Choose error: You can only pick 4 options."
                          (-> {:players [{:hand    (concat [chapel] (repeat 5 copper))
                                          :actions 1}]}
                              (play 0 :chapel)
                              (choose (repeat 5 :copper)))))))

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
                       :hand [copper]}]}))
    (is (= (play {:players [{:deck    (repeat 5 copper)
                             :hand    [council-room]
                             :actions 1
                             :buys    1}
                            {:deck       [copper copper]
                             :unaffected true}]}
                 0 :council-room)
           {:players [{:deck      [copper]
                       :hand      (repeat 4 copper)
                       :play-area [council-room]
                       :actions   0
                       :buys      2}
                      {:deck       [copper]
                       :hand       [copper]
                       :unaffected true}]}))))

(deftest festival-test
  (testing "Festival"
    (is (= (play {:players [{:deck    [copper]
                             :hand    [festival]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 0 :festival)
           {:players [{:deck      [copper]
                       :play-area [festival]
                       :actions   2
                       :coins     2
                       :buys      2}]}))))

(deftest gardens-test
  (testing "Gardens"
    (is (= (calc-victory-points {:deck (repeat 9 gardens)})
           0))
    (is (= (calc-victory-points {:deck (repeat 10 gardens)})
           10))
    (is (= (calc-victory-points {:deck (concat (repeat 10 gardens)
                                               (repeat 9 copper))})
           10))
    (is (= (calc-victory-points {:deck (concat (repeat 10 gardens)
                                               (repeat 10 copper))})
           20))))

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
                            :choice    :topdeck-from-discard
                            :source    :discard
                            :options   [:gold]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [harbinger]
                           :deck    [copper copper copper]
                           :discard [gold]
                           :actions 1}]}
               (play 0 :harbinger)
               (choose :gold))
           {:players [{:hand      [copper]
                       :deck      [gold copper copper]
                       :play-area [harbinger]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [harbinger]
                           :deck    [copper copper copper]
                           :discard [estate]
                           :actions 1}]}
               (play 0 :harbinger)
               (choose nil))
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
                       :actions   1}]}))
    (is (= (-> {:players [{:hand                [harbinger]
                           :deck                [copper copper copper]
                           :discard             (repeat 10 estate)
                           :approx-discard-size 11
                           :actions             1}]}
               (play 0 :harbinger))
           {:players      [{:hand                [copper]
                            :deck                [copper copper]
                            :discard             (repeat 10 estate)
                            :revealed-cards      {:discard 10}
                            :approx-discard-size 10
                            :play-area           [harbinger]
                            :actions             1}]
            :effect-stack [{:text      "You may put a card from your discard pile onto your deck."
                            :player-no 0
                            :choice    :topdeck-from-discard
                            :source    :discard
                            :options   (repeat 10 :estate)
                            :max       1}]}))))

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
                            :choice    ::dominion/library-set-aside
                            :source    :hand
                            :options   [:village]
                            :max       1}
                           {:player-no 0
                            :effect    [::dominion/library-draw]}
                           {:player-no 0
                            :effect    [:discard-all-set-aside]}]}))
    (is (= (-> {:players [{:hand    [smithy library copper]
                           :deck    [silver gold village estate duchy smithy province]
                           :actions 1}]}
               (play 0 :library)
               (choose nil))
           {:players [{:hand      [smithy copper silver gold village estate duchy]
                       :play-area [library]
                       :deck      [smithy province]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [smithy library copper]
                           :deck    [silver gold village estate duchy smithy province]
                           :actions 1}]}
               (play 0 :library)
               (choose :village))
           {:players      [{:hand      [smithy copper silver gold estate duchy smithy]
                            :play-area [library]
                            :set-aside [village]
                            :deck      [province]
                            :actions   0}]
            :effect-stack [{:text      "You may skip the Smithy; set it aside, discarding it afterwards."
                            :player-no 0
                            :choice    ::dominion/library-set-aside
                            :source    :hand
                            :options   [:smithy]
                            :max       1}
                           {:player-no 0
                            :effect    [::dominion/library-draw]}
                           {:player-no 0
                            :effect    [:discard-all-set-aside]}]}))
    (is (= (-> {:players [{:hand    [smithy library copper]
                           :deck    [silver gold village estate duchy smithy province]
                           :actions 1}]}
               (play 0 :library)
               (choose :village)
               (choose :smithy))
           {:players [{:hand      [smithy copper silver gold estate duchy province]
                       :play-area [library]
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
                       :play-area [merchant silver]
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
                       :play-area [merchant silver silver]
                       :actions   1
                       :coins     5}]}))
    (is (= (-> {:players [{:deck    [silver copper]
                           :hand    [merchant merchant]
                           :actions 1
                           :coins   0}]}
               (play 0 :merchant)
               (play 0 :merchant)
               (play 0 :silver))
           {:players [{:hand      [copper]
                       :play-area [merchant merchant silver]
                       :actions   1
                       :coins     4}]}))
    (is (= (-> {:players [{:triggers [merchant-trigger]}]}
               (clean-up {:player-no 0}))
           {:players [{:actions 0
                       :coins   0
                       :buys    0
                       :phase   :out-of-turn}]}))))

(deftest militia-test
  (testing "Militia"
    (is (= (play {:players [{:hand    [militia]
                             :actions 1
                             :coins   0}
                            {:hand (repeat 5 copper)}]}
                 0 :militia)
           {:players      [{:play-area [militia]
                            :actions   0
                            :coins     2}
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
    (is (= (-> {:players [{:hand    [militia]
                           :actions 1
                           :coins   0}
                          {:hand (repeat 5 copper)}]}
               (play 0 :militia)
               (choose [:copper :copper]))
           {:players [{:play-area [militia]
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
            :players [{:play-area [militia]
                       :actions   0
                       :coins     2}
                      {:hand    (repeat 3 copper)
                       :discard [copper copper]}]}))
    (is (= (-> {:players [{:hand    [militia]
                           :actions 1
                           :coins   0}
                          {:hand (repeat 6 copper)}]}
               (play 0 :militia)
               (choose [:copper :copper :copper]))
           {:players [{:play-area [militia]
                       :actions   0
                       :coins     2}
                      {:hand    (repeat 3 copper)
                       :discard (repeat 3 copper)}]}))))

(deftest mine-test
  (let [gold (assoc gold :id 1)]
    (testing "Mine"
      (is (= (play {:players [{:hand    [mine copper estate]
                               :actions 1}]}
                   0 :mine)
             {:players      [{:hand      [copper estate]
                              :play-area [mine]
                              :actions   0}]
              :effect-stack [{:text      "You may trash a Treasure from your hand."
                              :player-no 0
                              :choice    ::dominion/mine-trash
                              :source    :hand
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
                 (choose :copper))
             {:supply       (base/supply 2 8)
              :players      [{:hand      [estate]
                              :play-area [mine]
                              :actions   0}]
              :effect-stack [{:text      "Gain a Treasure to your hand costing up to $3."
                              :player-no 0
                              :choice    :gain-to-hand
                              :source    :supply
                              :options   [:copper :silver]
                              :min       1
                              :max       1}]
              :trash        [copper]}))
      (is (= (-> {:supply  [{:card copper :pile-size 0} {:card gold :pile-size 30}]
                  :players [{:hand    [mine copper estate]
                             :actions 1}]}
                 (play 0 :mine)
                 (choose :copper))
             {:supply  [{:card copper :pile-size 0} {:card gold :pile-size 30}]
              :players [{:hand      [estate]
                         :play-area [mine]
                         :actions   0}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [mine silver estate]
                             :actions 1}]}
                 (play 0 :mine)
                 (choose :silver)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:hand      [estate gold]
                         :play-area [mine]
                         :actions   0}]
              :trash   [silver]}))
      (testing "with cost reduction"
        (is (= (-> {:supply          (base/supply 2 8)
                    :cost-reductions [{:reduction 2}]
                    :players         [{:hand    [mine copper estate]
                                       :actions 1}]}
                   (play 0 :mine)
                   (choose :copper))
               {:supply          (base/supply 2 8)
                :cost-reductions [{:reduction 2}]
                :players         [{:hand      [estate]
                                   :play-area [mine]
                                   :actions   0}]
                :effect-stack    [{:text      "Gain a Treasure to your hand costing up to $3."
                                   :player-no 0
                                   :choice    :gain-to-hand
                                   :source    :supply
                                   :options   [:copper :silver]
                                   :min       1
                                   :max       1}]
                :trash           [copper]}))
        (is (= (-> {:supply          (base/supply 2 8)
                    :cost-reductions [{:reduction 2}]
                    :players         [{:hand    [mine silver estate]
                                       :actions 1}]}
                   (play 0 :mine)
                   (choose :silver))
               {:supply          (base/supply 2 8)
                :cost-reductions [{:reduction 2}]
                :players         [{:hand      [estate]
                                   :play-area [mine]
                                   :actions   0}]
                :effect-stack    [{:text      "Gain a Treasure to your hand costing up to $4."
                                   :player-no 0
                                   :choice    :gain-to-hand
                                   :source    :supply
                                   :options   [:copper :silver :gold]
                                   :min       1
                                   :max       1}]
                :trash           [silver]}))
        (is (= (-> {:supply          (base/supply 2 8)
                    :cost-reductions [{:reduction 3}]
                    :players         [{:hand    [mine copper estate]
                                       :actions 1}]}
                   (play 0 :mine)
                   (choose :copper))
               {:supply          (base/supply 2 8)
                :cost-reductions [{:reduction 3}]
                :players         [{:hand      [estate]
                                   :play-area [mine]
                                   :actions   0}]
                :effect-stack    [{:text      "Gain a Treasure to your hand costing up to $3."
                                   :player-no 0
                                   :choice    :gain-to-hand
                                   :source    :supply
                                   :options   [:copper :silver :gold]
                                   :min       1
                                   :max       1}]
                :trash           [copper]}))))))

(deftest moat-test
  (let [moat (assoc moat :id 1)]
    (testing "Moat"
      (testing "Action"
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
                           :play-area [moat]
                           :actions   0}]})))
      (testing "Reaction"
        (is (= (play {:players [{:hand    [militia]
                                 :actions 1
                                 :coins   0}
                                {:hand [moat]}]}
                     0 :militia)
               {:players      [{:play-area [militia]
                                :actions   0
                                :coins     0}
                               {:hand [moat]}]
                :effect-stack [{:text      "You may reveal a Reaction to react to the Attack."
                                :player-no 1
                                :choice    :reveal-reaction
                                :source    :hand
                                :options   [:moat]
                                :max       1}
                               {:player-no 0
                                :effect    [:give-coins 2]}
                               {:player-no 0
                                :effect    [:attack {:effects [[:discard-down-to 3]]}]}
                               {:player-no 1
                                :effect    [:clear-unaffected {:works :once}]}]}))
        (is (= (-> {:players [{:hand    [militia]
                               :actions 1
                               :coins   0}
                              {:hand [moat copper copper copper copper]}]}
                   (play 0 :militia)
                   (choose :moat))
               {:players [{:play-area [militia]
                           :actions   0
                           :coins     2}
                          {:hand [moat copper copper copper copper]}]}))
        (is (= (-> {:players [{:hand    [militia]
                               :actions 1
                               :coins   0}
                              {:hand [moat copper copper copper copper]}]}
                   (play 0 :militia)
                   (choose nil))
               {:players      [{:play-area [militia]
                                :actions   0
                                :coins     2}
                               {:hand [moat copper copper copper copper]}]
                :effect-stack [{:text      "Discard down to 3 cards in hand."
                                :player-no 1
                                :choice    :discard-from-hand
                                :source    :hand
                                :options   [:moat :copper :copper :copper :copper]
                                :min       2
                                :max       2}
                               {:player-no 1
                                :effect    [:clear-unaffected {:works :once}]}]}))
        (is (= (-> {:players [{:hand    [vassal]
                               :deck    [militia]
                               :actions 1
                               :coins   0}
                              {:hand [moat]}]}
                   (play 0 :vassal)
                   (choose :militia))
               {:players      [{:play-area [vassal militia]
                                :actions   0
                                :coins     2}
                               {:hand [moat]}]
                :effect-stack [{:text      "You may reveal a Reaction to react to the Attack."
                                :player-no 1
                                :choice    :reveal-reaction
                                :source    :hand
                                :options   [:moat]
                                :max       1}
                               {:player-no 0
                                :effect    [:give-coins 2]}
                               {:player-no 0
                                :effect    [:attack {:effects [[:discard-down-to 3]]}]}
                               {:player-no 1
                                :effect    [:clear-unaffected {:works :once}]}]}))
        (is (= (-> {:players [{:hand    [throne-room militia]
                               :actions 1}
                              {:hand [moat]}]}
                   (play 0 :throne-room)
                   (choose :militia))
               {:players      [{:play-area [throne-room militia]
                                :actions   0}
                               {:hand [moat]}]
                :effect-stack [{:text      "You may reveal a Reaction to react to the Attack."
                                :player-no 1
                                :choice    :reveal-reaction
                                :source    :hand
                                :options   [:moat]
                                :max       1}
                               {:player-no 0
                                :effect    [:give-coins 2]}
                               {:player-no 0
                                :effect    [:attack {:effects [[:discard-down-to 3]]}]}
                               {:player-no 1
                                :effect    [:clear-unaffected {:works :once}]}
                               {:effect    [:card-effect {:card militia}]
                                :player-no 0}
                               {:player-no 0
                                :effect    [:register-repeated-play {:target-id nil}]}]}))
        (is (= (-> {:players [{:hand    [throne-room militia]
                               :actions 1
                               :coins   0}
                              {:hand [moat]}]}
                   (play 0 :throne-room)
                   (choose :militia)
                   (choose :moat))
               {:players      [{:play-area [throne-room militia]
                                :actions   0
                                :coins     2}
                               {:hand [moat]}]
                :effect-stack [{:text      "You may reveal a Reaction to react to the Attack."
                                :player-no 1
                                :choice    :reveal-reaction
                                :source    :hand
                                :options   [:moat]
                                :max       1}
                               {:player-no 0
                                :effect    [:give-coins 2]}
                               {:player-no 0
                                :effect    [:attack {:effects [[:discard-down-to 3]]}]}
                               {:player-no 1
                                :effect    [:clear-unaffected {:works :once}]}
                               {:player-no 0
                                :effect    [:register-repeated-play {:target-id nil}]}]}))
        (testing "vs Minion"
          (is (= (-> {:players [{:hand    [minion]
                                 :actions 1}
                                {:hand [moat estate estate estate silver]
                                 :deck [copper copper]}]}
                     (play 0 :minion)
                     (choose :moat))
                 {:players      [{:play-area [minion]
                                  :actions   1}
                                 {:hand       [moat estate estate estate silver]
                                  :deck       [copper copper]
                                  :unaffected [{:works :once}]}]
                  :effect-stack [{:text      "Choose one:"
                                  :player-no 0
                                  :choice    ::intrigue/minion-choice
                                  :source    :special
                                  :options   [{:option :coins :text "+$2"}
                                              {:option :discard :text "Discard your hand, +4 Cards."}]
                                  :min       1
                                  :max       1}
                                 {:player-no 1
                                  :effect    [:clear-unaffected {:works :once}]}]}))
          (is (= (-> {:players [{:hand    [minion]
                                 :actions 1}
                                {:hand [moat estate estate estate silver]
                                 :deck [copper copper]}]}
                     (play 0 :minion)
                     (choose :moat)
                     (choose :discard))
                 {:players [{:play-area [minion]
                             :actions   1}
                            {:hand [moat estate estate estate silver]
                             :deck [copper copper]}]})))))))

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
                            :choice    ::dominion/moneylender-trash
                            :source    :hand
                            :options   [:copper :copper]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [moneylender copper copper estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :moneylender)
               (choose :copper))
           {:players [{:hand      [copper estate]
                       :play-area [moneylender]
                       :actions   0
                       :coins     3}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [moneylender copper copper estate]
                           :actions 1}]}
               (play 0 :moneylender)
               (choose nil))
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
                            :choice    :discard-from-hand
                            :source    :hand
                            :options   [:estate :copper]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 1}]
                :players [{:deck    [copper copper]
                           :hand    [poacher estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :poacher)
               (choose [:estate]))
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
                            :choice    :discard-from-hand
                            :source    :hand
                            :options   [:estate :silver :copper]
                            :min       2
                            :max       2}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 0}]
                :players [{:deck    [copper copper]
                           :hand    [poacher estate silver]
                           :actions 1
                           :coins   0}]}
               (play 0 :poacher)
               (choose [:copper :estate]))
           {:supply  [{:pile-size 0} {:pile-size 0}]
            :players [{:hand      [silver]
                       :deck      [copper]
                       :discard   [copper estate]
                       :play-area [poacher]
                       :actions   1
                       :coins     1}]}))
    (is (= (play {:supply  [{:pile-size 0}]
                  :players [{:hand    [poacher]
                             :actions 1
                             :coins   0}]}
                 0 :poacher)
           {:supply  [{:pile-size 0}]
            :players [{:play-area [poacher]
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
                            :choice    :discard-from-hand
                            :source    :hand
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
            :players [{:deck      [copper]
                       :play-area [poacher]
                       :discard   [copper]
                       :actions   1
                       :coins     1}]}))))

(deftest remodel-test
  (let [duchy (assoc duchy :id 1)]
    (testing "Remodel"
      (is (= (play {:players [{:hand    [remodel copper estate]
                               :actions 1}]}
                   0 :remodel)
             {:players      [{:hand      [copper estate]
                              :play-area [remodel]
                              :actions   0}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :choice    [:trash-and-gain {:extra-cost 2}]
                              :source    :hand
                              :options   [:copper :estate]
                              :min       1
                              :max       1}]}))
      (is (= (play {:players [{:hand    [remodel]
                               :actions 1}]}
                   0 :remodel)
             {:players [{:play-area [remodel]
                         :actions   0}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [remodel copper estate]
                             :actions 1}]}
                 (play 0 :remodel)
                 (choose :estate))
             {:supply       (base/supply 2 8)
              :players      [{:hand      [copper]
                              :play-area [remodel]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing up to $4."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:curse :estate :copper :silver]
                              :min       1
                              :max       1}]
              :trash        [estate]}))
      (is (= (-> {:supply  [{:card estate :pile-size 0} {:card silver :pile-size 40}]
                  :players [{:hand    [remodel copper estate]
                             :actions 1}]}
                 (play 0 :remodel)
                 (choose :copper))
             {:supply  [{:card estate :pile-size 0} {:card silver :pile-size 40}]
              :players [{:hand      [estate]
                         :play-area [remodel]
                         :actions   0}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [remodel silver estate]
                             :actions 1}]}
                 (play 0 :remodel)
                 (choose :silver)
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:hand      [estate]
                         :play-area [remodel]
                         :discard   [duchy]
                         :actions   0}]
              :trash   [silver]}))
      (testing "with cost reduction"
        (is (= (-> {:supply          (base/supply 2 8)
                    :cost-reductions [{:reduction 1}]
                    :players         [{:hand    [remodel copper estate]
                                       :actions 1}]}
                   (play 0 :remodel)
                   (choose :estate))
               {:supply          (base/supply 2 8)
                :cost-reductions [{:reduction 1}]
                :players         [{:hand      [copper]
                                   :play-area [remodel]
                                   :actions   0}]
                :effect-stack    [{:text      "Gain a card costing up to $3."
                                   :player-no 0
                                   :choice    :gain
                                   :source    :supply
                                   :options   [:curse :estate :copper :silver]
                                   :min       1
                                   :max       1}]
                :trash           [estate]}))
        (is (= (-> {:supply          (base/supply 2 8)
                    :cost-reductions [{:reduction 3}]
                    :players         [{:hand    [remodel copper estate]
                                       :actions 1}]}
                   (play 0 :remodel)
                   (choose :estate))
               {:supply          (base/supply 2 8)
                :cost-reductions [{:reduction 3}]
                :players         [{:hand      [copper]
                                   :play-area [remodel]
                                   :actions   0}]
                :effect-stack    [{:text      "Gain a card costing up to $2."
                                   :player-no 0
                                   :choice    :gain
                                   :source    :supply
                                   :options   [:curse :estate :duchy :copper :silver]
                                   :min       1
                                   :max       1}]
                :trash           [estate]}))))))

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
                            :choice    :trash-from-look-at
                            :source    :look-at
                            :options   [:silver :estate]}
                           {:player-no 0
                            :effect    [:give-choice {:text    "Discard any number of the top 2 cards of your deck."
                                                      :choice  :discard-from-look-at
                                                      :options [:player :look-at]}]}
                           {:player-no 0
                            :effect    [:give-choice {:text    "Put the rest back on top in any order."
                                                      :choice  :topdeck-from-look-at
                                                      :options [:player :look-at]
                                                      :min     2}]}]}))
    (is (= (-> {:players [{:deck    [copper silver estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (choose :estate))
           {:players      [{:hand      [copper]
                            :play-area [sentry]
                            :look-at   [silver]
                            :deck      [gold]
                            :actions   1}]
            :effect-stack [{:text      "Discard any number of the top 2 cards of your deck."
                            :player-no 0
                            :choice    :discard-from-look-at
                            :source    :look-at
                            :options   [:silver]}
                           {:player-no 0
                            :effect    [:give-choice {:text    "Put the rest back on top in any order."
                                                      :choice  :topdeck-from-look-at
                                                      :options [:player :look-at]
                                                      :min     2}]}]
            :trash        [estate]}))
    (is (= (-> {:players [{:deck    [copper silver estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (choose :estate)
               (choose nil))
           {:players      [{:hand      [copper]
                            :play-area [sentry]
                            :look-at   [silver]
                            :deck      [gold]
                            :actions   1}]
            :effect-stack [{:text      "Put the rest back on top in any order."
                            :player-no 0
                            :choice    :topdeck-from-look-at
                            :source    :look-at
                            :options   [:silver]
                            :min       1}]
            :trash        [estate]}))
    (is (= (-> {:players [{:deck    [copper silver estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (choose :estate)
               (choose nil)
               (choose :silver))
           {:players [{:hand      [copper]
                       :play-area [sentry]
                       :deck      [silver gold]
                       :actions   1}]
            :trash   [estate]}))
    (is (= (-> {:mode    :swift
                :players [{:deck    [copper silver estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (choose :estate)
               (choose nil))
           {:mode    :swift
            :players [{:hand      [copper]
                       :play-area [sentry]
                       :deck      [silver gold]
                       :actions   1}]
            :trash   [estate]}))
    (is (= (-> {:players [{:deck    [silver copper estate gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (choose [:estate :copper]))
           {:players [{:hand      [silver]
                       :play-area [sentry]
                       :deck      [gold]
                       :actions   1}]
            :trash   [estate copper]}))
    (is (= (-> {:players [{:deck    [silver province province gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (choose [])
               (choose [:province :province]))
           {:players [{:hand      [silver]
                       :play-area [sentry]
                       :deck      [gold]
                       :discard   [province province]
                       :actions   1}]}))
    (is (= (-> {:players [{:deck    [market silver market gold]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (choose [])
               (choose [])
               (choose [:silver :market]))
           {:players [{:hand      [market]
                       :play-area [sentry]
                       :deck      [market silver gold]
                       :actions   1}]}))
    (is (= (-> {:players [{:deck    [market copper]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry))
           {:players      [{:hand      [market]
                            :play-area [sentry]
                            :look-at   [copper]
                            :actions   1}]
            :effect-stack [{:text      "Trash any number of the top 2 cards of your deck."
                            :player-no 0
                            :choice    :trash-from-look-at
                            :source    :look-at
                            :options   [:copper]}
                           {:player-no 0
                            :effect    [:give-choice {:text    "Discard any number of the top 2 cards of your deck."
                                                      :choice  :discard-from-look-at
                                                      :options [:player :look-at]}]}
                           {:player-no 0
                            :effect    [:give-choice {:text    "Put the rest back on top in any order."
                                                      :choice  :topdeck-from-look-at
                                                      :options [:player :look-at]
                                                      :min     2}]}]}))
    (is (= (-> {:players [{:deck    [market copper]
                           :hand    [sentry]
                           :actions 1}]}
               (play 0 :sentry)
               (choose nil)
               (choose :copper))
           {:players [{:hand      [market]
                       :play-area [sentry]
                       :discard   [copper]
                       :actions   1}]}))))

(deftest smithy-test
  (testing "Smithy"
    (is (= (play {:players [{:deck    [copper copper copper]
                             :hand    [smithy]
                             :actions 1}]}
                 0 :smithy)
           {:players [{:hand      [copper copper copper]
                       :play-area [smithy]
                       :actions   0}]}))
    (is (= (play {:players [{:hand    [smithy]
                             :discard [copper copper copper]
                             :actions 1}]}
                 0 :smithy)
           {:players [{:hand      [copper copper copper]
                       :play-area [smithy]
                       :actions   0}]}))))

(deftest throne-room-test
  (let [throne-room (assoc throne-room :id 0)
        curse       (assoc curse :id 1)]
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
                              :card-id   0
                              :choice    [:repeat-action {:times 2}]
                              :source    :hand
                              :options   [:market]
                              :max       1}]}))
      (is (= (-> {:players [{:deck    [copper copper copper]
                             :hand    [throne-room market copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :throne-room)
                 (choose :market))
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
                 (choose nil))
             {:players [{:deck      [copper copper copper]
                         :hand      [market copper]
                         :play-area [throne-room]
                         :actions   0}]}))
      (is (= (-> {:players [{:deck    [witch copper copper silver]
                             :hand    [throne-room throne-room merchant]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :throne-room))
             {:players      [{:deck      [witch copper copper silver]
                              :hand      [merchant]
                              :play-area [throne-room throne-room]
                              :actions   0}]
              :effect-stack [{:text      "You may play an Action card from your hand twice."
                              :player-no 0
                              :card-id   0
                              :choice    [:repeat-action {:times 2}]
                              :source    :hand
                              :options   [:merchant]
                              :max       1}
                             {:player-no 0
                              :card-id   0
                              :effect    [:card-effect {:card throne-room}]}
                             {:player-no 0
                              :card-id   0
                              :effect    [:register-repeated-play {:target-id 0}]}]}))
      (is (= (-> {:players [{:deck    [witch copper copper silver]
                             :hand    [throne-room throne-room merchant]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :throne-room)
                 (choose :merchant))
             {:players      [{:deck      [copper silver]
                              :hand      [witch copper]
                              :play-area [throne-room throne-room merchant]
                              :triggers  [merchant-trigger merchant-trigger]
                              :actions   2}]
              :effect-stack [{:text      "You may play an Action card from your hand twice."
                              :player-no 0
                              :card-id   0
                              :choice    [:repeat-action {:times 2}]
                              :source    :hand
                              :options   [:witch]
                              :max       1}
                             {:player-no 0
                              :card-id   0
                              :effect    [:register-repeated-play {:target-id 0}]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:deck    [witch copper copper silver]
                             :hand    [throne-room throne-room merchant]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :throne-room)
                 (choose :throne-room)
                 (choose :merchant)
                 (choose :witch)
                 (play-treasures {:player-no 0}))
             {:supply  [{:card curse :pile-size 8}]
              :players [{:play-area [throne-room throne-room merchant witch copper copper silver]
                         :actions   2
                         :coins     6}
                        {:discard [curse curse]}]})))))

(deftest vassal-test
  (testing "Vassal"
    (is (= (play {:players [{:hand    [vassal]
                             :deck    [copper]
                             :actions 1
                             :coins   0}]}
                 0 :vassal)
           {:players [{:play-area [vassal]
                       :discard   [copper]
                       :actions   0
                       :coins     2}]}))
    (is (= (play {:players [{:hand    [vassal]
                             :actions 1
                             :coins   0}]}
                 0 :vassal)
           {:players [{:play-area [vassal]
                       :actions   0
                       :coins     2}]}))
    (is (= (play {:players [{:hand    [vassal]
                             :deck    [market copper]
                             :discard [market copper]
                             :actions 1
                             :coins   0}]}
                 0 :vassal)
           {:players      [{:play-area [vassal]
                            :deck      [copper]
                            :discard   [market copper market]
                            :actions   0
                            :coins     2}]
            :effect-stack [{:text      "You may play the discarded Action."
                            :player-no 0
                            :choice    ::dominion/vassal-play-action
                            :source    :discard
                            :options   [:market]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [vassal]
                           :deck    [market copper]
                           :discard [market copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :vassal)
               (choose :market))
           {:players [{:hand      [copper]
                       :play-area [vassal market]
                       :discard   [market copper]
                       :actions   1
                       :coins     3
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [vassal]
                           :deck    [market copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :vassal)
               (choose nil))
           {:players [{:play-area [vassal]
                       :deck      [copper]
                       :discard   [market]
                       :actions   0
                       :coins     2}]}))
    (let [curse (assoc curse :id 1)]
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [vassal]
                             :deck    [witch copper copper copper]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :vassal)
                 (choose :witch))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:hand      [copper copper]
                         :deck      [copper]
                         :play-area [vassal witch]
                         :actions   0
                         :coins     2}
                        {:discard [curse]}]})))))

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
  (let [curse (assoc curse :id 1)]
    (testing "Witch"
      (is (= (-> {:supply  [{:card curse :pile-size 20}]
                  :players [{:deck    (repeat 3 copper)
                             :hand    [witch]
                             :actions 1}
                            {:discard [copper copper]}
                            {:discard []}]}
                 (play 0 :witch))
             {:supply  [{:card curse :pile-size 18}]
              :players [{:deck      [copper]
                         :hand      [copper copper]
                         :play-area [witch]
                         :actions   0}
                        {:discard [copper copper curse]}
                        {:discard [curse]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 1}]
                  :players [{:deck    (repeat 3 copper)
                             :hand    [witch]
                             :actions 1}
                            {:discard [copper copper]}
                            {:discard []}]}
                 (play 0 :witch))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:deck      [copper]
                         :hand      [copper copper]
                         :play-area [witch]
                         :actions   0}
                        {:discard [copper copper curse]}
                        {:discard []}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 1}]
                  :players [{:discard [copper copper]}
                            {:deck    (repeat 3 copper)
                             :hand    [witch]
                             :actions 1}
                            {:discard []}]}
                 (play 1 :witch))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:discard [copper copper]}
                        {:deck      [copper]
                         :hand      [copper copper]
                         :play-area [witch]
                         :actions   0}
                        {:discard [curse]}]})))))

(deftest woodcutter-test
  (testing "Woodcutter"
    (is (= (play {:players [{:deck    [copper]
                             :hand    [woodcutter]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 0 :woodcutter)
           {:players [{:deck      [copper]
                       :play-area [woodcutter]
                       :actions   0
                       :coins     2
                       :buys      2}]}))))
(deftest workshop-test
  (let [silver (assoc silver :id 1)
        duchy  (assoc duchy :id 2)]
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
                              :choice    :gain
                              :source    :supply
                              :options   [:curse :estate :copper :silver]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [workshop copper]
                             :actions 1}]}
                 (play 0 :workshop)
                 (choose :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:hand      [copper]
                         :discard   [silver]
                         :play-area [workshop]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [workshop copper]
                             :actions 1}]}
                 (play 0 :workshop))
             {:supply  [{:card duchy :pile-size 8}]
              :players [{:hand      [copper]
                         :play-area [workshop]
                         :actions   0}]}))
      (testing "with cost reduction"
        (is (= (-> {:supply          [{:card duchy :pile-size 8}]
                    :cost-reductions [{:reduction 1}]
                    :players         [{:hand    [workshop copper]
                                       :actions 1}]}
                   (play 0 :workshop)
                   (choose :duchy))
               {:supply          [{:card duchy :pile-size 7}]
                :cost-reductions [{:reduction 1}]
                :players         [{:hand      [copper]
                                   :play-area [workshop]
                                   :discard   [duchy]
                                   :actions   0}]}))))))
