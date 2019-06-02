(ns dombot.cards.prosperity-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.prosperity :as prosperity :refer :all]
            [dombot.cards.dominion :as dominion :refer [market]]
            [dombot.utils :as ut]))

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
  (let [duchy (assoc duchy :id 1)]
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
              :trash        [copper copper estate estate estate]})))))

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