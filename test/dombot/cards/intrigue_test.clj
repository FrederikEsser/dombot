(ns dombot.cards.intrigue-test
  (:require [clojure.test :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.intrigue :as intrigue :refer :all]
            [dombot.utils :as ut]))

(deftest courtyard-test
  (testing "Courtyard"
    (is (= (play {:players [{:hand    [courtyard]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 0 :courtyard)
           {:players      [{:hand      [copper copper copper]
                            :play-area [courtyard]
                            :deck      []
                            :actions   0}]
            :effect-stack [{:text      "Put a card from your hand onto your deck."
                            :player-no 0
                            :choice    :topdeck-from-hand
                            :source    :hand
                            :options   [:copper :copper :copper]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [courtyard]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :courtyard)
               (choose :copper))
           {:players      [{:hand      [copper copper]
                            :play-area [courtyard]
                            :deck      [copper]
                            :actions   0}]}))))

(deftest upgrade-test
  (testing "Upgrade"
    (is (= (-> {:players [{:hand    [upgrade copper copper estate estate]
                           :deck    [silver]
                           :actions 1}]}
               (play 0 :upgrade))
           {:players      [{:hand      [copper copper estate estate silver]
                            :play-area [upgrade]
                            :deck      []
                            :actions   1}]
            :effect-stack [{:text      "Trash a card from your hand."
                            :player-no 0
                            :choice    ::intrigue/upgrade-trash
                            :source    :hand
                            :options   [:copper :copper :estate :estate :silver]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  [{:card copper :pile-size 46}
                          {:card silver :pile-size 40}
                          {:card gold :pile-size 30}]
                :players [{:hand    [upgrade copper copper estate estate]
                           :deck    [silver]
                           :actions 1}]}
               (play 0 :upgrade)
               (choose :estate))
           {:supply       [{:card copper :pile-size 46}
                           {:card silver :pile-size 40}
                           {:card gold :pile-size 30}]
            :players      [{:hand      [copper copper estate silver]
                            :play-area [upgrade]
                            :deck      []
                            :actions   1}]
            :effect-stack [{:text      "Gain a card costing exactly $3."
                            :player-no 0
                            :choice    :gain
                            :source    :supply
                            :options   [:silver]
                            :min       1
                            :max       1}]
            :trash        [estate]}))
    (is (= (-> {:supply  [{:card copper :pile-size 46}
                          {:card silver :pile-size 40}
                          {:card gold :pile-size 30}]
                :players [{:hand    [upgrade copper copper estate estate]
                           :deck    [silver]
                           :actions 1}]}
               (play 0 :upgrade)
               (choose :estate)
               (choose :silver))
           {:supply  [{:card copper :pile-size 46}
                      {:card silver :pile-size 39}
                      {:card gold :pile-size 30}]
            :players [{:hand      [copper copper estate silver]
                       :play-area [upgrade]
                       :deck      []
                       :discard   [silver]
                       :actions   1}]
            :trash   [estate]}))
    (is (= (-> {:supply  [{:card copper :pile-size 46}
                          {:card silver :pile-size 40}
                          {:card gold :pile-size 30}]
                :players [{:hand    [upgrade copper copper estate estate]
                           :deck    [silver]
                           :actions 1}]}
               (play 0 :upgrade)
               (choose :copper))
           {:supply  [{:card copper :pile-size 46}
                      {:card silver :pile-size 40}
                      {:card gold :pile-size 30}]
            :players [{:hand      [copper estate estate silver]
                       :play-area [upgrade]
                       :deck      []
                       :actions   1}]
            :trash   [copper]}))))