(ns dombot.cards.intrigue-test
  (:require [clojure.test :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.dominion :as dominion :refer [throne-room]]
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
           {:players [{:hand      [copper copper]
                       :play-area [courtyard]
                       :deck      [copper]
                       :actions   0}]}))))

(deftest mining-village-test
  (testing "Mining Village"
    (let [mining-village-1 (assoc mining-village :id 1)
          mining-village-2 (assoc mining-village :id 2)]
      (is (= (-> {:players [{:hand    [mining-village-1]
                             :deck    [silver copper]
                             :actions 1}]}
                 (play 0 :mining-village))
             {:players      [{:hand      [silver]
                              :play-area [mining-village-1]
                              :deck      [copper]
                              :actions   2}]
              :effect-stack [{:text      "You may trash this for +$2."
                              :player-no 0
                              :card-id   1
                              :choice    ::intrigue/mining-village-trash
                              :source    :play-area
                              :options   [:mining-village]
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [mining-village-1]
                             :deck    [silver copper]
                             :actions 1}]}
                 (play 0 :mining-village)
                 (choose nil))
             {:players [{:hand      [silver]
                         :play-area [mining-village-1]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [mining-village-1]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :mining-village)
                 (choose :mining-village))
             {:players [{:hand      [silver]
                         :play-area []
                         :deck      [copper]
                         :actions   2
                         :coins     2}]
              :trash   [mining-village-1]}))
      (is (= (-> {:players [{:hand    [throne-room mining-village-1]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :mining-village)
                 (choose :mining-village))
             {:players [{:hand      [silver copper]
                         :play-area [throne-room]
                         :deck      []
                         :actions   4
                         :coins     2}]
              :trash   [mining-village-1]}))
      (is (= (-> {:players [{:hand    [throne-room mining-village-1]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :mining-village)
                 (choose nil)
                 (choose :mining-village))
             {:players [{:hand      [silver copper]
                         :play-area [throne-room]
                         :deck      []
                         :actions   4
                         :coins     2}]
              :trash   [mining-village-1]}))
      (is (= (-> {:players [{:hand    [mining-village-1 mining-village-2]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :mining-village)
                 (choose nil)
                 (play 0 :mining-village)
                 (choose :mining-village))
             {:players [{:hand      [silver copper]
                         :play-area [mining-village-1]
                         :deck      []
                         :actions   3
                         :coins     2}]
              :trash   [mining-village-2]}))
      (is (= (-> {:players [{:hand    [mining-village-1 throne-room mining-village-2]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :mining-village)
                 (choose nil)
                 (play 0 :throne-room)
                 (choose :mining-village)
                 (choose :mining-village))
             {:players [{:hand      [silver copper]
                         :play-area [mining-village-1 throne-room]
                         :deck      []
                         :actions   5
                         :coins     2}]
              :trash   [mining-village-2]}))
      (is (= (-> {:players [{:hand    [throne-room throne-room mining-village-1 mining-village-2]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :throne-room)                      ; play throne-room-2 twice
                 (choose :mining-village)                   ; play mining-village-1 twice
                 (choose nil)                               ; don't trash mining-village-1
                 (choose nil)                               ; don't trash mining-village-1 (again)
                 (choose :mining-village)                   ; play mining-village-2 twice
                 (choose :mining-village))                  ; trash mining-village-2
             {:players [{:hand      [silver copper]
                         :play-area [throne-room throne-room mining-village-1]
                         :deck      []
                         :actions   8
                         :coins     2}]
              :trash   [mining-village-2]}))
      (is (thrown-with-msg? AssertionError #"Card has no id, but is referring to :this in :play-area."
                            (-> {:players [{:hand    [mining-village]
                                            :deck    [copper]
                                            :actions 1}]}
                                (play 0 :mining-village)))))))

(deftest pawn-test
  (testing "Pawn"
    (is (= (-> {:players [{:hand    [pawn]
                           :actions 1}]}
               (play 0 :pawn))
           {:players      [{:hand      []
                            :play-area [pawn]
                            :actions   0}]
            :effect-stack [{:text      "Choose two:"
                            :player-no 0
                            :choice    ::intrigue/pawn-choices
                            :source    :special
                            :options   [{:option :card :text "+1 Card"}
                                        {:option :action :text "+1 Action"}
                                        {:option :buy :text "+1 Buy"}
                                        {:option :coin :text "+$1"}]
                            :min       2
                            :max       2}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1}]}
               (play 0 :pawn)
               (choose [:card :action]))
           {:players [{:hand      [copper]
                       :deck      [estate]
                       :play-area [pawn]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :buys    1}]}
               (play 0 :pawn)
               (choose [:card :buy]))
           {:players [{:hand      [copper]
                       :deck      [estate]
                       :play-area [pawn]
                       :actions   0
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :pawn)
               (choose [:card :coin]))
           {:players [{:hand      [copper]
                       :deck      [estate]
                       :play-area [pawn]
                       :actions   0
                       :coins     1}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :buys    1}]}
               (play 0 :pawn)
               (choose [:action :buy]))
           {:players [{:hand      []
                       :deck      [copper estate]
                       :play-area [pawn]
                       :actions   1
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :pawn)
               (choose [:action :coin]))
           {:players [{:hand      []
                       :deck      [copper estate]
                       :play-area [pawn]
                       :actions   1
                       :coins     1}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :buys    1
                           :coins   0}]}
               (play 0 :pawn)
               (choose [:buy :coin]))
           {:players [{:hand      []
                       :deck      [copper estate]
                       :play-area [pawn]
                       :actions   0
                       :buys      2
                       :coins     1}]}))
    (is (thrown-with-msg? AssertionError #"The choices must be different."
                          (-> {:players [{:hand    [pawn]
                                          :deck    [copper estate]
                                          :actions 1}]}
                              (play 0 :pawn)
                              (choose [:card :card]))))))

(deftest upgrade-test
  (let [silver (assoc silver :id 1)]
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
              :trash   [copper]})))))