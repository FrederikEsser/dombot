(ns dombot.cards.adventures-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.adventures :as adventures :refer :all]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest raze-test
  (let [raze (assoc raze :id 0)]
    (testing "Raze"
      (is (= (-> {:players [{:hand    [raze estate]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 (play 0 :raze))
             {:players      [{:hand      [estate]
                              :play-area [raze]
                              :deck      [copper copper copper]
                              :actions   1}]
              :effect-stack [{:text      "Trash Raze or a card from your hand."
                              :player-no 0
                              :card-id   0
                              :choice    ::adventures/raze-trash-from-area
                              :source    :multi
                              :options   [{:area :play-area :card-name :raze}
                                          {:area :hand :card-name :estate}]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [raze silver]
                             :deck    [copper copper gold copper]
                             :actions 1}]}
                 (play 0 :raze)
                 (choose {:area :play-area :card-name :raze}))
             {:players      [{:hand    [silver]
                              :look-at [copper copper]
                              :deck    [gold copper]
                              :actions 1}]
              :effect-stack [{:text      "Look at 2 cards from the top of your deck. Put one of them into your hand and discard the rest."
                              :player-no 0
                              :choice    :take-from-look-at
                              :source    :look-at
                              :options   [:copper :copper]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:discard-all-look-at]}]
              :trash        [raze]}))
      (is (= (-> {:players [{:hand    [raze silver]
                             :deck    [copper copper gold copper]
                             :actions 1}]}
                 (play 0 :raze)
                 (choose {:area :hand :card-name :silver}))
             {:players      [{:play-area [raze]
                              :look-at   [copper copper gold]
                              :deck      [copper]
                              :actions   1}]
              :effect-stack [{:text      "Look at 3 cards from the top of your deck. Put one of them into your hand and discard the rest."
                              :player-no 0
                              :choice    :take-from-look-at
                              :source    :look-at
                              :options   [:copper :copper :gold]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:discard-all-look-at]}]
              :trash        [silver]}))
      (is (= (-> {:players [{:hand    [raze silver]
                             :deck    [copper copper gold copper]
                             :actions 1}]}
                 (play 0 :raze)
                 (choose {:area :hand :card-name :silver})
                 (choose :gold))
             {:players [{:hand      [gold]
                         :play-area [raze]
                         :deck      [copper]
                         :discard   [copper copper]
                         :actions   1}]
              :trash   [silver]}))
      (is (= (-> {:players [{:hand    [raze]
                             :actions 1}]}
                 (play 0 :raze)
                 (choose {:area :play-area :card-name :raze}))
             {:players [{:actions 1}]
              :trash   [raze]})))))
