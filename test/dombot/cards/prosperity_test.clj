(ns dombot.cards.prosperity-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.prosperity :as prosperity :refer :all]
            [dombot.cards.dominion :as dominion :refer [market]]
            [dombot.utils :as ut]))

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