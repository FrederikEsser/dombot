(ns dombot.cards.renaissance-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :as dominion :refer []]
            [dombot.cards.renaissance :as renaissance :refer :all]
            [dombot.utils :as ut]))

(deftest recruiter-test
  (testing "Recruiter"
    (is (= (-> {:players [{:hand    [recruiter estate estate]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :recruiter))
           {:players      [{:hand      [estate estate copper copper]
                            :play-area [recruiter]
                            :deck      [copper]
                            :actions   0}]
            :effect-stack [{:text      "Trash a cards from your hand."
                            :player-no 0
                            :choice    ::renaissance/recruiter-trash
                            :source    :hand
                            :options   [:estate :estate :copper :copper]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [recruiter estate estate]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :recruiter)
               (choose :estate))
           {:players [{:hand      [estate copper copper]
                       :play-area [recruiter]
                       :deck      [copper]
                       :actions   0
                       :villagers 2}]
            :trash   [estate]}))
    (is (= (-> {:players [{:hand    [recruiter estate estate]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :recruiter)
               (choose :copper))
           {:players [{:hand      [estate estate copper]
                       :play-area [recruiter]
                       :deck      [copper]
                       :actions   0}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [recruiter estate estate]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :recruiter)
               (choose :estate)
               (spend-villager 0))
           {:players [{:hand      [estate copper copper]
                       :play-area [recruiter]
                       :deck      [copper]
                       :actions   1
                       :villagers 1}]
            :trash   [estate]}))
    (is (thrown-with-msg? AssertionError #"You have no Villagers to spend."
                          (-> {:players [{:hand    [recruiter estate estate]
                                          :deck    [copper copper copper]
                                          :actions 1}]}
                              (play 0 :recruiter)
                              (choose :copper)
                              (spend-villager 0))))))

(deftest scholar-test
  (testing "Scholar"
    (is (= (-> {:players [{:hand    [scholar estate estate estate]
                           :deck    (repeat 7 copper)
                           :actions 1}]}
               (play 0 :scholar))
           {:players [{:hand      (repeat 7 copper)
                       :play-area [scholar]
                       :discard   [estate estate estate]
                       :actions   0}]}))))

(deftest villain-test
  (testing "Villain"
    (is (= (-> {:players [{:hand    [villain]
                           :actions 1}
                          {:hand [copper copper copper copper silver]}]}
               (play 0 :villain))
           {:players      [{:play-area [villain]
                            :actions   0
                            :coffers   2}
                           {:hand [copper copper copper copper silver]}]
            :effect-stack [{:text      "Discard a card costing $2 or more."
                            :player-no 1
                            :choice    :discard-from-hand
                            :source    :hand
                            :options   [:silver]
                            :min       1
                            :max       1}
                           {:player-no 1
                            :effect    [:clear-unaffected {:works :once}]}]}))
    (is (= (-> {:players [{:hand    [villain]
                           :actions 1}
                          {:hand [copper copper copper copper silver]}]}
               (play 0 :villain)
               (choose :silver))
           {:players [{:play-area [villain]
                       :actions   0
                       :coffers   2}
                      {:hand    [copper copper copper copper]
                       :discard [silver]}]}))
    (is (= (-> {:players [{:hand    [villain]
                           :actions 1}
                          {:hand [copper copper copper silver]}]}
               (play 0 :villain))
           {:players [{:play-area [villain]
                       :actions   0
                       :coffers   2}
                      {:hand [copper copper copper silver]}]}))
    (is (= (-> {:players [{:hand    [villain]
                           :actions 1}
                          {:hand [copper copper copper copper copper]}]}
               (play 0 :villain))
           {:players [{:play-area [villain]
                       :actions   0
                       :coffers   2}
                      {:hand           [copper copper copper copper copper]
                       :revealed-cards {:hand 5}}]}))))