(ns dombot.cards.renaissance-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :refer [throne-room]]
            [dombot.cards.intrigue :refer [lurker]]
            [dombot.cards.renaissance :as renaissance :refer :all]
            [dombot.utils :as ut]))

(deftest acting-troupe-test
  (let [acting-troupe (assoc acting-troupe :id 1)]
    (testing "Acting Troupe"
      (is (= (-> {:players [{:hand    [acting-troupe]
                             :actions 1}]}
                 (play 0 :acting-troupe))
             {:players [{:actions   0
                         :villagers 4}]
              :trash   [acting-troupe]}))
      (is (= (-> {:players [{:hand    [acting-troupe throne-room]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :acting-troupe))
             {:players [{:play-area [throne-room]
                         :actions   0
                         :villagers 8}]
              :trash   [acting-troupe]})))))

(deftest experiment-test
  (let [experiment (assoc experiment :id 1)]
    (testing "Experiment"
      (is (= (-> {:supply  [{:card experiment :pile-size 9}]
                  :players [{:deck    [copper copper copper]
                             :hand    [experiment]
                             :actions 1}]}
                 (play 0 :experiment))
             {:supply  [{:card experiment :pile-size 10}]
              :players [{:deck    [copper]
                         :hand    [copper copper]
                         :actions 1}]}))
      (is (= (-> {:supply  [{:card experiment :pile-size 9}]
                  :players [{:deck    [copper copper copper]
                             :hand    [experiment throne-room]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :experiment))
             {:supply  [{:card experiment :pile-size 10}]
              :players [{:hand      [copper copper copper]
                         :play-area [throne-room]
                         :actions   2}]}))
      (is (= (-> {:supply  [{:card experiment :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0
                        :card-name :experiment}))
             {:supply  [{:card experiment :pile-size 8}]
              :players [{:discard [experiment experiment]}]}))
      (is (= (-> {:supply  [{:card experiment :pile-size 1}]
                  :players [{}]}
                 (gain {:player-no 0
                        :card-name :experiment}))
             {:supply  [{:card experiment :pile-size 0}]
              :players [{:discard [experiment]}]}))
      (is (= (-> {:supply  [{:card experiment :pile-size 9}]
                  :players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [experiment]}
                 (play 0 :lurker)
                 (choose :gain)
                 (choose :experiment))
             {:supply  [{:card experiment :pile-size 8}]
              :players [{:play-area [lurker]
                         :discard   [experiment experiment]
                         :actions   1}]
              :trash   []})))))

(deftest hideout-test
  (let [curse (assoc curse :id 1)]
    (testing "Hideout"
      (is (= (-> {:players [{:hand    [hideout estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :hideout))
             {:players      [{:hand      [estate copper]
                              :play-area [hideout]
                              :deck      [copper]
                              :actions   2}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :choice    ::renaissance/hideout-trash
                              :source    :hand
                              :options   [:estate :copper]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [hideout estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :hideout)
                 (choose :copper))
             {:players [{:hand      [estate]
                         :play-area [hideout]
                         :deck      [copper]
                         :actions   2}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [hideout estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :hideout)
                 (choose :estate))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:hand      [copper]
                         :play-area [hideout]
                         :deck      [copper]
                         :discard   [curse]
                         :actions   2}]
              :trash   [estate]})))))

(deftest lackeys-test
  (let [lackeys (assoc lackeys :id 1)]
    (testing "Lackeys"
      (is (= (-> {:players [{:hand    [lackeys]
                             :deck    [copper copper gold]
                             :actions 1}]}
                 (play 0 :lackeys))
             {:players [{:hand      [copper copper]
                         :play-area [lackeys]
                         :deck      [gold]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card lackeys :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0
                        :card-name :lackeys}))
             {:supply  [{:card lackeys :pile-size 9}]
              :players [{:discard   [lackeys]
                         :villagers 2}]}))
      (is (= (-> {:supply  [{:card lackeys :pile-size 10}]
                  :players [{:hand    [lurker]
                             :actions 1}]}
                 (play 0 :lurker)
                 (choose :trash)
                 (choose :lackeys))
             {:supply  [{:card lackeys :pile-size 9}]
              :players [{:play-area [lurker]
                         :actions   1}]
              :trash   [lackeys]}))
      (is (= (-> {:supply  [{:card lackeys :pile-size 9}]
                  :players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [lackeys]}
                 (play 0 :lurker)
                 (choose :gain)
                 (choose :lackeys))
             {:supply  [{:card lackeys :pile-size 9}]
              :players [{:play-area [lurker]
                         :discard   [lackeys]
                         :actions   1
                         :villagers 2}]
              :trash   []})))))

(deftest mountain-village-test
  (testing "Mountain Village"
    (is (= (-> {:players [{:hand    [mountain-village]
                           :discard [copper gold]
                           :actions 1}]}
               (play 0 :mountain-village))
           {:players      [{:play-area [mountain-village]
                            :discard   [copper gold]
                            :actions   2}]
            :effect-stack [{:text          "Look through your discard pile and put a card from it into your hand."
                            :player-no     0
                            :choice        :take-from-discard
                            :source        :discard
                            :reveal-source true
                            :options       [:copper :gold]
                            :min           1
                            :max           1}]}))
    (is (= (-> {:players [{:hand    [mountain-village]
                           :discard [copper gold]
                           :actions 1}]}
               (play 0 :mountain-village)
               (choose :gold))
           {:players [{:hand      [gold]
                       :play-area [mountain-village]
                       :discard   [copper]
                       :actions   2}]}))
    (is (= (-> {:players [{:hand    [mountain-village]
                           :deck    [copper gold]
                           :actions 1}]}
               (play 0 :mountain-village))
           {:players [{:hand      [copper]
                       :play-area [mountain-village]
                       :deck      [gold]
                       :actions   2}]}))))

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