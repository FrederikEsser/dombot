(ns dombot.cards.dark-ages-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dark-ages :as dark-ages :refer :all]
            [dombot.cards.dominion :refer [militia]]
            [dombot.cards.intrigue :refer [harem]]
            [dombot.cards.kingdom :refer [setup-game]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 234 (f)))

(use-fixtures :each fixture)

(deftest armory-test
  (let [armory (assoc armory :id 0)
        duchy  (assoc duchy :id 1)]
    (testing "Armory"
      (is (= (-> {:supply  [{:card armory :pile-size 9}]
                  :players [{:hand    [armory]
                             :actions 1}]}
                 (play 0 :armory)
                 (choose :armory))
             {:supply  [{:card armory :pile-size 8}]
              :players [{:play-area [armory]
                         :deck      [armory]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [armory]
                             :actions 1}]}
                 (play 0 :armory))
             {:supply  [{:card duchy :pile-size 8}]
              :players [{:play-area [armory]
                         :actions   0}]})))))

(deftest bandit-camp-test
  (let [bandit-camp (assoc bandit-camp :id 0)]
    (testing "Bandit Camp"
      (let [spoils (assoc spoils :id 1)]
        (is (= (-> {:extra-cards [{:card spoils :pile-size 15}]
                    :players     [{:hand    [bandit-camp]
                                   :deck    [copper copper]
                                   :actions 1}]}
                   (play 0 :bandit-camp))
               {:extra-cards [{:card spoils :pile-size 14}]
                :players     [{:hand      [copper]
                               :play-area [bandit-camp]
                               :deck      [copper]
                               :discard   [spoils]
                               :actions   2}]}))
        (is (= (-> {:extra-cards [{:card spoils :pile-size 0}]
                    :players     [{:hand    [bandit-camp]
                                   :deck    [copper copper]
                                   :actions 1}]}
                   (play 0 :bandit-camp))
               {:extra-cards [{:card spoils :pile-size 0}]
                :players     [{:hand      [copper]
                               :play-area [bandit-camp]
                               :deck      [copper]
                               :actions   2}]})))
      (testing "setup"
        (is (= (-> {:supply [{:card bandit-camp :pile-size 10}]}
                   setup-game)
               {:extra-cards [{:card spoils :pile-size 15}]
                :supply      [{:card bandit-camp :pile-size 10}]}))))))

(deftest beggar-test
  (let [beggar (assoc beggar :id 0)
        copper (assoc copper :id 1)
        silver (assoc silver :id 2)]
    (testing "Beggar"
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [beggar]
                             :actions 1}]}
                 (play 0 :beggar))
             {:supply  [{:card copper :pile-size 43}]
              :players [{:hand      [copper copper copper]
                         :play-area [beggar]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 2}]
                  :players [{:hand    [beggar]
                             :actions 1}]}
                 (play 0 :beggar))
             {:supply  [{:card copper :pile-size 0}]
              :players [{:hand      [copper copper]
                         :play-area [beggar]
                         :actions   0}]}))
      (testing "Reaction"
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [militia]
                               :actions 1
                               :coins   0}
                              {:hand [beggar copper copper copper copper]}]}
                   (play 0 :militia)
                   (choose nil)                             ; do not react with Beggar
                   (choose [:copper :copper]))              ; discard to Militia
               {:supply  [{:card silver :pile-size 40}]
                :players [{:play-area [militia]
                           :actions   0
                           :coins     2}
                          {:hand    [beggar copper copper]
                           :discard [copper copper]}]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [militia]
                               :actions 1
                               :coins   0}
                              {:hand [beggar copper copper copper copper]}]}
                   (play 0 :militia)
                   (choose :beggar)                         ; react with Beggar
                   (choose :copper))                        ; discard to Militia
               {:supply  [{:card silver :pile-size 38}]
                :players [{:play-area [militia]
                           :actions   0
                           :coins     2}
                          {:hand    [copper copper copper]
                           :deck    [silver]
                           :discard [beggar silver copper]}]}))))))

(deftest forager-test
  (let [forager (assoc forager :id 0)]
    (testing "Forager"
      (is (= (-> {:players [{:hand    [forager estate]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :estate))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     0
                         :buys      2}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand    [forager estate]
                             :actions 1
                             :coins   0
                             :buys    1}]
                  :trash   [copper]}
                 (play 0 :forager)
                 (choose :estate))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     1
                         :buys      2}]
              :trash   [copper estate]}))
      (is (= (-> {:players [{:hand    [forager copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :copper))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     1
                         :buys      2}]
              :trash   [copper]}))
      (is (= (-> {:players [{:hand    [forager copper]
                             :actions 1
                             :coins   0
                             :buys    1}]
                  :trash   [copper]}
                 (play 0 :forager)
                 (choose :copper))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     1
                         :buys      2}]
              :trash   [copper copper]}))
      (is (= (-> {:players [{:hand    [forager copper]
                             :actions 1
                             :coins   0
                             :buys    1}]
                  :trash   [silver]}
                 (play 0 :forager)
                 (choose :copper))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     2
                         :buys      2}]
              :trash   [silver copper]})))))

(deftest poor-house-test
  (let [poor-house (assoc poor-house :id 0)]
    (testing "Poor House"
      (is (= (-> {:players [{:hand    [poor-house estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :poor-house))
             {:players [{:hand           [estate]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :coins          4}]}))
      (is (= (-> {:players [{:hand    [poor-house estate copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :poor-house))
             {:players [{:hand           [estate copper]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 2}
                         :actions        0
                         :coins          3}]}))
      (is (= (-> {:players [{:hand    [poor-house copper copper copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :poor-house))
             {:players [{:hand           [copper copper copper copper]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 4}
                         :actions        0
                         :coins          0}]}))
      (is (= (-> {:players [{:hand    [poor-house copper copper copper copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :poor-house))
             {:players [{:hand           [copper copper copper copper copper]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 5}
                         :actions        0
                         :coins          0}]}))
      (is (= (-> {:players [{:hand    [poor-house copper copper copper copper copper]
                             :actions 1
                             :coins   2}]}
                 (play 0 :poor-house))
             {:players [{:hand           [copper copper copper copper copper]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 5}
                         :actions        0
                         :coins          1}]})))))

(deftest spoils-test
  (let [spoils (assoc spoils :id 0)]
    (testing "Spoils"
      (is (= (-> {:extra-cards [{:card spoils :pile-size 14}]
                  :players     [{:hand  [spoils]
                                 :coins 0}]}
                 (play 0 :spoils))
             {:extra-cards [{:card spoils :pile-size 15}]
              :players     [{:coins 3}]})))))

(deftest squire-test
  (let [squire (assoc squire :id 0)
        silver (assoc silver :id 1)]
    (testing "Squire"
      (is (= (-> {:players [{:hand    [squire]
                             :actions 1
                             :coins   0}]}
                 (play 0 :squire)
                 (choose :actions))
             {:players [{:play-area [squire]
                         :actions   2
                         :coins     1}]}))
      (is (= (-> {:players [{:hand    [squire]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :squire)
                 (choose :buys))
             {:players [{:play-area [squire]
                         :actions   0
                         :coins     1
                         :buys      3}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [squire]
                             :actions 1
                             :coins   0}]}
                 (play 0 :squire)
                 (choose :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [squire]
                         :discard   [silver]
                         :actions   0
                         :coins     1}]}))
      (testing "on trash"
        (is (= (-> {:supply  [{:card squire :pile-size 9}]
                    :players [{:hand    [forager squire]
                               :actions 1
                               :coins   0
                               :buys    1}]}
                   (play 0 :forager)
                   (choose :squire))
               {:supply  [{:card squire :pile-size 9}]
                :players [{:play-area [forager]
                           :actions   1
                           :coins     0
                           :buys      2}]
                :trash   [squire]}))
        (let [militia (assoc militia :id 1)]
          (is (= (-> {:supply  [{:card militia :pile-size 10}]
                      :players [{:hand    [forager squire]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                     (play 0 :forager)
                     (choose :squire)
                     (choose :militia))
                 {:supply  [{:card militia :pile-size 9}]
                  :players [{:play-area [forager]
                             :discard   [militia]
                             :actions   1
                             :coins     0
                             :buys      2}]
                  :trash   [squire]})))))))

(deftest vagrant-test
  (let [vagrant (assoc vagrant :id 0)]
    (testing "Vagrant"
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper]
                         :play-area      [vagrant]
                         :deck           [copper]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper vagrant]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper]
                         :play-area      [vagrant]
                         :deck           [vagrant]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper curse]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper curse]
                         :play-area      [vagrant]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper estate]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper estate]
                         :play-area      [vagrant]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper harem]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper harem]
                         :play-area      [vagrant]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      #_(is (= (-> {:players [{:hand    [vagrant]
                               :deck    [copper ruined-market]
                               :actions 1}]}
                   (play 0 :vagrant))
               {:players [{:hand           [copper ruined-market]
                           :play-area      [vagrant]
                           :revealed-cards {:hand 1}
                           :actions        1}]}))
      #_(is (= (-> {:players [{:hand    [vagrant]
                               :deck    [copper necropolis]
                               :actions 1}]}
                   (play 0 :vagrant))
               {:players [{:hand           [copper necropolis]
                           :play-area      [vagrant]
                           :revealed-cards {:hand 1}
                           :actions        1}]})))))
