(ns dombot.cards.dark-ages-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dark-ages :as dark-ages :refer :all]
            [dombot.cards.dominion :refer [militia]]
            [dombot.cards.intrigue :refer [harem lurker]]
            [dombot.cards.adventures :refer [warrior]]
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

(deftest counterfeit-test
  (let [counterfeit (assoc counterfeit :id 0)]
    (testing "Counterfeit"
      (is (= (-> {:players [{:hand  [counterfeit]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit))
             {:players [{:play-area [counterfeit]
                         :coins     1
                         :buys      2}]}))
      (is (= (-> {:players [{:hand  [counterfeit copper]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [counterfeit]
                         :coins     1
                         :buys      2}]}))
      (is (= (-> {:players [{:hand  [counterfeit copper]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :copper))
             {:players [{:play-area [counterfeit]
                         :coins     3
                         :buys      2}]
              :trash   [copper]}))
      (is (= (-> {:players [{:hand  [counterfeit silver]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :silver))
             {:players [{:play-area [counterfeit]
                         :coins     5
                         :buys      2}]
              :trash   [silver]}))
      (is (= (-> {:extra-cards [{:card spoils :pile-size 14}]
                  :players     [{:hand  [counterfeit spoils]
                                 :coins 0
                                 :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :spoils))
             {:extra-cards [{:card spoils :pile-size 14}]
              :players     [{:play-area [counterfeit]
                             :coins     7
                             :buys      2}]
              :trash       [spoils]}))
      (is (= (-> {:players [{:hand  [counterfeit counterfeit copper copper]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :copper)
                 (play 0 :counterfeit)
                 (choose :copper))
             {:players [{:play-area [counterfeit counterfeit]
                         :coins     6
                         :buys      3}]
              :trash   [copper copper]}))
      (is (= (-> {:players [{:hand  [counterfeit counterfeit copper copper]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :counterfeit)
                 (choose :copper)
                 (choose :copper))
             {:players [{:play-area [counterfeit]
                         :coins     7
                         :buys      4}]
              :trash   [copper copper counterfeit]})))))

(deftest feodum-test
  (testing "Feodum"
    (testing "on trash"
      (let [feodum (assoc feodum :id 0)
            silver (assoc silver :id 0)]
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [forager feodum]
                               :actions 1
                               :coins   0
                               :buys    1}]}
                   (play 0 :forager)
                   (choose :feodum))
               {:supply  [{:card silver :pile-size 37}]
                :players [{:play-area [forager]
                           :discard   [silver silver silver]
                           :actions   1
                           :coins     0
                           :buys      2}]
                :trash   [feodum]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [warrior]
                               :actions 1}
                              {:deck [feodum]}]}
                   (play 0 :warrior))
               {:supply  [{:card silver :pile-size 37}]
                :players [{:play-area [warrior]
                           :actions   0}
                          {:discard [silver silver silver]}]
                :trash   [feodum]}))))
    (testing "victory points"
      (is (= (calc-victory-points {:deck (concat [feodum]
                                                 (repeat 2 silver))})
             0))
      (is (= (calc-victory-points {:deck (concat [feodum]
                                                 (repeat 3 silver))})
             1))
      (is (= (calc-victory-points {:deck (concat [feodum feodum]
                                                 (repeat 5 silver))})
             2))
      (is (= (calc-victory-points {:deck (concat [feodum feodum]
                                                 (repeat 6 silver))})
             4)))))

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

(deftest fortress-test
  (let [fortress (assoc fortress :id 0)]
    (testing "Fortress"
      (is (= (-> {:players [{:hand    [fortress]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :fortress))
             {:players [{:hand      [copper]
                         :play-area [fortress]
                         :deck      [copper]
                         :actions   2}]}))
      (testing "on trash"
        (is (= (-> {:players [{:hand    [forager fortress]
                               :actions 1
                               :coins   0
                               :buys    1}]}
                   (play 0 :forager)
                   (choose :fortress))
               {:players [{:hand      [fortress]
                           :play-area [forager]
                           :actions   1
                           :coins     0
                           :buys      2}]}))
        (is (= (-> {:players [{:hand    [warrior]
                               :actions 1}
                              {:deck [fortress]}]}
                   (play 0 :warrior))
               {:players [{:play-area [warrior]
                           :actions   0}
                          {:hand [fortress]}]}))
        (is (= (-> {:supply  [{:card fortress :pile-size 10}]
                    :players [{:hand    [lurker]
                               :actions 1}]}
                   (play 0 :lurker)
                   (choose :trash)
                   (choose :fortress))
               {:supply  [{:card fortress :pile-size 9}]
                :players [{:hand      [fortress]
                           :play-area [lurker]
                           :actions   1}]}))))))

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

(deftest shelters-test
  (testing "Hovel"
    (let [hovel  (assoc hovel :id 0)
          estate (assoc estate :id 1)
          silver (assoc silver :id 2)]
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand  [hovel]
                             :coins 2
                             :buys  1}]}
                 (buy-card 0 :estate)
                 (choose :hovel))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:discard [estate]
                         :coins   0
                         :buys    0}]
              :trash   [hovel]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand  [hovel]
                             :coins 2
                             :buys  1}]}
                 (buy-card 0 :estate)
                 (choose nil))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:hand    [hovel]
                         :discard [estate]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand  [hovel]
                             :coins 3
                             :buys  1}]}
                 (buy-card 0 :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:hand    [hovel]
                         :discard [silver]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [hovel armory]
                             :actions 1}]}
                 (play 0 :armory)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:hand      [hovel]
                         :play-area [armory]
                         :deck      [estate]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand  [hovel hovel]
                             :coins 2
                             :buys  1}]}
                 (buy-card 0 :estate)
                 (choose :hovel)
                 (choose :hovel))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:discard [estate]
                         :coins   0
                         :buys    0}]
              :trash   [hovel hovel]}))))
  (testing "Necropolis"
    (let [necropolis (assoc necropolis :id 0)]
      (is (= (-> {:players [{:hand    [necropolis]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :necropolis))
             {:players [{:play-area [necropolis]
                         :deck      [copper]
                         :actions   2}]}))))
  (testing "Overgrown Estate"
    (let [overgrown-estate (assoc overgrown-estate :id 0)]
      (is (= (-> {:players [{:hand    [forager overgrown-estate]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :overgrown-estate))
             {:players [{:hand      [copper]
                         :play-area [forager]
                         :deck      [copper]
                         :actions   1
                         :coins     0
                         :buys      2}]
              :trash   [overgrown-estate]}))
      (is (= (calc-victory-points {:deck [overgrown-estate]})
             0)))))

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
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper necropolis]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper necropolis]
                         :play-area      [vagrant]
                         :revealed-cards {:hand 1}
                         :actions        1}]})))))

(deftest wandering-minstrel-test
  (let [wandering-minstrel (assoc wandering-minstrel :id 0)]
    (testing "Wandering Minstrel"
      (is (= (-> {:players [{:hand    [wandering-minstrel]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :wandering-minstrel))
             {:players [{:hand      [copper]
                         :play-area [wandering-minstrel]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [wandering-minstrel]
                             :deck    [copper copper copper copper copper]
                             :actions 1}]}
                 (play 0 :wandering-minstrel))
             {:players [{:hand           [copper]
                         :play-area      [wandering-minstrel]
                         :deck           [copper]
                         :discard        [copper copper copper]
                         :revealed-cards {:discard 3}
                         :actions        2}]}))
      (is (= (-> {:players [{:hand    [wandering-minstrel]
                             :deck    [copper wandering-minstrel copper copper copper]
                             :actions 1}]}
                 (play 0 :wandering-minstrel)
                 (choose :wandering-minstrel))
             {:players [{:hand           [copper]
                         :play-area      [wandering-minstrel]
                         :deck           [wandering-minstrel copper]
                         :discard        [copper copper]
                         :revealed-cards {:deck    1
                                          :discard 2}
                         :actions        2}]}))
      (is (= (-> {:players [{:hand    [wandering-minstrel]
                             :deck    [copper wandering-minstrel gold vagrant copper]
                             :actions 1}]}
                 (play 0 :wandering-minstrel)
                 (choose [:wandering-minstrel :vagrant]))
             {:players [{:hand           [copper]
                         :play-area      [wandering-minstrel]
                         :deck           [vagrant wandering-minstrel copper]
                         :discard        [gold]
                         :revealed-cards {:deck    2
                                          :discard 1}
                         :actions        2}]}))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [wandering-minstrel]
                                            :deck    [copper wandering-minstrel gold vagrant copper]
                                            :actions 1}]}
                                (play 0 :wandering-minstrel)
                                (choose [:wandering-minstrel]))))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [wandering-minstrel]
                                            :deck    [copper wandering-minstrel gold vagrant copper]
                                            :actions 1}]}
                                (play 0 :wandering-minstrel)
                                (choose [:wandering-minstrel :vagrant :gold])))))))
