(ns dombot.cards.cornucopia-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.cornucopia :refer :all]
            [dombot.utils :as ut]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest farming-village-test
  (testing "Farming Village"
    (is (= (-> {:players [{:hand    [farming-village]
                           :deck    [copper copper]
                           :actions 1}]}
               (play 0 :farming-village))
           {:players [{:hand           [copper]
                       :play-area      [farming-village]
                       :deck           [copper]
                       :revealed-cards {:hand 1}
                       :actions        2}]}))
    (is (= (-> {:players [{:hand    [farming-village]
                           :deck    [farming-village copper]
                           :actions 1}]}
               (play 0 :farming-village))
           {:players [{:hand           [farming-village]
                       :play-area      [farming-village]
                       :deck           [copper]
                       :revealed-cards {:hand 1}
                       :actions        2}]}))
    (is (= (-> {:players [{:hand    [farming-village]
                           :deck    [estate estate copper copper]
                           :actions 1}]}
               (play 0 :farming-village))
           {:players [{:hand           [copper]
                       :play-area      [farming-village]
                       :deck           [copper]
                       :discard        [estate estate]
                       :revealed-cards {:hand    1
                                        :discard 2}
                       :actions        2}]}))))

(deftest hamlet-test
  (testing "Hamlet"
    (is (= (-> {:players [{:hand    [hamlet copper copper estate estate]
                           :deck    [silver silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :hamlet)
               (choose :estate)
               (choose :estate))
           {:players [{:hand      [copper copper silver]
                       :play-area [hamlet]
                       :deck      [silver]
                       :discard   [estate estate]
                       :actions   2
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [hamlet copper copper estate estate]
                           :deck    [silver silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :hamlet)
               (choose :estate)
               (choose nil))
           {:players [{:hand      [copper copper estate silver]
                       :play-area [hamlet]
                       :deck      [silver]
                       :discard   [estate]
                       :actions   2
                       :buys      1}]}))
    (is (= (-> {:players [{:hand    [hamlet copper copper estate estate]
                           :deck    [silver silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :hamlet)
               (choose nil)
               (choose :estate))
           {:players [{:hand      [copper copper estate silver]
                       :play-area [hamlet]
                       :deck      [silver]
                       :discard   [estate]
                       :actions   1
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [hamlet copper copper estate estate]
                           :deck    [silver silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :hamlet)
               (choose nil)
               (choose nil))
           {:players [{:hand      [copper copper estate estate silver]
                       :play-area [hamlet]
                       :deck      [silver]
                       :actions   1
                       :buys      1}]}))))

(deftest harvest-test
  (testing "Harvest"
    (is (= (-> {:players [{:hand    [harvest]
                           :deck    (repeat 5 copper)
                           :actions 1
                           :coins   0}]}
               (play 0 :harvest))
           {:players [{:play-area      [harvest]
                       :deck           [copper]
                       :discard        [copper copper copper copper]
                       :revealed-cards {:discard 4}
                       :actions        0
                       :coins          1}]}))
    (is (= (-> {:players [{:hand    [harvest]
                           :deck    [copper estate silver estate copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :harvest))
           {:players [{:play-area      [harvest]
                       :deck           [copper]
                       :discard        [copper estate silver estate]
                       :revealed-cards {:discard 4}
                       :actions        0
                       :coins          3}]}))))

(deftest hunting-party-test
  (testing "Hunting Party"
    (is (= (-> {:players [{:hand    [hunting-party copper copper copper copper]
                           :deck    [copper silver estate]
                           :actions 1}]}
               (play 0 :hunting-party))
           {:players [{:hand           [copper copper copper copper copper silver]
                       :play-area      [hunting-party]
                       :deck           [estate]
                       :revealed-cards {:hand 6}
                       :actions        1}]}))
    (is (= (-> {:players [{:hand    [hunting-party copper copper copper copper]
                           :deck    [silver copper estate]
                           :actions 1}]}
               (play 0 :hunting-party))
           {:players [{:hand           [copper copper copper copper silver estate]
                       :play-area      [hunting-party]
                       :discard        [copper]
                       :revealed-cards {:hand    6
                                        :discard 1}
                       :actions        1}]}))
    (is (= (-> {:players [{:hand    [hunting-party]
                           :deck    [silver copper estate]
                           :actions 1}]}
               (play 0 :hunting-party))
           {:players [{:hand           [silver copper]
                       :play-area      [hunting-party]
                       :deck           [estate]
                       :revealed-cards {:hand 2}
                       :actions        1}]}))
    (is (= (-> {:players [{:hand    [hunting-party]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :hunting-party))
           {:players [{:hand           [copper]
                       :play-area      [hunting-party]
                       :discard        [copper copper]
                       :revealed-cards {:hand    1
                                        :discard 2}
                       :actions        1}]}))))

(deftest menagerie-test
  (testing "Menagerie"
    (is (= (-> {:players [{:hand    [menagerie estate copper silver estate]
                           :deck    [copper copper copper copper]
                           :actions 1}]}
               (play 0 :menagerie))
           {:players [{:hand      [estate copper silver estate copper]
                       :play-area [menagerie]
                       :deck      [copper copper copper]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [menagerie estate copper silver gold]
                           :deck    [copper copper copper copper]
                           :actions 1}]}
               (play 0 :menagerie))
           {:players [{:hand      [estate copper silver gold copper copper copper]
                       :play-area [menagerie]
                       :deck      [copper]
                       :actions   1}]}))))

(deftest remake-test
  (let [silver (assoc silver :id 0)]
    (testing "Remake"
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card estate :pile-size 8}
                            {:card silver :pile-size 40}]
                  :players [{:hand    [remake copper copper estate estate]
                             :actions 1}]}
                 (play 0 :remake)
                 (choose :copper)
                 (choose :estate)
                 (choose :silver))
             {:supply  [{:card copper :pile-size 46}
                        {:card estate :pile-size 8}
                        {:card silver :pile-size 39}]
              :players [{:hand      [copper estate]
                         :play-area [remake]
                         :discard   [silver]
                         :actions   0}]
              :trash   [copper estate]}))
      (is (thrown-with-msg? AssertionError #"Choose error: You must pick an option"
                            (-> {:supply  [{:card copper :pile-size 46}
                                           {:card estate :pile-size 8}
                                           {:card silver :pile-size 40}]
                                 :players [{:hand    [remake copper copper estate estate]
                                            :actions 1}]}
                                (play 0 :remake)
                                (choose :copper)
                                (choose nil))))
      (is (thrown-with-msg? AssertionError #"Choose error: You must pick an option"
                            (-> {:supply  [{:card copper :pile-size 46}
                                           {:card estate :pile-size 8}
                                           {:card silver :pile-size 40}]
                                 :players [{:hand    [remake copper copper estate estate]
                                            :actions 1}]}
                                (play 0 :remake)
                                (choose nil)))))))