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