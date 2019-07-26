(ns dombot.cards.nocturne-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.nocturne :as nocturne :refer :all]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest tragic-hero-test
  (let [tragic-hero (assoc tragic-hero :id 0)]
    (testing "Tragic Hero"
      (is (= (-> {:players [{:hand    [tragic-hero copper copper copper copper]
                             :deck    [estate estate estate copper]
                             :actions 1
                             :buys    1}]}
                 (play 0 :tragic-hero))
             {:players [{:hand      [copper copper copper copper estate estate estate]
                         :play-area [tragic-hero]
                         :deck      [copper]
                         :actions   0
                         :buys      2}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:supply  [{:card gold :pile-size 30}]
                    :players [{:hand    [tragic-hero copper copper copper copper copper]
                               :deck    [estate estate estate copper]
                               :actions 1
                               :buys    1}]}
                   (play 0 :tragic-hero)
                   (choose :gold))
               {:supply  [{:card gold :pile-size 29}]
                :players [{:hand    [copper copper copper copper copper estate estate estate]
                           :deck    [copper]
                           :discard [gold]
                           :actions 0
                           :buys    2}]
                :trash   [tragic-hero]}))))))