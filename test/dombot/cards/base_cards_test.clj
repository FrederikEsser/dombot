(ns dombot.cards.base-cards-test
  (:require [clojure.test :refer :all]
            [dombot.operations :refer [play play-treasures]]
            [dombot.cards.base-cards :as base :refer :all]))

(deftest treasure-test
  (testing "Copper"
    (is (= (play {:players [{:hand  [copper]
                             :coins 0}]}
                 0 :copper)
           {:players [{:play-area [copper]
                       :coins     1}]})))
  (testing "Silver"
    (is (= (play {:players [{:hand  [silver]
                             :coins 0}]}
                 0 :silver)
           {:players [{:play-area [silver]
                       :coins     2}]})))
  (testing "Gold"
    (is (= (play {:players [{:hand  [gold]
                             :coins 0}]}
                 0 :gold)
           {:players [{:play-area [gold]
                       :coins     3}]}))))

(deftest play-treasures-test
  (testing "All treasures"
    (is (= (play-treasures {:players [{:hand  [gold silver copper {:name :smithy} copper]
                                       :coins 0}]}
                           0)
           {:players [{:hand      [{:name :smithy}]
                       :play-area [gold silver copper copper]
                       :coins     7}]}))))

