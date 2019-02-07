(ns dombot.cards.base-cards-test
  (:require [clojure.test :refer :all]
            [dombot.operations :refer [play play-treasures view-game]]
            [dombot.cards.base-cards :as base :refer :all]))

(deftest treasure-test
  (testing "Copper"
    (is (= (play {:players [{:hand  [copper]
                             :coins 0}]}
                 0 :copper)
           {:players [{:hand      []
                       :play-area [copper]
                       :coins     1}]})))
  (testing "Silver"
    (is (= (play {:players [{:hand  [silver]
                             :coins 0}]}
                 0 :silver)
           {:players [{:hand      []
                       :play-area [silver]
                       :coins     2}]})))
  (testing "Gold"
    (is (= (play {:players [{:hand  [gold]
                             :coins 0}]}
                 0 :gold)
           {:players [{:hand      []
                       :play-area [gold]
                       :coins     3}]}))))

(deftest play-treasures-test
  (testing "All treasures"
    (is (= (play-treasures {:players [{:hand  [gold silver copper {:name :smithy} copper]
                                       :coins 0}]}
                           0)
           {:players [{:hand      [{:name :smithy}]
                       :play-area [gold silver copper copper]
                       :coins     7}]}))))

(deftest view-test
  (testing "View game end"
    (is (= (view-game {:supply         [{:card {:name :province} :pile-size 0}]
                       :players        [{:name      :dombot
                                         :hand      [copper copper copper estate estate]
                                         :play-area []
                                         :deck      [copper copper copper copper estate]
                                         :discard   []}]
                       :current-player 0})
           {:players [{:name           :dombot
                       :cards          {:copper 7 :estate 3}
                       :victory-points 3}]}))))
