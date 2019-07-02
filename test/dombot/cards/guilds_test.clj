(ns dombot.cards.guilds-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.guilds :as guilds :refer :all]
            [dombot.utils :as ut]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest candlestick-maker-test
  (testing "Candlestick Maker"
    (is (= (-> {:players [{:hand    [candlestick-maker]
                           :actions 1
                           :buys    1}]}
               (play 0 :candlestick-maker))
           {:players [{:play-area [candlestick-maker]
                       :actions   1
                       :buys      2
                       :coffers   1}]}))))

(deftest plaza-test
  (testing "Plaza"
    (is (= (-> {:players [{:hand    [plaza]
                           :deck    [estate estate]
                           :actions 1}]}
               (play 0 :plaza))
           {:players [{:hand      [estate]
                       :play-area [plaza]
                       :deck      [estate]
                       :actions   2}]}))
    (is (= (-> {:players [{:hand    [plaza]
                           :deck    [copper copper]
                           :actions 1}]}
               (play 0 :plaza)
               (choose nil))
           {:players [{:hand      [copper]
                       :play-area [plaza]
                       :deck      [copper]
                       :actions   2}]}))
    (is (= (-> {:players [{:hand    [plaza]
                           :deck    [copper copper]
                           :actions 1}]}
               (play 0 :plaza)
               (choose :copper))
           {:players [{:play-area [plaza]
                       :deck      [copper]
                       :discard   [copper]
                       :actions   2
                       :coffers   1}]}))))
