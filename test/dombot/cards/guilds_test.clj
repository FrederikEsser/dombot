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

