(ns dombot.cards.renaissance-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :as dominion :refer []]
            [dombot.cards.renaissance :as renaissance :refer :all]
            [dombot.utils :as ut]))

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