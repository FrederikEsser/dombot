(ns dombot.cards.empires-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.empires :as empires :refer :all]
            [dombot.cards.renaissance :as renaissance :refer [patron]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest chariot-race-test
  (testing "Chariot Race"
    (is (= (-> {:players [{:hand    [chariot-race]
                           :deck    [silver]
                           :actions 1
                           :coins   0}
                          {:deck [estate]}]}
               (play 0 :chariot-race))
           {:players [{:hand           [silver]
                       :play-area      [chariot-race]
                       :revealed-cards {:hand 1}
                       :actions        1
                       :coins          1
                       :vp-tokens      1}
                      {:deck           [estate]
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:players [{:hand    [chariot-race]
                           :deck    [estate]
                           :actions 1
                           :coins   0}
                          {:deck [estate]}]}
               (play 0 :chariot-race))
           {:players [{:hand           [estate]
                       :play-area      [chariot-race]
                       :revealed-cards {:hand 1}
                       :actions        1
                       :coins          0}
                      {:deck           [estate]
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:players [{:hand    [chariot-race]
                           :deck    [estate]
                           :actions 1
                           :coins   0}
                          {}]}
               (play 0 :chariot-race))
           {:players [{:hand           [estate]
                       :play-area      [chariot-race]
                       :revealed-cards {:hand 1}
                       :actions        1
                       :coins          0}
                      {}]}))
    (is (= (-> {:players [{:hand    [chariot-race]
                           :actions 1
                           :coins   0}
                          {:deck [estate]}]}
               (play 0 :chariot-race))
           {:players [{:play-area [chariot-race]
                       :actions   1
                       :coins     0}
                      {:deck           [estate]
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:players [{:hand    [chariot-race]
                           :deck    [patron]
                           :actions 1
                           :coins   0}
                          {:discard [patron]}]}
               (play 0 :chariot-race))
           {:players [{:hand           [patron]
                       :play-area      [chariot-race]
                       :revealed-cards {:hand 1}
                       :actions        1
                       :coins          0
                       :coffers        1}
                      {:deck           [patron]
                       :revealed-cards {:deck 1}
                       :coffers        1}]}))))


(deftest delve-test
  (let [silver (assoc silver :id 0)]
    (testing "Delve"
      (is (= (-> {:events  {:delve delve}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :delve))
             {:events  {:delve delve}
              :supply  [{:card silver :pile-size 39}]
              :players [{:discard [silver]
                         :coins   2
                         :buys    1}]}))
      (is (= (-> {:events  {:delve delve}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :delve)
                 (buy-event 0 :delve))
             {:events  {:delve delve}
              :supply  [{:card silver :pile-size 38}]
              :players [{:discard [silver silver]
                         :coins   0
                         :buys    1}]})))))
