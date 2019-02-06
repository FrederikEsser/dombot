(ns dombot.model-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.front-end-model :refer :all]
            [dombot.cards.base-cards :refer :all]
            [dombot.cards.dominion :refer :all]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest supply-model-test
  (testing "Supply model"
    (is (= (model-supply {:supply [{:card copper :pile-size 46}
                                   {:card silver :pile-size 40}
                                   {:card estate :pile-size 8}]
                          :player {:coins 2
                                   :buys  1}})
           [{:name            "Copper"
             :type            "Treasure"
             :cost            0
             :number-of-cards 46
             :interaction     :buyable}
            {:name            "Silver"
             :type            "Treasure"
             :cost            3
             :number-of-cards 40}
            {:name            "Estate"
             :type            "Victory"
             :cost            2
             :number-of-cards 8
             :interaction     :buyable}]))
    (is (= (model-supply {:supply [{:card copper :pile-size 46}
                                   {:card silver :pile-size 40}
                                   {:card estate :pile-size 8}]
                          :player {:coins 2
                                   :buys  0}})
           [{:name            "Copper"
             :type            "Treasure"
             :cost            0
             :number-of-cards 46}
            {:name            "Silver"
             :type            "Treasure"
             :cost            3
             :number-of-cards 40}
            {:name            "Estate"
             :type            "Victory"
             :cost            2
             :number-of-cards 8}]))
    (is (= (model-supply {:supply [{:card copper :pile-size 0}
                                   {:card silver :pile-size 40}
                                   {:card estate :pile-size 8}]
                          :player {:coins 2
                                   :buys  1}})
           [{:name            "Copper"
             :type            "Treasure"
             :cost            0
             :number-of-cards 0}
            {:name            "Silver"
             :type            "Treasure"
             :cost            3
             :number-of-cards 40}
            {:name            "Estate"
             :type            "Victory"
             :cost            2
             :number-of-cards 8
             :interaction     :buyable}]))
    (testing "with choice"
      (is (= (model-supply {:supply [{:card copper :pile-size 46}
                                     {:card silver :pile-size 40}
                                     {:card estate :pile-size 8}]
                            :player {:coins 2
                                     :buys  1}
                            :choice {:source :hand}})
             [{:name            "Copper"
               :type            "Treasure"
               :cost            0
               :number-of-cards 46}
              {:name            "Silver"
               :type            "Treasure"
               :cost            3
               :number-of-cards 40}
              {:name            "Estate"
               :type            "Victory"
               :cost            2
               :number-of-cards 8}]))
      (is (= (model-supply {:supply [{:card copper :pile-size 46}
                                     {:card silver :pile-size 40}
                                     {:card estate :pile-size 8}]
                            :player {:coins 2
                                     :buys  1}
                            :choice {:source  :supply
                                     :options [:silver :estate]}})
             [{:name            "Copper"
               :type            "Treasure"
               :cost            0
               :number-of-cards 46}
              {:name            "Silver"
               :type            "Treasure"
               :cost            3
               :number-of-cards 40
               :interaction     :choosable}
              {:name            "Estate"
               :type            "Victory"
               :cost            2
               :number-of-cards 8
               :interaction     :choosable}])))))

(deftest discard-test
  (testing "Discard model"
    (is (= (model-discard {:player {:discard []}})
           [{:number-of-cards 0}]))
    (is (= (model-discard {:player {:discard [estate]}})
           [{:name            "Estate"
             :type            "Victory"
             :number-of-cards 1}]))
    (testing "randomized number of cards"
      (is (= (model-discard {:player {:discard [copper estate]}})
             [{:name            "Estate"
               :type            "Victory"
               :number-of-cards 1}]))
      (is (= (model-discard {:player {:discard [copper estate]}})
             [{:name            "Estate"
               :type            "Victory"
               :number-of-cards 3}]))
      (is (= (model-discard {:player {:discard [copper estate]}})
             [{:name            "Estate"
               :type            "Victory"
               :number-of-cards 2}])))
    (testing "with choice"
      (is (= (model-discard {:player {:discard [copper estate]}
                             :choice {:source :hand}})
             [{:name            "Estate"
               :type            "Victory"
               :number-of-cards 2}]))
      (is (= (model-discard {:player {:discard [copper estate]}
                             :choice {:source :discard :options [:estate]}})
             [{:name            "Estate"
               :type            "Victory"
               :interaction     :choosable
               :number-of-cards 3}]))
      (is (= (model-discard {:player {:discard [copper estate]}
                             :choice {:source :discard :reveal-source true :options [:estate]}})
             [{:name            "Copper"
               :type            "Treasure"
               :number-of-cards 1}
              {:name            "Estate"
               :type            "Victory"
               :interaction     :choosable
               :number-of-cards 1}])))))

(deftest hand-test
  (testing "Hand model"
    (is (= (model-hand {:player {:hand    [copper copper copper moat estate]
                                 :actions 1
                                 :phase   :action}})
           [{:name            "Copper"
             :type            "Treasure"
             :number-of-cards 3
             :interaction     :playable}
            {:name            "Moat"
             :type            "Action/Reaction"
             :number-of-cards 1
             :interaction     :playable}
            {:name            "Estate"
             :type            "Victory"
             :number-of-cards 1}]))
    (is (= (model-hand {:player {:hand    [copper copper copper moat estate]
                                 :actions 0
                                 :phase   :action}})
           [{:name            "Copper"
             :type            "Treasure"
             :number-of-cards 3
             :interaction     :playable}
            {:name            "Moat"
             :type            "Action/Reaction"
             :number-of-cards 1}
            {:name            "Estate"
             :type            "Victory"
             :number-of-cards 1}]))
    (is (= (model-hand {:player {:hand    [copper copper copper moat estate]
                                 :actions 1
                                 :phase   :pay}})
           [{:name            "Copper"
             :type            "Treasure"
             :number-of-cards 3
             :interaction     :playable}
            {:name            "Moat"
             :type            "Action/Reaction"
             :number-of-cards 1}
            {:name            "Estate"
             :type            "Victory"
             :number-of-cards 1}]))
    (is (= (model-hand {:player {:hand    [copper copper copper moat estate]
                                 :actions 1
                                 :phase   :buy}})
           [{:name            "Copper"
             :type            "Treasure"
             :number-of-cards 3}
            {:name            "Moat"
             :type            "Action/Reaction"
             :number-of-cards 1}
            {:name            "Estate"
             :type            "Victory"
             :number-of-cards 1}]))
    (testing "with choice"
      (is (= (model-hand {:player {:hand  [copper copper copper moat estate]
                                   :phase :buy}
                          :choice {:source  :hand
                                   :options [:moat]
                                   :max     1}})
             [{:name            "Copper"
               :type            "Treasure"
               :number-of-cards 3}
              {:name            "Moat"
               :type            "Action/Reaction"
               :number-of-cards 1
               :interaction     :choosable}
              {:name            "Estate"
               :type            "Victory"
               :number-of-cards 1}]))
      (is (= (model-hand {:player {:hand  [copper copper copper moat estate]
                                   :phase :buy}
                          :choice {:source  :hand
                                   :options [:estate]
                                   :min     1}})
             [{:name            "Copper"
               :type            "Treasure"
               :number-of-cards 3}
              {:name            "Moat"
               :type            "Action/Reaction"
               :number-of-cards 1}
              {:name            "Estate"
               :type            "Victory"
               :number-of-cards 1
               :interaction     :quick-choosable}]))
      (is (= (model-hand {:player {:hand  [copper copper copper moat estate]
                                   :phase :buy}
                          :choice {:source  :hand
                                   :options [:copper :copper :copper :moat :estate]
                                   :min     1
                                   :max     1}})
             [{:name            "Copper"
               :type            "Treasure"
               :number-of-cards 3
               :interaction     :quick-choosable}
              {:name            "Moat"
               :type            "Action/Reaction"
               :number-of-cards 1
               :interaction     :quick-choosable}
              {:name            "Estate"
               :type            "Victory"
               :number-of-cards 1
               :interaction     :quick-choosable}]))
      (is (= (model-hand {:player {:hand  [copper copper copper moat estate]
                                   :phase :buy}
                          :choice {:source  :supply
                                   :options [:estate :copper :silver :moat]
                                   :max     1}})
             [{:name            "Copper"
               :type            "Treasure"
               :number-of-cards 3}
              {:name            "Moat"
               :type            "Action/Reaction"
               :number-of-cards 1}
              {:name            "Estate"
               :type            "Victory"
               :number-of-cards 1}])))))

(deftest player-model-test
  (testing "Player model"
    (is (= (model-active-player {:player {:name      "John Doe"
                                          :hand      [copper copper remodel estate moat]
                                          :play-area [village]
                                          :deck      [copper estate]
                                          :discard   [copper estate silver]
                                          :actions   2
                                          :coins     0
                                          :buys      1
                                          :phase     :action}})
           {:name      "John Doe"
            :hand      [{:name            "Copper"
                         :type            "Treasure"
                         :number-of-cards 2
                         :interaction     :playable}
                        {:name            "Remodel"
                         :type            "Action"
                         :number-of-cards 1
                         :interaction     :playable}
                        {:name            "Estate"
                         :type            "Victory"
                         :number-of-cards 1}
                        {:name            "Moat"
                         :type            "Action/Reaction"
                         :number-of-cards 1
                         :interaction     :playable}]
            :play-area [{:name            "Village"
                         :type            "Action"
                         :number-of-cards 1}]
            :deck      {:number-of-cards 2}
            :discard   [{:name            "Silver"
                         :type            "Treasure"
                         :number-of-cards 4}]
            :actions   2
            :money     0
            :buys      1}))
    (is (= (model-active-player {:player {:name      "John Doe"
                                          :hand      [copper copper remodel estate moat]
                                          :play-area [village]
                                          :deck      [copper estate]
                                          :discard   [copper estate silver]
                                          :actions   2
                                          :coins     0
                                          :buys      1
                                          :phase     :action}
                                 :choice {:source  :hand
                                          :options [:estate :moat]}})
           {:name      "John Doe"
            :hand      [{:name            "Copper"
                         :type            "Treasure"
                         :number-of-cards 2}
                        {:name            "Remodel"
                         :type            "Action"
                         :number-of-cards 1}
                        {:name            "Estate"
                         :type            "Victory"
                         :number-of-cards 1
                         :interaction     :choosable}
                        {:name            "Moat"
                         :type            "Action/Reaction"
                         :number-of-cards 1
                         :interaction     :choosable}]
            :play-area [{:name            "Village"
                         :type            "Action"
                         :number-of-cards 1}]
            :deck      {:number-of-cards 2}
            :discard   [{:name            "Silver"
                         :type            "Treasure"
                         :number-of-cards 4}]
            :actions   2
            :money     0
            :buys      1}))
    (is (= (model-active-player {:player {:name      "John Doe"
                                          :hand      [copper copper remodel estate moat]
                                          :play-area [village]
                                          :deck      [copper estate]
                                          :discard   [copper estate silver]
                                          :actions   2
                                          :coins     0
                                          :buys      1
                                          :phase     :action}
                                 :choice {:source        :discard
                                          :reveal-source true
                                          :options       [:estate]}})
           {:name      "John Doe"
            :hand      [{:name            "Copper"
                         :type            "Treasure"
                         :number-of-cards 2}
                        {:name            "Remodel"
                         :type            "Action"
                         :number-of-cards 1}
                        {:name            "Estate"
                         :type            "Victory"
                         :number-of-cards 1}
                        {:name            "Moat"
                         :type            "Action/Reaction"
                         :number-of-cards 1}]
            :play-area [{:name            "Village"
                         :type            "Action"
                         :number-of-cards 1}]
            :deck      {:number-of-cards 2}
            :discard   [{:name            "Copper"
                         :type            "Treasure"
                         :number-of-cards 1}
                        {:name            "Estate"
                         :type            "Victory"
                         :number-of-cards 1
                         :interaction     :choosable}
                        {:name            "Silver"
                         :type            "Treasure"
                         :number-of-cards 1}]
            :actions   2
            :money     0
            :buys      1}))))
