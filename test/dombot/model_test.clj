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
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :cost            0
             :number-of-cards 46
             :interaction     :buyable}
            {:name            :silver
             :name.ui         "Silver"
             :type            #{:treasure}
             :cost            3
             :number-of-cards 40}
            {:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :cost            2
             :number-of-cards 8
             :interaction     :buyable}]))
    (is (= (model-supply {:supply [{:card copper :pile-size 46}
                                   {:card silver :pile-size 40}
                                   {:card estate :pile-size 8}]
                          :player {:coins 2
                                   :buys  0}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :cost            0
             :number-of-cards 46}
            {:name            :silver
             :name.ui         "Silver"
             :type            #{:treasure}
             :cost            3
             :number-of-cards 40}
            {:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :cost            2
             :number-of-cards 8}]))
    (is (= (model-supply {:supply [{:card copper :pile-size 0}
                                   {:card silver :pile-size 40}
                                   {:card estate :pile-size 8}]
                          :player {:coins 2
                                   :buys  1}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :cost            0
             :number-of-cards 0}
            {:name            :silver
             :name.ui         "Silver"
             :type            #{:treasure}
             :cost            3
             :number-of-cards 40}
            {:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
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
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :cost            0
               :number-of-cards 46}
              {:name            :silver
               :name.ui         "Silver"
               :type            #{:treasure}
               :cost            3
               :number-of-cards 40}
              {:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :cost            2
               :number-of-cards 8}]))
      (is (= (model-supply {:supply [{:card copper :pile-size 46}
                                     {:card silver :pile-size 40}
                                     {:card estate :pile-size 8}]
                            :player {:coins 2
                                     :buys  1}
                            :choice {:source  :supply
                                     :options [:silver :estate]}})
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :cost            0
               :number-of-cards 46}
              {:name            :silver
               :name.ui         "Silver"
               :type            #{:treasure}
               :cost            3
               :number-of-cards 40
               :interaction     :choosable}
              {:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :cost            2
               :number-of-cards 8
               :interaction     :choosable}])))))

(deftest discard-test
  (testing "Discard model"
    (is (= (model-discard {:player {:discard []}})
           [{:number-of-cards 0}]))
    (is (= (model-discard {:player {:discard [estate]}})
           [{:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 1}]))
    (testing "randomized number of cards"
      (is (= (model-discard {:player {:discard [copper estate]}})
             [{:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 1}]))
      (is (= (model-discard {:player {:discard [copper estate]}})
             [{:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 3}]))
      (is (= (model-discard {:player {:discard [copper estate]}})
             [{:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 2}])))
    (testing "with choice"
      (is (= (model-discard {:player {:discard [copper estate]}
                             :choice {:source :hand}})
             [{:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 2}]))
      (is (= (model-discard {:player {:discard [copper estate]}
                             :choice {:source :discard :options [:estate]}})
             [{:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :interaction     :choosable
               :number-of-cards 3}]))
      (is (= (model-discard {:player {:discard [copper estate]}
                             :choice {:source :discard :reveal-source true :options [:estate]}})
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :number-of-cards 1}
              {:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :interaction     :choosable
               :number-of-cards 1}])))))

(deftest hand-test
  (testing "Hand model"
    (is (= (model-hand {:player {:hand    [copper copper copper moat estate]
                                 :actions 1
                                 :phase   :action}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 3
             :interaction     :playable}
            {:name            :moat
             :name.ui         "Moat"
             :type            #{:action :reaction}
             :number-of-cards 1
             :interaction     :playable}
            {:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 1}]))
    (is (= (model-hand {:player {:hand    [copper copper copper moat estate]
                                 :actions 0
                                 :phase   :action}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 3
             :interaction     :playable}
            {:name            :moat
             :name.ui         "Moat"
             :type            #{:action :reaction}
             :number-of-cards 1}
            {:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 1}]))
    (is (= (model-hand {:player {:hand    [copper copper copper moat estate]
                                 :actions 1
                                 :phase   :pay}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 3
             :interaction     :playable}
            {:name            :moat
             :name.ui         "Moat"
             :type            #{:action :reaction}
             :number-of-cards 1}
            {:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 1}]))
    (is (= (model-hand {:player {:hand    [copper copper copper moat estate]
                                 :actions 1
                                 :phase   :buy}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 3}
            {:name            :moat
             :name.ui         "Moat"
             :type            #{:action :reaction}
             :number-of-cards 1}
            {:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 1}]))
    (testing "with choice"
      (is (= (model-hand {:player {:hand  [copper copper copper moat estate]
                                   :phase :buy}
                          :choice {:source  :hand
                                   :options [:moat]
                                   :max     1}})
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :number-of-cards 3}
              {:name            :moat
               :name.ui         "Moat"
               :type            #{:action :reaction}
               :number-of-cards 1
               :interaction     :choosable}
              {:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 1}]))
      (is (= (model-hand {:player {:hand  [copper copper copper moat estate]
                                   :phase :buy}
                          :choice {:source  :hand
                                   :options [:estate]
                                   :min     1}})
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :number-of-cards 3}
              {:name            :moat
               :name.ui         "Moat"
               :type            #{:action :reaction}
               :number-of-cards 1}
              {:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 1
               :interaction     :quick-choosable}]))
      (is (= (model-hand {:player {:hand  [copper copper copper moat estate]
                                   :phase :buy}
                          :choice {:source  :hand
                                   :options [:copper :copper :copper :moat :estate]
                                   :min     1
                                   :max     1}})
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :number-of-cards 3
               :interaction     :quick-choosable}
              {:name            :moat
               :name.ui         "Moat"
               :type            #{:action :reaction}
               :number-of-cards 1
               :interaction     :quick-choosable}
              {:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 1
               :interaction     :quick-choosable}]))
      (is (= (model-hand {:player {:hand  [copper copper copper moat estate]
                                   :phase :buy}
                          :choice {:source  :supply
                                   :options [:estate :copper :silver :moat]
                                   :max     1}})
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :number-of-cards 3}
              {:name            :moat
               :name.ui         "Moat"
               :type            #{:action :reaction}
               :number-of-cards 1}
              {:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
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
            :hand      [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 2
                         :interaction     :playable}
                        {:name            :remodel
                         :name.ui         "Remodel"
                         :type            #{:action}
                         :number-of-cards 1
                         :interaction     :playable}
                        {:name            :estate
                         :name.ui         "Estate"
                         :type            #{:victory}
                         :number-of-cards 1}
                        {:name            :moat
                         :name.ui         "Moat"
                         :type            #{:action :reaction}
                         :number-of-cards 1
                         :interaction     :playable}]
            :play-area [{:name            :village
                         :name.ui         "Village"
                         :type            #{:action}
                         :number-of-cards 1}]
            :deck      {:number-of-cards 2}
            :discard   [{:name            :silver
                         :name.ui         "Silver"
                         :type            #{:treasure}
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
            :hand      [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 2}
                        {:name            :remodel
                         :name.ui         "Remodel"
                         :type            #{:action}
                         :number-of-cards 1}
                        {:name            :estate
                         :name.ui         "Estate"
                         :type            #{:victory}
                         :number-of-cards 1
                         :interaction     :choosable}
                        {:name            :moat
                         :name.ui         "Moat"
                         :type            #{:action :reaction}
                         :number-of-cards 1
                         :interaction     :choosable}]
            :play-area [{:name            :village
                         :name.ui         "Village"
                         :type            #{:action}
                         :number-of-cards 1}]
            :deck      {:number-of-cards 2}
            :discard   [{:name            :silver
                         :name.ui         "Silver"
                         :type            #{:treasure}
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
            :hand      [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 2}
                        {:name            :remodel
                         :name.ui         "Remodel"
                         :type            #{:action}
                         :number-of-cards 1}
                        {:name            :estate
                         :name.ui         "Estate"
                         :type            #{:victory}
                         :number-of-cards 1}
                        {:name            :moat
                         :name.ui         "Moat"
                         :type            #{:action :reaction}
                         :number-of-cards 1}]
            :play-area [{:name            :village
                         :name.ui         "Village"
                         :type            #{:action}
                         :number-of-cards 1}]
            :deck      {:number-of-cards 2}
            :discard   [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 1}
                        {:name            :estate
                         :name.ui         "Estate"
                         :type            #{:victory}
                         :number-of-cards 1
                         :interaction     :choosable}
                        {:name            :silver
                         :name.ui         "Silver"
                         :type            #{:treasure}
                         :number-of-cards 1}]
            :actions   2
            :money     0
            :buys      1}))))
