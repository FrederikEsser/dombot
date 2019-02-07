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

(deftest deck-test
  (testing "Deck model"
    (is (= (model-deck {:player {:deck [copper copper copper]}})
           [{:name.ui         "Deck"
             :number-of-cards 3}]))
    (is (= (model-deck {:player {:deck    [copper copper copper]
                                 :look-at [estate estate]}})
           [{:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 2}
            {:name.ui         "Deck"
             :number-of-cards 3}]))
    (is (= (model-deck {:player {:deck     [copper copper copper]
                                 :revealed [estate gold]}})
           [{:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 1}
            {:name            :gold
             :name.ui         "Gold"
             :type            #{:treasure}
             :number-of-cards 1}
            {:name.ui         "Deck"
             :number-of-cards 3}]))))

(deftest discard-test
  (testing "Discard model"
    (is (= (model-discard {:player {:discard []}})
           []))
    (is (= (model-discard {:player {:discard             [estate]
                                    :approx-discard-size 1}})
           [{:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 1}]))
    (testing "randomized number of cards"
      (is (= (model-discard {:player {:discard             [copper estate]
                                      :approx-discard-size 1}})
             [{:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 1}]))
      (is (= (model-discard {:player {:discard             [copper estate]
                                      :approx-discard-size 2}})
             [{:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 2}]))
      (is (= (model-discard {:player {:discard             [copper estate]
                                      :approx-discard-size 3}})
             [{:name            :estate
               :name.ui         "Estate"
               :type            #{:victory}
               :number-of-cards 3}])))
    (testing "with choice"
      (is (= (model-discard {:player {:discard [copper estate]}
                             :choice {:source :hand}})
             [{:name    :estate
               :name.ui "Estate"
               :type    #{:victory}}]))
      (is (= (model-discard {:player {:discard [copper estate]}
                             :choice {:source :discard :options [:estate]}})
             [{:name        :estate
               :name.ui     "Estate"
               :type        #{:victory}
               :interaction :choosable}]))
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
    (is (= (model-hand true {:player {:hand    [copper copper copper moat estate]
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
    (is (= (model-hand true {:player {:hand    [copper copper copper moat estate]
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
    (is (= (model-hand true {:player {:hand    [copper copper copper moat estate]
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
    (is (= (model-hand true {:player {:hand    [copper copper copper moat estate]
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
      (is (= (model-hand true {:player {:hand  [copper copper copper moat estate]
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
      (is (= (model-hand true {:player {:hand  [copper copper copper moat estate]
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
      (is (= (model-hand true {:player {:hand  [copper copper copper moat estate]
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
      (is (= (model-hand true {:player {:hand  [copper copper copper moat estate]
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
    (is (= (model-player true {:player {:name      "John Doe"
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
            :deck      [{:name.ui         "Deck"
                         :number-of-cards 2}]
            :discard   [{:name    :silver
                         :name.ui "Silver"
                         :type    #{:treasure}}]
            :actions   2
            :money     0
            :buys      1}))
    (is (= (model-player true {:player {:name      "John Doe"
                                        :hand      [copper copper remodel estate moat]
                                        :play-area [village]
                                        :deck      [copper estate]
                                        :discard   [copper estate silver]
                                        :actions   2
                                        :coins     0
                                        :buys      1
                                        :phase     :action}
                               :choice {:text    "Choice text"
                                        :source  :hand
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
            :deck      [{:name.ui         "Deck"
                         :number-of-cards 2}]
            :discard   [{:name    :silver
                         :name.ui "Silver"
                         :type    #{:treasure}}]
            :actions   2
            :money     0
            :buys      1
            :choice    {:text "Choice text"}}))
    (is (= (model-player true {:player {:name      "John Doe"
                                        :hand      [copper copper remodel estate moat]
                                        :play-area [village]
                                        :deck      [copper estate]
                                        :discard   [copper estate silver]
                                        :actions   2
                                        :coins     0
                                        :buys      1
                                        :phase     :action}
                               :choice {:text          "Choice text"
                                        :source        :discard
                                        :reveal-source true
                                        :options       [:estate]
                                        :min           1
                                        :quick-choice  true}})
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
            :deck      [{:name.ui         "Deck"
                         :number-of-cards 2}]
            :discard   [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 1}
                        {:name            :estate
                         :name.ui         "Estate"
                         :type            #{:victory}
                         :number-of-cards 1
                         :interaction     :quick-choosable}
                        {:name            :silver
                         :name.ui         "Silver"
                         :type            #{:treasure}
                         :number-of-cards 1}]
            :actions   2
            :money     0
            :buys      1
            :choice    {:text         "Choice text"
                        :min          1
                        :quick-choice true}}))
    (is (= (model-player false {:player {:name      "John Doe"
                                         :hand      [copper copper remodel estate moat]
                                         :play-area []
                                         :deck      [copper estate]
                                         :discard   [copper estate silver]
                                         :actions   0
                                         :coins     0
                                         :buys      0
                                         :phase     :out-of-turn}})
           {:name      "John Doe"
            :hand      [{:name.ui         "Hand"
                         :number-of-cards 5}]
            :play-area []
            :deck      [{:name.ui         "Deck"
                         :number-of-cards 2}]
            :discard   [{:name    :silver
                         :name.ui "Silver"
                         :type    #{:treasure}}]
            :actions   0
            :money     0
            :buys      0}))
    (is (= (model-player false {:player {:name           "John Doe"
                                         :hand           [copper copper remodel estate moat]
                                         :hand-revealed? true
                                         :play-area      []
                                         :deck           [copper estate]
                                         :discard        [copper estate silver]
                                         :actions        0
                                         :coins          0
                                         :buys           0
                                         :phase          :out-of-turn}})
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
            :play-area []
            :deck      [{:name.ui         "Deck"
                         :number-of-cards 2}]
            :discard   [{:name    :silver
                         :name.ui "Silver"
                         :type    #{:treasure}}]
            :actions   0
            :money     0
            :buys      0}))
    (is (= (model-player false {:player {:name      "John Doe"
                                         :hand      [copper copper remodel estate moat]
                                         :play-area []
                                         :deck      [copper estate]
                                         :discard   [copper estate silver]
                                         :actions   0
                                         :coins     0
                                         :buys      0
                                         :phase     :action}
                                :choice {:text    "Discard down to 3 cards in hand."
                                         :source  :hand
                                         :min     2
                                         :max     2
                                         :options [:copper :copper :remodel :estate :moat]}})
           {:name      "John Doe"
            :hand      [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 2
                         :interaction     :choosable}
                        {:name            :remodel
                         :name.ui         "Remodel"
                         :type            #{:action}
                         :number-of-cards 1
                         :interaction     :choosable}
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
            :play-area []
            :deck      [{:name.ui         "Deck"
                         :number-of-cards 2}]
            :discard   [{:name    :silver
                         :name.ui "Silver"
                         :type    #{:treasure}}]
            :actions   0
            :money     0
            :buys      0
            :choice    {:text "Discard down to 3 cards in hand."
                        :min  2
                        :max  2}}))))

(deftest trash-model-test
  (testing "Trash model"
    (is (= (model-trash [] :full)
           []))
    (is (= (model-trash [] :short)
           []))
    (is (= (model-trash [copper copper estate] :full)
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 2}
            {:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 1}]))
    (is (= (model-trash [copper copper estate] :short)
           [{:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 3}]))))
