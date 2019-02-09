(ns dombot.view-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.front-end-view :refer :all]
            [dombot.cards.base-cards :refer :all]
            [dombot.cards.dominion :refer :all]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest supply-view-test
  (testing "Supply view"
    (is (= (view-supply {:supply [{:card copper :pile-size 46}
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
    (is (= (view-supply {:supply [{:card copper :pile-size 46}
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
    (is (= (view-supply {:supply [{:card copper :pile-size 0}
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
      (is (= (view-supply {:supply [{:card copper :pile-size 46}
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
      (is (= (view-supply {:supply [{:card copper :pile-size 46}
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
  (testing "Deck view"
    (is (= (view-deck {:player {:deck []}})
           {}))
    (is (= (view-deck {:player {:deck [copper copper copper]}})
           {:number-of-cards 3}))
    (is (= (view-deck {:player {:deck    [copper copper copper]
                                :look-at [estate estate]}})
           {:visible-cards   [{:name            :estate
                               :name.ui         "Estate"
                               :type            #{:victory}
                               :number-of-cards 2}]
            :number-of-cards 5}))
    (is (= (view-deck {:player {:deck     [copper copper copper]
                                :revealed [estate gold]}})
           {:visible-cards   [{:name    :estate
                               :name.ui "Estate"
                               :type    #{:victory}}
                              {:name    :gold
                               :name.ui "Gold"
                               :type    #{:treasure}}]
            :number-of-cards 5}))))

(deftest discard-test
  (testing "Discard view"
    (is (= (view-discard {:player {:discard []}})
           {}))
    (is (= (view-discard {:player {:discard             [estate]
                                   :approx-discard-size 1}})
           {:visible-cards   [{:name    :estate
                               :name.ui "Estate"
                               :type    #{:victory}}]
            :number-of-cards 1}))
    (testing "randomized number of cards"
      (is (= (view-discard {:player {:discard             [copper estate]
                                     :approx-discard-size 1}})
             {:visible-cards   [{:name    :estate
                                 :name.ui "Estate"
                                 :type    #{:victory}}]
              :number-of-cards 1}))
      (is (= (view-discard {:player {:discard             [copper estate]
                                     :approx-discard-size 2}})
             {:visible-cards   [{:name    :estate
                                 :name.ui "Estate"
                                 :type    #{:victory}}]
              :number-of-cards 2}))
      (is (= (view-discard {:player {:discard             [copper estate]
                                     :approx-discard-size 3}})
             {:visible-cards   [{:name    :estate
                                 :name.ui "Estate"
                                 :type    #{:victory}}]
              :number-of-cards 3})))
    (testing "with choice"
      (is (= (view-discard {:player {:discard             [copper estate]
                                     :approx-discard-size 2}
                            :choice {:source :hand}})
             {:visible-cards   [{:name    :estate
                                 :name.ui "Estate"
                                 :type    #{:victory}}]
              :number-of-cards 2}))
      (is (= (view-discard {:player {:discard             [copper estate]
                                     :approx-discard-size 2}
                            :choice {:source :discard :options [:estate]}})
             {:visible-cards   [{:name        :estate
                                 :name.ui     "Estate"
                                 :type        #{:victory}
                                 :interaction :choosable}]
              :number-of-cards 2}))
      (is (= (view-discard {:player {:discard             [copper estate]
                                     :approx-discard-size 2}
                            :choice {:source :discard :reveal-source true :options [:estate]}})
             {:visible-cards   [{:name    :copper
                                 :name.ui "Copper"
                                 :type    #{:treasure}}
                                {:name        :estate
                                 :name.ui     "Estate"
                                 :type        #{:victory}
                                 :interaction :choosable}]
              :number-of-cards 2})))
    (testing "revealed cards"
      (is (= (view-discard {:player {:discard             [copper silver estate estate]
                                     :approx-discard-size 4
                                     :revealed-cards      {:discard 2}}})
             {:visible-cards   [{:name            :estate
                                 :name.ui         "Estate"
                                 :type            #{:victory}
                                 :number-of-cards 2}]
              :number-of-cards 4}))
      (is (= (view-discard {:player {:discard             [copper silver gold estate]
                                     :approx-discard-size 4
                                     :revealed-cards      {:discard 2}}})
             {:visible-cards   [{:name    :gold
                                 :name.ui "Gold"
                                 :type    #{:treasure}}
                                {:name    :estate
                                 :name.ui "Estate"
                                 :type    #{:victory}}]
              :number-of-cards 4}))
      (is (= (view-discard {:player {:discard             [gold estate]
                                     :approx-discard-size 2
                                     :revealed-cards      {:discard 2}}})
             {:visible-cards   [{:name    :gold
                                 :name.ui "Gold"
                                 :type    #{:treasure}}
                                {:name    :estate
                                 :name.ui "Estate"
                                 :type    #{:victory}}]
              :number-of-cards 2}))
      (is (= (view-discard {:player {:discard             [gold estate]
                                     :approx-discard-size 3
                                     :revealed-cards      {:discard 2}}})
             {:visible-cards   [{:name    :gold
                                 :name.ui "Gold"
                                 :type    #{:treasure}}
                                {:name    :estate
                                 :name.ui "Estate"
                                 :type    #{:victory}}]
              :number-of-cards 3}))
      (is (= (view-discard {:player {:discard             [gold estate]
                                     :approx-discard-size 1
                                     :revealed-cards      {:discard 2}}})
             {:visible-cards   [{:name    :gold
                                 :name.ui "Gold"
                                 :type    #{:treasure}}
                                {:name    :estate
                                 :name.ui "Estate"
                                 :type    #{:victory}}]
              :number-of-cards 1})))))

(deftest hand-test
  (testing "Hand view"
    (is (= (view-hand true {:player {:hand    [copper copper copper moat estate]
                                     :actions 1
                                     :phase   :action}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 3
             :interaction     :playable}
            {:name        :moat
             :name.ui     "Moat"
             :type        #{:action :reaction}
             :interaction :playable}
            {:name    :estate
             :name.ui "Estate"
             :type    #{:victory}}]))
    (is (= (view-hand true {:player {:hand    [copper copper copper moat estate]
                                     :actions 0
                                     :phase   :action}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 3
             :interaction     :playable}
            {:name    :moat
             :name.ui "Moat"
             :type    #{:action :reaction}}
            {:name    :estate
             :name.ui "Estate"
             :type    #{:victory}}]))
    (is (= (view-hand true {:player {:hand    [copper copper copper moat estate]
                                     :actions 1
                                     :phase   :pay}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 3
             :interaction     :playable}
            {:name    :moat
             :name.ui "Moat"
             :type    #{:action :reaction}}
            {:name    :estate
             :name.ui "Estate"
             :type    #{:victory}}]))
    (is (= (view-hand true {:player {:hand    [copper copper copper moat estate]
                                     :actions 1
                                     :phase   :buy}})
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 3}
            {:name    :moat
             :name.ui "Moat"
             :type    #{:action :reaction}}
            {:name    :estate
             :name.ui "Estate"
             :type    #{:victory}}]))
    (testing "with choice"
      (is (= (view-hand true {:player {:hand  [copper copper copper moat estate]
                                       :phase :buy}
                              :choice {:source  :hand
                                       :options [:moat]
                                       :max     1}})
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :number-of-cards 3}
              {:name        :moat
               :name.ui     "Moat"
               :type        #{:action :reaction}
               :interaction :choosable}
              {:name    :estate
               :name.ui "Estate"
               :type    #{:victory}}]))
      (is (= (view-hand true {:player {:hand  [copper copper copper moat estate]
                                       :phase :buy}
                              :choice {:source  :hand
                                       :options [:estate]
                                       :min     1}})
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :number-of-cards 3}
              {:name    :moat
               :name.ui "Moat"
               :type    #{:action :reaction}}
              {:name        :estate
               :name.ui     "Estate"
               :type        #{:victory}
               :interaction :quick-choosable}]))
      (is (= (view-hand true {:player {:hand  [copper copper copper moat estate]
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
              {:name        :moat
               :name.ui     "Moat"
               :type        #{:action :reaction}
               :interaction :quick-choosable}
              {:name        :estate
               :name.ui     "Estate"
               :type        #{:victory}
               :interaction :quick-choosable}]))
      (is (= (view-hand true {:player {:hand  [copper copper copper moat estate]
                                       :phase :buy}
                              :choice {:source  :supply
                                       :options [:estate :copper :silver :moat]
                                       :max     1}})
             [{:name            :copper
               :name.ui         "Copper"
               :type            #{:treasure}
               :number-of-cards 3}
              {:name    :moat
               :name.ui "Moat"
               :type    #{:action :reaction}}
              {:name    :estate
               :name.ui "Estate"
               :type    #{:victory}}])))))

(deftest player-view-test
  (testing "Player view"
    (is (= (view-player true {:player {:name                "John Doe"
                                       :hand                [copper copper remodel estate moat]
                                       :play-area           [village]
                                       :deck                [copper estate]
                                       :discard             [copper estate silver]
                                       :approx-discard-size 3
                                       :actions             2
                                       :coins               0
                                       :buys                1
                                       :phase               :action}})
           {:name      "John Doe"
            :active?   true
            :hand      [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 2
                         :interaction     :playable}
                        {:name        :remodel
                         :name.ui     "Remodel"
                         :type        #{:action}
                         :interaction :playable}
                        {:name    :estate
                         :name.ui "Estate"
                         :type    #{:victory}}
                        {:name        :moat
                         :name.ui     "Moat"
                         :type        #{:action :reaction}
                         :interaction :playable}]
            :play-area [{:name    :village
                         :name.ui "Village"
                         :type    #{:action}}]
            :deck      {:number-of-cards 2}
            :discard   {:visible-cards   [{:name    :silver
                                           :name.ui "Silver"
                                           :type    #{:treasure}}]
                        :number-of-cards 3}
            :actions   2
            :money     0
            :buys      1}))
    (is (= (view-player true {:player {:name                "John Doe"
                                       :hand                [copper copper remodel estate moat]
                                       :play-area           [village]
                                       :deck                [copper estate]
                                       :discard             [copper estate silver]
                                       :approx-discard-size 3
                                       :actions             2
                                       :coins               0
                                       :buys                1
                                       :phase               :action}
                              :choice {:text    "Choice text"
                                       :source  :hand
                                       :options [:estate :moat]}})
           {:name      "John Doe"
            :active?   true
            :hand      [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 2}
                        {:name    :remodel
                         :name.ui "Remodel"
                         :type    #{:action}}
                        {:name        :estate
                         :name.ui     "Estate"
                         :type        #{:victory}
                         :interaction :choosable}
                        {:name        :moat
                         :name.ui     "Moat"
                         :type        #{:action :reaction}
                         :interaction :choosable}]
            :play-area [{:name    :village
                         :name.ui "Village"
                         :type    #{:action}}]
            :deck      {:number-of-cards 2}
            :discard   {:visible-cards   [{:name    :silver
                                           :name.ui "Silver"
                                           :type    #{:treasure}}]
                        :number-of-cards 3}
            :actions   2
            :money     0
            :buys      1
            :choice    {:text "Choice text"}}))
    (is (= (view-player true {:player {:name                "John Doe"
                                       :hand                [copper copper remodel estate moat]
                                       :play-area           [village]
                                       :deck                [copper estate]
                                       :discard             [copper estate silver]
                                       :approx-discard-size 3
                                       :actions             2
                                       :coins               0
                                       :buys                1
                                       :phase               :action}
                              :choice {:text          "Choice text"
                                       :source        :discard
                                       :reveal-source true
                                       :options       [:estate]
                                       :min           1
                                       :quick-choice  true}})
           {:name      "John Doe"
            :active?   true
            :hand      [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 2}
                        {:name    :remodel
                         :name.ui "Remodel"
                         :type    #{:action}}
                        {:name    :estate
                         :name.ui "Estate"
                         :type    #{:victory}}
                        {:name    :moat
                         :name.ui "Moat"
                         :type    #{:action :reaction}}]
            :play-area [{:name    :village
                         :name.ui "Village"
                         :type    #{:action}}]
            :deck      {:number-of-cards 2}
            :discard   {:visible-cards   [{:name    :copper
                                           :name.ui "Copper"
                                           :type    #{:treasure}}
                                          {:name        :estate
                                           :name.ui     "Estate"
                                           :type        #{:victory}
                                           :interaction :quick-choosable}
                                          {:name    :silver
                                           :name.ui "Silver"
                                           :type    #{:treasure}}]
                        :number-of-cards 3}
            :actions   2
            :money     0
            :buys      1
            :choice    {:text         "Choice text"
                        :min          1
                        :quick-choice true}})
        (is (= (view-player false {:player {:name                "John Doe"
                                            :hand                [copper copper remodel estate moat]
                                            :play-area           []
                                            :deck                [copper estate]
                                            :discard             [copper estate silver]
                                            :approx-discard-size 3
                                            :actions             0
                                            :coins               0
                                            :buys                0
                                            :phase               :out-of-turn}})
               {:name      "John Doe"
                :hand      [{:name.ui         "Hand"
                             :number-of-cards 5}]
                :play-area []
                :deck      {:number-of-cards 2}
                :discard   {:visible-cards   [{:name    :silver
                                               :name.ui "Silver"
                                               :type    #{:treasure}}]
                            :number-of-cards 3}
                :actions   0
                :money     0
                :buys      0})))
    (is (= (view-player false {:player {:name                "John Doe"
                                        :hand                [copper copper remodel estate moat]
                                        :revealed-cards      {:hand 5}
                                        :play-area           []
                                        :deck                [copper estate]
                                        :discard             [copper estate silver]
                                        :approx-discard-size 3
                                        :actions             0
                                        :coins               0
                                        :buys                0
                                        :phase               :out-of-turn}})
           {:name      "John Doe"
            :hand      [{:name            :copper
                         :name.ui         "Copper"
                         :type            #{:treasure}
                         :number-of-cards 2}
                        {:name    :remodel
                         :name.ui "Remodel"
                         :type    #{:action}}
                        {:name    :estate
                         :name.ui "Estate"
                         :type    #{:victory}}
                        {:name    :moat
                         :name.ui "Moat"
                         :type    #{:action :reaction}}]
            :play-area []
            :deck      {:number-of-cards 2}
            :discard   {:visible-cards   [{:name    :silver
                                           :name.ui "Silver"
                                           :type    #{:treasure}}]
                        :number-of-cards 3}
            :actions   0
            :money     0
            :buys      0}))
    (is (= (view-player false {:player {:name                "John Doe"
                                        :hand                [copper copper remodel estate moat]
                                        :play-area           []
                                        :deck                [copper estate]
                                        :discard             [copper estate silver]
                                        :approx-discard-size 3
                                        :actions             0
                                        :coins               0
                                        :buys                0
                                        :phase               :action}
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
                        {:name        :remodel
                         :name.ui     "Remodel"
                         :type        #{:action}
                         :interaction :choosable}
                        {:name        :estate
                         :name.ui     "Estate"
                         :type        #{:victory}
                         :interaction :choosable}
                        {:name        :moat
                         :name.ui     "Moat"
                         :type        #{:action :reaction}
                         :interaction :choosable}]
            :play-area []
            :deck      {:number-of-cards 2}
            :discard   {:visible-cards   [{:name    :silver
                                           :name.ui "Silver"
                                           :type    #{:treasure}}]
                        :number-of-cards 3}
            :actions   0
            :money     0
            :buys      0
            :choice    {:text "Discard down to 3 cards in hand."
                        :min  2
                        :max  2}}))
    (is (= (view-player true {:player {:name                "John Doe"
                                       :hand                []
                                       :play-area           [library]
                                       :deck                []
                                       :discard             []
                                       :approx-discard-size 0
                                       :set-aside           [festival village]
                                       :actions             0
                                       :coins               0
                                       :buys                0
                                       :phase               :action}})
           {:name      "John Doe"
            :active?   true
            :hand      []
            :play-area [{:name    :library
                         :name.ui "Library"
                         :type    #{:action}}]
            :set-aside [{:name    :festival
                         :name.ui "Festival"
                         :type    #{:action}}
                        {:name    :village
                         :name.ui "Village"
                         :type    #{:action}}]
            :deck      {}
            :discard   {}
            :actions   0
            :money     0
            :buys      0}))
    (is (= (view-player false {:player {:name           "John Doe"
                                        :hand           [copper copper remodel estate moat]
                                        :actions        0
                                        :coins          0
                                        :buys           0
                                        :phase          :end-of-game
                                        :victory-points 21
                                        :winner         true}})
           {:name           "John Doe"
            :hand           [{:name            :copper
                              :name.ui         "Copper"
                              :type            #{:treasure}
                              :number-of-cards 2}
                             {:name    :remodel
                              :name.ui "Remodel"
                              :type    #{:action}}
                             {:name    :estate
                              :name.ui "Estate"
                              :type    #{:victory}}
                             {:name    :moat
                              :name.ui "Moat"
                              :type    #{:action :reaction}}]
            :play-area      []
            :deck           {}
            :discard        {}
            :actions        0
            :money          0
            :buys           0
            :victory-points 21
            :winner         true}))))

(deftest trash-view-test
  (testing "Trash view"
    (is (= (view-trash [] :full)
           []))
    (is (= (view-trash [] :compressed)
           []))
    (is (= (view-trash [copper copper estate] :full)
           [{:name            :copper
             :name.ui         "Copper"
             :type            #{:treasure}
             :number-of-cards 2}
            {:name    :estate
             :name.ui "Estate"
             :type    #{:victory}}]))
    (is (= (view-trash [copper copper estate] :compressed)
           [{:name            :estate
             :name.ui         "Estate"
             :type            #{:victory}
             :number-of-cards 3}]))))