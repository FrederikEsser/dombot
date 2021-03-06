(ns dombot.cards.empires-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.empires :as empires :refer :all]
            [dombot.cards.dominion :refer [market merchant throne-room]]
            [dombot.cards.intrigue :refer [mill]]
            [dombot.cards.seaside :refer [ambassador embargo fishing-village outpost]]
            [dombot.cards.prosperity :as prosperity :refer [hoard]]
            [dombot.cards.adventures :as adventures :refer [caravan-guard]]
            [dombot.cards.nocturne :as nocturne :refer [crypt ghost]]
            [dombot.cards.renaissance :as renaissance :refer [patron spices canal capitalism citadel innovation piazza]]
            [dombot.cards.kingdom :refer [setup-game]]
            [dombot.utils :as ut])
  (:import (com.sun.javafx.scene.layout.region RepeatStruct)))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest split-pile-test
  (let [patrician (assoc patrician :id 0)
        emporium  (assoc emporium :id 1)]
    (testing "Split piles"
      (testing "gaining"
        (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 5}
                                            {:card emporium :pile-size 5}]}]
                    :players [{}]}
                   (gain {:player-no 0 :card-name :patrician}))
               {:supply  [{:split-pile [{:card patrician :pile-size 4}
                                        {:card emporium :pile-size 5}]}]
                :players [{:discard [patrician]}]}))
        (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                            {:card emporium :pile-size 5}]}]
                    :players [{}]}
                   (gain {:player-no 0 :card-name :patrician}))
               {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                        {:card emporium :pile-size 5}]}]
                :players [{}]}))
        (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 1}
                                            {:card emporium :pile-size 5}]}]
                    :players [{}]}
                   (gain {:player-no 0 :card-name :emporium}))
               {:supply  [{:split-pile [{:card patrician :pile-size 1}
                                        {:card emporium :pile-size 5}]}]
                :players [{}]}))
        (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                            {:card emporium :pile-size 5}]}]
                    :players [{}]}
                   (gain {:player-no 0 :card-name :emporium}))
               {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                        {:card emporium :pile-size 4}]}]
                :players [{:discard [emporium]}]})))
      (testing "empty pile"
        (is (= (-> {:supply [{:split-pile [{:card patrician :pile-size 5}
                                           {:card emporium :pile-size 5}]}]}
                   (ut/empty-supply-piles))
               0))
        (is (= (-> {:supply [{:split-pile [{:card patrician :pile-size 0}
                                           {:card emporium :pile-size 5}]}]}
                   (ut/empty-supply-piles))
               0))
        (is (= (-> {:supply [{:split-pile [{:card patrician :pile-size 0}
                                           {:card emporium :pile-size 0}]}]}
                   (ut/empty-supply-piles))
               1))
        (is (= (-> {:supply [{:split-pile [{:card patrician :pile-size 1}
                                           {:card emporium :pile-size 0}]}]}
                   (ut/empty-supply-piles))
               0)))
      (testing "choosing"
        (is (= (-> {:supply [{:split-pile [{:card patrician :pile-size 5}
                                           {:card emporium :pile-size 5}]}]}
                   (ut/options-from-supply 0 nil {:max-cost 4}))
               [:patrician]))
        (is (= (-> {:supply [{:split-pile [{:card patrician :pile-size 0}
                                           {:card emporium :pile-size 5}]}]}
                   (ut/options-from-supply 0 nil {:max-cost 4}))
               []))
        (is (= (-> {:supply [{:split-pile [{:card patrician :pile-size 5}
                                           {:card emporium :pile-size 5}]}]}
                   (ut/options-from-supply 0 nil {:max-cost 5}))
               [:patrician]))
        (is (= (-> {:supply [{:split-pile [{:card patrician :pile-size 0}
                                           {:card emporium :pile-size 5}]}]}
                   (ut/options-from-supply 0 nil {:max-cost 5}))
               [:emporium])))
      (testing "with tokens"
        (let [embargo (assoc embargo :id 2)
              curse   (assoc curse :id 3)]
          (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 5}
                                              {:card emporium :pile-size 5}]}]
                      :players [{:hand    [embargo]
                                 :actions 1
                                 :coins   0}]}
                     (play 0 :embargo)
                     (choose :patrician))
                 {:supply  [{:split-pile [{:card patrician :pile-size 5}
                                          {:card emporium :pile-size 5}]
                             :tokens     {:embargo {:number-of-tokens 1
                                                    :on-buy           [[:gain {:card-name :curse}]]}}}]
                  :players [{:actions 0
                             :coins   2}]
                  :trash   [embargo]}))
          (is (= (-> {:supply  [{:card curse :pile-size 10}
                                {:split-pile [{:card patrician :pile-size 5}
                                              {:card emporium :pile-size 5}]
                                 :tokens     {:embargo {:number-of-tokens 1
                                                        :on-buy           [[:gain {:card-name :curse}]]}}}]
                      :players [{:coins 2
                                 :buys  1}]}
                     (buy-card 0 :patrician))
                 {:supply  [{:card curse :pile-size 9}
                            {:split-pile [{:card patrician :pile-size 4}
                                          {:card emporium :pile-size 5}]
                             :tokens     {:embargo {:number-of-tokens 1
                                                    :on-buy           [[:gain {:card-name :curse}]]}}}]
                  :players [{:discard [curse patrician]
                             :coins   0
                             :buys    0}]}))
          (is (= (-> {:supply  [{:card curse :pile-size 10}
                                {:split-pile [{:card patrician :pile-size 0}
                                              {:card emporium :pile-size 5}]
                                 :tokens     {:embargo {:number-of-tokens 1
                                                        :on-buy           [[:gain {:card-name :curse}]]}}}]
                      :players [{:coins 5
                                 :buys  1}]}
                     (buy-card 0 :emporium))
                 {:supply  [{:card curse :pile-size 9}
                            {:split-pile [{:card patrician :pile-size 0}
                                          {:card emporium :pile-size 4}]
                             :tokens     {:embargo {:number-of-tokens 1
                                                    :on-buy           [[:gain {:card-name :curse}]]}}}]
                  :players [{:discard [curse emporium]
                             :coins   0
                             :buys    0}]}))))
      (testing "returning split-pile to supply"
        (let [ambassador (assoc ambassador :id 2)]
          (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 4}
                                              {:card emporium :pile-size 5}]}]
                      :players [{:hand    [ambassador patrician]
                                 :actions 1}
                                {}]}
                     (play 0 :ambassador)
                     (choose :patrician)
                     (choose :patrician))
                 {:supply  [{:split-pile [{:card patrician :pile-size 4}
                                          {:card emporium :pile-size 5}]}]
                  :players [{:play-area [ambassador]
                             :actions   0}
                            {:discard [patrician]}]}))
          (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                              {:card emporium :pile-size 3}]}]
                      :players [{:hand    [ambassador patrician patrician]
                                 :actions 1}
                                {}]}
                     (play 0 :ambassador)
                     (choose :patrician)
                     (choose [:patrician :patrician]))
                 {:supply  [{:split-pile [{:card patrician :pile-size 1}
                                          {:card emporium :pile-size 3}]}]
                  :players [{:play-area [ambassador]
                             :actions   0}
                            {:discard [patrician]}]}))
          (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                              {:card emporium :pile-size 3}]}]
                      :players [{:hand    [ambassador emporium emporium]
                                 :actions 1}
                                {}]}
                     (play 0 :ambassador)
                     (choose :emporium)
                     (choose [:emporium :emporium]))
                 {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                          {:card emporium :pile-size 4}]}]
                  :players [{:play-area [ambassador]
                             :actions   0}
                            {:discard [emporium]}]}))
          (ut/reset-ids!)
          (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 1}
                                              {:card emporium :pile-size 3}]}]
                      :players [{:hand    [ambassador emporium emporium]
                                 :actions 1}
                                {}]}
                     (play 0 :ambassador)
                     (choose :emporium)
                     (choose :emporium))
                 {:supply  [{:split-pile [{:card patrician :pile-size 1}
                                          {:card emporium :pile-size 3}]}]
                  :players [{:hand      [emporium]
                             :play-area [ambassador]
                             :actions   0}
                            {:discard [emporium]}]}))
          (ut/reset-ids!)
          (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 1}
                                              {:card emporium :pile-size 3}]}]
                      :players [{:hand    [ambassador emporium emporium]
                                 :actions 1}
                                {}]}
                     (play 0 :ambassador)
                     (choose :emporium)
                     (choose [:emporium :emporium]))
                 {:supply  [{:split-pile [{:card empires/emporium :pile-size 1}
                                          {:card patrician :pile-size 1}
                                          {:card emporium :pile-size 3}]}]
                  :players [{:play-area [ambassador]
                             :actions   0}
                            {:discard [emporium]}]}))))
      (testing "gaining the last card of a kind"
        (let [humble-castle (assoc humble-castle :id 0)
              gold          (assoc gold :id 1)]
          (is (= (-> {:supply  [{:card gold :pile-size 30}
                                {:split-pile [{:card humble-castle :pile-size 1}
                                              {:card crumbling-castle :pile-size 1}]}]
                      :players [{:play-area [hoard copper]
                                 :coins     3
                                 :buys      1}]}
                     (buy-card 0 :humble-castle))
                 {:supply  [{:card gold :pile-size 29}
                            {:split-pile [{:card humble-castle :pile-size 0}
                                          {:card crumbling-castle :pile-size 1}]}]
                  :players [{:play-area [hoard copper]
                             :discard   [gold humble-castle]
                             :coins     0
                             :buys      0}]}))
          (is (= (-> {:supply  [{:split-pile [{:card humble-castle :pile-size 1}
                                              {:card crumbling-castle :pile-size 1}]
                                 :tokens     {:trade-route {:number-of-tokens 1
                                                            :on-gain          [[::prosperity/trade-route-move-token]]}}}]
                      :players [{:coins 3
                                 :buys  1}]}
                     (buy-card 0 :humble-castle))
                 {:trade-route-mat 1
                  :supply          [{:split-pile [{:card humble-castle :pile-size 0}
                                                  {:card crumbling-castle :pile-size 1}]}]
                  :players         [{:discard [humble-castle]
                                     :coins   0
                                     :buys    0}]}))
          (is (= (-> {:supply  [{:split-pile [{:card humble-castle :pile-size 1}
                                              {:card crumbling-castle :pile-size 1}]}]
                      :players [{:coins    3
                                 :buys     1
                                 :triggers [{:name     :road-network
                                             :duration :game
                                             :event    :on-gain
                                             :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}]}
                                {:hand [copper copper copper copper copper]
                                 :deck [silver silver]}]}
                     (buy-card 0 :humble-castle))
                 {:supply  [{:split-pile [{:card humble-castle :pile-size 0}
                                          {:card crumbling-castle :pile-size 1}]}]
                  :players [{:discard  [humble-castle]
                             :coins    0
                             :buys     0
                             :triggers [{:name     :road-network
                                         :duration :game
                                         :event    :on-gain
                                         :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}]}
                            {:hand [copper copper copper copper copper silver]
                             :deck [silver]}]})))))))

(deftest debt-test
  (let [engineer (assoc engineer :id 0)
        silver   (assoc silver :id 1)]
    (testing "Debt: "
      (testing "Buying card with debt cost"
        (is (= (-> {:supply  [{:card engineer :pile-size 10}]
                    :players [{:coins 0
                               :buys  1}]}
                   (buy-card 0 :engineer))
               {:supply  [{:card engineer :pile-size 9}]
                :players [{:discard [engineer]
                           :coins   0
                           :debt    4
                           :buys    0}]}))
        (is (= (-> {:supply  [{:card engineer :pile-size 10}]
                    :players [{:coins 4
                               :buys  1}]}
                   (buy-card 0 :engineer))
               {:supply  [{:card engineer :pile-size 9}]
                :players [{:discard [engineer]
                           :coins   4
                           :debt    4
                           :buys    0}]}))
        (is (thrown-with-msg? AssertionError #"Buy error:"
                              (-> {:supply  [{:card engineer :pile-size 10}]
                                   :players [{:coins 0
                                              :debt  1
                                              :buys  1}]}
                                  (buy-card 0 :engineer)))))
      (testing "Buying while in debt"
        (is (= (-> {:supply  [{:card engineer :pile-size 10}]
                    :players [{:coins 1
                               :debt  1
                               :buys  1}]}
                   (buy-card 0 :engineer))
               {:supply  [{:card engineer :pile-size 9}]
                :players [{:discard [engineer]
                           :coins   0
                           :debt    4
                           :buys    0}]}))
        (is (thrown-with-msg? AssertionError #"Buy error:"
                              (-> {:supply  [{:card silver :pile-size 40}]
                                   :players [{:coins 3
                                              :debt  1
                                              :buys  1}]}
                                  (buy-card 0 :silver))))
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:coins 4
                               :debt  1
                               :buys  1}]}
                   (buy-card 0 :silver))
               {:supply  [{:card silver :pile-size 39}]
                :players [{:discard [silver]
                           :coins   0
                           :buys    0}]}))
        (is (thrown-with-msg? AssertionError #"Buy error:"
                              (-> {:events  {:delve delve}
                                   :supply  [{:card silver :pile-size 40}]
                                   :players [{:coins 2
                                              :debt  1
                                              :buys  1}]}
                                  (buy-event 0 :delve))))
        (is (= (-> {:events  {:delve delve}
                    :supply  [{:card silver :pile-size 40}]
                    :players [{:coins 3
                               :debt  1
                               :buys  1}]}
                   (buy-event 0 :delve))
               {:events  {:delve delve}
                :supply  [{:card silver :pile-size 39}]
                :players [{:discard [silver]
                           :coins   0
                           :buys    1}]}))
        (is (thrown-with-msg? AssertionError #"Buy error:"
                              (-> {:events  {:advance advance}
                                   :players [{:coins 0
                                              :debt  1
                                              :buys  1}]}
                                  (buy-event 0 :advance))))
        (is (thrown-with-msg? AssertionError #"Buy error:"
                              (-> {:events  {:delve delve}
                                   :supply  [{:card silver :pile-size 40}]
                                   :players [{:coins 2
                                              :debt  1
                                              :buys  1}]}
                                  (buy-event 0 :delve))))
        (is (thrown-with-msg? AssertionError #"Buy error:"
                              (-> {:projects {:canal canal}
                                   :players  [{:coins 7
                                               :debt  1
                                               :buys  1}]}
                                  (buy-project 0 :canal))))
        (is (= (-> {:projects {:canal canal}
                    :players  [{:coins 8
                                :debt  1
                                :buys  1}]}
                   (buy-project 0 :canal))
               {:projects {:canal (assoc canal :participants [{:player-no 0}])}
                :players  [{:coins           0
                            :buys            0
                            :cost-reductions [{:reduction 1}]}]})))
      (testing "Repaying debt"
        (is (= (-> {:players [{:actions 0
                               :coins   0
                               :debt    4
                               :buys    1
                               :phase   :buy}]}
                   (end-turn 0))
               {:current-player 0
                :players        [{:actions 1
                                  :coins   0
                                  :debt    4
                                  :buys    1
                                  :phase   :action}]}))
        (is (= (-> {:players [{:actions 0
                               :coins   1
                               :debt    4
                               :buys    1
                               :phase   :buy}]}
                   (end-turn 0))
               {:current-player 0
                :players        [{:actions 1
                                  :coins   0
                                  :debt    3
                                  :buys    1
                                  :phase   :action}]}))
        (is (= (-> {:players [{:actions 0
                               :coins   4
                               :debt    4
                               :buys    1
                               :phase   :buy}]}
                   (end-turn 0))
               {:current-player 0
                :players        [{:actions 1
                                  :coins   0
                                  :buys    1
                                  :phase   :action}]}))
        (is (= (-> {:players [{:actions 0
                               :coins   5
                               :debt    4
                               :buys    1
                               :phase   :buy}]}
                   (end-turn 0))
               {:current-player 0
                :players        [{:actions 1
                                  :coins   0
                                  :buys    1
                                  :phase   :action}]}))))))

(deftest archive-test
  (let [archive (assoc archive :id 0)]
    (testing "Archive"
      (is (= (-> {:players [{:hand    [archive]
                             :actions 1}]}
                 (play 0 :archive))
             {:players [{:play-area [archive]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [archive]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :archive))
             {:players [{:hand      [copper]
                         :play-area [archive]
                         :actions   1}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [archive]
                             :deck    [copper silver]
                             :actions 1}]}
                 (play 0 :archive)
                 (choose :silver))
             {:players [{:hand      [silver]
                         :play-area [archive]
                         :actions   1
                         :triggers  [(merge archive-trigger
                                            {:id        1
                                             :card-id   0
                                             :name      :archive
                                             :set-aside [copper]})]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [archive]
                             :deck    [copper silver gold]
                             :actions 1}]}
                 (play 0 :archive)
                 (choose :silver))
             {:players [{:hand      [silver]
                         :play-area [archive]
                         :actions   1
                         :triggers  [(merge archive-trigger
                                            {:id        1
                                             :card-id   0
                                             :name      :archive
                                             :set-aside [copper gold]})]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [archive]
                             :deck    [copper silver gold estate]
                             :actions 1}]}
                 (play 0 :archive)
                 (choose :gold))
             {:players [{:hand      [gold]
                         :play-area [archive]
                         :deck      [estate]
                         :actions   1
                         :triggers  [(merge archive-trigger
                                            {:id        1
                                             :card-id   0
                                             :name      :archive
                                             :set-aside [copper silver]})]}]}))
      (is (= (-> {:players [{:hand      [gold]
                             :play-area [archive]
                             :deck      [estate]
                             :actions   1
                             :phase     :action
                             :triggers  [(merge archive-trigger
                                                {:id        1
                                                 :card-id   0
                                                 :name      :archive
                                                 :set-aside [copper silver]})]}]}
                 (end-turn 0)
                 (choose :silver))
             {:current-player 0
              :players        [{:hand      [estate gold silver]
                                :play-area [archive]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action
                                :triggers  [(merge archive-trigger
                                                   {:id        1
                                                    :card-id   0
                                                    :name      :archive
                                                    :set-aside [copper]})]}]}))
      (is (= (-> {:players [{:play-area [archive]
                             :actions   1
                             :phase     :action
                             :triggers  [(merge archive-trigger
                                                {:id        1
                                                 :card-id   0
                                                 :name      :archive
                                                 :set-aside [copper]})]}]}
                 (end-turn 0)
                 (choose :copper))
             {:current-player 0
              :players        [{:hand      [copper]
                                :play-area [archive]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (let [crown (assoc crown :id 1)]
        (ut/reset-ids!)
        (is (= (-> {:players [{:hand    [crown archive]
                               :deck    [copper copper copper silver silver silver gold]
                               :actions 1
                               :phase   :action}]}
                   (play 0 :crown)
                   (choose :archive)
                   (choose :copper)
                   (choose :silver))
               {:players [{:hand          [copper silver]
                           :play-area     [crown archive]
                           :deck          [gold]
                           :actions       2
                           :phase         :action
                           :repeated-play [{:source 1 :target 0}]
                           :triggers      [(merge archive-trigger
                                                  {:id        1
                                                   :card-id   0
                                                   :name      :archive
                                                   :set-aside [copper copper]})
                                           (merge archive-trigger
                                                  {:id        2
                                                   :card-id   0
                                                   :name      :archive
                                                   :set-aside [silver silver]})]}]}))
        (is (= (-> {:players [{:play-area     [crown archive]
                               :actions       2
                               :phase         :action
                               :repeated-play [{:source 1 :target 0}]
                               :triggers      [(merge archive-trigger
                                                      {:id        1
                                                       :card-id   0
                                                       :name      :archive
                                                       :set-aside [copper copper]})
                                               (merge archive-trigger
                                                      {:id        2
                                                       :card-id   0
                                                       :name      :archive
                                                       :set-aside [silver silver]})]}]}
                   (end-turn 0)
                   (choose {:area :play-area :card-name :archive})
                   (choose :copper)
                   (choose :silver))
               {:current-player 0
                :players        [{:hand          [copper silver]
                                  :play-area     [crown archive]
                                  :actions       1
                                  :coins         0
                                  :buys          1
                                  :phase         :action
                                  :repeated-play [{:source 1 :target 0}]
                                  :triggers      [(merge archive-trigger
                                                         {:id        1
                                                          :card-id   0
                                                          :name      :archive
                                                          :set-aside [copper]})
                                                  (merge archive-trigger
                                                         {:id        2
                                                          :card-id   0
                                                          :name      :archive
                                                          :set-aside [silver]})]}]}))
        (is (= (-> {:players [{:play-area     [crown archive]
                               :actions       2
                               :phase         :action
                               :repeated-play [{:source 1 :target 0}]
                               :triggers      [(merge archive-trigger
                                                      {:id        1
                                                       :card-id   0
                                                       :name      :archive
                                                       :set-aside [copper]})
                                               (merge archive-trigger
                                                      {:id        2
                                                       :card-id   0
                                                       :name      :archive
                                                       :set-aside [silver]})]}]}
                   (end-turn 0)
                   (choose {:area :play-area :card-name :archive})
                   (choose :copper)
                   (choose :silver))
               {:current-player 0
                :players        [{:hand      [copper silver]
                                  :play-area [crown archive]
                                  :actions   1
                                  :coins     0
                                  :buys      1
                                  :phase     :action}]}))
        (is (= (-> {:players [{:play-area     [crown archive]
                               :actions       2
                               :phase         :action
                               :repeated-play [{:source 1 :target 0}]
                               :triggers      [(merge archive-trigger
                                                      {:id        1
                                                       :card-id   0
                                                       :name      :archive
                                                       :set-aside [copper copper]})
                                               (merge archive-trigger
                                                      {:id        2
                                                       :card-id   0
                                                       :name      :archive
                                                       :set-aside [silver]})]}]}
                   (end-turn 0)
                   (choose {:area :play-area :card-name :archive})
                   (choose :copper)
                   (choose :silver))
               {:current-player 0
                :players        [{:hand          [copper silver]
                                  :play-area     [crown archive]
                                  :actions       1
                                  :coins         0
                                  :buys          1
                                  :phase         :action
                                  :repeated-play [{:source 1 :target 0}]
                                  :triggers      [(merge archive-trigger
                                                         {:id        1
                                                          :card-id   0
                                                          :name      :archive
                                                          :set-aside [copper]})]}]}))))))

(deftest capital-test
  (let [capital  (assoc capital :id 0)
        crown    (assoc crown :id 1)
        province (assoc province :id 2)]
    (testing "Capital"
      (is (= (-> {:players [{:hand  [capital]
                             :coins 0
                             :buys  1}]}
                 (play 0 :capital))
             {:players [{:play-area [capital]
                         :coins     6
                         :buys      2}]}))
      (is (= (-> {:players [{:play-area [capital]
                             :coins     0
                             :buys      0
                             :phase     :buy}]}
                 (clean-up {:player-no 0}))
             {:players [{:hand    [capital]
                         :actions 0
                         :coins   0
                         :debt    6
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:players [{:play-area [capital]
                             :coins     4
                             :buys      0
                             :phase     :buy}]}
                 (clean-up {:player-no 0}))
             {:players [{:hand    [capital]
                         :actions 0
                         :coins   0
                         :debt    2
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:players [{:hand  [capital capital]
                             :coins 0
                             :buys  1
                             :phase :pay}]}
                 (play 0 :capital)
                 (play 0 :capital))
             {:players [{:play-area [capital capital]
                         :coins     12
                         :buys      3
                         :phase     :pay}]}))
      (is (= (-> {:supply  [{:card province :pile-size 8}]
                  :players [{:hand  [capital capital]
                             :deck  (repeat 5 copper)
                             :coins 0
                             :buys  1
                             :phase :pay}]}
                 (play 0 :capital)
                 (play 0 :capital)
                 (buy-card 0 :province)
                 (clean-up {:player-no 0}))
             {:supply  [{:card province :pile-size 7}]
              :players [{:hand    (repeat 5 copper)
                         :discard [province capital capital]
                         :actions 0
                         :coins   0
                         :debt    8
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:players [{:hand  [crown capital]
                             :coins 0
                             :buys  1
                             :phase :pay}]}
                 (play 0 :crown)
                 (choose :capital))
             {:players [{:play-area [crown capital]
                         :coins     12
                         :buys      3
                         :phase     :pay}]}))
      (is (= (-> {:supply  [{:card province :pile-size 8}]
                  :players [{:hand  [crown capital]
                             :deck  (repeat 5 copper)
                             :coins 0
                             :buys  1
                             :phase :pay}]}
                 (play 0 :crown)
                 (choose :capital)
                 (buy-card 0 :province)
                 (clean-up {:player-no 0}))
             {:supply  [{:card province :pile-size 7}]
              :players [{:hand    (repeat 5 copper)
                         :discard [province crown capital]
                         :actions 0
                         :coins   0
                         :debt    2
                         :buys    0
                         :phase   :out-of-turn}]}))
      (ut/reset-ids!)
      (let [crypt (assoc crypt :id 3)]
        (is (= (-> {:players [{:hand      [crypt]
                               :play-area [capital]
                               :phase     :buy}]}
                   (play 0 :crypt)
                   (choose :capital)
                   (clean-up {:player-no 0}))
               {:players [{:play-area [crypt]
                           :actions   0
                           :coins     0
                           :buys      0
                           :phase     :out-of-turn
                           :triggers  [(merge nocturne/crypt-trigger
                                              {:id        1
                                               :card-id   3
                                               :name      :crypt
                                               :set-aside [capital]})]}]}))))))

(deftest humble-castle-test
  (testing "Humble Castle"
    (is (= (-> {:players [{:hand  [humble-castle]
                           :coins 0}]}
               (play 0 :humble-castle))
           {:players [{:play-area [humble-castle]
                       :coins     1}]}))))

(deftest crumbling-castle-test
  (let [crumbling-castle (assoc crumbling-castle :id 0)
        silver           (assoc silver :id 1)]
    (testing "Crumbling Castle"
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:split-pile [{:card crumbling-castle :pile-size 1}
                                          {:card small-castle :pile-size 1}]}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-card 0 :crumbling-castle))
             {:supply  [{:card silver :pile-size 39}
                        {:split-pile [{:card crumbling-castle :pile-size 0}
                                      {:card small-castle :pile-size 1}]}]
              :players [{:discard   [silver crumbling-castle]
                         :coins     0
                         :buys      0
                         :vp-tokens 1}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 39}
                            {:split-pile [{:card crumbling-castle :pile-size 0}
                                          {:card small-castle :pile-size 1}]}]
                  :players [{:hand [crumbling-castle]}]}
                 (trash-from-hand {:player-no 0
                                   :card-name :crumbling-castle})
                 (check-stack))
             {:supply  [{:card silver :pile-size 38}
                        {:split-pile [{:card crumbling-castle :pile-size 0}
                                      {:card small-castle :pile-size 1}]}]
              :players [{:discard   [silver]
                         :vp-tokens 1}]
              :trash   [crumbling-castle]})))))

(deftest small-castle-test
  (let [small-castle (assoc small-castle :id 0)]
    (testing "Small Castle"
      (is (= (-> {:supply  [{:split-pile [{:card small-castle :pile-size 1}]}]
                  :players [{:hand    [small-castle]
                             :actions 1}]}
                 (play 0 :small-castle)
                 (choose {:area :play-area :card-name :small-castle})
                 (choose :small-castle))
             {:supply  [{:split-pile [{:card small-castle :pile-size 0}]}]
              :players [{:discard [small-castle]
                         :actions 0}]
              :trash   [small-castle]}))
      (is (= (-> {:supply  [{:split-pile [{:card small-castle :pile-size 1}]}]
                  :players [{:hand    [small-castle humble-castle]
                             :actions 1}]}
                 (play 0 :small-castle)
                 (choose {:area :hand :card-name :humble-castle})
                 (choose :small-castle))
             {:supply  [{:split-pile [{:card small-castle :pile-size 0}]}]
              :players [{:play-area [small-castle]
                         :discard   [small-castle]
                         :actions   0}]
              :trash   [humble-castle]}))
      (is (= (-> {:supply  [{:split-pile [{:card small-castle :pile-size 2}]}]
                  :players [{:hand    [throne-room small-castle]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :small-castle)
                 (choose {:area :play-area :card-name :small-castle})
                 (choose :small-castle))
             {:supply  [{:split-pile [{:card small-castle :pile-size 1}]}]
              :players [{:play-area [throne-room]
                         :discard   [small-castle]
                         :actions   0}]
              :trash   [small-castle]}))
      (is (= (-> {:supply  [{:split-pile [{:card small-castle :pile-size 2}]}]
                  :players [{:hand    [throne-room small-castle humble-castle]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :small-castle)                     ; Throne Room Small Castle
                 (choose {:area :hand :card-name :humble-castle}) ; trash Humble Castle
                 (choose :small-castle)                     ; gain Small Castle
                 (choose {:area :play-area :card-name :small-castle}) ; trash Small Castle
                 (choose :small-castle))                    ; gain Small Castle
             {:supply  [{:split-pile [{:card small-castle :pile-size 0}]}]
              :players [{:play-area [throne-room]
                         :discard   [small-castle small-castle]
                         :actions   0}]
              :trash   [humble-castle small-castle]}))
      (is (= (-> {:supply  [{:split-pile [{:card small-castle :pile-size 2}]}]
                  :players [{:hand    [throne-room small-castle humble-castle]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :small-castle)                     ; Throne Room Small Castle
                 (choose {:area :play-area :card-name :small-castle}) ; trash Small Castle
                 (choose :small-castle)                     ; gain Small Castle
                 (choose {:area :hand :card-name :humble-castle}) ; trash Humble Castle
                 (choose :small-castle))                    ; gain Small Castle
             {:supply  [{:split-pile [{:card small-castle :pile-size 0}]}]
              :players [{:play-area [throne-room]
                         :discard   [small-castle small-castle]
                         :actions   0}]
              :trash   [small-castle humble-castle]})))))

(deftest haunted-castle-test
  (let [haunted-castle (assoc haunted-castle :id 0)
        gold           (assoc gold :id 1)]
    (testing "Haunted Castle"
      (is (= (-> {:current-player 0
                  :supply         [{:card gold :pile-size 30}
                                   {:split-pile [{:card haunted-castle :pile-size 1}
                                                 {:card opulent-castle :pile-size 1}]}]
                  :players        [{:coins 6
                                    :buys  1}
                                   {:hand [copper copper copper copper copper]}]}
                 (buy-card 0 :haunted-castle)
                 (choose [:copper :copper]))
             {:current-player 0
              :supply         [{:card gold :pile-size 29}
                               {:split-pile [{:card haunted-castle :pile-size 0}
                                             {:card opulent-castle :pile-size 1}]}]
              :players        [{:discard [gold haunted-castle]
                                :coins   0
                                :buys    0}
                               {:hand [copper copper copper]
                                :deck [copper copper]}]}))
      (is (= (-> {:current-player 0
                  :supply         [{:card gold :pile-size 30}
                                   {:split-pile [{:card haunted-castle :pile-size 1}
                                                 {:card opulent-castle :pile-size 1}]}]
                  :players        [{:coins 6
                                    :buys  1}
                                   {:hand [copper copper copper copper]}]}
                 (buy-card 0 :haunted-castle))
             {:current-player 0
              :supply         [{:card gold :pile-size 29}
                               {:split-pile [{:card haunted-castle :pile-size 0}
                                             {:card opulent-castle :pile-size 1}]}]
              :players        [{:discard [gold haunted-castle]
                                :coins   0
                                :buys    0}
                               {:hand [copper copper copper copper]}]}))
      (is (= (-> {:current-player 1
                  :supply         [{:card gold :pile-size 30}
                                   {:split-pile [{:card haunted-castle :pile-size 1}
                                                 {:card opulent-castle :pile-size 1}]}]
                  :players        [{}
                                   {:hand [copper copper copper copper copper]}]}
                 (gain {:player-no 0
                        :card-name :haunted-castle}))
             {:current-player 1
              :supply         [{:card gold :pile-size 30}
                               {:split-pile [{:card haunted-castle :pile-size 0}
                                             {:card opulent-castle :pile-size 1}]}]
              :players        [{:discard [haunted-castle]}
                               {:hand [copper copper copper copper copper]}]})))))

(deftest opulent-castle-test
  (let [opulent-castle (assoc opulent-castle :id 0)]
    (testing "opulent Castle"
      (is (= (-> {:players [{:hand    [opulent-castle]
                             :actions 1
                             :coins   0}]}
                 (play 0 :opulent-castle))
             {:players [{:play-area [opulent-castle]
                         :actions   0
                         :coins     0}]}))
      (is (= (-> {:players [{:hand    [opulent-castle estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :opulent-castle)
                 (choose :estate))
             {:players [{:play-area [opulent-castle]
                         :discard   [estate]
                         :actions   0
                         :coins     2}]}))
      (is (= (-> {:players [{:hand    [opulent-castle estate humble-castle]
                             :actions 1
                             :coins   0}]}
                 (play 0 :opulent-castle)
                 (choose [:estate :humble-castle]))
             {:players [{:play-area [opulent-castle]
                         :discard   [estate humble-castle]
                         :actions   0
                         :coins     4}]})))))

(deftest sprawling-castle-test
  (let [sprawling-castle (assoc sprawling-castle :id 0)
        estate           (assoc estate :id 1)
        duchy            (assoc duchy :id 2)]
    (testing "sprawling Castle"
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card duchy :pile-size 8}
                            {:split-pile [{:card sprawling-castle :pile-size 1}
                                          {:card grand-castle :pile-size 1}]}]
                  :players [{:coins 8
                             :buys  1}]}
                 (buy-card 0 :sprawling-castle)
                 (choose :duchy))
             {:supply  [{:card estate :pile-size 8}
                        {:card duchy :pile-size 7}
                        {:split-pile [{:card sprawling-castle :pile-size 0}
                                      {:card grand-castle :pile-size 1}]}]
              :players [{:discard [duchy sprawling-castle]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card duchy :pile-size 8}
                            {:split-pile [{:card sprawling-castle :pile-size 1}]}]
                  :players [{:coins 8
                             :buys  1}]}
                 (buy-card 0 :sprawling-castle)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 5}
                        {:card duchy :pile-size 8}
                        {:split-pile [{:card sprawling-castle :pile-size 0}]}]
              :players [{:discard [estate estate estate sprawling-castle]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 2}
                            {:card duchy :pile-size 8}
                            {:split-pile [{:card sprawling-castle :pile-size 1}]}]
                  :players [{:coins 8
                             :buys  1}]}
                 (buy-card 0 :sprawling-castle)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 0}
                        {:card duchy :pile-size 8}
                        {:split-pile [{:card sprawling-castle :pile-size 0}]}]
              :players [{:discard [estate estate sprawling-castle]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card duchy :pile-size 0}
                            {:split-pile [{:card sprawling-castle :pile-size 1}]}]
                  :players [{:coins 8
                             :buys  1}]}
                 (buy-card 0 :sprawling-castle)
                 (choose :duchy))
             {:supply  [{:card estate :pile-size 8}
                        {:card duchy :pile-size 0}
                        {:split-pile [{:card sprawling-castle :pile-size 0}]}]
              :players [{:discard [sprawling-castle]
                         :coins   0
                         :buys    0}]})))))

(deftest grand-castle-test
  (let [grand-castle (assoc grand-castle :id 0)]
    (testing "Grand Castle"
      (is (= (-> {:supply  [{:split-pile [{:card grand-castle :pile-size 1}
                                          {:card kings-castle :pile-size 1}]}]
                  :players [{:hand      [copper]
                             :play-area [gold gold gold]
                             :coins     9
                             :buys      1}]}
                 (buy-card 0 :grand-castle))
             {:supply  [{:split-pile [{:card grand-castle :pile-size 0}
                                      {:card kings-castle :pile-size 1}]}]
              :players [{:hand           [copper]
                         :play-area      [gold gold gold]
                         :discard        [grand-castle]
                         :revealed-cards {:hand 1}
                         :coins          0
                         :buys           0}]}))
      (is (= (-> {:supply  [{:split-pile [{:card grand-castle :pile-size 1}
                                          {:card kings-castle :pile-size 1}]}]
                  :players [{:hand  [estate]
                             :coins 9
                             :buys  1}]}
                 (buy-card 0 :grand-castle))
             {:supply  [{:split-pile [{:card grand-castle :pile-size 0}
                                      {:card kings-castle :pile-size 1}]}]
              :players [{:hand           [estate]
                         :discard        [grand-castle]
                         :revealed-cards {:hand 1}
                         :coins          0
                         :buys           0
                         :vp-tokens      1}]}))
      (is (= (-> {:supply  [{:split-pile [{:card grand-castle :pile-size 1}
                                          {:card kings-castle :pile-size 1}]}]
                  :players [{:hand  [estate estate]
                             :coins 9
                             :buys  1}]}
                 (buy-card 0 :grand-castle))
             {:supply  [{:split-pile [{:card grand-castle :pile-size 0}
                                      {:card kings-castle :pile-size 1}]}]
              :players [{:hand           [estate estate]
                         :discard        [grand-castle]
                         :revealed-cards {:hand 2}
                         :coins          0
                         :buys           0
                         :vp-tokens      2}]}))
      (is (= (-> {:supply  [{:split-pile [{:card grand-castle :pile-size 1}
                                          {:card kings-castle :pile-size 1}]}]
                  :players [{:hand      [estate estate]
                             :play-area [humble-castle small-castle opulent-castle]
                             :coins     9
                             :buys      1}]}
                 (buy-card 0 :grand-castle))
             {:supply  [{:split-pile [{:card grand-castle :pile-size 0}
                                      {:card kings-castle :pile-size 1}]}]
              :players [{:hand           [estate estate]
                         :play-area      [humble-castle small-castle opulent-castle]
                         :discard        [grand-castle]
                         :revealed-cards {:hand 2}
                         :coins          0
                         :buys           0
                         :vp-tokens      5}]})))))

(deftest castles-victory-points-test
  (testing "Castles Victory Points"
    (is (= (calc-victory-points {:deck [humble-castle]})
           1))
    (is (= (calc-victory-points {:deck [humble-castle humble-castle]})
           4))
    (is (= (calc-victory-points {:deck [crumbling-castle]})
           1))
    (is (= (calc-victory-points {:deck [humble-castle crumbling-castle]})
           3))
    (is (= (calc-victory-points {:deck [small-castle]})
           2))
    (is (= (calc-victory-points {:deck [haunted-castle]})
           2))
    (is (= (calc-victory-points {:deck [opulent-castle]})
           3))
    (is (= (calc-victory-points {:deck [sprawling-castle]})
           4))
    (is (= (calc-victory-points {:deck [grand-castle]})
           5))
    (is (= (calc-victory-points {:deck [kings-castle]})
           2))
    (is (= (calc-victory-points {:deck [humble-castle kings-castle]})
           6))
    (is (= (calc-victory-points {:deck [humble-castle crumbling-castle small-castle haunted-castle
                                        opulent-castle sprawling-castle grand-castle kings-castle]})
           (+ 8 1 2 2 3 4 5 16))))
  (testing "Castles scores"
    (is (= (calc-score {:deck [humble-castle]})
           [{:card            humble-castle
             :vp-per-card     1
             :number-of-cards 1
             :victory-points  1
             :notes           "1 Castle"}]))
    (is (= (calc-score {:deck [kings-castle]})
           [{:card            kings-castle
             :vp-per-card     2
             :number-of-cards 1
             :victory-points  2
             :notes           "1 Castle"}]))
    (is (= (calc-score {:deck [humble-castle kings-castle]})
           [{:card            humble-castle
             :vp-per-card     2
             :number-of-cards 1
             :victory-points  2
             :notes           "2 Castles"}
            {:card            kings-castle
             :vp-per-card     4
             :number-of-cards 1
             :victory-points  4
             :notes           "2 Castles"}]))))

(deftest catapult-test
  (let [catapult (assoc catapult :id 0)
        curse    (assoc curse :id 1)]
    (testing "Catapult"
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [catapult estate copper catapult silver]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :catapult)
                 (choose :estate))
             {:supply  [{:card curse :pile-size 10}]
              :players [{:hand      [copper catapult silver]
                         :play-area [catapult]
                         :actions   0
                         :coins     1}
                        {:hand [copper copper copper copper copper]}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [catapult estate copper catapult silver]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :catapult)
                 (choose :copper)
                 (choose [:copper :copper]))
             {:supply  [{:card curse :pile-size 10}]
              :players [{:hand      [estate catapult silver]
                         :play-area [catapult]
                         :actions   0
                         :coins     1}
                        {:hand    [copper copper copper]
                         :discard [copper copper]}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [catapult estate copper catapult silver]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :catapult)
                 (choose :catapult))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:hand      [estate copper silver]
                         :play-area [catapult]
                         :actions   0
                         :coins     1}
                        {:hand    [copper copper copper copper copper]
                         :discard [curse]}]
              :trash   [catapult]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [catapult estate copper catapult silver]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :catapult)
                 (choose :silver)
                 (choose [:copper :copper]))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:hand      [estate copper catapult]
                         :play-area [catapult]
                         :actions   0
                         :coins     1}
                        {:hand    [copper copper copper]
                         :discard [curse copper copper]}]
              :trash   [silver]})))))

(deftest rocks-test
  (let [rocks  (assoc rocks :id 0)
        silver (assoc silver :id 1)]
    (testing "Rocks"
      (is (= (-> {:players [{:hand  [rocks]
                             :coins 0}]}
                 (play 0 :rocks))
             {:players [{:play-area [rocks]
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:split-pile [{:card rocks :pile-size 5}]}]
                  :players [{:coins 4
                             :buys  1
                             :phase :buy}]}
                 (buy-card 0 :rocks))
             {:supply  [{:card silver :pile-size 39}
                        {:split-pile [{:card rocks :pile-size 4}]}]
              :players [{:deck    [silver]
                         :discard [rocks]
                         :coins   0
                         :buys    0
                         :phase   :buy}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [catapult rocks]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :catapult)
                 (choose :rocks))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:hand      [silver]
                         :play-area [catapult]
                         :actions   0
                         :coins     1
                         :phase     :action}]
              :trash   [rocks]})))))

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

(deftest charm-test
  (let [charm        (assoc charm :id 0)
        silver       (assoc silver :id 1)
        chariot-race (assoc chariot-race :id 2)]
    (testing "Charm"
      (is (= (-> {:players [{:hand  [charm]
                             :coins 0
                             :buys  1}]}
                 (play 0 :charm)
                 (choose :coins))
             {:players [{:play-area [charm]
                         :coins     2
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card chariot-race :pile-size 10}]
                  :players [{:hand  [charm]
                             :coins 3
                             :buys  1}]}
                 (play 0 :charm)
                 (choose :gain)
                 (buy-card 0 :silver)
                 (choose :chariot-race))
             {:supply  [{:card silver :pile-size 39}
                        {:card chariot-race :pile-size 9}]
              :players [{:play-area [charm]
                         :discard   [chariot-race silver]
                         :coins     0
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card chariot-race :pile-size 10}]
                  :players [{:hand  [charm]
                             :coins 3
                             :buys  1}]}
                 (play 0 :charm)
                 (choose :gain)
                 (buy-card 0 :silver)
                 (choose nil))
             {:supply  [{:card silver :pile-size 39}
                        {:card chariot-race :pile-size 10}]
              :players [{:play-area [charm]
                         :discard   [silver]
                         :coins     0
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand  [charm]
                             :coins 3
                             :buys  1}]}
                 (play 0 :charm)
                 (choose :gain)
                 (buy-card 0 :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [charm]
                         :discard   [silver]
                         :coins     0
                         :buys      0}]})))))

(deftest city-quarter-test
  (let [city-quarter (assoc city-quarter :id 0)]
    (testing "City Quarter"
      (is (= (-> {:players [{:hand    [city-quarter]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 (play 0 :city-quarter))
             {:players [{:play-area [city-quarter]
                         :deck      [copper copper copper]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [city-quarter copper silver estate]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 (play 0 :city-quarter))
             {:players [{:hand           [copper silver estate]
                         :play-area      [city-quarter]
                         :deck           [copper copper copper]
                         :revealed-cards {:hand 3}
                         :actions        2}]}))
      (is (= (-> {:players [{:hand    [city-quarter forum]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 (play 0 :city-quarter))
             {:players [{:hand      [forum copper]
                         :play-area [city-quarter]
                         :deck      [copper copper]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [city-quarter forum forum]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 (play 0 :city-quarter))
             {:players [{:hand      [forum forum copper copper]
                         :play-area [city-quarter]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [city-quarter patron charm crown]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 (play 0 :city-quarter))
             {:players [{:hand      [patron charm crown copper copper]
                         :play-area [city-quarter]
                         :deck      [copper]
                         :actions   2
                         :coffers   1}]})))))

(deftest crown-test
  (let [crown (assoc crown :id 0)]
    (testing "Crown"
      (is (= (-> {:players [{:hand    [crown groundskeeper]
                             :deck    [copper copper copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :crown)
                 (choose :groundskeeper))
             {:players [{:hand      [copper copper]
                         :play-area [crown groundskeeper]
                         :deck      [copper]
                         :actions   2
                         :phase     :action}]}))
      (is (= (-> {:players [{:hand    [crown copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :crown))
             {:players [{:hand      [copper]
                         :play-area [crown]
                         :actions   0
                         :phase     :action}]}))
      (is (= (-> {:players [{:hand    [crown copper]
                             :actions 1
                             :coins   0
                             :phase   :pay}]}
                 (play 0 :crown)
                 (choose :copper))
             {:players [{:play-area [crown copper]
                         :actions   1
                         :coins     2
                         :phase     :pay}]}))
      (is (= (-> {:players [{:hand  [crown spices]
                             :coins 0
                             :buys  1
                             :phase :pay}]}
                 (play 0 :crown)
                 (choose :spices))
             {:players [{:play-area [crown spices]
                         :coins     4
                         :buys      3
                         :phase     :pay}]}))
      (is (= (-> {:players [{:hand  [crown crown gold spices]
                             :coins 0
                             :buys  1
                             :phase :pay}]}
                 (play 0 :crown)
                 (choose :crown)
                 (choose :gold)
                 (choose :spices))
             {:players [{:play-area [crown crown gold spices]
                         :coins     10
                         :buys      3
                         :phase     :pay}]}))
      (let [enchantress (assoc enchantress :id 1)]
        (ut/reset-ids!)
        (is (= (-> {:players [{:hand    [crown enchantress]
                               :actions 1
                               :phase   :action}]}
                   (play 0 :crown)
                   (choose :enchantress))
               {:players [{:play-area     [crown enchantress]
                           :actions       0
                           :repeated-play [{:source 0 :target 1}]
                           :triggers      [(get-trigger enchantress)
                                           (get-trigger enchantress 2)]
                           :phase         :action}]})))
      (let [fishing-village (assoc fishing-village :id 1)]
        (ut/reset-ids!)
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [crown fishing-village]
                                :actions 0
                                :coins   0
                                :phase   :pay}]}
                   (play 0 :crown)
                   (choose :fishing-village))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area     [crown fishing-village]
                            :actions       4
                            :coins         2
                            :repeated-play [{:source 0 :target 1}]
                            :triggers      [(get-trigger fishing-village)
                                            (get-trigger fishing-village 2)]
                            :phase         :pay}]}))
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [crown fishing-village]
                                :actions 0
                                :coins   0
                                :phase   :pay}]}
                   (play 0 :crown)
                   (choose :fishing-village)
                   (end-turn 0))
               {:current-player 0
                :projects       {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players        [{:play-area [crown fishing-village]
                                  :actions   3
                                  :coins     2
                                  :buys      1
                                  :phase     :action}]})))
      (let [merchant (assoc merchant :id 1)]
        (is (= (-> {:players [{:hand    [merchant copper crown silver]
                               :actions 1
                               :coins   0
                               :phase   :action}]}
                   (play 0 :merchant)
                   (play 0 :copper)
                   (play 0 :crown)
                   (choose :silver))
               {:players [{:play-area [merchant copper crown silver]
                           :actions   1
                           :coins     6
                           :phase     :pay}]}))))))

(deftest encampment-test
  (let [encampment (assoc encampment :id 0)
        plunder    (assoc plunder :id 1)]
    (testing "Encampment"
      (ut/reset-ids!)
      (is (= (-> {:supply  [{:split-pile [{:card encampment :pile-size 4}
                                          {:card plunder :pile-size 5}]}]
                  :players [{:hand    [encampment]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 (play 0 :encampment))
             {:supply  [{:split-pile [{:card encampment :pile-size 4}
                                      {:card plunder :pile-size 5}]}]
              :players [{:hand     [copper copper]
                         :deck     [copper]
                         :actions  2
                         :triggers [(merge empires/encampment-trigger
                                           {:id        1
                                            :set-aside [encampment]})]}]}))
      (is (= (-> {:supply  [{:split-pile [{:card encampment :pile-size 4}
                                          {:card plunder :pile-size 5}]}]
                  :players [{:hand    [encampment]
                             :deck    [copper copper copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :encampment)
                 (clean-up {:player-no 0}))
             {:supply  [{:split-pile [{:card encampment :pile-size 5}
                                      {:card plunder :pile-size 5}]}]
              :players [{:hand    [copper copper copper]
                         :actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:supply  [{:split-pile [{:card encampment :pile-size 4}
                                          {:card plunder :pile-size 5}]}]
                  :players [{:hand    [encampment]
                             :deck    [gold copper copper]
                             :actions 1}]}
                 (play 0 :encampment)
                 (choose :gold))
             {:supply  [{:split-pile [{:card encampment :pile-size 4}
                                      {:card plunder :pile-size 5}]}]
              :players [{:hand      [gold copper]
                         :play-area [encampment]
                         :deck      [copper]
                         :actions   2}]}))
      (ut/reset-ids!)
      (is (= (-> {:supply  [{:split-pile [{:card encampment :pile-size 4}
                                          {:card plunder :pile-size 5}]}]
                  :players [{:hand    [encampment]
                             :deck    [gold copper copper]
                             :actions 1}]}
                 (play 0 :encampment)
                 (choose nil))
             {:supply  [{:split-pile [{:card encampment :pile-size 4}
                                      {:card plunder :pile-size 5}]}]
              :players [{:hand     [gold copper]
                         :deck     [copper]
                         :actions  2
                         :triggers [(merge empires/encampment-trigger
                                           {:id        1
                                            :set-aside [encampment]})]}]}))
      (is (thrown-with-msg? AssertionError #"Buy error"
                            (-> {:supply  [{:split-pile [{:card encampment :pile-size 0}
                                                         {:card plunder :pile-size 4}]}]
                                 :players [{:hand    [encampment]
                                            :deck    [copper copper plunder]
                                            :actions 1
                                            :coins   2
                                            :buys    1
                                            :phase   :action}]}
                                (play 0 :encampment)
                                (buy-card 0 :encampment))))
      (is (= (-> {:supply  [{:split-pile [{:card encampment :pile-size 0}
                                          {:card plunder :pile-size 4}]}]
                  :players [{:hand    [encampment]
                             :deck    [copper copper plunder]
                             :actions 1
                             :coins   5
                             :buys    1
                             :phase   :action}]}
                 (play 0 :encampment)
                 (buy-card 0 :plunder)
                 (clean-up {:player-no 0}))
             {:supply  [{:split-pile [{:card encampment :pile-size 1}
                                      {:card plunder :pile-size 3}]}]
              :players [{:hand    [plunder plunder copper copper]
                         :actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]})))))

(deftest plunder-test
  (let [plunder (assoc plunder :id 0)]
    (testing "Plunder"
      (is (= (-> {:players [{:hand  [plunder]
                             :coins 0}]}
                 (play 0 :plunder))
             {:players [{:play-area [plunder]
                         :coins     2
                         :vp-tokens 1}]})))))

(deftest enchantress-test
  (let [enchantress (assoc enchantress :id 0)]
    (testing "Enchantress"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [enchantress]
                             :actions 1}]}
                 (play 0 :enchantress))
             {:players [{:play-area [enchantress]
                         :actions   0
                         :triggers  [(get-trigger enchantress)]}]}))
      (is (= (-> {:players [{:play-area [enchantress]
                             :deck      (repeat 10 copper)
                             :phase     :buy
                             :triggers  [(get-trigger enchantress)]}]}
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      (repeat 7 copper)
                                :play-area [enchantress]
                                :deck      [copper copper copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [enchantress]
                             :actions 1}
                            {}]}
                 (play 0 :enchantress)
                 (end-turn 0))
             {:current-player 1
              :players        [{:play-area [enchantress]
                                :actions   0
                                :coins     0
                                :buys      0
                                :triggers  [(get-trigger enchantress)]}
                               {:actions  1
                                :coins    0
                                :buys     1
                                :triggers [(assoc enchantress-trigger :id 2
                                                                      :card-id 0)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [enchantress]
                                           :actions 1}
                                          {:hand [enchantress copper copper copper copper]
                                           :deck [estate estate]}]}
                 (play 0 :enchantress)
                 (end-turn 0)
                 (play 1 :enchantress))
             {:track-played-actions? true
              :current-player        1
              :players               [{:play-area [enchantress]
                                       :actions   0
                                       :coins     0
                                       :buys      0
                                       :triggers  [(get-trigger enchantress)]}
                                      {:hand           [copper copper copper copper estate]
                                       :play-area      [enchantress]
                                       :deck           [estate]
                                       :actions        1
                                       :coins          0
                                       :buys           1
                                       :actions-played [0]
                                       :triggers       [(assoc enchantress-trigger :id 2
                                                                                   :card-id 0)]}]}))
      (ut/reset-ids!)
      (let [market (assoc market :id 1)]
        (is (= (-> {:track-played-actions? true
                    :players               [{:hand    [enchantress]
                                             :actions 1}
                                            {:hand [enchantress market copper copper copper]
                                             :deck [copper estate estate]}]}
                   (play 0 :enchantress)
                   (end-turn 0)
                   (play 1 :enchantress)
                   (play 1 :market))
               {:current-player        1
                :track-played-actions? true
                :players               [{:play-area [enchantress]
                                         :actions   0
                                         :coins     0
                                         :buys      0
                                         :triggers  [(get-trigger enchantress)]}
                                        {:hand           [copper copper copper copper estate]
                                         :play-area      [enchantress market]
                                         :deck           [estate]
                                         :actions        1
                                         :coins          1
                                         :buys           2
                                         :actions-played [0 1]
                                         :triggers       [(assoc enchantress-trigger :id 2
                                                                                     :card-id 0)]}]})))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [enchantress]
                                           :actions 1
                                           :phase   :action}
                                          {:hand  [enchantress copper copper copper copper]
                                           :deck  [estate estate copper copper copper copper]
                                           :phase :out-of-turn}]}
                 (play 0 :enchantress)
                 (end-turn 0)
                 (play 1 :enchantress)
                 (end-turn 1))
             {:track-played-actions? true
              :current-player        0
              :players               [{:play-area [enchantress]
                                       :actions   1
                                       :coins     0
                                       :buys      1
                                       :phase     :action}
                                      {:hand    [estate copper copper copper copper]
                                       :discard [copper copper copper copper estate enchantress]
                                       :actions 0
                                       :coins   0
                                       :buys    0
                                       :phase   :out-of-turn}]}))
      (ut/reset-ids!)
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [enchantress enchantress]
                                           :actions 2}
                                          {:hand [enchantress copper copper copper copper]
                                           :deck [estate estate]}]}
                 (play 0 :enchantress)
                 (play 0 :enchantress)
                 (end-turn 0)
                 (play 1 :enchantress))
             {:current-player        1
              :track-played-actions? true
              :players               [{:play-area [enchantress enchantress]
                                       :actions   0
                                       :coins     0
                                       :buys      0
                                       :triggers  [(get-trigger enchantress)
                                                   (get-trigger enchantress 3)]}
                                      {:hand           [copper copper copper copper estate]
                                       :play-area      [enchantress]
                                       :deck           [estate]
                                       :actions        1
                                       :coins          0
                                       :buys           1
                                       :actions-played [0]
                                       :triggers       [(assoc enchantress-trigger :id 2
                                                                                   :card-id 0)
                                                        (assoc enchantress-trigger :id 4
                                                                                   :card-id 0)]}]}))
      (ut/reset-ids!)
      (let [outpost-1 (assoc outpost :id 1)
            outpost-2 (assoc outpost :id 2)]
        (is (= (-> {:track-played-actions? true
                    :players               [{:hand    [enchantress]
                                             :actions 1}
                                            {:hand  [outpost-1]
                                             :deck  [outpost-2 enchantress copper copper estate estate]
                                             :phase :out-of-turn}]}
                   (play 0 :enchantress)
                   (end-turn 0)
                   (play 1 :outpost)
                   (play 1 :outpost)
                   (end-turn 1)
                   (play 1 :enchantress))
               {:current-player        1
                :track-played-actions? true
                :players               [{:play-area [enchantress]
                                         :actions   0
                                         :coins     0
                                         :buys      0
                                         :triggers  [(get-trigger enchantress)]}
                                        {:hand                     [copper copper estate]
                                         :play-area                [outpost-2 enchantress]
                                         :deck                     [estate]
                                         :discard                  [outpost-1]
                                         :actions                  1
                                         :coins                    0
                                         :buys                     1
                                         :phase                    :action
                                         :previous-turn-was-yours? true
                                         :actions-played           [0]
                                         :triggers                 [(assoc enchantress-trigger :id 2
                                                                                               :card-id 0)]}]})))
      (is (= (-> {:track-played-actions? true
                  :players               [{:deck     [enchantress gold]
                                           :phase    :out-of-turn
                                           :triggers [(get-project-trigger piazza)
                                                      (assoc enchantress-trigger :id 2
                                                                                 :card-id 0)]}]}
                 (start-turn {:player-no 0}))
             {:current-player        0
              :track-played-actions? true
              :players               [{:hand           [gold]
                                       :play-area      [enchantress]
                                       :revealed-cards {:play-area 1}
                                       :actions        2
                                       :coins          0
                                       :buys           1
                                       :phase          :action
                                       :actions-played [0]
                                       :triggers       [(get-project-trigger piazza)
                                                        (assoc enchantress-trigger :id 2
                                                                                   :card-id 0)]}]}))
      (is (= (-> {:track-played-actions? true
                  :current-player        0
                  :supply                [{:card enchantress :pile-size 9}]
                  :players               [{:deck     [copper copper]
                                           :actions  0
                                           :coins    3
                                           :buys     1
                                           :triggers [(get-project-trigger innovation)
                                                      (assoc enchantress-trigger :id 2
                                                                                 :card-id 0)]}]}
                 (buy-card 0 :enchantress)
                 (choose :enchantress))
             {:track-played-actions? true
              :current-player        0
              :supply                [{:card enchantress :pile-size 8}]
              :players               [{:hand           [copper]
                                       :play-area      [enchantress]
                                       :deck           [copper]
                                       :actions        1
                                       :coins          0
                                       :buys           0
                                       :actions-played [0]
                                       :triggers       [(get-project-trigger innovation)
                                                        (assoc enchantress-trigger :id 2
                                                                                   :card-id 0)]}]}))
      (let [ghost  (assoc ghost :id 1)
            market (assoc market :id 2)]
        (ut/reset-ids! 2)
        (is (= (-> {:track-played-actions? true
                    :players               [{:play-area [ghost]
                                             :deck      [copper copper]
                                             :actions   1
                                             :phase     :out-of-turn
                                             :triggers  [(merge nocturne/ghost-trigger
                                                                {:id        1
                                                                 :card-id   1
                                                                 :name      :ghost
                                                                 :set-aside [enchantress]})
                                                         (assoc enchantress-trigger :id 2
                                                                                    :card-id 0)]}]}
                   (start-turn {:player-no 0}))
               {:track-played-actions? true
                :current-player        0
                :players               [{:hand           [copper]
                                         :play-area      [ghost enchantress]
                                         :deck           [copper]
                                         :actions        2
                                         :coins          0
                                         :buys           1
                                         :actions-played [0 0]
                                         :repeated-play  [{:source 1 :target 0}]
                                         :phase          :action
                                         :triggers       [(assoc enchantress-trigger :id 2
                                                                                     :card-id 0)
                                                          (get-trigger enchantress 3)]}]}))
        (is (= (-> {:track-played-actions? true
                    :players               [{:play-area [ghost]
                                             :deck      [copper copper]
                                             :actions   1
                                             :phase     :out-of-turn
                                             :triggers  [(merge nocturne/ghost-trigger
                                                                {:id        1
                                                                 :card-id   1
                                                                 :name      :ghost
                                                                 :set-aside [market]})
                                                         (assoc enchantress-trigger :id 2
                                                                                    :card-id 0)]}]}
                   (start-turn {:player-no 0}))
               {:current-player        0
                :track-played-actions? true
                :players               [{:hand           [copper copper]
                                         :play-area      [ghost market]
                                         :actions        3
                                         :coins          1
                                         :buys           2
                                         :actions-played [2 2]
                                         :phase          :action
                                         :triggers       [(assoc enchantress-trigger :id 2
                                                                                     :card-id 0)]}]})))
      (ut/reset-ids!)
      (is (= (-> {:track-played-actions? true
                  :current-player        0
                  :players               [{:hand     [enchantress]
                                           :deck     [copper copper]
                                           :actions  1
                                           :triggers [(get-project-trigger citadel)
                                                      (assoc enchantress-trigger :id 2
                                                                                 :card-id 0)]}]}
                 (play 0 :enchantress))
             {:track-played-actions? true
              :current-player        0
              :players               [{:hand           [copper]
                                       :play-area      [enchantress]
                                       :deck           [copper]
                                       :actions        1
                                       :actions-played [0 0]
                                       :triggers       [(get-project-trigger citadel)
                                                        (assoc enchantress-trigger :id 2
                                                                                   :card-id 0)
                                                        (get-trigger enchantress)]}]}))
      (ut/reset-ids!)
      (let [caravan-guard (assoc caravan-guard :id 1)]
        (is (= (-> {:players [{:hand    [enchantress]
                               :actions 1}
                              {:hand     [caravan-guard copper copper copper copper]
                               :deck     [estate estate]
                               :actions  0
                               :phase    :out-of-turn
                               :triggers [(assoc enchantress-trigger :id 1
                                                                     :card-id 0)]}]}
                   (play 0 :enchantress)
                   (choose :caravan-guard))
               {:players [{:play-area [enchantress]
                           :actions   0
                           :triggers  [(get-trigger enchantress)]}
                          {:hand      [copper copper copper copper estate]
                           :play-area [caravan-guard]
                           :deck      [estate]
                           :actions   1
                           :phase     :out-of-turn
                           :triggers  [(assoc enchantress-trigger :id 1
                                                                  :card-id 0)
                                       (get-trigger caravan-guard 2)
                                       (assoc enchantress-trigger :id 3
                                                                  :card-id 0)]}]}))))))

(deftest engineer-test
  (let [engineer    (assoc engineer :id 0)
        silver      (assoc silver :id 1)
        throne-room (assoc throne-room :id 2)]
    (testing "Engineer"
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card engineer :pile-size 9}]
                  :players [{:hand    [engineer]
                             :actions 1}]}
                 (play 0 :engineer)
                 (choose :silver)
                 (choose nil))
             {:supply  [{:card silver :pile-size 39}
                        {:card engineer :pile-size 9}]
              :players [{:discard   [silver]
                         :play-area [engineer]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card engineer :pile-size 9}]
                  :players [{:hand    [engineer]
                             :actions 1}]}
                 (play 0 :engineer)
                 (choose :silver)
                 (choose :engineer)
                 (choose :silver))
             {:supply  [{:card silver :pile-size 38}
                        {:card engineer :pile-size 9}]
              :players [{:discard [silver silver]
                         :actions 0}]
              :trash   [engineer]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card engineer :pile-size 9}]
                  :players [{:hand    [throne-room engineer]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :engineer)
                 (choose :silver)                           ; gain Silver
                 (choose :engineer)                         ; trash Engineer
                 (choose :silver)                           ; gain Silver
                 (choose :silver))                          ; gain Silver
             {:supply  [{:card silver :pile-size 37}
                        {:card engineer :pile-size 9}]
              :players [{:play-area [throne-room]
                         :discard   [silver silver silver]
                         :actions   0}]
              :trash   [engineer]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:supply  [{:card silver :pile-size 40}
                                           {:card engineer :pile-size 9}]
                                 :players [{:hand    [engineer copper]
                                            :actions 1}]}
                                (play 0 :engineer)
                                (choose :engineer)))))))

(deftest farmers-market-test
  (let [farmers-market (assoc farmers-market :id 0)]
    (testing "farmers-market"
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
              :players [{:play-area [farmers-market]
                         :actions   0
                         :coins     1
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9 :tokens {:victory-point {:number-of-tokens 2}}}]
              :players [{:play-area [farmers-market]
                         :actions   0
                         :coins     2
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9 :tokens {:victory-point {:number-of-tokens 3}}}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9 :tokens {:victory-point {:number-of-tokens 4}}}]
              :players [{:play-area [farmers-market]
                         :actions   0
                         :coins     4
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9 :tokens {:victory-point {:number-of-tokens 4}}}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9}]
              :players [{:actions   0
                         :coins     0
                         :buys      2
                         :vp-tokens 4}]
              :trash   [farmers-market]})))))

(deftest forum-test
  (let [forum (assoc forum :id 0)]
    (testing "Forum"
      (is (= (-> {:players [{:hand    [forum]
                             :deck    [copper silver estate estate]
                             :actions 1}]}
                 (play 0 :forum)
                 (choose [:copper :estate]))
             {:players [{:hand      [silver]
                         :play-area [forum]
                         :deck      [estate]
                         :discard   [copper estate]
                         :actions   1}]}))
      (is (= (-> {:supply  [{:card forum :pile-size 10}]
                  :players [{:coins 7
                             :buys  1}]}
                 (buy-card 0 :forum))
             {:supply  [{:card forum :pile-size 9}]
              :players [{:discard [forum]
                         :coins   2
                         :buys    1}]})))))

(deftest gladiator-test
  (let [gladiator (assoc gladiator :id 0)]
    (testing "Gladiator"
      (is (= (-> {:supply  [{:split-pile [{:card gladiator :pile-size 4}
                                          {:card fortune :pile-size 5}]}]
                  :players [{:hand    [gladiator]
                             :actions 1
                             :coins   0}
                            {:hand [estate]}]}
                 (play 0 :gladiator))
             {:supply  [{:split-pile [{:card gladiator :pile-size 3}
                                      {:card fortune :pile-size 5}]}]
              :players [{:play-area [gladiator]
                         :actions   0
                         :coins     3}
                        {:hand [estate]}]
              :trash   [gladiator]}))
      (is (= (-> {:supply  [{:split-pile [{:card gladiator :pile-size 4}
                                          {:card fortune :pile-size 5}]}]
                  :players [{:hand    [gladiator copper]
                             :actions 1
                             :coins   0}
                            {:hand [estate]}]}
                 (play 0 :gladiator)
                 (choose :copper))
             {:supply  [{:split-pile [{:card gladiator :pile-size 3}
                                      {:card fortune :pile-size 5}]}]
              :players [{:hand           [copper]
                         :play-area      [gladiator]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :coins          3}
                        {:hand [estate]}]
              :trash   [gladiator]}))
      (is (= (-> {:supply  [{:split-pile [{:card gladiator :pile-size 4}
                                          {:card fortune :pile-size 5}]}]
                  :players [{:hand    [gladiator estate]
                             :actions 1
                             :coins   0}
                            {:hand [estate]}]}
                 (play 0 :gladiator)
                 (choose :estate)
                 (choose nil))
             {:supply  [{:split-pile [{:card gladiator :pile-size 3}
                                      {:card fortune :pile-size 5}]}]
              :players [{:hand           [estate]
                         :play-area      [gladiator]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :coins          3}
                        {:hand [estate]}]
              :trash   [gladiator]}))
      (is (= (-> {:supply  [{:split-pile [{:card gladiator :pile-size 4}
                                          {:card fortune :pile-size 5}]}]
                  :players [{:hand    [gladiator estate]
                             :actions 1
                             :coins   0}
                            {:hand [estate]}]}
                 (play 0 :gladiator)
                 (choose :estate)
                 (choose :estate))
             {:supply  [{:split-pile [{:card gladiator :pile-size 4}
                                      {:card fortune :pile-size 5}]}]
              :players [{:hand           [estate]
                         :play-area      [gladiator]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :coins          2}
                        {:hand           [estate]
                         :revealed-cards {:hand 1}}]}))
      (is (= (-> {:supply  [{:split-pile [{:card gladiator :pile-size 0}
                                          {:card fortune :pile-size 5}]}]
                  :players [{:hand    [gladiator copper]
                             :actions 1
                             :coins   0}
                            {:hand [estate]}]}
                 (play 0 :gladiator)
                 (choose :copper))
             {:supply  [{:split-pile [{:card gladiator :pile-size 0}
                                      {:card fortune :pile-size 5}]}]
              :players [{:hand           [copper]
                         :play-area      [gladiator]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :coins          3}
                        {:hand [estate]}]}))
      (is (= (-> {:supply  [{:split-pile [{:card gladiator :pile-size 4}
                                          {:card fortune :pile-size 5}]}]
                  :players [{:hand    [gladiator patron]
                             :actions 1
                             :coins   0}
                            {:hand [patron]}]}
                 (play 0 :gladiator)
                 (choose :patron)
                 (choose :patron))
             {:supply  [{:split-pile [{:card gladiator :pile-size 4}
                                      {:card fortune :pile-size 5}]}]
              :players [{:hand           [patron]
                         :play-area      [gladiator]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :coins          2
                         :coffers        1}
                        {:hand           [patron]
                         :revealed-cards {:hand 1}
                         :coffers        1}]})))))

(deftest fortune-test
  (let [fortune (assoc fortune :id 0)]
    (testing "Fortune"
      (is (= (-> {:players [{:hand  [fortune]
                             :coins 0
                             :buys  1}]}
                 (play 0 :fortune))
             {:players [{:play-area        [fortune]
                         :fortune-doubled? true
                         :coins            0
                         :buys             2}]}))
      (is (= (-> {:players [{:hand  [fortune]
                             :coins 1
                             :buys  1}]}
                 (play 0 :fortune))
             {:players [{:play-area        [fortune]
                         :fortune-doubled? true
                         :coins            2
                         :buys             2}]}))
      (is (= (-> {:players [{:hand  [fortune]
                             :coins 3
                             :buys  1}]}
                 (play 0 :fortune))
             {:players [{:play-area        [fortune]
                         :fortune-doubled? true
                         :coins            6
                         :buys             2}]}))
      (is (= (-> {:players [{:hand  [fortune fortune]
                             :coins 3
                             :buys  1}]}
                 (play 0 :fortune)
                 (play 0 :fortune))
             {:players [{:play-area        [fortune fortune]
                         :fortune-doubled? true
                         :coins            6
                         :buys             3}]}))
      (is (= (-> {:players [{:hand  [fortune]
                             :coins 3
                             :buys  1}]}
                 (play 0 :fortune)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand    [fortune]
                                :actions 1
                                :coins   0
                                :buys    1}]}))
      (testing "on gain"
        (let [gold (assoc gold :id 1)]
          (is (= (-> {:supply  [{:card gold :pile-size 30}
                                {:split-pile [{:card gladiator :pile-size 0}
                                              {:card fortune :pile-size 5}]}]
                      :players [{:play-area [gold]}]}
                     (gain {:player-no 0 :card-name :fortune}))
                 {:supply  [{:card gold :pile-size 30}
                            {:split-pile [{:card gladiator :pile-size 0}
                                          {:card fortune :pile-size 4}]}]
                  :players [{:play-area [gold]
                             :discard   [fortune]}]}))
          (is (= (-> {:supply  [{:card gold :pile-size 30}
                                {:split-pile [{:card gladiator :pile-size 0}
                                              {:card fortune :pile-size 5}]}]
                      :players [{:play-area [gladiator gold]}]}
                     (gain {:player-no 0 :card-name :fortune}))
                 {:supply  [{:card gold :pile-size 29}
                            {:split-pile [{:card gladiator :pile-size 0}
                                          {:card fortune :pile-size 4}]}]
                  :players [{:play-area [gladiator gold]
                             :discard   [gold fortune]}]}))
          (is (= (-> {:supply  [{:card gold :pile-size 30}
                                {:split-pile [{:card gladiator :pile-size 0}
                                              {:card fortune :pile-size 5}]}]
                      :players [{:play-area [gladiator gladiator gold]}]}
                     (gain {:player-no 0 :card-name :fortune}))
                 {:supply  [{:card gold :pile-size 28}
                            {:split-pile [{:card gladiator :pile-size 0}
                                          {:card fortune :pile-size 4}]}]
                  :players [{:play-area [gladiator gladiator gold]
                             :discard   [gold gold fortune]}]})))))))

(deftest groundskeeper-test
  (let [groundskeeper (assoc groundskeeper :id 0)
        estate        (assoc estate :id 1)
        silver        (assoc silver :id 2)]
    (testing "Groundskeeper"
      (is (= (-> {:players [{:hand    [groundskeeper estate copper]
                             :deck    [silver estate]
                             :actions 1}]}
                 (play 0 :groundskeeper))
             {:players [{:hand      [estate copper silver]
                         :play-area [groundskeeper]
                         :deck      [estate]
                         :actions   1}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:play-area [groundskeeper]
                             :coins     3
                             :buys      1}]}
                 (buy-card 0 :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [groundskeeper]
                         :discard   [silver]
                         :coins     0
                         :buys      0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:play-area [groundskeeper]
                             :coins     2
                             :buys      1}]}
                 (buy-card 0 :estate))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:play-area [groundskeeper]
                         :discard   [estate]
                         :coins     0
                         :buys      0
                         :vp-tokens 1}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:play-area [groundskeeper groundskeeper]}]}
                 (gain {:player-no 0 :card-name :estate}))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:play-area [groundskeeper groundskeeper]
                         :discard   [estate]
                         :vp-tokens 2}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:play-area [groundskeeper]
                             :coins     4
                             :buys      2}]}
                 (buy-card 0 :estate)
                 (buy-card 0 :estate))
             {:supply  [{:card estate :pile-size 6}]
              :players [{:play-area [groundskeeper]
                         :discard   [estate estate]
                         :coins     0
                         :buys      0
                         :vp-tokens 2}]})))))

(deftest legionary-test
  (let [legionary (assoc legionary :id 0)]
    (testing "Legionary"
      (is (= (-> {:players [{:hand    [legionary]
                             :actions 1
                             :coins   0}]}
                 (play 0 :legionary))
             {:players [{:play-area [legionary]
                         :actions   0
                         :coins     3}]}))
      (is (= (-> {:players [{:hand    [legionary silver gold]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper estate estate]
                             :deck [estate copper]}]}
                 (play 0 :legionary)
                 (choose :gold)
                 (choose [:estate :estate :copper]))
             {:players [{:hand      [silver gold]
                         :play-area [legionary]
                         :actions   0
                         :coins     3}
                        {:hand    [copper copper estate]
                         :deck    [copper]
                         :discard [estate estate copper]}]}))
      (is (= (-> {:players [{:hand    [legionary silver gold]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper estate estate]
                             :deck [estate copper]}]}
                 (play 0 :legionary)
                 (choose nil))
             {:players [{:hand      [silver gold]
                         :play-area [legionary]
                         :actions   0
                         :coins     3}
                        {:hand [copper copper copper estate estate]
                         :deck [estate copper]}]})))))

(deftest patrician-test
  (let [patrician (assoc patrician :id 0)]
    (testing "Patrician"
      (is (= (-> {:players [{:hand    [patrician]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :patrician))
             {:players [{:hand           [copper]
                         :play-area      [patrician]
                         :deck           [copper]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [patrician]
                             :deck    [copper emporium]
                             :actions 1}]}
                 (play 0 :patrician))
             {:players [{:hand           [copper emporium]
                         :play-area      [patrician]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [patrician]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :patrician))
             {:players [{:hand      [copper]
                         :play-area [patrician]
                         :actions   1}]})))))

(deftest emporium-test
  (let [emporium (assoc emporium :id 0)]
    (testing "emporium"
      (is (= (-> {:players [{:hand    [emporium]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :emporium))
             {:players [{:hand      [copper]
                         :play-area [emporium]
                         :deck      [copper]
                         :actions   1
                         :coins     1}]}))
      (testing "on buy"
        (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                            {:card emporium :pile-size 5}]}]
                    :players [{:coins 5
                               :buys  1}]}
                   (buy-card 0 :emporium))
               {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                        {:card emporium :pile-size 4}]}]
                :players [{:discard [emporium]
                           :coins   0
                           :buys    0}]}))
        (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                            {:card emporium :pile-size 5}]}]
                    :players [{:play-area [patrician patrician patrician patrician gold]
                               :coins     5
                               :buys      1}]}
                   (buy-card 0 :emporium))
               {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                        {:card emporium :pile-size 4}]}]
                :players [{:play-area [patrician patrician patrician patrician gold]
                           :discard   [emporium]
                           :coins     0
                           :buys      0}]}))
        (is (= (-> {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                            {:card emporium :pile-size 5}]}]
                    :players [{:play-area [patrician patrician patrician patrician patrician]
                               :coins     5
                               :buys      1}]}
                   (buy-card 0 :emporium))
               {:supply  [{:split-pile [{:card patrician :pile-size 0}
                                        {:card emporium :pile-size 4}]}]
                :players [{:play-area [patrician patrician patrician patrician patrician]
                           :discard   [emporium]
                           :coins     0
                           :buys      0
                           :vp-tokens 2}]}))))))

(deftest royal-blacksmith-test
  (let [royal-blacksmith (assoc royal-blacksmith :id 0)]
    (testing "Royal Blacksmith"
      (is (= (-> {:players [{:hand    [royal-blacksmith]
                             :deck    (repeat 7 silver)
                             :actions 1}]}
                 (play 0 :royal-blacksmith))
             {:players [{:hand           (repeat 5 silver)
                         :play-area      [royal-blacksmith]
                         :deck           [silver silver]
                         :revealed-cards {:hand 5}
                         :actions        0}]}))
      (is (= (-> {:players [{:hand    [royal-blacksmith]
                             :deck    (repeat 7 copper)
                             :actions 1}]}
                 (play 0 :royal-blacksmith))
             {:players [{:play-area [royal-blacksmith]
                         :discard   (repeat 5 copper)
                         :deck      [copper copper]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [royal-blacksmith copper silver estate]
                             :deck    [copper silver gold estate patron duchy]
                             :actions 1}]}
                 (play 0 :royal-blacksmith))
             {:players [{:hand      [silver estate silver gold estate patron]
                         :play-area [royal-blacksmith]
                         :discard   [copper copper]
                         :deck      [duchy]
                         :actions   0
                         :coffers   1}]})))))

(deftest sacrifice-test
  (let [sacrifice (assoc sacrifice :id 0)]
    (testing "Sacrifice"
      (is (= (-> {:players [{:hand    [sacrifice copper estate chariot-race]
                             :deck    [copper silver estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :sacrifice)
                 (choose :copper))
             {:players [{:hand      [estate chariot-race]
                         :play-area [sacrifice]
                         :deck      [copper silver estate]
                         :actions   0
                         :coins     2}]
              :trash   [copper]}))
      (is (= (-> {:players [{:hand    [sacrifice copper estate chariot-race]
                             :deck    [copper silver estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :sacrifice)
                 (choose :estate))
             {:players [{:hand      [copper chariot-race]
                         :play-area [sacrifice]
                         :deck      [copper silver estate]
                         :actions   0
                         :coins     0
                         :vp-tokens 2}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand    [sacrifice copper estate chariot-race]
                             :deck    [copper silver estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :sacrifice)
                 (choose :chariot-race))
             {:players [{:hand      [copper estate copper silver]
                         :play-area [sacrifice]
                         :deck      [estate]
                         :actions   2
                         :coins     0}]
              :trash   [chariot-race]}))
      (is (= (-> {:players [{:hand    [sacrifice mill]
                             :deck    [copper silver estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :sacrifice)
                 (choose :mill))
             {:players [{:hand      [copper silver]
                         :play-area [sacrifice]
                         :deck      [estate]
                         :actions   2
                         :coins     0
                         :vp-tokens 2}]
              :trash   [mill]})))))

(deftest settlers-test
  (let [settlers (assoc settlers :id 0)]
    (testing "Settlers"
      (is (= (-> {:players [{:hand    [settlers]
                             :deck    [estate estate]
                             :actions 1}]}
                 (play 0 :settlers))
             {:players [{:hand      [estate]
                         :play-area [settlers]
                         :deck      [estate]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [settlers]
                             :deck    [estate estate]
                             :discard [copper]
                             :actions 1}]}
                 (play 0 :settlers)
                 (choose :copper))
             {:players [{:hand      [estate copper]
                         :play-area [settlers]
                         :deck      [estate]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [settlers]
                             :deck    [estate estate]
                             :discard [copper]
                             :actions 1}]}
                 (play 0 :settlers)
                 (choose nil))
             {:players [{:hand      [estate]
                         :play-area [settlers]
                         :deck      [estate]
                         :discard   [copper]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [settlers]
                             :discard [copper copper]
                             :actions 1}]}
                 (play 0 :settlers))
             {:players [{:hand      [copper]
                         :play-area [settlers]
                         :deck      [copper]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [settlers]
                             :deck    [estate]
                             :discard [silver copper gold]
                             :actions 1}]}
                 (play 0 :settlers)
                 (choose :copper))
             {:players [{:hand      [estate copper]
                         :play-area [settlers]
                         :discard   [silver gold]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [settlers]
                             :deck    [estate]
                             :discard [silver gold]
                             :actions 1}]}
                 (play 0 :settlers))
             {:players [{:hand           [estate]
                         :play-area      [settlers]
                         :discard        [silver gold]
                         :actions        1
                         :revealed-cards {:discard 2}}]})))))

(deftest bustling-village-test
  (let [bustling-village (assoc bustling-village :id 0)]
    (testing "Bustling Village"
      (is (= (-> {:players [{:hand    [bustling-village]
                             :deck    [estate estate]
                             :actions 1}]}
                 (play 0 :bustling-village))
             {:players [{:hand      [estate]
                         :play-area [bustling-village]
                         :deck      [estate]
                         :actions   3}]}))
      (is (= (-> {:players [{:hand    [bustling-village]
                             :deck    [estate estate]
                             :discard [settlers]
                             :actions 1}]}
                 (play 0 :bustling-village)
                 (choose :settlers))
             {:players [{:hand      [estate settlers]
                         :play-area [bustling-village]
                         :deck      [estate]
                         :actions   3}]}))
      (is (= (-> {:players [{:hand    [bustling-village]
                             :deck    [estate estate]
                             :discard [settlers]
                             :actions 1}]}
                 (play 0 :bustling-village)
                 (choose nil))
             {:players [{:hand      [estate]
                         :play-area [bustling-village]
                         :deck      [estate]
                         :discard   [settlers]
                         :actions   3}]}))
      (is (= (-> {:players [{:hand    [bustling-village]
                             :discard [settlers settlers]
                             :actions 1}]}
                 (play 0 :bustling-village))
             {:players [{:hand      [settlers]
                         :play-area [bustling-village]
                         :deck      [settlers]
                         :actions   3}]}))
      (is (= (-> {:players [{:hand    [bustling-village]
                             :deck    [estate]
                             :discard [silver settlers gold]
                             :actions 1}]}
                 (play 0 :bustling-village)
                 (choose :settlers))
             {:players [{:hand      [estate settlers]
                         :play-area [bustling-village]
                         :discard   [silver gold]
                         :actions   3}]}))
      (is (= (-> {:players [{:hand    [bustling-village]
                             :deck    [estate]
                             :discard [silver gold]
                             :actions 1}]}
                 (play 0 :bustling-village))
             {:players [{:hand           [estate]
                         :play-area      [bustling-village]
                         :discard        [silver gold]
                         :actions        3
                         :revealed-cards {:discard 2}}]})))))

(deftest temple-test
  (let [temple (assoc temple :id 0)]
    (testing "Temple"
      (is (= (-> {:supply  [{:card temple :pile-size 9}]
                  :players [{:hand    [temple copper copper estate]
                             :actions 1}]}
                 (play 0 :temple)
                 (choose [:copper :estate]))
             {:supply  [{:card temple :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
              :players [{:hand      [copper]
                         :play-area [temple]
                         :actions   0
                         :vp-tokens 1}]
              :trash   [copper estate]}))
      (is (= (-> {:supply  [{:card temple :pile-size 9}]
                  :players [{:hand    [temple copper copper estate]
                             :actions 1}]}
                 (play 0 :temple)
                 (choose [:estate]))
             {:supply  [{:card temple :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
              :players [{:hand      [copper copper]
                         :play-area [temple]
                         :actions   0
                         :vp-tokens 1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card temple :pile-size 9}]
                  :players [{:hand    [temple copper]
                             :actions 1}]}
                 (play 0 :temple)
                 (choose :copper))
             {:supply  [{:card temple :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
              :players [{:play-area [temple]
                         :actions   0
                         :vp-tokens 1}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card temple :pile-size 9}]
                  :players [{:hand    [temple]
                             :actions 1}]}
                 (play 0 :temple))
             {:supply  [{:card temple :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
              :players [{:play-area [temple]
                         :actions   0
                         :vp-tokens 1}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:supply  [{:card temple :pile-size 9}]
                                 :players [{:hand    [temple copper copper estate]
                                            :actions 1}]}
                                (play 0 :temple)
                                (choose [:copper :copper :estate]))))
      (is (= (-> {:supply  [{:card temple :pile-size 9 :tokens {:victory-point {:number-of-tokens 3}}}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-card 0 :temple))
             {:supply  [{:card temple :pile-size 8}]
              :players [{:discard   [temple]
                         :coins     0
                         :buys      0
                         :vp-tokens 3}]})))))

(deftest villa-test
  (let [villa (assoc villa :id 0)]
    (testing "Villa"
      (is (= (-> {:players [{:hand    [villa]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :villa))
             {:players [{:play-area [villa]
                         :actions   2
                         :coins     1
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card villa :pile-size 10}]
                  :players [{:actions 0
                             :coins   4
                             :buys    1
                             :phase   :buy}]}
                 (buy-card 0 :villa))
             {:supply  [{:card villa :pile-size 9}]
              :players [{:hand    [villa]
                         :actions 1
                         :coins   0
                         :buys    0
                         :phase   :action}]}))
      (is (= (-> {:supply  [{:card villa :pile-size 10}]
                  :players [{:actions 0
                             :coins   0
                             :buys    0
                             :phase   :out-of-turn}]}
                 (gain {:player-no 0
                        :card-name :villa}))
             {:supply  [{:card villa :pile-size 9}]
              :players [{:hand    [villa]
                         :actions 1
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:current-player 1
                  :supply         [{:card villa :pile-size 10}]
                  :players        [{:actions 0
                                    :coins   0
                                    :buys    0
                                    :phase   :out-of-turn}
                                   {}]}
                 (gain {:player-no 0
                        :card-name :villa})
                 (end-turn 1))
             {:current-player 0
              :supply         [{:card villa :pile-size 9}]
              :players        [{:hand    [villa]
                                :actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}
                               {:actions 0
                                :coins   0
                                :buys    0}]})))))

(deftest wild-hunt-test
  (let [wild-hunt (assoc wild-hunt :id 0)
        estate    (assoc estate :id 1)]
    (testing "Wild Hunt"
      (is (= (-> {:supply  [{:card wild-hunt :pile-size 9}]
                  :players [{:hand    [wild-hunt]
                             :deck    [copper copper copper copper]
                             :actions 1}]}
                 (play 0 :wild-hunt)
                 (choose :cards))
             {:supply  [{:card wild-hunt :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
              :players [{:hand      [copper copper copper]
                         :play-area [wild-hunt]
                         :deck      [copper]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card wild-hunt :pile-size 9 :tokens {:victory-point {:number-of-tokens 2}}}]
                  :players [{:hand    [wild-hunt]
                             :actions 1}]}
                 (play 0 :wild-hunt)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 7}
                        {:card wild-hunt :pile-size 9}]
              :players [{:play-area [wild-hunt]
                         :discard   [estate]
                         :actions   0
                         :vp-tokens 2}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 0}
                            {:card wild-hunt :pile-size 9 :tokens {:victory-point {:number-of-tokens 2}}}]
                  :players [{:hand    [wild-hunt]
                             :actions 1}]}
                 (play 0 :wild-hunt)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 0}
                        {:card wild-hunt :pile-size 9 :tokens {:victory-point {:number-of-tokens 2}}}]
              :players [{:play-area [wild-hunt]
                         :actions   0}]})))))

;; EVENTS

(deftest advance-test
  (let [legionary (assoc legionary :id 0)]
    (testing "Advance"
      (is (= (-> {:events  {:advance advance}
                  :supply  [{:card legionary :pile-size 10}]
                  :players [{:hand  [chariot-race copper estate]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :advance))
             {:events       {:advance advance}
              :supply       [{:card legionary :pile-size 10}]
              :players      [{:hand  [chariot-race copper estate]
                              :coins 0
                              :buys  0}]
              :effect-stack [{:text      "You may trash an Action card from your hand."
                              :player-no 0
                              :choice    ::empires/advance-trash
                              :source    :hand
                              :options   [:chariot-race]
                              :max       1}]}))
      (is (= (-> {:events  {:advance advance}
                  :supply  [{:card legionary :pile-size 10}]
                  :players [{:hand  [chariot-race copper estate]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :advance)
                 (choose :chariot-race)
                 (choose :legionary))
             {:events  {:advance advance}
              :supply  [{:card legionary :pile-size 9}]
              :players [{:hand    [copper estate]
                         :discard [legionary]
                         :coins   0
                         :buys    0}]
              :trash   [chariot-race]}))
      (is (= (-> {:events  {:advance advance}
                  :supply  [{:card legionary :pile-size 10}]
                  :players [{:hand  [chariot-race copper estate]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :advance)
                 (choose nil))
             {:events  {:advance advance}
              :supply  [{:card legionary :pile-size 10}]
              :players [{:hand  [chariot-race copper estate]
                         :coins 0
                         :buys  0}]})))))

(deftest annex-test
  (let [duchy (assoc duchy :id 0)]
    (testing "Annex"
      (is (= (-> {:events  {:annex annex}
                  :supply  [{:card duchy :pile-size 8}]
                  :players [{:deck  [copper silver gold]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :annex))
             {:events  {:annex annex}
              :supply  [{:card duchy :pile-size 7}]
              :players [{:deck    [silver copper gold]
                         :discard [duchy]
                         :coins   0
                         :debt    8
                         :buys    0}]}))
      (is (= (-> {:events  {:annex annex}
                  :supply  [{:card duchy :pile-size 7}]
                  :players [{:discard [duchy]
                             :coins   0
                             :buys    1}]}
                 (buy-event 0 :annex)
                 (choose :duchy))
             {:events  {:annex annex}
              :supply  [{:card duchy :pile-size 6}]
              :players [{:discard [duchy duchy]
                         :coins   0
                         :debt    8
                         :buys    0}]}))
      (is (= (-> {:events  {:annex annex}
                  :supply  [{:card duchy :pile-size 7}]
                  :players [{:discard [gold]
                             :coins   0
                             :buys    1}]}
                 (buy-event 0 :annex)
                 (choose nil))
             {:events  {:annex annex}
              :supply  [{:card duchy :pile-size 6}]
              :players [{:deck    [gold]
                         :discard [duchy]
                         :coins   0
                         :debt    8
                         :buys    0}]}))
      (is (= (-> {:events  {:annex annex}
                  :supply  [{:card duchy :pile-size 7}]
                  :players [{:discard [estate estate]
                             :coins   0
                             :buys    1}]}
                 (buy-event 0 :annex)
                 (choose [:estate :estate]))
             {:events  {:annex annex}
              :supply  [{:card duchy :pile-size 6}]
              :players [{:discard [estate estate duchy]
                         :coins   0
                         :debt    8
                         :buys    0}]}))
      (is (= (-> {:events  {:annex annex}
                  :supply  [{:card duchy :pile-size 7}]
                  :players [{:discard [silver silver]
                             :coins   0
                             :buys    1}]}
                 (buy-event 0 :annex)
                 (choose nil))
             {:events  {:annex annex}
              :supply  [{:card duchy :pile-size 6}]
              :players [{:deck    [silver silver]
                         :discard [duchy]
                         :coins   0
                         :debt    8
                         :buys    0}]}))
      (is (= (-> {:events  {:annex annex}
                  :supply  [{:card duchy :pile-size 8}]
                  :players [{:discard [silver silver estate duchy province copper silver]
                             :coins   0
                             :buys    1}]}
                 (buy-event 0 :annex)
                 (choose [:estate :duchy :province :copper]))
             {:events  {:annex annex}
              :supply  [{:card duchy :pile-size 7}]
              :players [{:deck    [silver silver silver]
                         :discard [estate duchy province copper duchy]
                         :coins   0
                         :debt    8
                         :buys    0}]}))
      (is (= (-> {:events  {:annex annex}
                  :supply  [{:card duchy :pile-size 8}]
                  :players [{:deck    (repeat 2 copper)
                             :discard (concat (repeat 2 silver)
                                              (repeat 2 gold)
                                              (repeat 6 estate))
                             :coins   0
                             :buys    1}]}
                 (buy-event 0 :annex)
                 (choose (repeat 5 :estate)))
             {:events  {:annex annex}
              :supply  [{:card duchy :pile-size 7}]
              :players [{:deck    [estate silver copper copper silver gold gold]
                         :discard (concat (repeat 5 estate) [duchy])
                         :coins   0
                         :debt    8
                         :buys    0}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:events  {:annex annex}
                                 :supply  [{:card duchy :pile-size 8}]
                                 :players [{:deck    (repeat 2 copper)
                                            :discard (concat (repeat 2 silver)
                                                             (repeat 2 gold)
                                                             (repeat 6 estate))
                                            :coins   0
                                            :buys    1}]}
                                (buy-event 0 :annex)
                                (choose (repeat 6 :estate)))))
      (is (= (-> {:events  {:annex annex}
                  :supply  [{:card duchy :pile-size 0}]
                  :players [{:discard [gold gold gold duchy duchy duchy]
                             :coins   0
                             :buys    1}]}
                 (buy-event 0 :annex)
                 (choose [:duchy :duchy :duchy]))
             {:events  {:annex annex}
              :supply  [{:card duchy :pile-size 0}]
              :players [{:deck    [gold gold gold]
                         :discard [duchy duchy duchy]
                         :coins   0
                         :debt    8
                         :buys    0}]})))))

(deftest banquet-test
  (let [copper    (assoc copper :id 0)
        legionary (assoc legionary :id 1)]
    (testing "Banquet"
      (is (= (-> {:events  {:banquet banquet}
                  :supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}
                            {:card gold :pile-size 30}
                            {:card duchy :pile-size 8}
                            {:card legionary :pile-size 10}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-event 0 :banquet))
             {:events       {:banquet banquet}
              :supply       [{:card copper :pile-size 44}
                             {:card silver :pile-size 40}
                             {:card gold :pile-size 30}
                             {:card duchy :pile-size 8}
                             {:card legionary :pile-size 10}]
              :players      [{:discard [copper copper]
                              :coins   0
                              :buys    0}]
              :effect-stack [{:text      "Gain a non-Victory card costing up to $5."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:copper :silver :legionary]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:events  {:banquet banquet}
                  :supply  [{:card copper :pile-size 46}
                            {:card legionary :pile-size 10}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-event 0 :banquet)
                 (choose :legionary))
             {:events  {:banquet banquet}
              :supply  [{:card copper :pile-size 44}
                        {:card legionary :pile-size 9}]
              :players [{:discard [copper copper legionary]
                         :coins   0
                         :buys    0}]})))))

(deftest conquest-test
  (let [silver (assoc silver :id 0)]
    (testing "Conquest"
      (is (= (-> {:track-gained-cards? true
                  :events              {:conquest conquest}
                  :supply              [{:card silver :pile-size 40}]
                  :players             [{:coins 6
                                         :buys  1}]}
                 (buy-event 0 :conquest))
             {:track-gained-cards? true
              :events              {:conquest conquest}
              :supply              [{:card silver :pile-size 38}]
              :players             [{:discard      [silver silver]
                                     :coins        0
                                     :buys         0
                                     :vp-tokens    2
                                     :gained-cards [{:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}
                                                    {:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}]}]}))
      (is (= (-> {:track-gained-cards? true
                  :events              {:conquest conquest}
                  :supply              [{:card silver :pile-size 39}]
                  :players             [{:coins        6
                                         :buys         1
                                         :gained-cards [{:cost  3
                                                         :name  :silver
                                                         :types #{:treasure}}
                                                        {:cost  6
                                                         :name  :gold
                                                         :types #{:treasure}}]}]}
                 (buy-event 0 :conquest))
             {:track-gained-cards? true
              :events              {:conquest conquest}
              :supply              [{:card silver :pile-size 37}]
              :players             [{:discard      [silver silver]
                                     :coins        0
                                     :buys         0
                                     :vp-tokens    3
                                     :gained-cards [{:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}
                                                    {:cost  6
                                                     :name  :gold
                                                     :types #{:treasure}}
                                                    {:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}
                                                    {:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}]}]})))))

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

(deftest dominate-test
  (let [province (assoc province :id 0)]
    (testing "Dominate"
      (is (= (-> {:events  {:dominate dominate}
                  :supply  [{:card province :pile-size 8}]
                  :players [{:coins 14
                             :buys  1}]}
                 (buy-event 0 :dominate))
             {:events  {:dominate dominate}
              :supply  [{:card province :pile-size 7}]
              :players [{:discard   [province]
                         :coins     0
                         :buys      0
                         :vp-tokens 9}]}))
      (is (= (-> {:events  {:dominate dominate}
                  :supply  [{:card province :pile-size 0}]
                  :players [{:coins 14
                             :buys  1}]}
                 (buy-event 0 :dominate))
             {:events  {:dominate dominate}
              :supply  [{:card province :pile-size 0}]
              :players [{:coins 0
                         :buys  0}]})))))

(deftest ritual-test
  (let [curse (assoc curse :id 0)]
    (testing "Ritual"
      (is (= (-> {:events  {:ritual ritual}
                  :supply  [{:card curse :pile-size 10}]
                  :players [{:hand  [gold]
                             :coins 4
                             :buys  1}]}
                 (buy-event 0 :ritual)
                 (choose :gold))
             {:events  {:ritual ritual}
              :supply  [{:card curse :pile-size 9}]
              :players [{:discard   [curse]
                         :coins     0
                         :buys      0
                         :vp-tokens 6}]
              :trash   [gold]}))
      (is (= (-> {:events  {:ritual ritual}
                  :supply  [{:card curse :pile-size 10}]
                  :players [{:hand  [copper]
                             :coins 4
                             :buys  1}]}
                 (buy-event 0 :ritual)
                 (choose :copper))
             {:events  {:ritual ritual}
              :supply  [{:card curse :pile-size 9}]
              :players [{:discard [curse]
                         :coins   0
                         :buys    0}]
              :trash   [copper]}))
      (is (= (-> {:events  {:ritual ritual}
                  :supply  [{:card curse :pile-size 10}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :ritual))
             {:events  {:ritual ritual}
              :supply  [{:card curse :pile-size 9}]
              :players [{:discard [curse]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:events  {:ritual ritual}
                  :supply  [{:card curse :pile-size 0}]
                  :players [{:hand  [gold]
                             :coins 4
                             :buys  1}]}
                 (buy-event 0 :ritual))
             {:events  {:ritual ritual}
              :supply  [{:card curse :pile-size 0}]
              :players [{:hand  [gold]
                         :coins 0
                         :buys  0}]})))))

(deftest salt-the-earth-test
  (testing "Salt the Earth"
    (is (= (-> {:events  {:salt-the-earth salt-the-earth}
                :supply  (base/supply 2 8)
                :players [{:coins 4
                           :buys  1}]}
               (buy-event 0 :salt-the-earth))
           {:events       {:salt-the-earth salt-the-earth}
            :supply       (base/supply 2 8)
            :players      [{:coins     0
                            :buys      0
                            :vp-tokens 1}]
            :effect-stack [{:text      "Trash a Victory card from the Supply."
                            :player-no 0
                            :choice    :trash-from-supply
                            :source    :supply
                            :options   [:estate :duchy :province]
                            :min       1
                            :max       1}]}))
    (let [province (assoc province :id 0)]
      (is (= (-> {:events  {:salt-the-earth salt-the-earth}
                  :supply  [{:card province :pile-size 8}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :salt-the-earth)
                 (choose :province))
             {:events  {:salt-the-earth salt-the-earth}
              :supply  [{:card province :pile-size 7}]
              :players [{:coins     0
                         :buys      0
                         :vp-tokens 1}]
              :trash   [province]})))))

(deftest tax-test
  (testing "Tax"
    (testing "setup"
      (is (= (-> {:events  {:tax tax}
                  :supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}
                            {:card gold :pile-size 30}
                            {:card engineer :pile-size 10}]
                  :players [{}]}
                 setup-game)
             {:events  {:tax tax}
              :supply  [{:card copper :pile-size 46 :tokens {:debt {:number-of-tokens 1
                                                                    :on-buy           [[::empires/take-debt]]}}}
                        {:card silver :pile-size 40 :tokens {:debt {:number-of-tokens 1
                                                                    :on-buy           [[::empires/take-debt]]}}}
                        {:card gold :pile-size 30 :tokens {:debt {:number-of-tokens 1
                                                                  :on-buy           [[::empires/take-debt]]}}}
                        {:card engineer :pile-size 10 :tokens {:debt {:number-of-tokens 1
                                                                      :on-buy           [[::empires/take-debt]]}}}]
              :players [{}]}))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:events  {:tax tax}
                    :supply  [{:card silver :pile-size 40 :tokens {:debt {:number-of-tokens 1
                                                                          :on-buy           [[::empires/take-debt]]}}}]
                    :players [{:coins 3
                               :buys  1}]}
                   (buy-card 0 :silver))
               {:events  {:tax tax}
                :supply  [{:card silver :pile-size 39}]
                :players [{:discard [silver]
                           :coins   0
                           :debt    1
                           :buys    0}]}))
        (is (= (-> {:events  {:tax tax}
                    :supply  [{:card silver :pile-size 40 :tokens {:debt {:number-of-tokens 2
                                                                          :on-buy           [[::empires/take-debt]]}}}]
                    :players [{:coins 3
                               :buys  1}]}
                   (buy-card 0 :silver))
               {:events  {:tax tax}
                :supply  [{:card silver :pile-size 39}]
                :players [{:discard [silver]
                           :coins   0
                           :debt    2
                           :buys    0}]}))
        (is (= (-> {:events  {:tax tax}
                    :supply  [{:card silver :pile-size 40}]
                    :players [{:coins 2
                               :buys  1}]}
                   (buy-event 0 :tax)
                   (choose :silver))
               {:events  {:tax tax}
                :supply  [{:card silver :pile-size 40 :tokens {:debt {:number-of-tokens 2
                                                                      :on-buy           [[::empires/take-debt]]}}}]
                :players [{:coins 0
                           :buys  0}]}))
        (is (= (-> {:events  {:tax tax}
                    :supply  [{:card silver :pile-size 40 :tokens {:debt {:number-of-tokens 1
                                                                          :on-buy           [[::empires/take-debt]]}}}]
                    :players [{:coins 2
                               :buys  1}]}
                   (buy-event 0 :tax)
                   (choose :silver))
               {:events  {:tax tax}
                :supply  [{:card silver :pile-size 40 :tokens {:debt {:number-of-tokens 3
                                                                      :on-buy           [[::empires/take-debt]]}}}]
                :players [{:coins 0
                           :buys  0}]}))))))

(deftest triumph-test
  (let [estate (assoc estate :id 0)]
    (testing "Triumph"
      (is (= (-> {:track-gained-cards? true
                  :events              {:triumph triumph}
                  :supply              [{:card estate :pile-size 8}]
                  :players             [{:coins 0
                                         :buys  1}]}
                 (buy-event 0 :triumph))
             {:track-gained-cards? true
              :events              {:triumph triumph}
              :supply              [{:card estate :pile-size 7}]
              :players             [{:discard      [estate]
                                     :coins        0
                                     :debt         5
                                     :buys         0
                                     :vp-tokens    1
                                     :gained-cards [{:cost  2
                                                     :name  :estate
                                                     :types #{:victory}}]}]}))
      (is (= (-> {:track-gained-cards? true
                  :events              {:triumph triumph}
                  :supply              [{:card estate :pile-size 8}]
                  :players             [{:coins        0
                                         :buys         1
                                         :gained-cards [{:cost  3
                                                         :name  :silver
                                                         :types #{:treasure}}]}]}
                 (buy-event 0 :triumph))
             {:track-gained-cards? true
              :events              {:triumph triumph}
              :supply              [{:card estate :pile-size 7}]
              :players             [{:discard      [estate]
                                     :coins        0
                                     :debt         5
                                     :buys         0
                                     :vp-tokens    2
                                     :gained-cards [{:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}
                                                    {:cost  2
                                                     :name  :estate
                                                     :types #{:victory}}]}]}))
      (is (= (-> {:track-gained-cards? true
                  :events              {:triumph triumph}
                  :supply              [{:card estate :pile-size 8}]
                  :players             [{:coins        0
                                         :buys         1
                                         :gained-cards [{:cost  3
                                                         :name  :silver
                                                         :types #{:treasure}}
                                                        {:cost  5
                                                         :name  :duchy
                                                         :types #{:victory}}]}]}
                 (buy-event 0 :triumph))
             {:track-gained-cards? true
              :events              {:triumph triumph}
              :supply              [{:card estate :pile-size 7}]
              :players             [{:discard      [estate]
                                     :coins        0
                                     :debt         5
                                     :buys         0
                                     :vp-tokens    3
                                     :gained-cards [{:cost  3
                                                     :name  :silver
                                                     :types #{:treasure}}
                                                    {:cost  5
                                                     :name  :duchy
                                                     :types #{:victory}}
                                                    {:cost  2
                                                     :name  :estate
                                                     :types #{:victory}}]}]}))
      (is (= (-> {:events  {:triumph triumph}
                  :supply  [{:card estate :pile-size 0}]
                  :players [{:coins        4
                             :buys         1
                             :gained-cards [{:cost  3
                                             :name  :silver
                                             :types #{:treasure}}]}]}
                 (buy-event 0 :triumph))
             {:events  {:triumph triumph}
              :supply  [{:card estate :pile-size 0}]
              :players [{:coins        4
                         :debt         5
                         :buys         0
                         :gained-cards [{:cost  3
                                         :name  :silver
                                         :types #{:treasure}}]}]})))))

(deftest wedding-test
  (let [gold (assoc gold :id 0)]
    (testing "Wedding"
      (is (= (-> {:events  {:wedding wedding}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :wedding))
             {:events  {:wedding wedding}
              :supply  [{:card gold :pile-size 29}]
              :players [{:discard   [gold]
                         :coins     0
                         :debt      3
                         :buys      0
                         :vp-tokens 1}]}))
      (is (= (-> {:events  {:wedding wedding}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:coins 7
                             :buys  1}]}
                 (buy-event 0 :wedding))
             {:events  {:wedding wedding}
              :supply  [{:card gold :pile-size 29}]
              :players [{:discard   [gold]
                         :coins     3
                         :debt      3
                         :buys      0
                         :vp-tokens 1}]}))
      (is (thrown-with-msg? AssertionError #"Buy error:"
                            (-> {:events  {:wedding wedding}
                                 :supply  [{:card gold :pile-size 30}]
                                 :players [{:coins 4
                                            :debt  1
                                            :buys  1}]}
                                (buy-event 0 :wedding))))
      (is (= (-> {:events  {:wedding wedding}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:coins 5
                             :debt  1
                             :buys  1}]}
                 (buy-event 0 :wedding))
             {:events  {:wedding wedding}
              :supply  [{:card gold :pile-size 29}]
              :players [{:discard   [gold]
                         :coins     0
                         :debt      3
                         :buys      0
                         :vp-tokens 1}]})))))

(deftest windfall-test
  (let [gold (assoc gold :id 0)]
    (testing "windfall"
      (is (= (-> {:events  {:windfall windfall}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:hand      [estate]
                             :play-area [silver silver copper]
                             :coins     5
                             :buys      1}]}
                 (buy-event 0 :windfall))
             {:events  {:windfall windfall}
              :supply  [{:card gold :pile-size 27}]
              :players [{:hand      [estate]
                         :play-area [silver silver copper]
                         :discard   [gold gold gold]
                         :coins     0
                         :buys      0}]}))
      (is (= (-> {:events  {:windfall windfall}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:deck  [estate]
                             :coins 5
                             :buys  1}]}
                 (buy-event 0 :windfall))
             {:events  {:windfall windfall}
              :supply  [{:card gold :pile-size 30}]
              :players [{:deck  [estate]
                         :coins 0
                         :buys  0}]}))
      (is (= (-> {:events  {:windfall windfall}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:discard [estate]
                             :coins   5
                             :buys    1}]}
                 (buy-event 0 :windfall))
             {:events  {:windfall windfall}
              :supply  [{:card gold :pile-size 30}]
              :players [{:discard [estate]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:events  {:windfall windfall}
                  :supply  [{:card gold :pile-size 2}]
                  :players [{:coins 5
                             :buys  1}]}
                 (buy-event 0 :windfall))
             {:events  {:windfall windfall}
              :supply  [{:card gold :pile-size 0}]
              :players [{:discard [gold gold]
                         :coins   0
                         :buys    0}]})))))

(deftest aqueduct-test
  (let [silver (assoc silver :id 1)
        estate (assoc estate :id 2)]
    (testing "Aqueduct"
      (is (= (-> {:landmarks {:aqueduct aqueduct}
                  :supply    [{:card silver :pile-size 40 :tokens {:victory-point {:number-of-tokens 8}}}]
                  :players   [{:triggers [(assoc aqueduct-treasure-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :silver}))
             {:landmarks {:aqueduct (assoc aqueduct :vp-tokens 1)}
              :supply    [{:card silver :pile-size 39 :tokens {:victory-point {:number-of-tokens 7}}}]
              :players   [{:discard  [silver]
                           :triggers [(assoc aqueduct-treasure-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:aqueduct (assoc aqueduct :vp-tokens 3)}
                  :supply    [{:card silver :pile-size 33 :tokens {:victory-point {:number-of-tokens 1}}}]
                  :players   [{:triggers [(assoc aqueduct-treasure-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :silver}))
             {:landmarks {:aqueduct (assoc aqueduct :vp-tokens 4)}
              :supply    [{:card silver :pile-size 32}]
              :players   [{:discard  [silver]
                           :triggers [(assoc aqueduct-treasure-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:aqueduct aqueduct}
                  :supply    [{:card silver :pile-size 32}]
                  :players   [{:triggers [(assoc aqueduct-treasure-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :silver}))
             {:landmarks {:aqueduct aqueduct}
              :supply    [{:card silver :pile-size 31}]
              :players   [{:discard  [silver]
                           :triggers [(assoc aqueduct-treasure-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:aqueduct (assoc aqueduct :vp-tokens 1)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:triggers [(assoc aqueduct-victory-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :estate}))
             {:landmarks {:aqueduct aqueduct}
              :supply    [{:card estate :pile-size 7}]
              :players   [{:discard   [estate]
                           :vp-tokens 1
                           :triggers  [(assoc aqueduct-victory-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:aqueduct (assoc aqueduct :vp-tokens 4)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:triggers [(assoc aqueduct-victory-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :estate}))
             {:landmarks {:aqueduct aqueduct}
              :supply    [{:card estate :pile-size 7}]
              :players   [{:discard   [estate]
                           :vp-tokens 4
                           :triggers  [(assoc aqueduct-victory-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:aqueduct aqueduct}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:triggers [(assoc aqueduct-victory-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :estate}))
             {:landmarks {:aqueduct aqueduct}
              :supply    [{:card estate :pile-size 7}]
              :players   [{:discard  [estate]
                           :triggers [(assoc aqueduct-victory-trigger :id 1)]}]}))
      (testing "Setup"
        (ut/reset-ids!)
        (is (= (-> {:landmarks {:aqueduct aqueduct}
                    :supply    [{:card copper :pile-size 46}
                                {:card silver :pile-size 40}
                                {:card gold :pile-size 30}
                                {:card charm :pile-size 10}]
                    :players   [{}]}
                   setup-game)
               {:landmarks {:aqueduct aqueduct}
                :supply    [{:card copper :pile-size 46}
                            {:card silver :pile-size 40 :tokens {:victory-point {:number-of-tokens 8}}}
                            {:card gold :pile-size 30 :tokens {:victory-point {:number-of-tokens 8}}}
                            {:card charm :pile-size 10}]
                :players   [{:triggers [(assoc aqueduct-treasure-trigger :id 1)
                                        (assoc aqueduct-victory-trigger :id 2)]}]}))))))

(deftest arena-test
  (testing "Arena"
    (is (= (-> {:landmarks {:arena (assoc arena :vp-tokens 12)}
                :players   [{:hand     [copper copper]
                             :coins    0
                             :phase    :action
                             :triggers [(assoc arena-trigger :id 1)]}]}
               (play 0 :copper))
           {:landmarks {:arena (assoc arena :vp-tokens 12)}
            :players   [{:hand      [copper]
                         :play-area [copper]
                         :coins     1
                         :phase     :pay
                         :triggers  [(assoc arena-trigger :id 1)]}]}))
    (is (= (-> {:landmarks {:arena (assoc arena :vp-tokens 12)}
                :players   [{:hand     [copper catapult]
                             :coins    0
                             :phase    :action
                             :triggers [(assoc arena-trigger :id 1)]}]}
               (play 0 :copper)
               (choose nil))
           {:landmarks {:arena (assoc arena :vp-tokens 12)}
            :players   [{:hand      [catapult]
                         :play-area [copper]
                         :coins     1
                         :phase     :pay
                         :triggers  [(assoc arena-trigger :id 1)]}]}))
    (is (= (-> {:landmarks {:arena (assoc arena :vp-tokens 12)}
                :players   [{:hand     [copper catapult]
                             :coins    0
                             :phase    :action
                             :triggers [(assoc arena-trigger :id 1)]}]}
               (play 0 :copper)
               (choose :catapult))
           {:landmarks {:arena (assoc arena :vp-tokens 10)}
            :players   [{:play-area [copper]
                         :discard   [catapult]
                         :coins     1
                         :vp-tokens 2
                         :phase     :pay
                         :triggers  [(assoc arena-trigger :id 1)]}]}))
    (is (thrown-with-msg? AssertionError #"Choose error"
                          (-> {:landmarks {:arena (assoc arena :vp-tokens 12)}
                               :players   [{:hand     [copper catapult catapult]
                                            :coins    0
                                            :phase    :action
                                            :triggers [(assoc arena-trigger :id 1)]}]}
                              (play 0 :copper)
                              (choose [:catapult :catapult]))))
    (testing "Setup"
      (ut/reset-ids!)
      (is (= (-> {:landmarks {:arena arena}
                  :players   [{} {}]}
                 setup-game)
             {:landmarks {:arena (assoc arena :vp-tokens 12)}
              :players   [{:triggers [(assoc arena-trigger :id 1)]}
                          {:triggers [(assoc arena-trigger :id 2)]}]})))))

(deftest bandit-ford-scoring-test
  (testing "Bandit Ford"
    (is (= (calculate-score {:landmarks {:bandit-ford bandit-ford}
                             :players   [{:hand [copper]}]})
           {:landmarks {:bandit-ford bandit-ford}
            :players   [{:hand           [copper]
                         :score          []
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:bandit-ford bandit-ford}
                             :players   [{:hand [silver]}]})
           {:landmarks {:bandit-ford bandit-ford}
            :players   [{:hand           [silver]
                         :score          [{:landmark        bandit-ford
                                           :card            silver
                                           :vp-per-card     -2
                                           :number-of-cards 1
                                           :victory-points  -2}]
                         :victory-points -2}]}))
    (is (= (calculate-score {:landmarks {:bandit-ford bandit-ford}
                             :players   [{:hand [gold]}]})
           {:landmarks {:bandit-ford bandit-ford}
            :players   [{:hand           [gold]
                         :score          [{:landmark        bandit-ford
                                           :card            gold
                                           :vp-per-card     -2
                                           :number-of-cards 1
                                           :victory-points  -2}]
                         :victory-points -2}]}))
    (is (= (calculate-score {:landmarks {:bandit-ford bandit-ford}
                             :players   [{:hand [silver silver gold province]}]})
           {:landmarks {:bandit-ford bandit-ford}
            :players   [{:hand           [silver silver gold province]
                         :score          [{:landmark        bandit-ford
                                           :card            gold
                                           :vp-per-card     -2
                                           :number-of-cards 1
                                           :victory-points  -2}
                                          {:landmark        bandit-ford
                                           :card            silver
                                           :vp-per-card     -2
                                           :number-of-cards 2
                                           :victory-points  -4}
                                          {:card            province
                                           :vp-per-card     6
                                           :number-of-cards 1
                                           :victory-points  6}]
                         :victory-points 0}]}))))

(deftest battlefield-test
  (testing "Battlefield"
    (let [estate (assoc estate :id 0)]
      (is (= (-> {:landmarks {:battlefield (assoc battlefield :vp-tokens 12)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:triggers [(assoc battlefield-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :estate}))
             {:landmarks {:battlefield (assoc battlefield :vp-tokens 10)}
              :supply    [{:card estate :pile-size 7}]
              :players   [{:discard   [estate]
                           :vp-tokens 2
                           :triggers  [(assoc battlefield-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:battlefield (assoc battlefield :vp-tokens 2)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:triggers [(assoc battlefield-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :estate}))
             {:landmarks {:battlefield battlefield}
              :supply    [{:card estate :pile-size 7}]
              :players   [{:discard   [estate]
                           :vp-tokens 2}]}))
      (is (= (-> {:landmarks {:battlefield (assoc battlefield :vp-tokens 1)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:triggers [(assoc battlefield-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :estate}))
             {:landmarks {:battlefield battlefield}
              :supply    [{:card estate :pile-size 7}]
              :players   [{:discard   [estate]
                           :vp-tokens 1}]})))
    (let [silver (assoc silver :id 0)]
      (is (= (-> {:landmarks {:battlefield (assoc battlefield :vp-tokens 12)}
                  :supply    [{:card silver :pile-size 40}]
                  :players   [{:triggers [(assoc battlefield-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :silver}))
             {:landmarks {:battlefield (assoc battlefield :vp-tokens 12)}
              :supply    [{:card silver :pile-size 39}]
              :players   [{:discard  [silver]
                           :triggers [(assoc battlefield-trigger :id 1)]}]})))))

(deftest basilica-test
  (testing "Basilica"
    (let [estate (assoc estate :id 0)]
      (is (= (-> {:landmarks {:basilica (assoc basilica :vp-tokens 12)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:coins    4
                               :buys     1
                               :triggers [(assoc basilica-trigger :id 1)]}]}
                 (buy-card {:player-no 0 :card-name :estate}))
             {:landmarks {:basilica (assoc basilica :vp-tokens 10)}
              :supply    [{:card estate :pile-size 7}]
              :players   [{:discard   [estate]
                           :coins     2
                           :buys      0
                           :vp-tokens 2
                           :triggers  [(assoc basilica-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:basilica (assoc basilica :vp-tokens 12)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:coins    3
                               :buys     1
                               :triggers [(assoc basilica-trigger :id 1)]}]}
                 (buy-card {:player-no 0 :card-name :estate}))
             {:landmarks {:basilica (assoc basilica :vp-tokens 12)}
              :supply    [{:card estate :pile-size 7}]
              :players   [{:discard  [estate]
                           :coins    1
                           :buys     0
                           :triggers [(assoc basilica-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:basilica (assoc basilica :vp-tokens 12)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:coins    6
                               :buys     2
                               :triggers [(assoc basilica-trigger :id 1)]}]}
                 (buy-card {:player-no 0 :card-name :estate})
                 (buy-card {:player-no 0 :card-name :estate}))
             {:landmarks {:basilica (assoc basilica :vp-tokens 8)}
              :supply    [{:card estate :pile-size 6}]
              :players   [{:discard   [estate estate]
                           :coins     2
                           :buys      0
                           :vp-tokens 4
                           :triggers  [(assoc basilica-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:basilica (assoc basilica :vp-tokens 12)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:coins    6
                               :buys     3
                               :triggers [(assoc basilica-trigger :id 1)]}]}
                 (buy-card {:player-no 0 :card-name :estate})
                 (buy-card {:player-no 0 :card-name :estate})
                 (buy-card {:player-no 0 :card-name :estate}))
             {:landmarks {:basilica (assoc basilica :vp-tokens 8)}
              :supply    [{:card estate :pile-size 5}]
              :players   [{:discard   [estate estate estate]
                           :coins     0
                           :buys      0
                           :vp-tokens 4
                           :triggers  [(assoc basilica-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:basilica (assoc basilica :vp-tokens 12)}
                  :supply    [{:card estate :pile-size 8}]
                  :players   [{:coins    4
                               :buys     1
                               :triggers [(assoc basilica-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :estate}))
             {:landmarks {:basilica (assoc basilica :vp-tokens 12)}
              :supply    [{:card estate :pile-size 7}]
              :players   [{:discard  [estate]
                           :coins    4
                           :buys     1
                           :triggers [(assoc basilica-trigger :id 1)]}]})))))

(deftest baths-test
  (testing "Baths"
    (let [estate (assoc estate :id 0)]
      (is (= (-> {:track-gained-cards? true
                  :landmarks           {:baths (assoc baths :vp-tokens 12)}
                  :supply              [{:card estate :pile-size 8}]
                  :players             [{:phase    :action
                                         :triggers [(assoc baths-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :estate})
                 (end-turn 0))
             {:current-player      0
              :track-gained-cards? true
              :landmarks           {:baths (assoc baths :vp-tokens 12)}
              :supply              [{:card estate :pile-size 7}]
              :players             [{:hand     [estate]
                                     :actions  1
                                     :coins    0
                                     :buys     1
                                     :phase    :action
                                     :triggers [(assoc baths-trigger :id 1)]}]}))
      (is (= (-> {:track-gained-cards? true
                  :landmarks           {:baths (assoc baths :vp-tokens 12)}
                  :players             [{:phase    :action
                                         :triggers [(assoc baths-trigger :id 1)]}]}
                 (end-turn 0))
             {:current-player      0
              :track-gained-cards? true
              :landmarks           {:baths (assoc baths :vp-tokens 10)}
              :players             [{:actions   1
                                     :coins     0
                                     :buys      1
                                     :vp-tokens 2
                                     :phase     :action
                                     :triggers  [(assoc baths-trigger :id 1)]}]}))
      (let [outpost (assoc outpost :id 1)]
        (is (= (-> {:track-gained-cards? true
                    :landmarks           {:baths (assoc baths :vp-tokens 12)}
                    :players             [{:hand     [outpost]
                                           :deck     (repeat 5 copper)
                                           :actions  1
                                           :phase    :action
                                           :triggers [(assoc baths-trigger :id 1)]}]}
                   (play 0 :outpost)
                   (end-turn 0))
               {:current-player      0
                :track-gained-cards? true
                :landmarks           {:baths (assoc baths :vp-tokens 10)}
                :players             [{:hand                     [copper copper copper]
                                       :play-area                [outpost]
                                       :deck                     [copper copper]
                                       :actions                  1
                                       :coins                    0
                                       :buys                     1
                                       :vp-tokens                2
                                       :phase                    :action
                                       :previous-turn-was-yours? true
                                       :triggers                 [(assoc baths-trigger :id 1)]}]}))))))

(deftest colonnade-test
  (testing "Colonnade"
    (let [enchantress (assoc enchantress :id 0)
          silver      (assoc silver :id 1)]
      (is (= (-> {:landmarks {:colonnade (assoc colonnade :vp-tokens 12)}
                  :supply    [{:card enchantress :pile-size 10}]
                  :players   [{:coins    3
                               :buys     1
                               :triggers [(assoc colonnade-trigger :id 1)]}]}
                 (buy-card 0 :enchantress))
             {:landmarks {:colonnade (assoc colonnade :vp-tokens 12)}
              :supply    [{:card enchantress :pile-size 9}]
              :players   [{:discard  [enchantress]
                           :coins    0
                           :buys     0
                           :triggers [(assoc colonnade-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:colonnade (assoc colonnade :vp-tokens 12)}
                  :supply    [{:card enchantress :pile-size 10}]
                  :players   [{:play-area [enchantress]
                               :coins     3
                               :buys      1
                               :triggers  [(assoc colonnade-trigger :id 1)]}]}
                 (buy-card 0 :enchantress))
             {:landmarks {:colonnade (assoc colonnade :vp-tokens 10)}
              :supply    [{:card enchantress :pile-size 9}]
              :players   [{:play-area [enchantress]
                           :discard   [enchantress]
                           :coins     0
                           :buys      0
                           :vp-tokens 2
                           :triggers  [(assoc colonnade-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:colonnade (assoc colonnade :vp-tokens 12)}
                  :supply    [{:card enchantress :pile-size 10}]
                  :players   [{:play-area [enchantress]
                               :triggers  [(assoc colonnade-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :enchantress}))
             {:landmarks {:colonnade (assoc colonnade :vp-tokens 12)}
              :supply    [{:card enchantress :pile-size 9}]
              :players   [{:play-area [enchantress]
                           :discard   [enchantress]
                           :triggers  [(assoc colonnade-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:colonnade (assoc colonnade :vp-tokens 12)}
                  :supply    [{:card silver :pile-size 40}]
                  :players   [{:play-area [silver]
                               :coins     3
                               :buys      1
                               :triggers  [(assoc colonnade-trigger :id 1)]}]}
                 (buy-card 0 :silver))
             {:landmarks {:colonnade (assoc colonnade :vp-tokens 12)}
              :supply    [{:card silver :pile-size 39}]
              :players   [{:play-area [silver]
                           :discard   [silver]
                           :coins     0
                           :buys      0
                           :triggers  [(assoc colonnade-trigger :id 1)]}]})))))

(deftest defiled-shrine-test
  (let [chariot-race (assoc chariot-race :id 1)
        crown        (assoc crown :id 1)
        curse        (assoc curse :id 2)]
    (testing "Defiled Shrine"
      (is (= (-> {:landmarks {:defiled-shrine defiled-shrine}
                  :supply    [{:card chariot-race :pile-size 10 :tokens {:victory-point {:number-of-tokens 2}}}]
                  :players   [{:triggers [(assoc defiled-shrine-action-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :chariot-race}))
             {:landmarks {:defiled-shrine (assoc defiled-shrine :vp-tokens 1)}
              :supply    [{:card chariot-race :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
              :players   [{:discard  [chariot-race]
                           :triggers [(assoc defiled-shrine-action-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:defiled-shrine (assoc defiled-shrine :vp-tokens 3)}
                  :supply    [{:card chariot-race :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
                  :players   [{:triggers [(assoc defiled-shrine-action-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :chariot-race}))
             {:landmarks {:defiled-shrine (assoc defiled-shrine :vp-tokens 4)}
              :supply    [{:card chariot-race :pile-size 8}]
              :players   [{:discard  [chariot-race]
                           :triggers [(assoc defiled-shrine-action-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:defiled-shrine defiled-shrine}
                  :supply    [{:card chariot-race :pile-size 8}]
                  :players   [{:triggers [(assoc defiled-shrine-action-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :chariot-race}))
             {:landmarks {:defiled-shrine defiled-shrine}
              :supply    [{:card chariot-race :pile-size 7}]
              :players   [{:discard  [chariot-race]
                           :triggers [(assoc defiled-shrine-action-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:aqueduct       aqueduct
                              :defiled-shrine defiled-shrine}
                  :supply    [{:card crown :pile-size 10 :tokens {:victory-point {:number-of-tokens 2}}}]
                  :players   [{:triggers [(assoc aqueduct-treasure-trigger :id 1)
                                          (assoc defiled-shrine-action-trigger :id 2)]}]}
                 (gain {:player-no 0 :card-name :crown}))
             {:landmarks {:aqueduct       (assoc aqueduct :vp-tokens 1)
                          :defiled-shrine (assoc defiled-shrine :vp-tokens 1)}
              :supply    [{:card crown :pile-size 9}]
              :players   [{:discard  [crown]
                           :triggers [(assoc aqueduct-treasure-trigger :id 1)
                                      (assoc defiled-shrine-action-trigger :id 2)]}]}))
      (is (= (-> {:landmarks {:defiled-shrine (assoc defiled-shrine :vp-tokens 4)}
                  :supply    [{:card curse :pile-size 8}]
                  :players   [{:triggers [(assoc defiled-shrine-curse-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :curse}))
             {:landmarks {:defiled-shrine (assoc defiled-shrine :vp-tokens 4)}
              :supply    [{:card curse :pile-size 7}]
              :players   [{:discard  [curse]
                           :triggers [(assoc defiled-shrine-curse-trigger :id 1)]}]}))
      (is (= (-> {:landmarks {:defiled-shrine (assoc defiled-shrine :vp-tokens 4)}
                  :supply    [{:card curse :pile-size 8}]
                  :players   [{:coins    0
                               :buys     1
                               :triggers [(assoc defiled-shrine-curse-trigger :id 1)]}]}
                 (buy-card {:player-no 0 :card-name :curse}))
             {:landmarks {:defiled-shrine defiled-shrine}
              :supply    [{:card curse :pile-size 7}]
              :players   [{:discard   [curse]
                           :coins     0
                           :buys      0
                           :vp-tokens 4
                           :triggers  [(assoc defiled-shrine-curse-trigger :id 1)]}]}))
      (testing "Setup"
        (ut/reset-ids!)
        (is (= (-> {:landmarks {:defiled-shrine defiled-shrine}
                    :supply    [{:card engineer :pile-size 10}
                                {:card temple :pile-size 10}
                                {:card crown :pile-size 10}
                                {:card charm :pile-size 10}
                                (castles-pile 2)
                                (settlers-bustling-village-pile 2)]
                    :players   [{}]}
                   setup-game))
            {:landmarks {:defiled-shrine defiled-shrine}
             :supply    [{:card engineer :pile-size 10 :tokens {:victory-point {:number-of-tokens 2}}}
                         {:card temple :pile-size 10}
                         {:card crown :pile-size 10 :tokens {:victory-point {:number-of-tokens 2}}}
                         {:card charm :pile-size 10}
                         (castles-pile 2)
                         (assoc (settlers-bustling-village-pile 2)
                           :tokens {:victory-point {:number-of-tokens 2}})]
             :players   [{:triggers [(assoc defiled-shrine-action-trigger :id 1)
                                     (assoc defiled-shrine-curse-trigger :id 2)]}]})))))

(deftest fountain-scoring-test
  (testing "Fountain"
    (is (= (calculate-score {:landmarks {:fountain fountain}
                             :players   [{}]})
           {:landmarks {:fountain fountain}
            :players   [{:score          [{:landmark       fountain
                                           :victory-points 0
                                           :notes          "0 Coppers"}]
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:fountain fountain}
                             :players   [{:hand [copper]}]})
           {:landmarks {:fountain fountain}
            :players   [{:hand           [copper]
                         :score          [{:landmark       fountain
                                           :victory-points 0
                                           :notes          "1 Copper"}]
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:fountain fountain}
                             :players   [{:hand (repeat 9 copper)}]})
           {:landmarks {:fountain fountain}
            :players   [{:hand           (repeat 9 copper)
                         :score          [{:landmark       fountain
                                           :victory-points 0
                                           :notes          "9 Coppers"}]
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:fountain fountain}
                             :players   [{:hand (repeat 10 copper)}]})
           {:landmarks {:fountain fountain}
            :players   [{:hand           (repeat 10 copper)
                         :score          [{:landmark       fountain
                                           :victory-points 15
                                           :notes          "10 Coppers"}]
                         :victory-points 15}]}))
    (is (= (calculate-score {:landmarks {:fountain fountain}
                             :players   [{:hand (repeat 20 copper)}]})
           {:landmarks {:fountain fountain}
            :players   [{:hand           (repeat 20 copper)
                         :score          [{:landmark       fountain
                                           :victory-points 15
                                           :notes          "20 Coppers"}]
                         :victory-points 15}]}))))

(deftest keep-scoring-test
  (testing "Keep"
    (is (= (calculate-score {:landmarks {:keep keep-lm}
                             :players   [{:hand [copper]}
                                         {:hand [copper]}]})
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [copper]
                         :score          [{:landmark        keep-lm
                                           :card            copper
                                           :number-of-cards 1
                                           :victory-points  5}]
                         :victory-points 5}
                        {:hand           [copper]
                         :score          [{:landmark        keep-lm
                                           :card            copper
                                           :number-of-cards 1
                                           :victory-points  5}]
                         :victory-points 5}]}))
    (is (= (calculate-score {:landmarks {:keep keep-lm}
                             :players   [{:hand [copper]}
                                         {:hand [copper silver]}]})
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [copper]
                         :score          [{:landmark        keep-lm
                                           :card            copper
                                           :number-of-cards 1
                                           :victory-points  5}]
                         :victory-points 5}
                        {:hand           [copper silver]
                         :score          [{:landmark        keep-lm
                                           :card            copper
                                           :number-of-cards 1
                                           :victory-points  5}
                                          {:landmark        keep-lm
                                           :card            silver
                                           :number-of-cards 1
                                           :victory-points  5}]
                         :victory-points 10}]}))
    (is (= (calculate-score {:landmarks {:keep keep-lm}
                             :players   [{:hand [copper copper]}
                                         {:hand [copper silver]}]})
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [copper copper]
                         :score          [{:landmark        keep-lm
                                           :card            copper
                                           :number-of-cards 2
                                           :victory-points  5}]
                         :victory-points 5}
                        {:hand           [copper silver]
                         :score          [{:landmark        keep-lm
                                           :card            silver
                                           :number-of-cards 1
                                           :victory-points  5}]
                         :victory-points 5}]}))
    (is (= (calculate-score {:landmarks {:keep keep-lm}
                             :players   [{:hand [province]}
                                         {:hand [copper silver]}]})
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [province]
                         :score          [{:card            province
                                           :vp-per-card     6
                                           :number-of-cards 1
                                           :victory-points  6}]
                         :victory-points 6}
                        {:hand           [copper silver]
                         :score          [{:landmark        keep-lm
                                           :card            copper
                                           :number-of-cards 1
                                           :victory-points  5}
                                          {:landmark        keep-lm
                                           :card            silver
                                           :number-of-cards 1
                                           :victory-points  5}]
                         :victory-points 10}]}))
    (is (= (calculate-score {:landmarks {:keep keep-lm}
                             :players   [{:hand [archive crown]}
                                         {:hand [copper copper silver]}
                                         {:hand [copper silver silver gold]}]})
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [archive crown]
                         :score          [{:landmark        keep-lm
                                           :card            crown
                                           :number-of-cards 1
                                           :victory-points  5}]
                         :victory-points 5}
                        {:hand           [copper copper silver]
                         :score          [{:landmark        keep-lm
                                           :card            copper
                                           :number-of-cards 2
                                           :victory-points  5}]
                         :victory-points 5}
                        {:hand           [copper silver silver gold]
                         :score          [{:landmark        keep-lm
                                           :card            gold
                                           :number-of-cards 1
                                           :victory-points  5}
                                          {:landmark        keep-lm
                                           :card            silver
                                           :number-of-cards 2
                                           :victory-points  5}]
                         :victory-points 10}]}))))

(deftest labyrinth-test
  (testing "Labyrinth"
    (let [silver (assoc silver :id 0)]
      (is (= (-> {:track-gained-cards? true
                  :landmarks           {:labyrinth (assoc labyrinth :vp-tokens 12)}
                  :supply              [{:card silver :pile-size 40}]
                  :players             [{:triggers [(assoc labyrinth-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :silver}))
             {:track-gained-cards? true
              :landmarks           {:labyrinth (assoc labyrinth :vp-tokens 12)}
              :supply              [{:card silver :pile-size 39}]
              :players             [{:discard      [silver]
                                     :gained-cards [{:name :silver :cost 3 :types #{:treasure}}]
                                     :triggers     [(assoc labyrinth-trigger :id 1)]}]}))
      (is (= (-> {:track-gained-cards? true
                  :landmarks           {:labyrinth (assoc labyrinth :vp-tokens 12)}
                  :supply              [{:card silver :pile-size 39}]
                  :players             [{:discard      [silver]
                                         :gained-cards [{:name :silver :cost 3 :types #{:treasure}}]
                                         :triggers     [(assoc labyrinth-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :silver}))
             {:track-gained-cards? true
              :landmarks           {:labyrinth (assoc labyrinth :vp-tokens 10)}
              :supply              [{:card silver :pile-size 38}]
              :players             [{:discard      [silver silver]
                                     :gained-cards [{:name :silver :cost 3 :types #{:treasure}}
                                                    {:name :silver :cost 3 :types #{:treasure}}]
                                     :vp-tokens    2
                                     :triggers     [(assoc labyrinth-trigger :id 1)]}]}))
      (is (= (-> {:track-gained-cards? true
                  :landmarks           {:labyrinth (assoc labyrinth :vp-tokens 10)}
                  :supply              [{:card silver :pile-size 38}]
                  :players             [{:discard      [silver silver]
                                         :gained-cards [{:name :silver :cost 3 :types #{:treasure}}
                                                        {:name :silver :cost 3 :types #{:treasure}}]
                                         :vp-tokens    2
                                         :triggers     [(assoc labyrinth-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :silver}))
             {:track-gained-cards? true
              :landmarks           {:labyrinth (assoc labyrinth :vp-tokens 10)}
              :supply              [{:card silver :pile-size 37}]
              :players             [{:discard      [silver silver silver]
                                     :gained-cards [{:name :silver :cost 3 :types #{:treasure}}
                                                    {:name :silver :cost 3 :types #{:treasure}}
                                                    {:name :silver :cost 3 :types #{:treasure}}]
                                     :vp-tokens    2
                                     :triggers     [(assoc labyrinth-trigger :id 1)]}]}))
      (let [rocks (assoc rocks :id 2)]
        (is (= (-> {:track-gained-cards? true
                    :landmarks           {:labyrinth (assoc labyrinth :vp-tokens 12)}
                    :supply              [{:card silver :pile-size 40}
                                          {:split-pile [{:card rocks :pile-size 5}]}]
                    :players             [{:coins    4
                                           :buys     1
                                           :phase    :buy
                                           :triggers [(assoc labyrinth-trigger :id 1)]}]}
                   (buy-card 0 :rocks))
               {:track-gained-cards? true
                :landmarks           {:labyrinth (assoc labyrinth :vp-tokens 10)}
                :supply              [{:card silver :pile-size 39}
                                      {:split-pile [{:card rocks :pile-size 4}]}]
                :players             [{:deck         [silver]
                                       :discard      [rocks]
                                       :gained-cards [{:name :silver :cost 3 :types #{:treasure}}
                                                      {:name :rocks :cost 4 :types #{:treasure} :bought true}]
                                       :coins        0
                                       :buys         0
                                       :vp-tokens    2
                                       :phase        :buy
                                       :triggers     [(assoc labyrinth-trigger :id 1)]}]}))))))

(deftest museum-scoring-test
  (testing "Museum"
    (is (= (calculate-score {:landmarks {:museum museum}
                             :players   [{:hand [copper]}]})
           {:landmarks {:museum museum}
            :players   [{:hand           [copper]
                         :score          [{:landmark        museum
                                           :vp-per-card     2
                                           :number-of-cards 1
                                           :victory-points  2}]
                         :victory-points 2}]}))
    (is (= (calculate-score {:landmarks {:museum museum}
                             :players   [{:hand [copper silver]}]})
           {:landmarks {:museum museum}
            :players   [{:hand           [copper silver]
                         :score          [{:landmark        museum
                                           :vp-per-card     2
                                           :number-of-cards 2
                                           :victory-points  4}]
                         :victory-points 4}]}))
    (is (= (calculate-score {:landmarks {:museum museum}
                             :players   [{:hand [copper silver gold archive crown]}]})
           {:landmarks {:museum museum}
            :players   [{:hand           [copper silver gold archive crown]
                         :score          [{:landmark        museum
                                           :vp-per-card     2
                                           :number-of-cards 5
                                           :victory-points  10}]
                         :victory-points 10}]}))
    (is (= (calculate-score {:landmarks {:museum museum}
                             :players   [{:hand [copper silver gold estate duchy province]}]})
           {:landmarks {:museum museum}
            :players   [{:hand           [copper silver gold estate duchy province]
                         :score          [{:card            estate
                                           :vp-per-card     1
                                           :number-of-cards 1
                                           :victory-points  1}
                                          {:landmark        museum
                                           :vp-per-card     2
                                           :number-of-cards 6
                                           :victory-points  12}
                                          {:card            duchy
                                           :vp-per-card     3
                                           :number-of-cards 1
                                           :victory-points  3}
                                          {:card            province
                                           :vp-per-card     6
                                           :number-of-cards 1
                                           :victory-points  6}]
                         :victory-points 22}]}))
    (is (= (calculate-score {:landmarks {:museum museum}
                             :players   [{:hand (repeat 3 copper)}]})
           {:landmarks {:museum museum}
            :players   [{:hand           (repeat 3 copper)
                         :score          [{:landmark        museum
                                           :vp-per-card     2
                                           :number-of-cards 1
                                           :victory-points  2}]
                         :victory-points 2}]}))
    (is (= (calculate-score {:landmarks {:museum museum}
                             :players   [{:hand (concat (repeat 3 copper) (repeat 3 estate))}]})
           {:landmarks {:museum museum}
            :players   [{:hand           (concat (repeat 3 copper) (repeat 3 estate))
                         :score          [{:card            estate
                                           :vp-per-card     1
                                           :number-of-cards 3
                                           :victory-points  3}
                                          {:landmark        museum
                                           :vp-per-card     2
                                           :number-of-cards 2
                                           :victory-points  4}]
                         :victory-points 7}]}))))

(deftest obelisk-test
  (let [obelisk-of-engineer (assoc obelisk :chosen-cards #{:engineer})
        obelisk-of-settlers (assoc obelisk :chosen-cards #{:settlers :bustling-village})]
    (testing "Obelisk"
      (is (= (calculate-score {:landmarks {:obelisk obelisk-of-engineer}
                               :players   [{:hand [engineer]}]})
             {:landmarks {:obelisk obelisk-of-engineer}
              :players   [{:hand           [engineer]
                           :score          [{:landmark        obelisk-of-engineer
                                             :card            engineer
                                             :vp-per-card     2
                                             :number-of-cards 1
                                             :victory-points  2}]
                           :victory-points 2}]}))
      (is (= (calculate-score {:landmarks {:obelisk obelisk-of-settlers}
                               :players   [{:hand [settlers settlers bustling-village]}]})
             {:landmarks {:obelisk obelisk-of-settlers}
              :players   [{:hand           [settlers settlers bustling-village]
                           :score          [{:landmark        obelisk-of-settlers
                                             :card            bustling-village
                                             :vp-per-card     2
                                             :number-of-cards 1
                                             :victory-points  2}
                                            {:landmark        obelisk-of-settlers
                                             :card            settlers
                                             :vp-per-card     2
                                             :number-of-cards 2
                                             :victory-points  4}]
                           :victory-points 6}]}))
      (is (= (calculate-score {:landmarks {:obelisk obelisk-of-engineer}
                               :players   [{:hand [engineer settlers copper]}]})
             {:landmarks {:obelisk obelisk-of-engineer}
              :players   [{:hand           [engineer settlers copper]
                           :score          [{:landmark        obelisk-of-engineer
                                             :card            engineer
                                             :vp-per-card     2
                                             :number-of-cards 1
                                             :victory-points  2}]
                           :victory-points 2}]}))
      (testing "Setup"
        (is (= (-> {:landmarks {:obelisk obelisk}
                    :supply    [{:card copper :pile-size 46}
                                {:card silver :pile-size 40}
                                {:card gold :pile-size 30}
                                {:card engineer :pile-size 10}]
                    :players   [{}]}
                   setup-game)
               {:landmarks {:obelisk obelisk-of-engineer}
                :supply    [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}
                            {:card gold :pile-size 30}
                            {:card engineer :pile-size 10}]
                :players   [{}]}))))))

(deftest orchard-scoring-test
  (testing "Orchard"
    (is (= (calculate-score {:landmarks {:orchard orchard}
                             :players   [{:hand (concat (repeat 3 copper) (repeat 3 estate))}]})
           {:landmarks {:orchard orchard}
            :players   [{:hand           (concat (repeat 3 copper) (repeat 3 estate))
                         :score          [{:card            estate
                                           :vp-per-card     1
                                           :number-of-cards 3
                                           :victory-points  3}]
                         :victory-points 3}]}))
    (is (= (calculate-score {:landmarks {:orchard orchard}
                             :players   [{:hand (repeat 2 engineer)}]})
           {:landmarks {:orchard orchard}
            :players   [{:hand           (repeat 2 engineer)
                         :score          []
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:orchard orchard}
                             :players   [{:hand (repeat 3 engineer)}]})
           {:landmarks {:orchard orchard}
            :players   [{:hand           (repeat 3 engineer)
                         :score          [{:landmark        orchard
                                           :card            engineer
                                           :number-of-cards 3
                                           :victory-points  4}]
                         :victory-points 4}]}))
    (is (= (calculate-score {:landmarks {:orchard orchard}
                             :players   [{:hand (concat (repeat 6 engineer) (repeat 3 villa))}]})
           {:landmarks {:orchard orchard}
            :players   [{:hand           (concat (repeat 6 engineer) (repeat 3 villa))
                         :score          [{:landmark        orchard
                                           :card            engineer
                                           :number-of-cards 6
                                           :victory-points  4}
                                          {:landmark        orchard
                                           :card            villa
                                           :number-of-cards 3
                                           :victory-points  4}]
                         :victory-points 8}]}))
    (is (= (calculate-score {:landmarks {:orchard orchard}
                             :players   [{:hand (repeat 3 mill)}]})
           {:landmarks {:orchard orchard}
            :players   [{:hand           (repeat 3 mill)
                         :score          [{:landmark        orchard
                                           :card            mill
                                           :number-of-cards 3
                                           :victory-points  4}
                                          {:card            mill
                                           :vp-per-card     1
                                           :number-of-cards 3
                                           :victory-points  3}]
                         :victory-points 7}]}))))

(deftest palace-scoring-test
  (testing "Palace"
    (is (= (calculate-score {:landmarks {:palace palace}
                             :players   [{:hand (repeat 7 copper)}]})
           {:landmarks {:palace palace}
            :players   [{:hand           (repeat 7 copper)
                         :score          [{:landmark        palace
                                           :vp-per-card     3
                                           :number-of-cards 0
                                           :victory-points  0}]
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:palace palace}
                             :players   [{:hand (concat (repeat 7 copper)
                                                        (repeat 3 silver)
                                                        (repeat 1 gold))}]})
           {:landmarks {:palace palace}
            :players   [{:hand           (concat (repeat 7 copper)
                                                 (repeat 3 silver)
                                                 (repeat 1 gold))
                         :score          [{:landmark        palace
                                           :vp-per-card     3
                                           :number-of-cards 1
                                           :victory-points  3}]
                         :victory-points 3}]}))
    (is (= (calculate-score {:landmarks {:palace palace}
                             :players   [{:hand (concat (repeat 3 copper)
                                                        (repeat 3 silver)
                                                        (repeat 5 gold))}]})
           {:landmarks {:palace palace}
            :players   [{:hand           (concat (repeat 3 copper)
                                                 (repeat 3 silver)
                                                 (repeat 5 gold))
                         :score          [{:landmark        palace
                                           :vp-per-card     3
                                           :number-of-cards 3
                                           :victory-points  9}]
                         :victory-points 9}]}))
    (is (= (calculate-score {:landmarks {:palace palace}
                             :players   [{:hand (concat (repeat 3 silver)
                                                        (repeat 5 gold))}]})
           {:landmarks {:palace palace}
            :players   [{:hand           (concat (repeat 3 silver)
                                                 (repeat 5 gold))
                         :score          [{:landmark        palace
                                           :vp-per-card     3
                                           :number-of-cards 0
                                           :victory-points  0}]
                         :victory-points 0}]}))))

(deftest tomb-test
  (testing "Tomb"
    (is (= (-> {:landmarks {:tomb tomb}
                :players   [{:hand     [sacrifice copper]
                             :actions  1
                             :coins    0
                             :triggers [(assoc tomb-trigger :id 1)]}]}
               (play 0 :sacrifice)
               (choose :copper))
           {:landmarks {:tomb tomb}
            :players   [{:play-area [sacrifice]
                         :actions   0
                         :coins     2
                         :vp-tokens 1
                         :triggers  [(assoc tomb-trigger :id 1)]}]
            :trash     [copper]}))
    (is (= (-> {:landmarks {:tomb tomb}
                :supply    [{:card temple :pile-size 9}]
                :players   [{:hand     [temple copper estate]
                             :actions  1
                             :triggers [(assoc tomb-trigger :id 1)]}]}
               (play 0 :temple)
               (choose [:copper :estate]))
           {:landmarks {:tomb tomb}
            :supply    [{:card temple :pile-size 9 :tokens {:victory-point {:number-of-tokens 1}}}]
            :players   [{:play-area [temple]
                         :actions   0
                         :vp-tokens 3
                         :triggers  [(assoc tomb-trigger :id 1)]}]
            :trash     [copper estate]}))))

(deftest tower-scoring-test
  (testing "Tower"
    (is (= (calculate-score {:landmarks {:tower tower}
                             :supply    [{:card engineer :pile-size 0}]
                             :players   [{:hand [engineer]}]})
           {:landmarks {:tower tower}
            :supply    [{:card engineer :pile-size 0}]
            :players   [{:hand           [engineer]
                         :score          [{:landmark        tower
                                           :card            engineer
                                           :vp-per-card     1
                                           :number-of-cards 1
                                           :victory-points  1}]
                         :victory-points 1}]}))
    (is (= (calculate-score {:landmarks {:tower tower}
                             :supply    [{:card engineer :pile-size 0}]
                             :players   [{:hand (repeat 5 engineer)}]})
           {:landmarks {:tower tower}
            :supply    [{:card engineer :pile-size 0}]
            :players   [{:hand           (repeat 5 engineer)
                         :score          [{:landmark        tower
                                           :card            engineer
                                           :vp-per-card     1
                                           :number-of-cards 5
                                           :victory-points  5}]
                         :victory-points 5}]}))
    (is (= (calculate-score {:landmarks {:tower tower}
                             :supply    [{:card engineer :pile-size 1}]
                             :players   [{:hand (repeat 5 engineer)}]})
           {:landmarks {:tower tower}
            :supply    [{:card engineer :pile-size 1}]
            :players   [{:hand           (repeat 5 engineer)
                         :score          []
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:tower tower}
                             :supply    [{:card province :pile-size 0}]
                             :players   [{:hand [province]}]})
           {:landmarks {:tower tower}
            :supply    [{:card province :pile-size 0}]
            :players   [{:hand           [province]
                         :score          [{:card            province
                                           :vp-per-card     6
                                           :number-of-cards 1
                                           :victory-points  6}]
                         :victory-points 6}]}))
    (is (= (calculate-score {:landmarks {:tower tower}
                             :supply    [{:card curse :pile-size 0}]
                             :players   [{:hand (repeat 4 curse)}]})
           {:landmarks {:tower tower}
            :supply    [{:card curse :pile-size 0}]
            :players   [{:hand           (repeat 4 curse)
                         :score          [{:card            curse
                                           :vp-per-card     -1
                                           :number-of-cards 4
                                           :victory-points  -4}
                                          {:landmark        tower
                                           :card            curse
                                           :vp-per-card     1
                                           :number-of-cards 4
                                           :victory-points  4}]
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:tower tower}
                             :supply    [{:split-pile [{:card encampment :pile-size 0}
                                                       {:card plunder :pile-size 0}]}]
                             :players   [{:hand [encampment encampment plunder plunder plunder]}]})
           {:landmarks {:tower tower}
            :supply    [{:split-pile [{:card encampment :pile-size 0}
                                      {:card plunder :pile-size 0}]}]
            :players   [{:hand           [encampment encampment plunder plunder plunder]
                         :score          [{:landmark        tower
                                           :card            encampment
                                           :vp-per-card     1
                                           :number-of-cards 2
                                           :victory-points  2}
                                          {:landmark        tower
                                           :card            plunder
                                           :vp-per-card     1
                                           :number-of-cards 3
                                           :victory-points  3}]
                         :victory-points 5}]}))
    (is (= (calculate-score {:landmarks {:tower tower}
                             :supply    [{:split-pile [{:card encampment :pile-size 0}
                                                       {:card plunder :pile-size 1}]}]
                             :players   [{:hand [encampment encampment plunder plunder plunder]}]})
           {:landmarks {:tower tower}
            :supply    [{:split-pile [{:card encampment :pile-size 0}
                                      {:card plunder :pile-size 1}]}]
            :players   [{:hand           [encampment encampment plunder plunder plunder]
                         :score          []
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks   {:tower tower}
                             :extra-cards [{:card ghost :pile-size 0}]
                             :players     [{:hand [ghost]}]})
           {:landmarks   {:tower tower}
            :extra-cards [{:card ghost :pile-size 0}]
            :players     [{:hand           [ghost]
                           :score          []
                           :victory-points 0}]}))))

(deftest triumphal-arch-scoring-test
  (testing "Triumphal Arch"
    (is (= (calculate-score {:landmarks {:triumphal-arch triumphal-arch}
                             :players   [{:hand [engineer]}]})
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           [engineer]
                         :score          []
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:triumphal-arch triumphal-arch}
                             :players   [{:hand [engineer villa]}]})
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           [engineer villa]
                         :score          [{:landmark        triumphal-arch
                                           :card            villa
                                           :vp-per-card     3
                                           :number-of-cards 1
                                           :victory-points  3}]
                         :victory-points 3}]}))
    (is (= (calculate-score {:landmarks {:triumphal-arch triumphal-arch}
                             :players   [{:hand [engineer engineer engineer villa]}]})
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           [engineer engineer engineer villa]
                         :score          [{:landmark        triumphal-arch
                                           :card            villa
                                           :vp-per-card     3
                                           :number-of-cards 1
                                           :victory-points  3}]
                         :victory-points 3}]}))
    (is (= (calculate-score {:landmarks {:triumphal-arch triumphal-arch}
                             :players   [{:hand [engineer engineer villa villa enchantress]}]})
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           [engineer engineer villa villa enchantress]
                         :score          [{:landmark        triumphal-arch
                                           :card            villa
                                           :vp-per-card     3
                                           :number-of-cards 2
                                           :victory-points  6}]
                         :victory-points 6}]}))
    (is (= (calculate-score {:landmarks {:triumphal-arch triumphal-arch}
                             :players   [{:hand (concat (repeat 7 copper)
                                                        [engineer engineer villa])}]})
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           (concat (repeat 7 copper)
                                                 [engineer engineer villa])
                         :score          [{:landmark        triumphal-arch
                                           :card            villa
                                           :vp-per-card     3
                                           :number-of-cards 1
                                           :victory-points  3}]
                         :victory-points 3}]}))
    (is (= (calculate-score {:landmarks {:triumphal-arch triumphal-arch}
                             :players   [{:hand (concat (repeat 7 copper)
                                                        (repeat 3 estate)
                                                        [engineer engineer villa])}]})
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           (concat (repeat 7 copper)
                                                 (repeat 3 estate)
                                                 [engineer engineer villa])
                         :score          [{:card            estate
                                           :vp-per-card     1
                                           :number-of-cards 3
                                           :victory-points  3}
                                          {:landmark        triumphal-arch
                                           :card            villa
                                           :vp-per-card     3
                                           :number-of-cards 1
                                           :victory-points  3}]
                         :victory-points 6}]}))
    (is (= (calculate-score {:landmarks {:triumphal-arch triumphal-arch}
                             :players   [{:hand (concat (repeat 7 copper)
                                                        (repeat 3 mill)
                                                        [engineer engineer villa])}]})
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           (concat (repeat 7 copper)
                                                 (repeat 3 mill)
                                                 [engineer engineer villa])
                         :score          [{:card            mill
                                           :vp-per-card     1
                                           :number-of-cards 3
                                           :victory-points  3}
                                          {:landmark        triumphal-arch
                                           :card            engineer
                                           :vp-per-card     3
                                           :number-of-cards 2
                                           :victory-points  6}]
                         :victory-points 9}]}))))

(deftest wall-scoring-test
  (testing "Wall"
    (is (= (calculate-score {:landmarks {:wall wall}
                             :players   [{:hand (repeat 10 copper)}]})
           {:landmarks {:wall wall}
            :players   [{:hand           (repeat 10 copper)
                         :score          [{:landmark        wall
                                           :vp-per-card     -1
                                           :number-of-cards 0
                                           :victory-points  0
                                           :notes           "10 cards"}]
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:wall wall}
                             :players   [{:hand (repeat 15 copper)}]})
           {:landmarks {:wall wall}
            :players   [{:hand           (repeat 15 copper)
                         :score          [{:landmark        wall
                                           :vp-per-card     -1
                                           :number-of-cards 0
                                           :victory-points  0
                                           :notes           "15 cards"}]
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:wall wall}
                             :players   [{:hand (repeat 16 copper)}]})
           {:landmarks {:wall wall}
            :players   [{:hand           (repeat 16 copper)
                         :score          [{:landmark        wall
                                           :vp-per-card     -1
                                           :number-of-cards 1
                                           :victory-points  -1
                                           :notes           "16 cards"}]
                         :victory-points -1}]}))
    (is (= (calculate-score {:landmarks {:wall wall}
                             :players   [{:hand (repeat 30 copper)}]})
           {:landmarks {:wall wall}
            :players   [{:hand           (repeat 30 copper)
                         :score          [{:landmark        wall
                                           :vp-per-card     -1
                                           :number-of-cards 15
                                           :victory-points  -15
                                           :notes           "30 cards"}]
                         :victory-points -15}]}))))

(deftest wolf-den-scoring-test
  (testing "Wolf Den"
    (is (= (calculate-score {:landmarks {:wolf-den wolf-den}
                             :players   [{:hand [copper]}]})
           {:landmarks {:wolf-den wolf-den}
            :players   [{:hand           [copper]
                         :score          [{:landmark        wolf-den
                                           :card            copper
                                           :vp-per-card     -3
                                           :number-of-cards 1
                                           :victory-points  -3}]
                         :victory-points -3}]}))
    (is (= (calculate-score {:landmarks {:wolf-den wolf-den}
                             :players   [{:hand [copper copper]}]})
           {:landmarks {:wolf-den wolf-den}
            :players   [{:hand           [copper copper]
                         :score          []
                         :victory-points 0}]}))
    (is (= (calculate-score {:landmarks {:wolf-den wolf-den}
                             :players   [{:hand [copper copper duchy]}]})
           {:landmarks {:wolf-den wolf-den}
            :players   [{:hand           [copper copper duchy]
                         :score          [{:landmark        wolf-den
                                           :card            duchy
                                           :vp-per-card     -3
                                           :number-of-cards 1
                                           :victory-points  -3}
                                          {:card            duchy
                                           :vp-per-card     3
                                           :number-of-cards 1
                                           :victory-points  3}]
                         :victory-points 0}]}))))
