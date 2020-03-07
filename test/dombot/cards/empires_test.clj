(ns dombot.cards.empires-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.empires :as empires :refer :all]
            [dombot.cards.dominion :refer [market throne-room]]
            [dombot.cards.intrigue :refer [mill]]
            [dombot.cards.seaside :refer [ambassador embargo fishing-village outpost]]
            [dombot.cards.prosperity :as prosperity :refer [hoard]]
            [dombot.cards.adventures :as adventures :refer [caravan-guard]]
            [dombot.cards.nocturne :as nocturne :refer [ghost]]
            [dombot.cards.renaissance :as renaissance :refer [patron spices capitalism citadel innovation piazza]]
            [dombot.utils :as ut]))

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
                             :tokens     [{:token-type :embargo
                                           :on-buy     [[:gain {:card-name :curse}]]}]}]
                  :players [{:actions 0
                             :coins   2}]
                  :trash   [embargo]}))
          (is (= (-> {:supply  [{:card curse :pile-size 10}
                                {:split-pile [{:card patrician :pile-size 5}
                                              {:card emporium :pile-size 5}]
                                 :tokens     [{:token-type :embargo
                                               :on-buy     [[:gain {:card-name :curse}]]}]}]
                      :players [{:coins 2
                                 :buys  1}]}
                     (buy-card 0 :patrician))
                 {:supply  [{:card curse :pile-size 9}
                            {:split-pile [{:card patrician :pile-size 4}
                                          {:card emporium :pile-size 5}]
                             :tokens     [{:token-type :embargo
                                           :on-buy     [[:gain {:card-name :curse}]]}]}]
                  :players [{:discard [curse patrician]
                             :coins   0
                             :buys    0}]}))
          (is (= (-> {:supply  [{:card curse :pile-size 10}
                                {:split-pile [{:card patrician :pile-size 0}
                                              {:card emporium :pile-size 5}]
                                 :tokens     [{:token-type :embargo
                                               :on-buy     [[:gain {:card-name :curse}]]}]}]
                      :players [{:coins 5
                                 :buys  1}]}
                     (buy-card 0 :emporium))
                 {:supply  [{:card curse :pile-size 9}
                            {:split-pile [{:card patrician :pile-size 0}
                                          {:card emporium :pile-size 4}]
                             :tokens     [{:token-type :embargo
                                           :on-buy     [[:gain {:card-name :curse}]]}]}]
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
                                 :tokens     [{:token-type :trade-route
                                               :on-gain    [[::prosperity/trade-route-move-token]]}]}]
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
           (+ 8 1 2 2 3 4 5 16)))))

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
                                  :phase     :action}]}))))))

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
              :players [{:hand    [plunder copper copper plunder]
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

(deftest farmers-market-test
  (let [farmers-market (assoc farmers-market :id 0)]
    (testing "farmers-market"
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}]}]
              :players [{:play-area [farmers-market]
                         :actions   0
                         :coins     1
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}]}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}
                                                                    {:token-type :victory-point}]}]
              :players [{:play-area [farmers-market]
                         :actions   0
                         :coins     2
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}
                                                                        {:token-type :victory-point}
                                                                        {:token-type :victory-point}]}]
                  :players [{:hand    [farmers-market]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :farmers'-market))
             {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}
                                                                    {:token-type :victory-point}
                                                                    {:token-type :victory-point}
                                                                    {:token-type :victory-point}]}]
              :players [{:play-area [farmers-market]
                         :actions   0
                         :coins     4
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card farmers-market :pile-size 9 :tokens [{:token-type :victory-point}
                                                                        {:token-type :victory-point}
                                                                        {:token-type :victory-point}
                                                                        {:token-type :victory-point}]}]
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
             {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}]}]
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
             {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}]}]
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
             {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}]}]
              :players [{:play-area [temple]
                         :actions   0
                         :vp-tokens 1}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card temple :pile-size 9}]
                  :players [{:hand    [temple]
                             :actions 1}]}
                 (play 0 :temple))
             {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}]}]
              :players [{:play-area [temple]
                         :actions   0
                         :vp-tokens 1}]}))
      (is (thrown-with-msg? AssertionError #"Choose error: All choices must be different: Copper, Copper, Estate"
                            (-> {:supply  [{:card temple :pile-size 9}]
                                 :players [{:hand    [temple copper copper estate]
                                            :actions 1}]}
                                (play 0 :temple)
                                (choose [:copper :copper :estate]))))
      (is (= (-> {:supply  [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}
                                                                {:token-type :victory-point}
                                                                {:token-type :victory-point}]}]
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
             {:supply  [{:card wild-hunt :pile-size 9 :tokens [{:token-type :victory-point}]}]
              :players [{:hand      [copper copper copper]
                         :play-area [wild-hunt]
                         :deck      [copper]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card wild-hunt :pile-size 9 :tokens [{:token-type :victory-point}
                                                                   {:token-type :victory-point}]}]
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
                            {:card wild-hunt :pile-size 9 :tokens [{:token-type :victory-point}
                                                                   {:token-type :victory-point}]}]
                  :players [{:hand    [wild-hunt]
                             :actions 1}]}
                 (play 0 :wild-hunt)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 0}
                        {:card wild-hunt :pile-size 9 :tokens [{:token-type :victory-point}
                                                               {:token-type :victory-point}]}]
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

(deftest landmark-scoring-test-test
  (testing "Landmark scoring"
    (is (= (-> {:landmarks {:bandit-ford bandit-ford}
                :players   [{:hand [copper]}]}
               calculate-victory-points)
           {:landmarks {:bandit-ford bandit-ford}
            :players   [{:hand           [copper]
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:bandit-ford bandit-ford}
                :players   [{:hand [silver]}]}
               calculate-victory-points)
           {:landmarks {:bandit-ford bandit-ford}
            :players   [{:hand           [silver]
                         :victory-points -2}]}))
    (is (= (-> {:landmarks {:bandit-ford bandit-ford}
                :players   [{:hand [gold]}]}
               calculate-victory-points)
           {:landmarks {:bandit-ford bandit-ford}
            :players   [{:hand           [gold]
                         :victory-points -2}]}))
    (is (= (-> {:landmarks {:bandit-ford bandit-ford}
                :players   [{:hand [silver silver gold province]}]}
               calculate-victory-points)
           {:landmarks {:bandit-ford bandit-ford}
            :players   [{:hand           [silver silver gold province]
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:fountain fountain}
                :players   [{:hand (repeat 9 copper)}]}
               calculate-victory-points)
           {:landmarks {:fountain fountain}
            :players   [{:hand           (repeat 9 copper)
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:fountain fountain}
                :players   [{:hand (repeat 10 copper)}]}
               calculate-victory-points)
           {:landmarks {:fountain fountain}
            :players   [{:hand           (repeat 10 copper)
                         :victory-points 15}]}))
    (is (= (-> {:landmarks {:fountain fountain}
                :players   [{:hand (repeat 20 copper)}]}
               calculate-victory-points)
           {:landmarks {:fountain fountain}
            :players   [{:hand           (repeat 20 copper)
                         :victory-points 15}]}))
    (is (= (-> {:landmarks {:keep keep-lm}
                :players   [{:hand [copper]}
                            {:hand [copper]}]}
               calculate-victory-points)
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [copper]
                         :victory-points 5}
                        {:hand           [copper]
                         :victory-points 5}]}))
    (is (= (-> {:landmarks {:keep keep-lm}
                :players   [{:hand [copper]}
                            {:hand [copper silver]}]}
               calculate-victory-points)
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [copper]
                         :victory-points 5}
                        {:hand           [copper silver]
                         :victory-points 10}]}))
    (is (= (-> {:landmarks {:keep keep-lm}
                :players   [{:hand [copper copper]}
                            {:hand [copper silver]}]}
               calculate-victory-points)
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [copper copper]
                         :victory-points 5}
                        {:hand           [copper silver]
                         :victory-points 5}]}))
    (is (= (-> {:landmarks {:keep keep-lm}
                :players   [{:hand [province]}
                            {:hand [copper silver]}]}
               calculate-victory-points)
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [province]
                         :victory-points 6}
                        {:hand           [copper silver]
                         :victory-points 10}]}))
    (is (= (-> {:landmarks {:keep keep-lm}
                :players   [{:hand [archive crown]}
                            {:hand [copper copper silver]}
                            {:hand [copper silver silver gold]}]}
               calculate-victory-points)
           {:landmarks {:keep keep-lm}
            :players   [{:hand           [archive crown]
                         :victory-points 5}
                        {:hand           [copper copper silver]
                         :victory-points 5}
                        {:hand           [copper silver silver gold]
                         :victory-points 10}]}))
    (is (= (-> {:landmarks {:museum museum}
                :players   [{:hand [copper]}]}
               calculate-victory-points)
           {:landmarks {:museum museum}
            :players   [{:hand           [copper]
                         :victory-points 2}]}))
    (is (= (-> {:landmarks {:museum museum}
                :players   [{:hand [copper silver]}]}
               calculate-victory-points)
           {:landmarks {:museum museum}
            :players   [{:hand           [copper silver]
                         :victory-points 4}]}))
    (is (= (-> {:landmarks {:museum museum}
                :players   [{:hand [copper silver gold archive crown]}]}
               calculate-victory-points)
           {:landmarks {:museum museum}
            :players   [{:hand           [copper silver gold archive crown]
                         :victory-points 10}]}))
    (is (= (-> {:landmarks {:museum museum}
                :players   [{:hand [copper silver gold estate duchy province]}]}
               calculate-victory-points)
           {:landmarks {:museum museum}
            :players   [{:hand           [copper silver gold estate duchy province]
                         :victory-points 22}]}))
    (is (= (-> {:landmarks {:museum museum}
                :players   [{:hand (repeat 3 copper)}]}
               calculate-victory-points)
           {:landmarks {:museum museum}
            :players   [{:hand           (repeat 3 copper)
                         :victory-points 2}]}))
    (is (= (-> {:landmarks {:museum museum}
                :players   [{:hand (concat (repeat 3 copper) (repeat 3 estate))}]}
               calculate-victory-points)
           {:landmarks {:museum museum}
            :players   [{:hand           (concat (repeat 3 copper) (repeat 3 estate))
                         :victory-points 7}]}))
    (is (= (-> {:landmarks {:obelisk (assoc obelisk :chosen-cards #{:forum})}
                :players   [{:hand [forum]}]}
               calculate-victory-points)
           {:landmarks {:obelisk (assoc obelisk :chosen-cards #{:forum})}
            :players   [{:hand           [forum]
                         :victory-points 2}]}))
    (is (= (-> {:landmarks {:obelisk (assoc obelisk :chosen-cards #{:settlers :bustling-village})}
                :players   [{:hand [settlers settlers bustling-village]}]}
               calculate-victory-points)
           {:landmarks {:obelisk (assoc obelisk :chosen-cards #{:settlers :bustling-village})}
            :players   [{:hand           [settlers settlers bustling-village]
                         :victory-points 6}]}))
    (is (= (-> {:landmarks {:obelisk (assoc obelisk :chosen-cards #{:forum})}
                :players   [{:hand [forum settlers copper]}]}
               calculate-victory-points)
           {:landmarks {:obelisk (assoc obelisk :chosen-cards #{:forum})}
            :players   [{:hand           [forum settlers copper]
                         :victory-points 2}]}))
    (is (= (-> {:landmarks {:orchard orchard}
                :players   [{:hand (concat (repeat 3 copper) (repeat 3 estate))}]}
               calculate-victory-points)
           {:landmarks {:orchard orchard}
            :players   [{:hand           (concat (repeat 3 copper) (repeat 3 estate))
                         :victory-points 3}]}))
    (is (= (-> {:landmarks {:orchard orchard}
                :players   [{:hand (repeat 2 forum)}]}
               calculate-victory-points)
           {:landmarks {:orchard orchard}
            :players   [{:hand           (repeat 2 forum)
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:orchard orchard}
                :players   [{:hand (repeat 3 forum)}]}
               calculate-victory-points)
           {:landmarks {:orchard orchard}
            :players   [{:hand           (repeat 3 forum)
                         :victory-points 4}]}))
    (is (= (-> {:landmarks {:orchard orchard}
                :players   [{:hand (concat (repeat 3 forum) (repeat 3 villa))}]}
               calculate-victory-points)
           {:landmarks {:orchard orchard}
            :players   [{:hand           (concat (repeat 3 forum) (repeat 3 villa))
                         :victory-points 8}]}))
    (is (= (-> {:landmarks {:orchard orchard}
                :players   [{:hand (repeat 3 mill)}]}
               calculate-victory-points)
           {:landmarks {:orchard orchard}
            :players   [{:hand           (repeat 3 mill)
                         :victory-points 7}]}))
    (is (= (-> {:landmarks {:palace palace}
                :players   [{:hand (repeat 7 copper)}]}
               calculate-victory-points)
           {:landmarks {:palace palace}
            :players   [{:hand           (repeat 7 copper)
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:palace palace}
                :players   [{:hand (concat (repeat 7 copper)
                                           (repeat 3 silver)
                                           (repeat 2 gold))}]}
               calculate-victory-points)
           {:landmarks {:palace palace}
            :players   [{:hand           (concat (repeat 7 copper)
                                                 (repeat 3 silver)
                                                 (repeat 2 gold))
                         :victory-points 6}]}))
    (is (= (-> {:landmarks {:palace palace}
                :players   [{:hand (concat (repeat 3 copper)
                                           (repeat 3 silver)
                                           (repeat 5 gold))}]}
               calculate-victory-points)
           {:landmarks {:palace palace}
            :players   [{:hand           (concat (repeat 3 copper)
                                                 (repeat 3 silver)
                                                 (repeat 5 gold))
                         :victory-points 9}]}))
    (is (= (-> {:landmarks {:palace palace}
                :players   [{:hand (concat (repeat 3 silver)
                                           (repeat 5 gold))}]}
               calculate-victory-points)
           {:landmarks {:palace palace}
            :players   [{:hand           (concat (repeat 3 silver)
                                                 (repeat 5 gold))
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:tower tower}
                :supply    [{:card forum :pile-size 0}]
                :players   [{:hand [forum]}]}
               calculate-victory-points)
           {:landmarks {:tower tower}
            :supply    [{:card forum :pile-size 0}]
            :players   [{:hand           [forum]
                         :victory-points 1}]}))
    (is (= (-> {:landmarks {:tower tower}
                :supply    [{:card forum :pile-size 0}]
                :players   [{:hand (repeat 5 forum)}]}
               calculate-victory-points)
           {:landmarks {:tower tower}
            :supply    [{:card forum :pile-size 0}]
            :players   [{:hand           (repeat 5 forum)
                         :victory-points 5}]}))
    (is (= (-> {:landmarks {:tower tower}
                :supply    [{:card forum :pile-size 1}]
                :players   [{:hand (repeat 5 forum)}]}
               calculate-victory-points)
           {:landmarks {:tower tower}
            :supply    [{:card forum :pile-size 1}]
            :players   [{:hand           (repeat 5 forum)
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:tower tower}
                :supply    [{:card province :pile-size 0}]
                :players   [{:hand [province]}]}
               calculate-victory-points)
           {:landmarks {:tower tower}
            :supply    [{:card province :pile-size 0}]
            :players   [{:hand           [province]
                         :victory-points 6}]}))
    (is (= (-> {:landmarks {:tower tower}
                :supply    [{:card curse :pile-size 0}]
                :players   [{:hand (repeat 4 curse)}]}
               calculate-victory-points)
           {:landmarks {:tower tower}
            :supply    [{:card curse :pile-size 0}]
            :players   [{:hand           (repeat 4 curse)
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:tower tower}
                :supply    [{:split-pile [{:card encampment :pile-size 0}
                                          {:card plunder :pile-size 0}]}]
                :players   [{:hand [encampment encampment plunder plunder plunder]}]}
               calculate-victory-points)
           {:landmarks {:tower tower}
            :supply    [{:split-pile [{:card encampment :pile-size 0}
                                      {:card plunder :pile-size 0}]}]
            :players   [{:hand           [encampment encampment plunder plunder plunder]
                         :victory-points 5}]}))
    (is (= (-> {:landmarks {:tower tower}
                :supply    [{:split-pile [{:card encampment :pile-size 0}
                                          {:card plunder :pile-size 1}]}]
                :players   [{:hand [encampment encampment plunder plunder plunder]}]}
               calculate-victory-points)
           {:landmarks {:tower tower}
            :supply    [{:split-pile [{:card encampment :pile-size 0}
                                      {:card plunder :pile-size 1}]}]
            :players   [{:hand           [encampment encampment plunder plunder plunder]
                         :victory-points 0}]}))
    (is (= (-> {:landmarks   {:tower tower}
                :extra-cards [{:card ghost :pile-size 0}]
                :players     [{:hand [ghost]}]}
               calculate-victory-points)
           {:landmarks   {:tower tower}
            :extra-cards [{:card ghost :pile-size 0}]
            :players     [{:hand           [ghost]
                           :victory-points 0}]}))
    (is (= (-> {:landmarks {:triumphal-arch triumphal-arch}
                :players   [{:hand [forum]}]}
               calculate-victory-points)
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           [forum]
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:triumphal-arch triumphal-arch}
                :players   [{:hand [forum villa]}]}
               calculate-victory-points)
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           [forum villa]
                         :victory-points 3}]}))
    (is (= (-> {:landmarks {:triumphal-arch triumphal-arch}
                :players   [{:hand [forum forum forum villa]}]}
               calculate-victory-points)
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           [forum forum forum villa]
                         :victory-points 3}]}))
    (is (= (-> {:landmarks {:triumphal-arch triumphal-arch}
                :players   [{:hand [forum forum villa villa enchantress]}]}
               calculate-victory-points)
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           [forum forum villa villa enchantress]
                         :victory-points 6}]}))
    (is (= (-> {:landmarks {:triumphal-arch triumphal-arch}
                :players   [{:hand (concat (repeat 7 copper)
                                           [forum forum villa])}]}
               calculate-victory-points)
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           (concat (repeat 7 copper)
                                                 [forum forum villa])
                         :victory-points 3}]}))
    (is (= (-> {:landmarks {:triumphal-arch triumphal-arch}
                :players   [{:hand (concat (repeat 7 copper)
                                           (repeat 3 estate)
                                           [forum forum villa])}]}
               calculate-victory-points)
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           (concat (repeat 7 copper)
                                                 (repeat 3 estate)
                                                 [forum forum villa])
                         :victory-points 6}]}))
    (is (= (-> {:landmarks {:triumphal-arch triumphal-arch}
                :players   [{:hand (concat (repeat 7 copper)
                                           (repeat 3 mill)
                                           [forum forum villa])}]}
               calculate-victory-points)
           {:landmarks {:triumphal-arch triumphal-arch}
            :players   [{:hand           (concat (repeat 7 copper)
                                                 (repeat 3 mill)
                                                 [forum forum villa])
                         :victory-points 9}]}))
    (is (= (-> {:landmarks {:wall wall}
                :players   [{:hand (repeat 10 copper)}]}
               calculate-victory-points)
           {:landmarks {:wall wall}
            :players   [{:hand           (repeat 10 copper)
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:wall wall}
                :players   [{:hand (repeat 15 copper)}]}
               calculate-victory-points)
           {:landmarks {:wall wall}
            :players   [{:hand           (repeat 15 copper)
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:wall wall}
                :players   [{:hand (repeat 16 copper)}]}
               calculate-victory-points)
           {:landmarks {:wall wall}
            :players   [{:hand           (repeat 16 copper)
                         :victory-points -1}]}))
    (is (= (-> {:landmarks {:wall wall}
                :players   [{:hand (repeat 30 copper)}]}
               calculate-victory-points)
           {:landmarks {:wall wall}
            :players   [{:hand           (repeat 30 copper)
                         :victory-points -15}]}))
    (is (= (-> {:landmarks {:wolf-den wolf-den}
                :players   [{:hand [copper]}]}
               calculate-victory-points)
           {:landmarks {:wolf-den wolf-den}
            :players   [{:hand           [copper]
                         :victory-points -3}]}))
    (is (= (-> {:landmarks {:wolf-den wolf-den}
                :players   [{:hand [copper copper]}]}
               calculate-victory-points)
           {:landmarks {:wolf-den wolf-den}
            :players   [{:hand           [copper copper]
                         :victory-points 0}]}))
    (is (= (-> {:landmarks {:wolf-den wolf-den}
                :players   [{:hand [copper copper duchy]}]}
               calculate-victory-points)
           {:landmarks {:wolf-den wolf-den}
            :players   [{:hand           [copper copper duchy]
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
                                     :triggers     [(assoc labyrinth-trigger :id 1)]}]})))))

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
            :supply    [{:card temple :pile-size 9 :tokens [{:token-type :victory-point}]}]
            :players   [{:play-area [temple]
                         :actions   0
                         :vp-tokens 3
                         :triggers  [(assoc tomb-trigger :id 1)]}]
            :trash     [copper estate]}))))
