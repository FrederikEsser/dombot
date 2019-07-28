(ns dombot.cards.nocturne-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :refer [witch]]
            [dombot.cards.nocturne :as nocturne :refer :all]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest changeling-test
  (let [changeling (assoc changeling :id 0)]
    (testing "Changeling"
      (is (= (-> {:players [{:hand      [changeling copper]
                             :play-area [silver conclave]}]}
                 (play 0 :changeling))
             {:players      [{:hand      [copper]
                              :play-area [silver conclave]}]
              :trash        [changeling]
              :effect-stack [{:text      "Gain a copy of a card you have in play."
                              :player-no 0
                              :card-id   0
                              :choice    :gain
                              :source    :play-area
                              :options   [:silver :conclave]
                              :min       1
                              :max       1}]}))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:supply  [{:card silver :pile-size 40}
                              {:card conclave :pile-size 0}]
                    :players [{:hand      [changeling copper]
                               :play-area [silver conclave]}]}
                   (play 0 :changeling)
                   (choose :silver))
               {:supply  [{:card silver :pile-size 39}
                          {:card conclave :pile-size 0}]
                :players [{:hand      [copper]
                           :play-area [silver conclave]
                           :discard   [silver]}]
                :trash   [changeling]})))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card conclave :pile-size 0}]
                  :players [{:hand      [changeling copper]
                             :play-area [silver conclave]}]}
                 (play 0 :changeling)
                 (choose :conclave))
             {:supply  [{:card silver :pile-size 40}
                        {:card conclave :pile-size 0}]
              :players [{:hand      [copper]
                         :play-area [silver conclave]}]
              :trash   [changeling]}))
      (let [conclave (assoc conclave :id 1)]
        (is (= (-> {:supply  [{:card changeling :pile-size 10}
                              {:card conclave :pile-size 10}]
                    :players [{:triggers [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :conclave})
                   (choose nil))
               {:supply  [{:card changeling :pile-size 10}
                          {:card conclave :pile-size 9}]
                :players [{:discard  [conclave]
                           :triggers [changeling-trigger]}]}))
        (is (= (-> {:supply  [{:card changeling :pile-size 10}
                              {:card conclave :pile-size 10}]
                    :players [{:triggers [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :conclave})
                   (choose :conclave))
               {:supply  [{:card changeling :pile-size 9}
                          {:card conclave :pile-size 10}]
                :players [{:discard  [changeling]
                           :triggers [changeling-trigger]}]}))
        (is (= (-> {:supply  [{:card changeling :pile-size 10}
                              {:card conclave :pile-size 10}]
                    :players [{:triggers [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :changeling}))
               {:supply  [{:card changeling :pile-size 9}
                          {:card conclave :pile-size 10}]
                :players [{:discard  [changeling]
                           :triggers [changeling-trigger]}]}))
        (is (= (-> {:supply  [{:card changeling :pile-size 0}
                              {:card conclave :pile-size 10}]
                    :players [{:triggers [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :conclave}))
               {:supply  [{:card changeling :pile-size 0}
                          {:card conclave :pile-size 9}]
                :players [{:discard  [conclave]
                           :triggers [changeling-trigger]}]}))
        (is (= (-> {:track-gained-cards? true
                    :supply              [{:card changeling :pile-size 10}
                                          {:card conclave :pile-size 10}]
                    :players             [{:gained-cards []
                                           :triggers     [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :conclave})
                   (choose :conclave))
               {:track-gained-cards? true
                :supply              [{:card changeling :pile-size 9}
                                      {:card conclave :pile-size 10}]
                :players             [{:discard      [changeling]
                                       :gained-cards [{:name :conclave :cost 4 :types #{:action}}]
                                       :triggers     [changeling-trigger]}]}))))))

(deftest cobbler-test
  (let [cobbler (assoc cobbler :id 0)]
    (testing "Cobbler"
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand [cobbler]}]}
                 (play 0 :cobbler)
                 (end-turn 0))
             {:supply         (base/supply 2 8)
              :current-player 0
              :players        [{:play-area [cobbler]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action
                                :triggers  [(get-trigger cobbler)]}]
              :effect-stack   [{:text      "Gain a card to your hand costing up to $4."
                                :player-no 0
                                :card-id   0
                                :choice    :gain-to-hand
                                :source    :supply
                                :options   [:curse :estate :copper :silver]
                                :min       1
                                :max       1}
                               {:player-no 0
                                :effect    [:remove-triggers
                                            {:trigger :at-start-turn}]}
                               {:player-no 0
                                :effect    [:sync-repeated-play]}]})))))

(deftest conclave-test
  (testing "Conclave"
    (is (= (-> {:players [{:hand    [conclave]
                           :actions 1
                           :coins   0}]}
               (play 0 :conclave))
           {:players [{:play-area [conclave]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [conclave conclave]
                           :actions 1
                           :coins   0}]}
               (play 0 :conclave))
           {:players [{:hand      [conclave]
                       :play-area [conclave]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [conclave tragic-hero]
                           :actions 1
                           :coins   0}]}
               (play 0 :conclave))
           {:players      [{:hand      [tragic-hero]
                            :play-area [conclave]
                            :actions   0
                            :coins     2}]
            :effect-stack [{:text      "You may play an Action card from your hand that you don't have a copy of in play."
                            :player-no 0
                            :choice    ::nocturne/conclave-play-action
                            :source    :hand
                            :options   [:tragic-hero]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [conclave tragic-hero]
                           :deck    [copper copper copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :conclave)
               (choose :tragic-hero))
           {:players [{:hand      [copper copper copper]
                       :play-area [conclave tragic-hero]
                       :actions   1
                       :coins     2
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [conclave tragic-hero]
                           :deck    [copper copper copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :conclave)
               (choose nil))
           {:players [{:hand      [tragic-hero]
                       :deck      [copper copper copper]
                       :play-area [conclave]
                       :actions   0
                       :coins     2}]}))))

(deftest den-of-sin-test
  (let [den-of-sin (assoc den-of-sin :id 0)]
    (testing "Den of Sin"
      (is (= (-> {:players [{:hand [den-of-sin]
                             :deck [copper copper copper copper copper copper silver silver]}]}
                 (play 0 :den-of-sin)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper copper silver]
                                :play-area [den-of-sin]
                                :deck      [silver]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:supply  [{:card den-of-sin :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0 :card-name :den-of-sin}))
             {:supply  [{:card den-of-sin :pile-size 9}]
              :players [{:hand [den-of-sin]}]})))))

(deftest ghost-town-test
  (let [ghost-town (assoc ghost-town :id 0)]
    (testing "Ghost Town"
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :action}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :pay}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :buy}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :night}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (is (= (-> {:players [{:hand [ghost-town]
                             :deck [copper copper copper copper copper copper silver]}]}
                 (play 0 :ghost-town)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper copper]
                                :play-area [ghost-town]
                                :deck      [silver]
                                :actions   2
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:supply  [{:card ghost-town :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0 :card-name :ghost-town}))
             {:supply  [{:card ghost-town :pile-size 9}]
              :players [{:hand [ghost-town]}]})))))

(deftest guardian-test
  (let [guardian (assoc guardian :id 0)]
    (testing "guardian"
      (is (= (-> {:players [{:hand [guardian]}]}
                 (play 0 :guardian))
             {:players [{:play-area  [guardian]
                         :unaffected [{:card-id 0}]
                         :triggers   [(get-trigger guardian)]}]}))
      (let [curse (assoc curse :id 1)]
        (is (= (-> {:supply  [{:card curse :pile-size 10}]
                    :players [{:hand    [witch]
                               :actions 1}
                              {:play-area  [guardian]
                               :unaffected [{:card-id 0}]
                               :triggers   [(get-trigger guardian)]}]}
                   (play 0 :witch))
               {:supply  [{:card curse :pile-size 10}]
                :players [{:play-area [witch]
                           :actions   0}
                          {:play-area  [guardian]
                           :unaffected [{:card-id 0}]
                           :triggers   [(get-trigger guardian)]}]})))
      (is (= (-> {:players [{:hand [guardian]}]}
                 (play 0 :guardian)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [guardian]
                                :actions   1
                                :coins     1
                                :buys      1
                                :phase     :action}]})))))

(deftest monastery-test
  (let [monastery (assoc monastery :id 0)]
    (testing "Monastery"
      (is (= (-> {:players [{:hand         [monastery estate copper]
                             :gained-cards [{:name :silver :types #{:treasure} :cost 3}]}]}
                 (play 0 :monastery))
             {:players      [{:hand         [estate copper]
                              :play-area    [monastery]
                              :gained-cards [{:name :silver :types #{:treasure} :cost 3}]}]
              :effect-stack [{:text      "Trash up to 1 card from your hand or Coppers you have in play."
                              :player-no 0
                              :choice    :trash-from-area
                              :source    :multi
                              :options   [{:area :hand :card-name :estate}
                                          {:area :hand :card-name :copper}]
                              :max       1}]}))
      (is (= (-> {:players [{:hand         [monastery estate copper]
                             :play-area    [silver copper]
                             :gained-cards [{:name :silver :types #{:treasure} :cost 3}
                                            {:name :silver :types #{:treasure} :cost 3}]}]}
                 (play 0 :monastery))
             {:players      [{:hand         [estate copper]
                              :play-area    [silver copper monastery]
                              :gained-cards [{:name :silver :types #{:treasure} :cost 3}
                                             {:name :silver :types #{:treasure} :cost 3}]}]
              :effect-stack [{:text      "Trash up to 2 cards from your hand or Coppers you have in play."
                              :player-no 0
                              :choice    :trash-from-area
                              :source    :multi
                              :options   [{:area :hand :card-name :estate}
                                          {:area :hand :card-name :copper}
                                          {:area :play-area :card-name :copper}]
                              :max       2}]}))
      (is (= (-> {:players [{:hand         [monastery estate copper]
                             :play-area    [silver copper]
                             :gained-cards [{:name :silver :types #{:treasure} :cost 3}]}]}
                 (play 0 :monastery)
                 (choose {:area :hand :card-name :estate}))
             {:players [{:hand         [copper]
                         :play-area    [silver copper monastery]
                         :gained-cards [{:name :silver :types #{:treasure} :cost 3}]}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand         [monastery estate copper]
                             :play-area    [silver copper]
                             :gained-cards [{:name :silver :types #{:treasure} :cost 3}
                                            {:name :silver :types #{:treasure} :cost 3}]}]}
                 (play 0 :monastery)
                 (choose [{:area :hand :card-name :copper}
                          {:area :play-area :card-name :copper}]))
             {:players [{:hand         [estate]
                         :play-area    [silver monastery]
                         :gained-cards [{:name :silver :types #{:treasure} :cost 3}
                                        {:name :silver :types #{:treasure} :cost 3}]}]
              :trash   [copper copper]})))))

(deftest night-watchman-test
  (testing "Night Watchman"
    (is (= (-> {:players [{:hand [night-watchman]
                           :deck [copper copper estate estate silver estate]}]}
               (play 0 :night-watchman)
               (choose nil))
           {:players [{:play-area [night-watchman]
                       :deck      [copper copper estate estate silver estate]}]}))
    (is (= (-> {:players [{:hand [night-watchman]
                           :deck [copper copper estate estate silver estate]}]}
               (play 0 :night-watchman)
               (choose [:estate :estate]))
           {:players [{:play-area [night-watchman]
                       :deck      [copper copper silver estate]
                       :discard   [estate estate]}]}))
    (is (= (-> {:players [{:hand [night-watchman]
                           :deck [copper copper estate estate silver estate]}]}
               (play 0 :night-watchman)
               (choose [:estate :estate :copper :copper :silver]))
           {:players [{:play-area [night-watchman]
                       :deck      [estate]
                       :discard   [estate estate copper copper silver]}]}))))

(deftest pooka-test
  (let [pooka (assoc pooka :id 0)]
    (testing "Pooka"
      (is (= (-> {:players [{:hand    [pooka estate copper cursed-gold]
                             :actions 1}]}
                 (play 0 :pooka))
             {:players      [{:hand      [estate copper cursed-gold]
                              :play-area [pooka]
                              :actions   0}]
              :effect-stack [{:text      "You may trash a Treasure other than Cursed Gold from your hand, for +4 Cards."
                              :player-no 0
                              :card-id   0
                              :choice    ::nocturne/pooka-trash
                              :source    :hand
                              :options   [:copper]
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [pooka estate copper cursed-gold]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 (play 0 :pooka)
                 (choose nil))
             {:players [{:hand      [estate copper cursed-gold]
                         :play-area [pooka]
                         :deck      (repeat 5 copper)
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [pooka estate copper cursed-gold]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 (play 0 :pooka)
                 (choose :copper))
             {:players [{:hand      [estate cursed-gold copper copper copper copper]
                         :play-area [pooka]
                         :deck      [copper]
                         :actions   0}]
              :trash   [copper]}))
      (let [curse (assoc curse :id 1)]
        (is (= (-> {:supply  [{:card curse :pile-size 10}]
                    :players [{:hand  [cursed-gold]
                               :coins 0}]}
                   (play 0 :cursed-gold))
               {:supply  [{:card curse :pile-size 9}]
                :players [{:play-area [cursed-gold]
                           :discard   [curse]
                           :coins     3}]}))))))

(deftest raider-test
  (let [raider (assoc raider :id 0)]
    (testing "Raider"
      (is (= (-> {:players [{:hand      [raider copper]
                             :play-area [silver conclave]}
                            {:hand [conclave copper raider silver gold]}]}
                 (play 0 :raider))
             {:players      [{:hand      [copper]
                              :play-area [silver conclave raider]
                              :triggers  [(get-trigger raider)]}
                             {:hand [conclave copper raider silver gold]}]
              :effect-stack [{:player-no 1
                              :text      "Discard a copy of a card the attacker has in play."
                              :choice    :discard-from-hand
                              :source    :hand
                              :options   [:conclave :raider :silver]
                              :min       1
                              :max       1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand      [raider copper]
                             :play-area [silver conclave]}
                            {:hand [conclave copper raider silver gold]}]}
                 (play 0 :raider)
                 (choose :silver))
             {:players [{:hand      [copper]
                         :play-area [silver conclave raider]
                         :triggers  [(get-trigger raider)]}
                        {:hand    [conclave copper raider gold]
                         :discard [silver]}]}))
      (is (= (-> {:players [{:hand      [raider copper]
                             :play-area [silver conclave]}
                            {:hand [conclave copper raider gold]}]}
                 (play 0 :raider))
             {:players [{:hand      [copper]
                         :play-area [silver conclave raider]
                         :triggers  [(get-trigger raider)]}
                        {:hand [conclave copper raider gold]}]}))
      (is (= (-> {:players [{:hand      [raider copper]
                             :play-area [silver conclave]}
                            {:hand (repeat 5 copper)}]}
                 (play 0 :raider))
             {:players [{:hand      [copper]
                         :play-area [silver conclave raider]
                         :triggers  [(get-trigger raider)]}
                        {:hand           (repeat 5 copper)
                         :revealed-cards {:hand 5}}]}))
      (is (= (-> {:players [{:hand [raider]}]}
                 (play 0 :raider)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [raider]
                                :actions   1
                                :coins     3
                                :buys      1
                                :phase     :action}]})))))

(deftest shepherd-test
  (let [shepherd (assoc shepherd :id 0)]
    (testing "Shepherd"
      (is (= (-> {:players [{:hand    [shepherd copper copper]
                             :actions 1}]}
                 (play 0 :shepherd))
             {:players [{:hand      [copper copper]
                         :play-area [shepherd]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [shepherd copper copper estate estate]
                             :actions 1}]}
                 (play 0 :shepherd)
                 (choose nil))
             {:players [{:hand      [copper copper estate estate]
                         :play-area [shepherd]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [shepherd copper copper estate estate]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 (play 0 :shepherd)
                 (choose :estate))
             {:players [{:hand           [copper copper estate copper copper]
                         :play-area      [shepherd]
                         :deck           (repeat 3 copper)
                         :discard        [estate]
                         :revealed-cards {:discard 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [shepherd copper copper estate estate]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 (play 0 :shepherd)
                 (choose [:estate :estate]))
             {:players [{:hand           [copper copper copper copper copper copper]
                         :play-area      [shepherd]
                         :deck           [copper]
                         :discard        [estate estate]
                         :revealed-cards {:discard 2}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand  [pasture]
                             :coins 0}]}
                 (play 0 :pasture))
             {:players [{:play-area [pasture]
                         :coins     1}]}))
      (is (= (calc-victory-points {:deck [pasture]})
             0))
      (is (= (calc-victory-points {:deck [pasture estate]})
             2))
      (is (= (calc-victory-points {:deck [pasture estate estate]})
             4))
      (is (= (calc-victory-points {:deck [pasture estate estate estate]})
             6)))))

(deftest tragic-hero-test
  (let [tragic-hero (assoc tragic-hero :id 0)]
    (testing "Tragic Hero"
      (is (= (-> {:players [{:hand    [tragic-hero copper copper copper copper]
                             :deck    [estate estate estate copper]
                             :actions 1
                             :buys    1}]}
                 (play 0 :tragic-hero))
             {:players [{:hand      [copper copper copper copper estate estate estate]
                         :play-area [tragic-hero]
                         :deck      [copper]
                         :actions   0
                         :buys      2}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:supply  [{:card gold :pile-size 30}]
                    :players [{:hand    [tragic-hero copper copper copper copper copper]
                               :deck    [estate estate estate copper]
                               :actions 1
                               :buys    1}]}
                   (play 0 :tragic-hero)
                   (choose :gold))
               {:supply  [{:card gold :pile-size 29}]
                :players [{:hand    [copper copper copper copper copper estate estate estate]
                           :deck    [copper]
                           :discard [gold]
                           :actions 0
                           :buys    2}]
                :trash   [tragic-hero]}))))))