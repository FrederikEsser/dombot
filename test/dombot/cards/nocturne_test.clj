(ns dombot.cards.nocturne-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :refer [throne-room witch]]
            [dombot.cards.intrigue :refer [lurker]]
            [dombot.cards.nocturne :as nocturne :refer :all]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest boons-test
  (testing "Boons"
    (testing "The Earth's Gift"
      (is (= (-> {:boons   {:deck [earth-gift]}
                  :players [{:hand [estate]}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [earth-gift]}
              :players [{:hand [estate]}]}))
      (is (= (-> {:boons   {:deck [earth-gift]}
                  :players [{:hand [estate copper]}]}
                 (receive-boon {:player-no 0})
                 (choose nil))
             {:boons   {:discard [earth-gift]}
              :players [{:hand [estate copper]}]}))
      (let [bard (assoc bard :id 1)]
        (is (= (-> {:boons   {:deck [earth-gift]}
                    :supply  [{:card bard :pile-size 9}]
                    :players [{:hand [estate copper]}]}
                   (receive-boon {:player-no 0})
                   (choose :copper)
                   (choose :bard))
               {:boons   {:discard [earth-gift]}
                :supply  [{:card bard :pile-size 8}]
                :players [{:hand    [estate]
                           :discard [copper bard]}]}))))
    (testing "The Field's Gift"
      (is (= (-> {:boons   {:deck [field-gift]}
                  :players [{:actions 0
                             :coins   0}]}
                 (receive-boon {:player-no 0}))
             {:boons   {}
              :players [{:actions  1
                         :coins    1
                         :boons    [field-gift]
                         :triggers [{:trigger  :at-clean-up
                                     :duration :once
                                     :effects  [[:return-boon {:boon-name :the-field's-gift}]]}]}]}))
      (is (= (-> {:boons   {:deck [field-gift]}
                  :players [{:actions 0
                             :coins   0}]}
                 (receive-boon {:player-no 0})
                 (clean-up {:player-no 0}))
             {:boons   {:discard [field-gift]}
              :players [{:actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]})))
    (testing "The Flame's Gift"
      (is (= (-> {:boons   {:deck [flame-gift]}
                  :players [{:hand [estate]}]}
                 (receive-boon {:player-no 0})
                 (choose nil))
             {:boons   {:discard [flame-gift]}
              :players [{:hand [estate]}]}))
      (is (= (-> {:boons   {:deck [flame-gift]}
                  :players [{:hand [estate]}]}
                 (receive-boon {:player-no 0})
                 (choose :estate))
             {:boons   {:discard [flame-gift]}
              :players [{}]
              :trash   [estate]})))
    (testing "The Forest's Gift"
      (is (= (-> {:boons   {:deck [forest-gift]}
                  :players [{:coins 0
                             :buys  1}]}
                 (receive-boon {:player-no 0}))
             {:boons   {}
              :players [{:coins    1
                         :buys     2
                         :boons    [forest-gift]
                         :triggers [{:trigger  :at-clean-up
                                     :duration :once
                                     :effects  [[:return-boon {:boon-name :the-forest's-gift}]]}]}]}))
      (is (= (-> {:boons   {:deck [forest-gift]}
                  :players [{:coins 0
                             :buys  1}]}
                 (receive-boon {:player-no 0})
                 (clean-up {:player-no 0}))
             {:boons   {:discard [forest-gift]}
              :players [{:actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]})))
    (testing "The Moon's Gift"
      (is (= (-> {:boons   {:deck [moon-gift]}
                  :players [{}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [moon-gift]}
              :players [{}]}))
      (is (= (-> {:boons   {:deck [moon-gift]}
                  :players [{:deck    [bard]
                             :discard [estate gold copper]}]}
                 (receive-boon {:player-no 0})
                 (choose :gold))
             {:boons   {:discard [moon-gift]}
              :players [{:deck    [gold bard]
                         :discard [estate copper]}]})))
    (testing "The Mountain's Gift"
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:boons   {:deck [mountain-gift]}
                    :supply  [{:card silver :pile-size 40}]
                    :players [{}]}
                   (receive-boon {:player-no 0}))
               {:boons   {:discard [mountain-gift]}
                :supply  [{:card silver :pile-size 39}]
                :players [{:discard [silver]}]}))))
    (testing "The River's Gift"
      (is (= (-> {:boons   {:deck [river-gift]}
                  :players [{}]}
                 (receive-boon {:player-no 0}))
             {:boons   {}
              :players [{:boons    [river-gift]
                         :triggers [{:trigger  :at-draw-hand
                                     :duration :once
                                     :effects  [[:draw 1]]}
                                    {:trigger  :at-clean-up
                                     :duration :once
                                     :effects  [[:return-boon {:boon-name :the-river's-gift}]]}]}]}))
      (is (= (-> {:boons   {:deck [river-gift]}
                  :players [{:deck (repeat 7 copper)}
                            {:hand (repeat 5 copper)}]}
                 (receive-boon {:player-no 0})
                 (end-turn 0))
             {:current-player 1
              :boons          {:discard [river-gift]}
              :players        [{:hand    (repeat 6 copper)
                                :deck    [copper]
                                :actions 0
                                :coins   0
                                :buys    0
                                :phase   :out-of-turn}
                               {:hand    (repeat 5 copper)
                                :actions 1
                                :coins   0
                                :buys    1}]})))
    (testing "The Sea's Gift"
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:deck [copper copper]}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [sea-gift]}
              :players [{:hand [copper]
                         :deck [copper]}]}))
      (is (= (-> {:boons   {:discard [sea-gift]}
                  :players [{:deck [copper copper]}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [sea-gift]}
              :players [{:hand [copper]
                         :deck [copper]}]})))
    (testing "The Sky's Gift"
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [sky-gift]}
              :players [{}]}))
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{:hand [copper]}]}
                 (receive-boon {:player-no 0})
                 (choose :copper))
             {:boons   {:discard [sky-gift]}
              :players [{:discard [copper]}]}))
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{:hand [copper]}]}
                 (receive-boon {:player-no 0})
                 (choose nil))
             {:boons   {:discard [sky-gift]}
              :players [{:hand [copper]}]}))
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{:hand [copper copper]}]}
                 (receive-boon {:player-no 0})
                 (choose [:copper :copper]))
             {:boons   {:discard [sky-gift]}
              :players [{:discard [copper copper]}]}))
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{:hand [copper copper copper]}]}
                 (receive-boon {:player-no 0})
                 (choose nil))
             {:boons   {:discard [sky-gift]}
              :players [{:hand [copper copper copper]}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:boons   {:deck [sky-gift]}
                    :supply  [{:card gold :pile-size 30}]
                    :players [{:hand [copper copper copper]}]}
                   (receive-boon {:player-no 0})
                   (choose [:copper :copper :copper]))
               {:boons   {:discard [sky-gift]}
                :supply  [{:card gold :pile-size 29}]
                :players [{:discard [copper copper copper gold]}]}))))
    (testing "The Sun's Gift"
      (is (= (-> {:boons   {:deck [sun-gift]}
                  :players [{:deck [copper copper estate silver estate]}]}
                 (receive-boon {:player-no 0})
                 (choose nil)
                 (choose [:copper :copper :estate :silver]))
             {:boons   {:discard [sun-gift]}
              :players [{:deck [silver estate copper copper estate]}]}))
      (is (= (-> {:boons   {:deck [sun-gift]}
                  :players [{:deck [copper copper estate silver estate]}]}
                 (receive-boon {:player-no 0})
                 (choose [:estate])
                 (choose [:copper :copper :silver]))
             {:boons   {:discard [sun-gift]}
              :players [{:deck    [silver copper copper estate]
                         :discard [estate]}]}))
      (is (= (-> {:boons   {:deck [sun-gift]}
                  :players [{:deck [copper copper estate silver estate]}]}
                 (receive-boon {:player-no 0})
                 (choose [:estate :copper :copper :silver]))
             {:boons   {:discard [sun-gift]}
              :players [{:deck    [estate]
                         :discard [estate copper copper silver]}]})))
    (testing "The Swamp's Gift"
      (let [will-o-wisp (assoc will-o-wisp :id 1)]
        (is (= (-> {:boons       {:deck [swamp-gift]}
                    :extra-cards [{:card will-o-wisp :pile-size 12}]
                    :players     [{}]}
                   (receive-boon {:player-no 0}))
               {:boons       {:discard [swamp-gift]}
                :extra-cards [{:card will-o-wisp :pile-size 11}]
                :players     [{:discard [will-o-wisp]}]}))))
    (testing "The Wind's Gift"
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [wind-gift]}
              :players [{}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [copper]}]}
                 (receive-boon {:player-no 0})
                 (choose :copper))
             {:boons   {:discard [wind-gift]}
              :players [{:discard [copper]}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [copper copper copper]}]}
                 (receive-boon {:player-no 0})
                 (choose [:copper :copper]))
             {:boons   {:discard [wind-gift]}
              :players [{:hand    [copper]
                         :discard [copper copper]}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [estate]
                             :deck [copper]}]}
                 (receive-boon {:player-no 0})
                 (choose [:estate :copper]))
             {:boons   {:discard [wind-gift]}
              :players [{:discard [estate copper]}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [estate copper estate gold]
                             :deck [copper silver]}]}
                 (receive-boon {:player-no 0})
                 (choose [:estate :estate]))
             {:boons   {:discard [wind-gift]}
              :players [{:hand    [copper gold copper silver]
                         :discard [estate estate]}]})))))

(deftest bard-test
  (let [bard (assoc bard :id 0)]
    (testing "Bard"
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [bard]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :bard))
             {:boons   {:discard [sea-gift]}
              :players [{:hand      [copper]
                         :play-area [bard]
                         :deck      [copper]
                         :actions   0
                         :coins     2}]})))))

(deftest cemetery-test
  (let [cemetery (assoc cemetery :id 0)]
    (testing "Cemetery"
      (is (= (calc-victory-points {:deck [cemetery]})
             2))
      (is (= (-> {:supply  [{:card cemetery :pile-size 8}]
                  :players [{}]}
                 (gain {:player-no 0
                        :card-name :cemetery}))
             {:supply  [{:card cemetery :pile-size 7}]
              :players [{:discard [cemetery]}]}))
      (is (= (-> {:supply  [{:card cemetery :pile-size 8}]
                  :players [{:hand [estate]}]}
                 (gain {:player-no 0
                        :card-name :cemetery})
                 (choose nil))
             {:supply  [{:card cemetery :pile-size 7}]
              :players [{:hand    [estate]
                         :discard [cemetery]}]}))
      (is (= (-> {:supply  [{:card cemetery :pile-size 8}]
                  :players [{:hand [estate]}]}
                 (gain {:player-no 0
                        :card-name :cemetery})
                 (choose :estate))
             {:supply  [{:card cemetery :pile-size 7}]
              :players [{:discard [cemetery]}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card cemetery :pile-size 8}]
                  :players [{:hand [estate estate estate copper copper]}]}
                 (gain {:player-no 0
                        :card-name :cemetery})
                 (choose [:estate :estate :estate :copper]))
             {:supply  [{:card cemetery :pile-size 7}]
              :players [{:hand    [copper]
                         :discard [cemetery]}]
              :trash   [estate estate estate copper]})))
    (let [haunted-mirror (assoc haunted-mirror :id 1)
          ghost          (assoc ghost :id 2)]
      (testing "Haunted Mirror"
        (is (= (-> {:players [{:hand  [haunted-mirror]
                               :coins 0}]}
                   (play 0 :haunted-mirror))
               {:players [{:play-area [haunted-mirror]
                           :coins     1}]}))
        (is (= (-> {:supply      [{:card cemetery :pile-size 8}]
                    :extra-cards [{:card ghost :pile-size 6}]
                    :players     [{:hand [haunted-mirror]}]}
                   (gain {:player-no 0
                          :card-name :cemetery})
                   (choose :haunted-mirror))
               {:supply      [{:card cemetery :pile-size 7}]
                :extra-cards [{:card ghost :pile-size 6}]
                :players     [{:discard [cemetery]}]
                :trash       [haunted-mirror]}))
        (is (= (-> {:supply      [{:card cemetery :pile-size 8}]
                    :extra-cards [{:card ghost :pile-size 6}]
                    :players     [{:hand [haunted-mirror conclave]}]}
                   (gain {:player-no 0
                          :card-name :cemetery})
                   (choose :haunted-mirror)
                   (choose nil))
               {:supply      [{:card cemetery :pile-size 7}]
                :extra-cards [{:card ghost :pile-size 6}]
                :players     [{:hand    [conclave]
                               :discard [cemetery]}]
                :trash       [haunted-mirror]}))
        (is (= (-> {:supply      [{:card cemetery :pile-size 8}]
                    :extra-cards [{:card ghost :pile-size 6}]
                    :players     [{:hand [haunted-mirror conclave]}]}
                   (gain {:player-no 0
                          :card-name :cemetery})
                   (choose :haunted-mirror)
                   (choose :conclave))
               {:supply      [{:card cemetery :pile-size 7}]
                :extra-cards [{:card ghost :pile-size 5}]
                :players     [{:discard [conclave ghost cemetery]}]
                :trash       [haunted-mirror]}))))))

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
                                       :triggers     [changeling-trigger]}]})))
      (let [ghost (assoc ghost :id 1)]
        (is (= (-> {:extra-cards [{:card ghost :pile-size 6}]
                    :supply      [{:card changeling :pile-size 10}]
                    :players     [{:triggers [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :ghost :from :extra-cards})
                   (choose :ghost))
               {:extra-cards [{:card ghost :pile-size 6}]
                :supply      [{:card changeling :pile-size 9}]
                :players     [{:discard  [changeling]
                               :triggers [changeling-trigger]}]})))
      (testing "gaining from Trash"
        (let [conclave (assoc conclave :id 1)]
          (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                {:card conclave :pile-size 9}]
                      :players [{:triggers [changeling-trigger]}]
                      :trash   [conclave]}
                     (gain {:player-no 0 :card-name :conclave :from :trash})
                     (choose :conclave))
                 {:supply  [{:card changeling :pile-size 9}
                            {:card conclave :pile-size 10}]
                  :players [{:discard  [changeling]
                             :triggers [changeling-trigger]}]
                  :trash   []})))
        (let [ghost (assoc ghost :id 1)]
          (is (= (-> {:extra-cards [{:card ghost :pile-size 5}]
                      :supply      [{:card changeling :pile-size 10}]
                      :players     [{:triggers [changeling-trigger]}]
                      :trash       [ghost]}
                     (gain {:player-no 0 :card-name :ghost :from :trash})
                     (choose :ghost))
                 {:extra-cards [{:card ghost :pile-size 6}]
                  :supply      [{:card changeling :pile-size 9}]
                  :players     [{:discard  [changeling]
                                 :triggers [changeling-trigger]}]
                  :trash       []})))
        (let [zombie-apprentice (assoc zombie-apprentice :id 1)]
          (is (= (-> {:supply  [{:card changeling :pile-size 10}]
                      :players [{:triggers [changeling-trigger]}]
                      :trash   [zombie-apprentice]}
                     (gain {:player-no 0 :card-name :zombie-apprentice :from :trash}))
                 {:supply  [{:card changeling :pile-size 10}]
                  :players [{:discard  [zombie-apprentice]
                             :triggers [changeling-trigger]}]
                  :trash   []})))))))

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

(deftest crypt-test
  (let [crypt (assoc crypt :id 0)]
    (testing "Crypt"
      (is (= (-> {:players [{:hand [crypt]}]}
                 (play 0 :crypt))
             {:players [{:play-area [crypt]}]}))
      (is (= (-> {:players [{:hand      [crypt]
                             :play-area [conclave guardian]}]}
                 (play 0 :crypt))
             {:players [{:play-area [conclave guardian crypt]}]}))
      (is (= (-> {:players [{:hand      [crypt]
                             :play-area [copper]}]}
                 (play 0 :crypt)
                 (choose nil))
             {:players [{:play-area [copper crypt]}]}))
      (is (= (-> {:players [{:hand      [crypt]
                             :play-area [gold]}]}
                 (play 0 :crypt)
                 (choose :gold))
             {:players [{:play-area [crypt]
                         :triggers  [(merge crypt-trigger
                                            {:card-id   0
                                             :set-aside [gold]})]}]}))
      (is (= (-> {:players [{:hand      [crypt]
                             :play-area [copper silver gold]}]}
                 (play 0 :crypt)
                 (choose [:copper :silver :gold]))
             {:players [{:play-area [crypt]
                         :triggers  [(merge crypt-trigger
                                            {:card-id   0
                                             :set-aside [copper silver gold]})]}]}))
      (is (= (-> {:players [{:play-area [crypt]
                             :deck      (repeat 6 copper)
                             :triggers  [(merge crypt-trigger
                                                {:card-id   0
                                                 :set-aside [copper silver gold]})]}]}
                 (end-turn 0)
                 (choose :gold))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper gold]
                                :play-area [crypt]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action
                                :triggers  [(merge crypt-trigger
                                                   {:card-id   0
                                                    :set-aside [copper silver]})]}]}))
      (is (= (-> {:players [{:play-area [crypt]
                             :deck      (repeat 6 copper)
                             :triggers  [(merge crypt-trigger
                                                {:card-id   0
                                                 :set-aside [copper silver]})]}]}
                 (end-turn 0)
                 (choose :copper))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper copper]
                                :play-area [crypt]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action
                                :triggers  [(merge crypt-trigger
                                                   {:card-id   0
                                                    :set-aside [silver]})]}]}))
      (is (= (-> {:players [{:play-area [crypt]
                             :deck      (repeat 6 copper)
                             :triggers  [(merge crypt-trigger
                                                {:card-id   0
                                                 :set-aside [silver]})]}]}
                 (end-turn 0)
                 (choose :silver))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper silver]
                                :play-area [crypt]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:mode    :swift
                  :players [{:play-area [crypt]
                             :deck      (repeat 6 copper)
                             :triggers  [(merge crypt-trigger
                                                {:card-id   0
                                                 :set-aside [silver]})]}]}
                 (end-turn 0))
             {:mode           :swift
              :current-player 0
              :players        [{:hand      [copper copper copper copper copper silver]
                                :play-area [crypt]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]})))))

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

(deftest devils-workshop-test
  (let [devils-workshop (assoc devils-workshop :id 0)]
    (testing "Devil's Workshop"
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:track-gained-cards? true
                    :supply              [{:card gold :pile-size 30}]
                    :players             [{:hand [devils-workshop]}]}
                   (play 0 :devil's-workshop))
               {:track-gained-cards? true
                :supply              [{:card gold :pile-size 29}]
                :players             [{:play-area    [devils-workshop]
                                       :discard      [gold]
                                       :gained-cards [{:name :gold :cost 6 :types #{:treasure}}]}]})))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:track-gained-cards? true
                    :supply              [{:card silver :pile-size 40}]
                    :players             [{:hand         [devils-workshop]
                                           :gained-cards [{:name :gold :cost 6 :types #{:treasure}}]}]}
                   (play 0 :devil's-workshop)
                   (choose :silver))
               {:track-gained-cards? true
                :supply              [{:card silver :pile-size 39}]
                :players             [{:play-area    [devils-workshop]
                                       :discard      [silver]
                                       :gained-cards [{:name :gold :cost 6 :types #{:treasure}}
                                                      {:name :silver :cost 3 :types #{:treasure}}]}]})))
      (let [imp (assoc imp :id 1)]
        (is (= (-> {:track-gained-cards? true
                    :extra-cards         [{:card imp :pile-size 13}]
                    :players             [{:hand         [devils-workshop]
                                           :gained-cards [{:name :gold :cost 6 :types #{:treasure}}
                                                          {:name :silver :cost 3 :types #{:treasure}}]}]}
                   (play 0 :devil's-workshop))
               {:track-gained-cards? true
                :extra-cards         [{:card imp :pile-size 12}]
                :players             [{:play-area    [devils-workshop]
                                       :discard      [imp]
                                       :gained-cards [{:name :gold :cost 6 :types #{:treasure}}
                                                      {:name :silver :cost 3 :types #{:treasure}}
                                                      {:name :imp :cost 2 :types #{:action :spirit}}]}]}))))))

(deftest exorcist-test
  (let [exorcist    (assoc exorcist :id 0)
        will-o-wisp (assoc will-o-wisp :id 1)
        imp         (assoc imp :id 2)
        ghost       (assoc ghost :id 3)]
    (testing "Exorcist"
      (is (= (-> {:players [{:hand [exorcist]}]}
                 (play 0 :exorcist))
             {:players [{:play-area [exorcist]}]}))
      (is (= (-> {:extra-cards (vals spirit-piles)
                  :players     [{:hand [exorcist copper]}]}
                 (play 0 :exorcist)
                 (choose :copper))
             {:extra-cards (vals spirit-piles)
              :players     [{:play-area [exorcist]}]
              :trash       [copper]}))
      (is (= (-> {:extra-cards [{:card will-o-wisp :pile-size 12}]
                  :players     [{:hand [exorcist estate]}]}
                 (play 0 :exorcist)
                 (choose :estate)
                 (choose :will-o'-wisp))
             {:extra-cards [{:card will-o-wisp :pile-size 11}]
              :players     [{:play-area [exorcist]
                             :discard   [will-o-wisp]}]
              :trash       [estate]}))
      (is (thrown-with-msg? AssertionError #"Choose error: Imp is not a valid option."
                            (-> {:extra-cards (vals spirit-piles)
                                 :players     [{:hand [exorcist estate]}]}
                                (play 0 :exorcist)
                                (choose :estate)
                                (choose :imp))))
      (is (thrown-with-msg? AssertionError #"Choose error: Wish is not a valid option."
                            (-> {:extra-cards [{:card wish :pile-size 12}
                                               {:card will-o-wisp :pile-size 12}]
                                 :players     [{:hand [exorcist estate]}]}
                                (play 0 :exorcist)
                                (choose :estate)
                                (choose :wish))))
      (is (= (-> {:extra-cards [{:card imp :pile-size 13}]
                  :players     [{:hand [exorcist silver]}]}
                 (play 0 :exorcist)
                 (choose :silver)
                 (choose :imp))
             {:extra-cards [{:card imp :pile-size 12}]
              :players     [{:play-area [exorcist]
                             :discard   [imp]}]
              :trash       [silver]}))
      (is (= (-> {:extra-cards [{:card ghost :pile-size 6}]
                  :players     [{:hand [exorcist duchy]}]}
                 (play 0 :exorcist)
                 (choose :duchy)
                 (choose :ghost))
             {:extra-cards [{:card ghost :pile-size 5}]
              :players     [{:play-area [exorcist]
                             :discard   [ghost]}]
              :trash       [duchy]})))))

(deftest ghost-test
  (let [ghost (assoc ghost :id 0)]
    (testing "Ghost"
      (is (= (-> {:players [{:hand [ghost]
                             :deck [conclave estate]}]}
                 (play 0 :ghost))
             {:players [{:play-area      [ghost]
                         :deck           [estate]
                         :revealed-cards {:ghost 1}
                         :triggers       [(merge ghost-trigger
                                                 {:card-id   0
                                                  :set-aside [conclave]})]}]}))
      (is (= (-> {:players [{:hand [ghost]
                             :deck [estate conclave estate]}]}
                 (play 0 :ghost))
             {:players [{:play-area      [ghost]
                         :deck           [estate]
                         :discard        [estate]
                         :revealed-cards {:discard 1
                                          :ghost   1}
                         :triggers       [(merge ghost-trigger
                                                 {:card-id   0
                                                  :set-aside [conclave]})]}]}))
      (is (= (-> {:players [{:hand    [ghost]
                             :deck    [estate]
                             :discard [copper]}]}
                 (play 0 :ghost))
             {:players [{:play-area      [ghost]
                         :discard        [estate copper]
                         :revealed-cards {:discard 2}}]}))
      (is (= (-> {:players [{:play-area [ghost]
                             :triggers  [(merge ghost-trigger
                                                {:card-id   0
                                                 :set-aside [conclave]})]}]}
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [ghost conclave]
                                :actions   1
                                :coins     4
                                :buys      1
                                :phase     :action}]})))))

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

(deftest imp-test
  (testing "Imp"
    (is (= (-> {:players [{:hand    [imp]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :imp))
           {:players [{:hand      [copper copper]
                       :play-area [imp]
                       :deck      [copper]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [imp imp]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :imp))
           {:players [{:hand      [imp copper copper]
                       :play-area [imp]
                       :deck      [copper]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [imp]
                           :deck    [conclave copper copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :imp)
               (choose :conclave))
           {:players [{:hand      [copper]
                       :play-area [imp conclave]
                       :deck      [copper]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [imp]
                           :deck    [conclave copper copper]
                           :actions 1}]}
               (play 0 :imp)
               (choose nil))
           {:players [{:hand      [conclave copper]
                       :play-area [imp]
                       :deck      [copper]
                       :actions   0}]}))))

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

(deftest necromancer-test
  (testing "Necromancer"
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1
                           :coins   0}]
                :trash   [conclave]}
               (play 0 :necromancer)
               (choose :conclave))
           {:players [{:play-area [necromancer]
                       :actions   0
                       :coins     2}]
            :trash   [(assoc conclave :face :down)]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1
                           :coins   0}]
                :trash   [conclave conclave]}
               (play 0 :necromancer)
               (choose :conclave))
           {:players [{:play-area [necromancer]
                       :actions   0
                       :coins     2}]
            :trash   [(assoc conclave :face :down) conclave]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1
                           :coins   2}]
                :trash   [(assoc conclave :face :down) conclave]}
               (play 0 :necromancer)
               (choose :conclave))
           {:players [{:play-area [necromancer]
                       :actions   0
                       :coins     4}]
            :trash   [(assoc conclave :face :down) (assoc conclave :face :down)]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1
                           :coins   4}]
                :trash   [(assoc conclave :face :down) (assoc conclave :face :down)]}
               (play 0 :necromancer))
           {:players [{:play-area [necromancer]
                       :actions   0
                       :coins     4}]
            :trash   [(assoc conclave :face :down) (assoc conclave :face :down)]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1}]
                :trash   [secret-cave]}
               (play 0 :necromancer))
           {:players [{:play-area [necromancer]
                       :actions   0}]
            :trash   [secret-cave]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1}]
                :trash   [(assoc zombie-apprentice :face :down)]}
               (clean-up {:player-no 0}))
           {:players [{:hand    [necromancer]
                       :actions 0
                       :coins   0
                       :buys    0
                       :phase   :out-of-turn}]
            :trash   [zombie-apprentice]}))
    (let [zombie-apprentice (assoc zombie-apprentice :id 1)]
      (is (= (-> {:players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [(assoc zombie-apprentice :face :down)]}
                 (play 0 :lurker)
                 (choose :gain)
                 (choose :zombie-apprentice))
             {:players [{:play-area [lurker]
                         :discard   [zombie-apprentice]
                         :actions   1}]
              :trash   []})))
    (testing "Zombie Apprentice"
      (is (= (-> {:players [{:hand    [necromancer]
                             :actions 1}]
                  :trash   [zombie-apprentice]}
                 (play 0 :necromancer)
                 (choose :zombie-apprentice))
             {:players [{:play-area [necromancer]
                         :actions   0}]
              :trash   [(assoc zombie-apprentice :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer conclave]
                             :actions 1}]
                  :trash   [zombie-apprentice]}
                 (play 0 :necromancer)
                 (choose :zombie-apprentice)
                 (choose nil))
             {:players [{:hand      [conclave]
                         :play-area [necromancer]
                         :actions   0}]
              :trash   [(assoc zombie-apprentice :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer conclave]
                             :deck    [copper copper copper copper]
                             :actions 1}]
                  :trash   [zombie-apprentice]}
                 (play 0 :necromancer)
                 (choose :zombie-apprentice)
                 (choose :conclave))
             {:players [{:hand      [copper copper copper]
                         :play-area [necromancer]
                         :deck      [copper]
                         :actions   1}]
              :trash   [(assoc zombie-apprentice :face :down)
                        conclave]})))
    (testing "Zombie Mason"
      (is (= (-> {:players [{:hand    [necromancer]
                             :actions 1}]
                  :trash   [zombie-mason]}
                 (play 0 :necromancer)
                 (choose :zombie-mason))
             {:players [{:play-area [necromancer]
                         :actions   0}]
              :trash   [(assoc zombie-mason :face :down)]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [necromancer]
                             :deck    [copper]
                             :actions 1}]
                  :trash   [zombie-mason]}
                 (play 0 :necromancer)
                 (choose :zombie-mason)
                 (choose nil))
             {:supply  (base/supply 2 8)
              :players [{:play-area [necromancer]
                         :actions   0}]
              :trash   [(assoc zombie-mason :face :down)
                        copper]}))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [necromancer]
                               :discard [estate estate]
                               :actions 1}]
                    :trash   [zombie-mason]}
                   (play 0 :necromancer)
                   (choose :zombie-mason)
                   (choose :silver))
               {:supply  [{:card silver :pile-size 39}]
                :players [{:play-area [necromancer]
                           :deck      [estate]
                           :discard   [silver]
                           :actions   0}]
                :trash   [(assoc zombie-mason :face :down)
                          estate]}))))
    (testing "Zombie Spy"
      (is (= (-> {:players [{:hand    [necromancer]
                             :actions 1}]
                  :trash   [zombie-spy]}
                 (play 0 :necromancer)
                 (choose :zombie-spy))
             {:players [{:play-area [necromancer]
                         :actions   1}]
              :trash   [(assoc zombie-spy :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer]
                             :deck    [copper]
                             :actions 1}]
                  :trash   [zombie-spy]}
                 (play 0 :necromancer)
                 (choose :zombie-spy))
             {:players [{:hand      [copper]
                         :play-area [necromancer]
                         :actions   1}]
              :trash   [(assoc zombie-spy :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer]
                             :deck    [copper silver estate]
                             :actions 1}]
                  :trash   [zombie-spy]}
                 (play 0 :necromancer)
                 (choose :zombie-spy)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [necromancer]
                         :deck      [silver estate]
                         :actions   1}]
              :trash   [(assoc zombie-spy :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer]
                             :deck    [copper estate silver]
                             :actions 1}]
                  :trash   [zombie-spy]}
                 (play 0 :necromancer)
                 (choose :zombie-spy)
                 (choose :estate))
             {:players [{:hand      [copper]
                         :play-area [necromancer]
                         :deck      [silver]
                         :discard   [estate]
                         :actions   1}]
              :trash   [(assoc zombie-spy :face :down)]})))))

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
              :trash   [copper]})))
    (testing "Cursed Gold"
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

(deftest secret-cave-test
  (let [secret-cave (assoc secret-cave :id 0)]
    (testing "Secret Cave"
      (is (= (-> {:players [{:hand    [secret-cave]
                             :actions 1}]}
                 (play 0 :secret-cave))
             {:players [{:play-area [secret-cave]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [secret-cave estate estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave))
             {:players      [{:hand      [estate estate copper]
                              :play-area [secret-cave]
                              :deck      [copper]
                              :actions   1}]
              :effect-stack [{:text      "You may discard 3 cards, for +$3 next turn."
                              :player-no 0
                              :card-id   0
                              :choice    ::nocturne/secret-cave-discard
                              :source    :hand
                              :options   [:estate :estate :copper]
                              :min       3
                              :max       3
                              :optional? true}]}))
      (is (= (-> {:players [{:hand    [secret-cave estate estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose nil))
             {:players [{:hand      [estate estate copper]
                         :play-area [secret-cave]
                         :deck      [copper]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [secret-cave estate estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose [:estate :estate :copper]))
             {:players [{:play-area [secret-cave]
                         :deck      [copper]
                         :discard   [estate estate copper]
                         :actions   1
                         :triggers  [(merge secret-cave-trigger
                                            {:card-id 0})]}]}))
      (is (= (-> {:players [{:hand    [secret-cave estate estate]
                             :deck    (repeat 7 copper)
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose [:estate :estate :copper])
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      (repeat 5 copper)
                                :play-area [secret-cave]
                                :deck      [copper]
                                :discard   [estate estate copper]
                                :actions   1
                                :coins     3
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:players [{:hand    [secret-cave estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose [:estate :copper]))
             {:players [{:play-area [secret-cave]
                         :deck      [copper]
                         :discard   [estate copper]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [secret-cave]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [secret-cave]
                         :deck      [copper]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [secret-cave]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose [:copper]))
             {:players [{:play-area [secret-cave]
                         :deck      [copper]
                         :discard   [copper]
                         :actions   1}]}))))
  (let [magic-lamp (assoc magic-lamp :id 0)]
    (testing "Magic Lamp"
      (let [wish (assoc wish :id 1)]
        (is (= (-> {:players [{:hand  [magic-lamp]
                               :coins 0}]}
                   (play 0 :magic-lamp))
               {:players [{:play-area [magic-lamp]
                           :coins     1}]}))
        (is (= (-> {:extra-cards [{:card wish :pile-size 12}]
                    :players     [{:hand      [magic-lamp]
                                   :play-area [secret-cave gold silver copper]
                                   :coins     6}]}
                   (play 0 :magic-lamp))
               {:extra-cards [{:card wish :pile-size 12}]
                :players     [{:play-area [secret-cave gold silver copper magic-lamp]
                               :coins     7}]}))
        (is (= (-> {:extra-cards [{:card wish :pile-size 12}]
                    :players     [{:hand      [magic-lamp]
                                   :play-area [secret-cave shepherd gold silver copper]
                                   :coins     6}]}
                   (play 0 :magic-lamp))
               {:extra-cards [{:card wish :pile-size 9}]
                :players     [{:play-area [secret-cave shepherd gold silver copper]
                               :discard   [wish wish wish]
                               :coins     7}]
                :trash       [magic-lamp]}))
        (is (= (-> {:extra-cards [{:card wish :pile-size 12}]
                    :players     [{:hand      [magic-lamp]
                                   :play-area [secret-cave shepherd shepherd gold silver copper]
                                   :coins     6}]}
                   (play 0 :magic-lamp))
               {:extra-cards [{:card wish :pile-size 12}]
                :players     [{:play-area [secret-cave shepherd shepherd gold silver copper magic-lamp]
                               :coins     7}]}))))))

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
                         :actions        1}]})))
    (testing "Pasture"
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

(deftest tracker-test
  (let [tracker (assoc tracker :id 0)]
    (testing "Tracker"
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [tracker]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :tracker))
             {:boons   {:discard [sea-gift]}
              :players [{:hand      [copper]
                         :play-area [tracker]
                         :deck      [copper]
                         :actions   0
                         :coins     1}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:supply  [{:card gold :pile-size 30}]
                    :players [{:play-area [tracker]
                               :deck      [copper]}]}
                   (gain {:player-no 0 :card-name :gold})
                   (choose :gold))
               {:supply  [{:card gold :pile-size 29}]
                :players [{:play-area [tracker]
                           :deck      [gold copper]}]}))))
    (testing "Pouch"
      (is (= (-> {:players [{:hand  [pouch]
                             :coins 0
                             :buys  1}]}
                 (play 0 :pouch))
             {:players [{:play-area [pouch]
                         :coins     1
                         :buys      2}]})))))

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

(deftest will-o-wisp-test
  (testing "Will-o'-Wisp"
    (let [will-o-wisp (assoc will-o-wisp :id 0)
          estate      (assoc estate :id 1)]
      (is (= (-> {:players [{:hand    [will-o-wisp]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :will-o'-wisp))
             {:players [{:hand           [silver]
                         :play-area      [will-o-wisp]
                         :deck           [silver copper]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [will-o-wisp]
                             :deck    [silver estate copper]
                             :actions 1}]}
                 (play 0 :will-o'-wisp))
             {:players [{:hand           [silver estate]
                         :play-area      [will-o-wisp]
                         :deck           [copper]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [will-o-wisp]
                             :deck    [silver]
                             :actions 1}]}
                 (play 0 :will-o'-wisp))
             {:players [{:hand      [silver]
                         :play-area [will-o-wisp]
                         :actions   1}]})))))

(deftest wish-test
  (testing "Wish"
    (let [wish (assoc wish :id 0)]
      (is (= (-> {:supply      (base/supply 2 8)
                  :extra-cards [{:card wish :pile-size 11}]
                  :players     [{:hand    [wish]
                                 :actions 1}]}
                 (play 0 :wish))
             {:supply       (base/supply 2 8)
              :extra-cards  [{:card wish :pile-size 12}]
              :players      [{:actions 1}]
              :effect-stack [{:text      "Gain a card to your hand costing up to $6."
                              :player-no 0
                              :card-id   0
                              :choice    :gain-to-hand
                              :source    :supply
                              :options   [:curse :estate :duchy :copper :silver :gold]
                              :min       1
                              :max       1}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:supply      [{:card gold :pile-size 30}]
                    :extra-cards [{:card wish :pile-size 11}]
                    :players     [{:hand    [wish]
                                   :actions 1}]}
                   (play 0 :wish)
                   (choose :gold))
               {:supply      [{:card gold :pile-size 29}]
                :extra-cards [{:card wish :pile-size 12}]
                :players     [{:hand    [gold]
                               :actions 1}]}))
        (is (= (-> {:supply      [{:card gold :pile-size 30}]
                    :extra-cards [{:card wish :pile-size 11}]
                    :players     [{:hand    [throne-room wish]
                                   :actions 1}]}
                   (play 0 :throne-room)
                   (choose :wish)
                   (choose :gold))
               {:supply      [{:card gold :pile-size 29}]
                :extra-cards [{:card wish :pile-size 12}]
                :players     [{:hand      [gold]
                               :play-area [throne-room]
                               :actions   2}]}))))))