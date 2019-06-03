(ns dombot.cards.prosperity-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.prosperity :as prosperity :refer :all]
            [dombot.cards.dominion :as dominion :refer [market]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest bank-test
  (testing "Bank"
    (is (= (-> {:players [{:hand  [bank]
                           :coins 0}]}
               (play 0 :bank))
           {:players [{:play-area [bank]
                       :coins     1}]}))
    (is (= (-> {:players [{:hand      [bank]
                           :play-area [copper]
                           :coins     1}]}
               (play 0 :bank))
           {:players [{:play-area [copper bank]
                       :coins     3}]}))))

(deftest city-test
  (testing "City"
    (is (= (-> {:players [{:hand    [city]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :city))
           {:players [{:hand      [copper]
                       :play-area [city]
                       :deck      [copper copper]
                       :actions   2}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 1}]
                :players [{:hand    [city]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :city))
           {:supply  [{:pile-size 0} {:pile-size 1}]
            :players [{:hand      [copper copper]
                       :play-area [city]
                       :deck      [copper]
                       :actions   2}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 0} {:pile-size 1}]
                :players [{:hand    [city]
                           :deck    [copper copper copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :city))
           {:supply  [{:pile-size 0} {:pile-size 0} {:pile-size 1}]
            :players [{:hand      [copper copper]
                       :play-area [city]
                       :deck      [copper]
                       :actions   2
                       :coins     1
                       :buys      2}]}))
    (is (= (-> {:supply  [{:pile-size 0} {:pile-size 0} {:pile-size 0}]
                :players [{:hand    [city]
                           :deck    [copper copper copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :city))
           {:supply  [{:pile-size 0} {:pile-size 0} {:pile-size 0}]
            :players [{:hand      [copper copper]
                       :play-area [city]
                       :deck      [copper]
                       :actions   2
                       :coins     1
                       :buys      2}]}))))

(deftest expand-test
  (let [duchy (assoc duchy :id 1)]
    (testing "Expand"
      (is (= (-> {:players [{:hand    [expand copper estate]
                             :actions 1}]}
                 (play 0 :expand))
             {:players      [{:hand      [copper estate]
                              :play-area [expand]
                              :actions   0}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :choice    [:trash-and-gain {:extra-cost 3}]
                              :source    :hand
                              :options   [:copper :estate]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [expand]
                             :actions 1}]}
                 (play 0 :expand))
             {:players [{:play-area [expand]
                         :actions   0}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [expand copper estate]
                             :actions 1}]}
                 (play 0 :expand)
                 (choose :estate))
             {:supply       (base/supply 2 8)
              :players      [{:hand      [copper]
                              :play-area [expand]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing up to $5."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:curse :estate :duchy :copper :silver]
                              :min       1
                              :max       1}]
              :trash        [estate]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [expand silver estate]
                             :actions 1}]}
                 (play 0 :expand)
                 (choose :estate)
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:hand      [silver]
                         :play-area [expand]
                         :discard   [duchy]
                         :actions   0}]
              :trash   [estate]})))))

(deftest forge-test
  (let [duchy (assoc duchy :id 1)]
    (testing "Forge"
      (is (= (-> {:players [{:hand    [forge copper estate estate estate copper]
                             :actions 1}]}
                 (play 0 :forge))
             {:players      [{:hand      [copper estate estate estate copper]
                              :play-area [forge]
                              :actions   0}]
              :effect-stack [{:text      "Trash any number of cards from your hand."
                              :player-no 0
                              :choice    ::prosperity/forge-trash
                              :source    :hand
                              :options   [:copper :estate :estate :estate :copper]}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [forge copper estate estate estate copper]
                             :actions 1}]}
                 (play 0 :forge)
                 (choose nil))
             {:supply       (base/supply 2 8)
              :players      [{:hand      [copper estate estate estate copper]
                              :play-area [forge]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing exactly 0."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:curse :copper]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [forge copper estate estate estate copper]
                             :actions 1}]}
                 (play 0 :forge)
                 (choose [:copper :copper]))
             {:supply       (base/supply 2 8)
              :players      [{:hand      [estate estate estate]
                              :play-area [forge]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing exactly 0."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:curse :copper]
                              :min       1
                              :max       1}]
              :trash        [copper copper]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [forge copper estate estate estate copper]
                             :actions 1}]}
                 (play 0 :forge)
                 (choose [:copper :copper :estate]))
             {:supply       (base/supply 2 8)
              :players      [{:hand      [estate estate]
                              :play-area [forge]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing exactly 2."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:estate]
                              :min       1
                              :max       1}]
              :trash        [copper copper estate]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [forge copper estate estate estate copper]
                             :actions 1}]}
                 (play 0 :forge)
                 (choose [:copper :copper :estate :estate]))
             {:supply  (base/supply 2 8)
              :players [{:hand      [estate]
                         :play-area [forge]
                         :actions   0}]
              :trash   [copper copper estate estate]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [forge copper estate estate estate copper]
                             :actions 1}]}
                 (play 0 :forge)
                 (choose [:copper :copper :estate :estate :estate]))
             {:supply       (base/supply 2 8)
              :players      [{:play-area [forge]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing exactly 6."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:gold]
                              :min       1
                              :max       1}]
              :trash        [copper copper estate estate estate]})))))

(deftest kings-court-test
  (let [kings-court (assoc kings-court :id 0)]
    (testing "King's Court"
      (is (= (-> {:players [{:hand    [kings-court market estate]
                             :deck    [copper copper copper copper]
                             :actions 1}]}
                 (play 0 :king's-court))
             {:players      [{:deck      [copper copper copper copper]
                              :hand      [market estate]
                              :play-area [kings-court]
                              :actions   0}]
              :effect-stack [{:text      "You may play an Action card from your hand three times."
                              :player-no 0
                              :card-id   0
                              :choice    [:repeat-action {:times 3}]
                              :source    :hand
                              :options   [:market]
                              :max       1}]}))
      (is (= (-> {:players [{:deck    [copper copper copper copper]
                             :hand    [kings-court market estate]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :king's-court)
                 (choose :market))
             {:players [{:deck      [copper]
                         :hand      [estate copper copper copper]
                         :play-area [kings-court market]
                         :actions   3
                         :coins     3
                         :buys      4}]}))
      (is (= (-> {:players [{:deck    [copper copper copper]
                             :hand    [kings-court market copper]
                             :actions 1}]}
                 (play 0 :king's-court)
                 (choose nil))
             {:players [{:deck      [copper copper copper]
                         :hand      [market copper]
                         :play-area [kings-court]
                         :actions   0}]})))))

(deftest loan-test
  (testing "Loan"
    (is (= (-> {:players [{:hand  [loan]
                           :deck  [copper estate]
                           :coins 0}]}
               (play 0 :loan))
           {:players      [{:play-area [loan]
                            :deck      [estate]
                            :revealed  [copper]
                            :coins     1}]
            :effect-stack [{:text      "You may trash the revealed Copper."
                            :player-no 0
                            :choice    :trash-from-revealed
                            :source    :revealed
                            :options   [:copper]
                            :min       0
                            :max       1}
                           {:player-no 0
                            :effect    [:discard-all-revealed]}]}))
    (is (= (-> {:players [{:hand  [loan]
                           :deck  [copper estate]
                           :coins 0}]}
               (play 0 :loan)
               (choose nil))
           {:players [{:play-area      [loan]
                       :deck           [estate]
                       :discard        [copper]
                       :revealed-cards {:discard 1}
                       :coins          1}]}))
    (is (= (-> {:players [{:hand  [loan]
                           :deck  [copper estate]
                           :coins 0}]}
               (play 0 :loan)
               (choose :copper))
           {:players [{:play-area [loan]
                       :deck      [estate]
                       :coins     1}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand  [loan]
                           :deck  [estate copper estate]
                           :coins 0}]}
               (play 0 :loan)
               (choose :copper))
           {:players [{:play-area      [loan]
                       :deck           [estate]
                       :discard        [estate]
                       :revealed-cards {:discard 1}
                       :coins          1}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [loan]
                           :deck    [estate]
                           :discard [duchy]
                           :coins   0}]}
               (play 0 :loan))
           {:players [{:play-area      [loan]
                       :discard        [estate duchy]
                       :revealed-cards {:discard 2}
                       :coins          1}]}))
    (is (= (-> {:players [{:hand  [loan copper]
                           :deck  [copper estate]
                           :coins 0}]}
               (play-treasures 0)
               (choose :copper))
           {:players [{:play-area [loan copper]
                       :deck      [estate]
                       :coins     2}]
            :trash   [copper]}))))

(deftest mountebank-test
  (let [curse (assoc curse :id 0)
        copper (assoc copper :id 1)]
    (testing "Mountebank"
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card copper :pile-size 46}]
                  :players [{:hand    [mountebank]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :mountebank))
             {:supply  [{:card curse :pile-size 9}
                        {:card copper :pile-size 45}]
              :players [{:play-area [mountebank]
                         :actions   0
                         :coins     2}
                        {:discard [curse copper]}]}))
      (is (= (-> {:players [{:hand    [mountebank]
                             :actions 1
                             :coins   0}
                            {:hand [curse]}]}
                 (play 0 :mountebank))
             {:players      [{:play-area [mountebank]
                              :actions   0
                              :coins     2}
                             {:hand [curse]}]
              :effect-stack [{:text      "You may discard a Curse."
                              :player-no 1
                              :choice    ::prosperity/mountebank-discard-curse
                              :source    :hand
                              :options   [:curse]
                              :max       1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand    [mountebank]
                             :actions 1
                             :coins   0}
                            {:hand [curse]}]}
                 (play 0 :mountebank)
                 (choose :curse))
             {:players [{:play-area [mountebank]
                         :actions   0
                         :coins     2}
                        {:discard [curse]}]})))
    (is (= (-> {:supply  [{:card curse :pile-size 10}
                          {:card copper :pile-size 46}]
                :players [{:hand    [mountebank]
                           :actions 1
                           :coins   0}
                          {:hand [curse]}]}
               (play 0 :mountebank)
               (choose nil))
           {:supply  [{:card curse :pile-size 9}
                      {:card copper :pile-size 45}]
            :players [{:play-area [mountebank]
                       :actions   0
                       :coins     2}
                      {:hand    [curse]
                       :discard [curse copper]}]}))))

(deftest vault-test
  (testing "Vault"
    (is (= (-> {:players [{:hand    [vault copper copper estate estate]
                           :deck    [copper estate silver]
                           :actions 1}]}
               (play 0 :vault))
           {:players      [{:hand      [copper copper estate estate copper estate]
                            :play-area [vault]
                            :deck      [silver]
                            :actions   0}]
            :effect-stack [{:text      "Discard any number of cards for +$1 each."
                            :player-no 0
                            :choice    ::prosperity/vault-discard
                            :source    :hand
                            :options   [:copper :copper :estate :estate :copper :estate]}
                           {:player-no 0
                            :effect    [:other-players {:effects [[:give-choice {:text      "You may discard 2 cards, to draw a card."
                                                                                 :choice    ::prosperity/vault-discard-2
                                                                                 :options   [:player :hand]
                                                                                 :min       2
                                                                                 :max       2
                                                                                 :optional? true}]]}]}]}))
    (is (= (-> {:players [{:hand    [vault copper copper estate estate]
                           :deck    [copper estate silver]
                           :actions 1
                           :coins   0}]}
               (play 0 :vault)
               (choose [:estate :estate :estate]))
           {:players [{:hand      [copper copper copper]
                       :play-area [vault]
                       :deck      [silver]
                       :discard   [estate estate estate]
                       :actions   0
                       :coins     3}]})))
  (is (= (-> {:players [{:hand    [vault]
                         :deck    [silver silver]
                         :actions 1
                         :coins   0}
                        {:hand [estate estate copper]
                         :deck [copper copper]}]}
             (play 0 :vault)
             (choose nil))
         {:players      [{:hand      [silver silver]
                          :play-area [vault]
                          :actions   0
                          :coins     0}
                         {:hand [estate estate copper]
                          :deck [copper copper]}]
          :effect-stack [{:text      "You may discard 2 cards, to draw a card."
                          :player-no 1
                          :choice    ::prosperity/vault-discard-2
                          :source    :hand
                          :options   [:estate :estate :copper]
                          :min       2
                          :max       2
                          :optional? true}]}))
  (is (= (-> {:players [{:hand    [vault]
                         :deck    [silver silver]
                         :actions 1
                         :coins   0}
                        {:hand [estate estate copper]
                         :deck [copper copper]}]}
             (play 0 :vault)
             (choose nil)
             (choose nil))
         {:players [{:hand      [silver silver]
                     :play-area [vault]
                     :actions   0
                     :coins     0}
                    {:hand [estate estate copper]
                     :deck [copper copper]}]}))
  (is (= (-> {:players [{:hand    [vault]
                         :deck    [silver silver]
                         :actions 1
                         :coins   0}
                        {:hand [estate estate copper]
                         :deck [copper copper]}]}
             (play 0 :vault)
             (choose nil)
             (choose [:estate :estate]))
         {:players [{:hand      [silver silver]
                     :play-area [vault]
                     :actions   0
                     :coins     0}
                    {:hand    [copper copper]
                     :deck    [copper]
                     :discard [estate estate]}]}))
  (is (= (-> {:players [{:hand    [vault]
                         :deck    [silver silver]
                         :actions 1
                         :coins   0}
                        {:hand [estate]
                         :deck [copper copper]}]}
             (play 0 :vault)
             (choose nil)
             (choose [:estate]))
         {:players [{:hand      [silver silver]
                     :play-area [vault]
                     :actions   0
                     :coins     0}
                    {:deck    [copper copper]
                     :discard [estate]}]})))

(deftest venture-test
  (testing "Venture"
    (is (= (-> {:players [{:hand  [venture]
                           :deck  [copper estate]
                           :coins 0}]}
               (play 0 :venture))
           {:players [{:play-area      [venture copper]
                       :deck           [estate]
                       :revealed-cards {:play-area 1}
                       :coins          2}]}))
    (is (= (-> {:players [{:hand  [venture]
                           :deck  [estate copper estate]
                           :coins 0}]}
               (play 0 :venture))
           {:players [{:play-area      [venture copper]
                       :deck           [estate]
                       :discard        [estate]
                       :revealed-cards {:play-area 1
                                        :discard   1}
                       :coins          2}]}))
    (is (= (-> {:players [{:hand    [venture]
                           :deck    [estate]
                           :discard [duchy]
                           :coins   0}]}
               (play 0 :venture))
           {:players [{:play-area      [venture]
                       :discard        [estate duchy]
                       :revealed-cards {:discard 2}
                       :coins          1}]}))
    (is (= (-> {:players [{:hand  [venture copper]
                           :deck  [copper estate]
                           :coins 0}]}
               (play-treasures 0))
           {:players [{:play-area      [copper venture copper]
                       :deck           [estate]
                       :revealed-cards {:play-area 1}
                       :coins          3}]}))
    (is (= (-> {:players [{:hand    [venture]
                           :deck    [estate loan duchy]
                           :discard (repeat 7 copper)
                           :coins   0}]}
               (play 0 :venture)
               (choose :copper))
           {:players [{:play-area      [venture loan]
                       :deck           (repeat 6 copper)
                       :discard        [estate duchy]
                       :revealed-cards {:play-area 1
                                        :discard   2}
                       :coins          2}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [venture loan]
                           :deck    [copper gold]
                           :coins   0}]}
               (play-treasures 0)
               (choose :copper))
           {:players [{:play-area      [loan venture gold]
                       :revealed-cards {:play-area 1}
                       :coins          5}]
            :trash   [copper]}))))

(deftest workers-village-test
  (testing "Worker's Village"
    (is (= (-> {:players [{:hand    [workers-village]
                           :deck    [estate estate]
                           :actions 1
                           :buys    1}]}
               (play 0 :worker's-village))
           {:players [{:hand      [estate]
                       :play-area [workers-village]
                       :deck      [estate]
                       :actions   2
                       :buys      2}]}))))