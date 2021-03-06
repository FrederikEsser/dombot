(ns dombot.cards.seaside-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :refer [moat throne-room witch]]
            [dombot.cards.seaside :as seaside :refer :all]
            [dombot.cards.renaissance :refer [improve]]
            [dombot.utils :as ut]))

(deftest ambassador-test
  (let [copper (assoc copper :id 1)
        estate (assoc estate :id 2)]
    (testing "Ambassador"
      (is (= (-> {:players [{:hand    [ambassador copper copper copper estate]
                             :actions 1}]}
                 (play 0 :ambassador))
             {:players      [{:hand      [copper copper copper estate]
                              :play-area [ambassador]
                              :actions   0}]
              :effect-stack [{:text      "Reveal a card from your hand."
                              :player-no 0
                              :choice    ::seaside/ambassador-reveal
                              :source    :hand
                              :options   [:copper :copper :copper :estate]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [ambassador copper copper copper estate]
                             :actions 1}]}
                 (play 0 :ambassador)
                 (choose :copper))
             {:players      [{:hand           [copper copper estate copper]
                              :play-area      [ambassador]
                              :revealed-cards {:hand 1}
                              :actions        0}]
              :effect-stack [{:text      "Return up to 2 copies of it to the Supply."
                              :player-no 0
                              :choice    :return-to-supply
                              :source    :hand
                              :options   [:copper :copper :copper]
                              :max       2}
                             {:player-no 0
                              :effect    [:attack {:effects [[:gain {:card-name :copper}]]}]}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [ambassador copper copper copper estate]
                             :actions 1}
                            {}]}
                 (play 0 :ambassador)
                 (choose :copper)
                 (choose [:copper :copper]))
             {:supply  [{:card copper :pile-size 47}]
              :players [{:hand      [estate copper]
                         :play-area [ambassador]
                         :actions   0}
                        {:discard [copper]}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [ambassador copper copper copper estate]
                             :actions 1}
                            {}]}
                 (play 0 :ambassador)
                 (choose :copper)
                 (choose :copper))
             {:supply  [{:card copper :pile-size 46}]
              :players [{:hand      [copper estate copper]
                         :play-area [ambassador]
                         :actions   0}
                        {:discard [copper]}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [ambassador copper copper copper estate]
                             :actions 1}
                            {}]}
                 (play 0 :ambassador)
                 (choose :copper)
                 (choose nil))
             {:supply  [{:card copper :pile-size 45}]
              :players [{:hand           [copper copper estate copper]
                         :play-area      [ambassador]
                         :revealed-cards {:hand 1}
                         :actions        0}
                        {:discard [copper]}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [ambassador copper estate]
                             :actions 1}
                            {}]}
                 (play 0 :ambassador)
                 (choose :copper)
                 (choose nil))
             {:supply  [{:card copper :pile-size 45}]
              :players [{:hand           [estate copper]
                         :play-area      [ambassador]
                         :revealed-cards {:hand 1}
                         :actions        0}
                        {:discard [copper]}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [ambassador copper copper copper estate]
                             :actions 1}
                            {}]}
                 (play 0 :ambassador)
                 (choose :estate)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 8}]
              :players [{:hand      [copper copper copper]
                         :play-area [ambassador]
                         :actions   0}
                        {:discard [estate]}]})))))

(deftest bazaar-test
  (testing "Bazaar"
    (is (= (-> {:players [{:deck    [copper copper copper]
                           :hand    [bazaar]
                           :actions 1
                           :coins   0}]}
               (play 0 :bazaar))
           {:players [{:deck      [copper copper]
                       :hand      [copper]
                       :play-area [bazaar]
                       :actions   2
                       :coins     1}]}))))

(deftest caravan-test
  (let [caravan-1   (assoc caravan :id 1)
        caravan-2   (assoc caravan :id 2)
        throne-room (assoc throne-room :id 3)]
    (testing "Caravan"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [caravan-1 estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver]
                             :actions 1}]}
                 (play 0 :caravan))
             {:players [{:hand      [estate estate estate copper copper]
                         :play-area [caravan-1]
                         :deck      [copper copper copper copper copper silver]
                         :actions   1
                         :triggers  [(get-trigger caravan-1)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [caravan-1 estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver]
                             :actions 1}]}
                 (play 0 :caravan)
                 (clean-up {:player-no 0}))
             {:players [{:hand      [copper copper copper copper copper]
                         :play-area [caravan-1]
                         :deck      [silver]
                         :discard   [estate estate estate copper copper]
                         :actions   0
                         :coins     0
                         :buys      0
                         :triggers  [(get-trigger caravan-1)]}]}))
      (is (= (-> {:players [{:hand    [caravan-1 estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :caravan)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper silver]
                                :play-area [caravan-1]
                                :discard   [estate estate estate copper copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [caravan-1 caravan-2 copper copper copper]
                             :deck    [copper copper estate estate copper copper silver]
                             :actions 1}]}
                 (play 0 :caravan)
                 (play 0 :caravan)
                 (clean-up {:player-no 0}))
             {:players [{:hand      [estate estate copper copper silver]
                         :play-area [caravan-1 caravan-2]
                         :discard   [copper copper copper copper copper]
                         :actions   0
                         :coins     0
                         :buys      0
                         :triggers  [(get-trigger caravan-1)
                                     (get-trigger caravan-2 2)]}]}))
      (is (= (-> {:players [{:hand    [caravan-1 caravan-2 copper copper copper]
                             :deck    [copper copper estate estate copper copper silver]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :caravan)
                 (play 0 :caravan)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [estate estate copper copper silver copper copper]
                                :play-area [caravan-1 caravan-2]
                                :deck      [copper copper copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [caravan-1 throne-room copper copper copper]
                             :deck    [copper copper estate estate copper copper silver]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :caravan)
                 (clean-up {:player-no 0}))
             {:players [{:hand          [estate estate copper copper silver]
                         :play-area     [throne-room caravan-1]
                         :discard       [copper copper copper copper copper]
                         :actions       0
                         :coins         0
                         :buys          0
                         :triggers      [(get-trigger caravan-1)
                                         (get-trigger caravan-1 2)]
                         :repeated-play [{:source 3 :target 1}]}]}))
      (is (= (-> {:players [{:hand    [caravan-1 throne-room copper copper copper]
                             :deck    [copper copper estate estate copper copper silver]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :throne-room)
                 (choose :caravan)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [estate estate copper copper silver copper copper]
                                :play-area [throne-room caravan-1]
                                :deck      [copper copper copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]})))))

(deftest cutpurse-test
  (testing "Cutpurse"
    (is (= (-> {:players [{:hand    [cutpurse]
                           :actions 1
                           :coins   0}
                          {:hand [copper copper estate estate copper]}]}
               (play 0 :cutpurse))
           {:players      [{:play-area [cutpurse]
                            :actions   0
                            :coins     2}
                           {:hand [copper copper estate estate copper]}]
            :effect-stack [{:text      "Discard a Copper."
                            :player-no 1
                            :choice    :discard-from-hand
                            :source    :hand
                            :options   [:copper :copper :copper]
                            :min       1
                            :max       1}
                           {:player-no 1
                            :effect    [:clear-unaffected {:works :once}]}]}))
    (is (= (-> {:players [{:hand    [cutpurse]
                           :actions 1
                           :coins   0}
                          {:hand [copper copper estate estate copper]}]}
               (play 0 :cutpurse)
               (choose :copper))
           {:players [{:play-area [cutpurse]
                       :actions   0
                       :coins     2}
                      {:hand    [copper estate estate copper]
                       :discard [copper]}]}))
    (is (= (-> {:players [{:hand    [cutpurse]
                           :actions 1
                           :coins   0}
                          {:hand [estate estate]}]}
               (play 0 :cutpurse))
           {:players [{:play-area [cutpurse]
                       :actions   0
                       :coins     2}
                      {:hand           [estate estate]
                       :revealed-cards {:hand 2}}]}))
    (is (= (-> {:players [{:hand    [cutpurse]
                           :actions 1
                           :coins   0}
                          {:hand [copper]}]}
               (play 0 :cutpurse)
               (choose :copper))
           {:players [{:play-area [cutpurse]
                       :actions   0
                       :coins     2}
                      {:discard [copper]}]}))))

(deftest embargo-test
  (let [embargo (assoc embargo :id 1)
        silver  (assoc silver :id 2)
        curse   (assoc curse :id 3)]
    (testing "Embargo"
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [embargo]
                             :actions 1
                             :coins   0}]}
                 (play 0 :embargo))
             {:supply       (base/supply 2 8)
              :players      [{:actions 0
                              :coins   2}]
              :trash        [embargo]
              :effect-stack [{:text      "Add an Embargo token to a Supply pile."
                              :player-no 0
                              :card-id   1
                              :choice    ::seaside/embargo-place-token
                              :source    :supply
                              :options   [:curse :estate :duchy :province :copper :silver :gold]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [embargo]
                             :actions 1
                             :coins   0}]}
                 (play 0 :embargo)
                 (choose :silver))
             {:supply  [{:card   silver :pile-size 40
                         :tokens {:embargo {:number-of-tokens 1
                                            :on-buy           [[:gain {:card-name :curse}]]}}}]
              :players [{:actions 0
                         :coins   2}]
              :trash   [embargo]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card   silver :pile-size 40
                             :tokens {:embargo {:number-of-tokens 1
                                                :on-buy           [[:gain {:card-name :curse}]]}}}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-card 0 :silver))
             {:supply  [{:card curse :pile-size 9}
                        {:card   silver :pile-size 39
                         :tokens {:embargo {:number-of-tokens 1
                                            :on-buy           [[:gain {:card-name :curse}]]}}}]
              :players [{:coins   0
                         :buys    0
                         :discard [curse silver]}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [throne-room embargo]
                             :actions 1
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :embargo)
                 (choose :silver)
                 (choose :silver))
             {:supply  [{:card   silver :pile-size 40
                         :tokens {:embargo {:number-of-tokens 2
                                            :on-buy           [[:gain {:card-name :curse}]]}}}]
              :players [{:play-area [throne-room]
                         :actions   0
                         :coins     4}]
              :trash   [embargo]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card   silver :pile-size 40
                             :tokens {:embargo {:number-of-tokens 2
                                                :on-buy           [[:gain {:card-name :curse}]]}}}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-card 0 :silver))
             {:supply  [{:card curse :pile-size 8}
                        {:card   silver :pile-size 39
                         :tokens {:embargo {:number-of-tokens 2
                                            :on-buy           [[:gain {:card-name :curse}]]}}}]
              :players [{:coins   0
                         :buys    0
                         :discard [curse curse silver]}]})))))

(deftest explorer-test
  (let [silver (assoc silver :id 1)
        gold   (assoc gold :id 2)]
    (testing "Explorer"
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [explorer]
                             :actions 1}]}
                 (play 0 :explorer))
             {:supply  [{:card silver :pile-size 39}
                        {:card gold :pile-size 30}]
              :players [{:hand      [silver]
                         :play-area [explorer]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [explorer province]
                             :actions 1}]}
                 (play 0 :explorer))
             {:supply       [{:card silver :pile-size 40}
                             {:card gold :pile-size 30}]
              :players      [{:hand      [province]
                              :play-area [explorer]
                              :actions   0}]
              :effect-stack [{:text      "You may reveal a Province from your hand."
                              :player-no 0
                              :choice    ::seaside/explorer-choice
                              :source    :hand
                              :options   [:province]
                              :max       1}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [explorer province]
                             :actions 1}]}
                 (play 0 :explorer)
                 (choose :province))
             {:supply  [{:card silver :pile-size 40}
                        {:card gold :pile-size 29}]
              :players [{:hand           [gold province]
                         :play-area      [explorer]
                         :revealed-cards {:hand 1}
                         :actions        0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [explorer province]
                             :actions 1}]}
                 (play 0 :explorer)
                 (choose nil))
             {:supply  [{:card silver :pile-size 39}
                        {:card gold :pile-size 30}]
              :players [{:hand      [province silver]
                         :play-area [explorer]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card gold :pile-size 0}]
                  :players [{:hand    [explorer province]
                             :actions 1}]}
                 (play 0 :explorer)
                 (choose :province))
             {:supply  [{:card silver :pile-size 40}
                        {:card gold :pile-size 0}]
              :players [{:hand           [province]
                         :play-area      [explorer]
                         :actions        0
                         :revealed-cards {:hand 1}}]})))))

(deftest fishing-village-test
  (let [fishing-village (assoc fishing-village :id 1)]
    (testing "Fishing Village"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [fishing-village]
                             :actions 1
                             :coins   0}]}
                 (play 0 :fishing-village))
             {:players [{:play-area [fishing-village]
                         :actions   2
                         :coins     1
                         :triggers  [(get-trigger fishing-village)]}]}))
      (is (= (-> {:players [{:hand    [fishing-village]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :fishing-village)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [fishing-village]
                                :actions   2
                                :coins     1
                                :buys      1
                                :phase     :action}]})))))

(deftest ghost-ship-test
  (testing "Ghost Ship"
    (is (= (play {:players [{:hand    [ghost-ship]
                             :deck    [copper copper copper]
                             :actions 1}
                            {:hand (repeat 5 copper)}]}
                 0 :ghost-ship)
           {:players      [{:hand      [copper copper]
                            :play-area [ghost-ship]
                            :deck      [copper]
                            :actions   0}
                           {:hand (repeat 5 copper)}]
            :effect-stack [{:text      "Put cards from your hand onto your deck until you have 3 cards in hand."
                            :player-no 1
                            :choice    :topdeck-from-hand
                            :source    :hand
                            :options   (repeat 5 :copper)
                            :min       2
                            :max       2}
                           {:player-no 1
                            :effect    [:clear-unaffected {:works :once}]}]}))
    (is (= (-> {:players [{:hand    [ghost-ship]
                           :deck    [copper copper copper]
                           :actions 1}
                          {:hand (repeat 5 copper)
                           :deck [estate]}]}
               (play 0 :ghost-ship)
               (choose [:copper :copper]))
           {:players [{:hand      [copper copper]
                       :play-area [ghost-ship]
                       :deck      [copper]
                       :actions   0}
                      {:hand (repeat 3 copper)
                       :deck [copper copper estate]}]}))
    (is (= (-> {:players [{:hand    [ghost-ship]
                           :deck    [copper copper copper]
                           :actions 1}
                          {:hand (repeat 4 copper)
                           :deck [estate]}]}
               (play 0 :ghost-ship)
               (choose :copper))
           {:players [{:hand      [copper copper]
                       :play-area [ghost-ship]
                       :deck      [copper]
                       :actions   0}
                      {:hand (repeat 3 copper)
                       :deck [copper estate]}]}))
    (is (= (-> {:players [{:hand    [ghost-ship]
                           :deck    [copper copper copper]
                           :actions 1}
                          {:hand (repeat 6 copper)
                           :deck [estate]}]}
               (play 0 :ghost-ship)
               (choose [:copper :copper :copper]))
           {:players [{:hand      [copper copper]
                       :play-area [ghost-ship]
                       :deck      [copper]
                       :actions   0}
                      {:hand (repeat 3 copper)
                       :deck [copper copper copper estate]}]}))))

(deftest haven-test
  (let [haven       (assoc haven :id 1)
        throne-room (assoc throne-room :id 2)]
    (testing "Haven"
      (is (= (-> {:players [{:hand    [haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :haven))
             {:players      [{:hand      [estate copper]
                              :play-area [haven]
                              :deck      [copper]
                              :actions   1}]
              :effect-stack [{:text      "Set aside a card from your hand."
                              :player-no 0
                              :card-id   1
                              :choice    ::seaside/haven-set-aside
                              :source    :hand
                              :options   [:estate :copper]
                              :min       1
                              :max       1}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :haven)
                 (choose :copper))
             {:players [{:hand      [estate]
                         :play-area [haven]
                         :deck      [copper]
                         :actions   1
                         :triggers  [(merge set-aside=>hand-trigger
                                            {:id        1
                                             :card-id   1
                                             :name      :haven
                                             :set-aside [copper]})]}]}))
      (is (= (-> {:players [{:hand    [haven estate]
                             :deck    [copper copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :haven)
                 (choose :copper)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper estate copper]
                                :play-area [haven]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:players [{:hand    [haven]
                             :actions 1}]}
                 (play 0 :haven))
             {:players [{:play-area [haven]
                         :actions   1}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [throne-room haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :haven)
                 (choose :copper)
                 (choose :estate))
             {:players [{:hand          [copper]
                         :play-area     [throne-room haven]
                         :actions       2
                         :triggers      [(merge set-aside=>hand-trigger
                                                {:id        1
                                                 :card-id   1
                                                 :name      :haven
                                                 :set-aside [copper]})
                                         (merge set-aside=>hand-trigger
                                                {:id        2
                                                 :card-id   1
                                                 :name      :haven
                                                 :set-aside [estate]})]
                         :repeated-play [{:source 2
                                          :target 1}]}]}))
      (is (= (-> {:players [{:hand    [throne-room haven estate]
                             :deck    [copper copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :throne-room)
                 (choose :haven)
                 (choose :copper)
                 (choose :estate)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper estate]
                                :play-area [throne-room haven]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]})))))

(deftest island-test
  (let [island (assoc island :id 1)]
    (testing "Island"
      (is (= (-> {:players [{:hand    [island estate copper]
                             :actions 1}]}
                 (play 0 :island))
             {:players      [{:hand       [estate copper]
                              :island-mat [island]
                              :actions    0}]
              :effect-stack [{:text      "Put a card from your hand on the Island Mat."
                              :player-no 0
                              :card-id   1
                              :choice    ::seaside/island-put
                              :source    :hand
                              :options   [:estate :copper]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [island estate copper]
                             :actions 1}]}
                 (play 0 :island)
                 (choose :estate))
             {:players [{:hand       [copper]
                         :island-mat [island estate]
                         :actions    0}]}))
      (is (= (-> {:players [{:hand    [island]
                             :actions 1}]}
                 (play 0 :island))
             {:players [{:island-mat [island]
                         :actions    0}]}))
      (is (= (-> {:players [{:hand    [throne-room island estate copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :island)
                 (choose :estate)
                 (choose :copper))
             {:players [{:play-area  [throne-room]
                         :island-mat [island estate copper]
                         :actions    0}]}))
      (is (= (calc-victory-points {:deck [island]}) 2)))))

(deftest lighthouse-test
  (let [lighthouse-1 (assoc lighthouse :id 1)
        lighthouse-2 (assoc lighthouse :id 2)]
    (testing "Lighthouse"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [lighthouse-1]
                             :actions 1
                             :coins   0}]}
                 (play 0 :lighthouse))
             {:players [{:play-area  [lighthouse-1]
                         :actions    1
                         :coins      1
                         :unaffected [{:card-id 1}]
                         :triggers   [(get-trigger lighthouse-1)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [lighthouse-1]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :lighthouse)
                 (end-turn 0))
             {:current-player 1
              :players        [{:play-area  [lighthouse-1]
                                :actions    0
                                :coins      0
                                :buys       0
                                :unaffected [{:card-id 1}]
                                :triggers   [(get-trigger lighthouse-1)]}
                               {:actions 1
                                :coins   0
                                :buys    1}]}))
      (let [curse (assoc curse :id 1)]
        (is (= (-> {:supply  [{:card curse :pile-size 10}]
                    :players [{:hand    [witch]
                               :actions 1}
                              {:play-area  [lighthouse-1]
                               :unaffected [{:card-id 1}]
                               :triggers   [(get-trigger lighthouse-1)]}]}
                   (play 0 :witch))
               {:supply  [{:card curse :pile-size 10}]
                :players [{:play-area [witch]
                           :actions   0}
                          {:play-area  [lighthouse-1]
                           :unaffected [{:card-id 1}]
                           :triggers   [(get-trigger lighthouse-1)]}]}))
        (ut/reset-ids!)
        (is (= (-> {:supply  [{:card curse :pile-size 20}]
                    :players [{:hand    [lighthouse-1]
                               :deck    [moat]
                               :actions 1
                               :coins   0}
                              {:hand [witch]}
                              {:hand [witch]}]}
                   (play 0 :lighthouse)
                   (end-turn 0)
                   (play 1 :witch)
                   (end-turn 1)
                   (play 2 :witch))
               {:current-player 2
                :supply         [{:card curse :pile-size 18}]
                :players        [{:hand       [moat]
                                  :play-area  [lighthouse-1]
                                  :actions    0
                                  :coins      0
                                  :buys       0
                                  :unaffected [{:card-id 1}]
                                  :triggers   [(get-trigger lighthouse-1)]}
                                 {:hand    [witch]
                                  :discard [curse]
                                  :actions 0
                                  :coins   0
                                  :buys    0}
                                 {:hand      [curse]
                                  :play-area [witch]
                                  :actions   0
                                  :coins     0
                                  :buys      1}]})))
      (is (= (-> {:players [{:hand    [lighthouse-1]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :lighthouse)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [lighthouse-1]
                                :actions   1
                                :coins     1
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:players [{:hand    [lighthouse-1 lighthouse-2]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :lighthouse)
                 (play 0 :lighthouse)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [lighthouse-1 lighthouse-2]
                                :actions   1
                                :coins     2
                                :buys      1
                                :phase     :action}]})))))

(deftest lookout-test
  (testing "Lookout"
    (is (= (-> {:players [{:hand    [lookout]
                           :deck    [estate gold province copper]
                           :actions 1}]}
               (play 0 :lookout))
           {:players      [{:play-area [lookout]
                            :look-at   [estate gold province]
                            :deck      [copper]
                            :actions   1}]
            :effect-stack [{:text      "Trash one of the top 3 cards of your deck."
                            :player-no 0
                            :choice    :trash-from-look-at
                            :source    :look-at
                            :options   [:estate :gold :province]
                            :min       1
                            :max       1}
                           {:player-no 0
                            :effect    [:give-choice {:text    "Discard one of the top 3 cards of your deck."
                                                      :choice  :discard-from-look-at
                                                      :options [:player :look-at]
                                                      :min     1
                                                      :max     1}]}
                           {:player-no 0
                            :effect    [:move-card {:from          :look-at
                                                    :from-position :top
                                                    :to            :deck
                                                    :to-position   :top}]}]}))
    (is (= (-> {:players [{:hand    [lookout]
                           :deck    [estate gold province copper]
                           :actions 1}]}
               (play 0 :lookout)
               (choose :estate))
           {:players      [{:play-area [lookout]
                            :look-at   [gold province]
                            :deck      [copper]
                            :actions   1}]
            :effect-stack [{:text      "Discard one of the top 3 cards of your deck."
                            :player-no 0
                            :choice    :discard-from-look-at
                            :source    :look-at
                            :options   [:gold :province]
                            :min       1
                            :max       1}
                           {:player-no 0
                            :effect    [:move-card {:from          :look-at
                                                    :from-position :top
                                                    :to            :deck
                                                    :to-position   :top}]}]
            :trash        [estate]}))
    (is (= (-> {:players [{:hand    [lookout]
                           :deck    [estate gold province copper]
                           :actions 1}]}
               (play 0 :lookout)
               (choose :estate)
               (choose :province))
           {:players [{:play-area [lookout]
                       :deck      [gold copper]
                       :discard   [province]
                       :actions   1}]
            :trash   [estate]}))))

(deftest merchant-ship-test
  (let [merchant-ship (assoc merchant-ship :id 1)]
    (testing "Merchant Ship"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [merchant-ship]
                             :actions 1
                             :coins   0}]}
                 (play 0 :merchant-ship))
             {:players [{:play-area [merchant-ship]
                         :actions   0
                         :coins     2
                         :triggers  [(get-trigger merchant-ship)]}]}))
      (is (= (-> {:players [{:hand    [merchant-ship]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :merchant-ship)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [merchant-ship]
                                :actions   1
                                :coins     2
                                :buys      1
                                :phase     :action}]})))))

(deftest native-village-test
  (testing "Native Village"
    (is (= (-> {:players [{:hand    [native-village]
                           :actions 1}]}
               (play 0 :native-village))
           {:players      [{:play-area [native-village]
                            :actions   2}]
            :effect-stack [{:text      "Choose one:"
                            :player-no 0
                            :choice    ::seaside/native-village-choice
                            :source    :special
                            :options   [{:option :put :text "Put the top card of your deck face down on your Native Village mat."}
                                        {:option :take :text "Put all the cards from your mat into your hand."}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [native-village]
                           :deck    [copper copper]
                           :actions 1}]}
               (play 0 :native-village)
               (choose :put))
           {:players [{:play-area          [native-village]
                       :deck               [copper]
                       :native-village-mat [copper]
                       :actions            2}]}))
    (is (= (-> {:players [{:hand               [native-village]
                           :deck               [copper copper]
                           :native-village-mat [estate]
                           :actions            1}]}
               (play 0 :native-village)
               (choose :put))
           {:players [{:play-area          [native-village]
                       :deck               [copper]
                       :native-village-mat [estate copper]
                       :actions            2}]}))
    (is (= (-> {:players [{:hand               [native-village]
                           :native-village-mat [copper copper]
                           :actions            1}]}
               (play 0 :native-village)
               (choose :take))
           {:players [{:hand      [copper copper]
                       :play-area [native-village]
                       :actions   2}]}))
    (is (= (-> {:players [{:hand               [native-village]
                           :native-village-mat [copper copper]
                           :actions            1}]}
               (play 0 :native-village)
               (choose :put))
           {:players [{:play-area          [native-village]
                       :native-village-mat [copper copper]
                       :actions            2}]}))
    (is (= (-> {:players [{:hand    [native-village]
                           :actions 1}]}
               (play 0 :native-village)
               (choose :take))
           {:players [{:play-area [native-village]
                       :actions   2}]}))))

(deftest navigator-test
  (testing "Navigator"
    (is (= (-> {:players [{:hand    [navigator]
                           :deck    [copper copper estate estate silver estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :navigator))
           {:players      [{:play-area [navigator]
                            :deck      [estate]
                            :look-at   [copper copper estate estate silver]
                            :actions   0
                            :coins     2}]
            :effect-stack [{:text      "Look at the top 5 cards of your deck."
                            :player-no 0
                            :choice    ::seaside/navigator-choice
                            :source    :special
                            :options   [{:option :discard :text "Discard them all."}
                                        {:option :topdeck-same-order :text "Put them back on your deck in the same order."}
                                        {:option :topdeck :text "Put them back on your deck in any order."}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [navigator]
                           :deck    [copper copper estate estate silver estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :navigator)
               (choose :discard))
           {:players [{:play-area [navigator]
                       :deck      [estate]
                       :discard   [copper copper estate estate silver]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [navigator]
                           :deck    [copper copper estate estate silver estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :navigator)
               (choose :topdeck-same-order))
           {:players [{:play-area [navigator]
                       :deck      [copper copper estate estate silver estate]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [navigator]
                           :deck    [copper copper estate estate silver estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :navigator)
               (choose :topdeck))
           {:players      [{:play-area [navigator]
                            :deck      [estate]
                            :look-at   [copper copper estate estate silver]
                            :actions   0
                            :coins     2}]
            :effect-stack [{:text      "Put them back on your deck in any order."
                            :player-no 0
                            :choice    :topdeck-from-look-at
                            :source    :look-at
                            :options   [:copper :copper :estate :estate :silver]
                            :min       5
                            :max       5}]}))
    (is (= (-> {:players [{:hand    [navigator]
                           :deck    [copper copper estate estate silver estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :navigator)
               (choose :topdeck)
               (choose [:estate :estate :copper :copper :silver]))
           {:players [{:play-area [navigator]
                       :deck      [silver copper copper estate estate estate]
                       :actions   0
                       :coins     2}]}))))

(deftest outpost-test
  (let [outpost (assoc outpost :id 1)]
    (testing "Outpost"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [outpost]
                             :actions 1}]}
                 (play 0 :outpost))
             {:players [{:play-area                [outpost]
                         :actions                  0
                         :previous-turn-was-yours? true
                         :triggers                 [(merge outpost-trigger
                                                           {:id      1
                                                            :card-id 1
                                                            :name    :outpost})]}]}))
      (is (= (-> {:players [{:hand            [outpost]
                             :deck            (repeat 5 copper)
                             :actions         1
                             :phase           :action
                             :number-of-turns 1}
                            {}]}
                 (play 0 :outpost)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand                     [copper copper copper]
                                :play-area                [outpost]
                                :deck                     [copper copper]
                                :actions                  1
                                :coins                    0
                                :buys                     1
                                :phase                    :action
                                :previous-turn-was-yours? true
                                :number-of-turns          1}
                               {}]}))
      (is (= (-> {:players [{:hand                     [outpost]
                             :deck                     (repeat 5 copper)
                             :actions                  1
                             :phase                    :action
                             :previous-turn-was-yours? true
                             :number-of-turns          1}
                            {}]}
                 (play 0 :outpost)
                 (end-turn 0))
             {:current-player 1
              :players        [{:hand            [copper copper copper copper copper]
                                :discard         [outpost]
                                :actions         0
                                :coins           0
                                :buys            0
                                :phase           :out-of-turn
                                :number-of-turns 2}
                               {:actions 1
                                :buys    1
                                :coins   0}]})))))

(deftest pearl-diver-test
  (testing "Pearl Diver"
    (is (= (-> {:players [{:hand    [pearl-diver]
                           :deck    [copper estate silver]
                           :actions 1}]}
               (play 0 :pearl-diver))
           {:players      [{:hand      [copper]
                            :play-area [pearl-diver]
                            :deck      [estate]
                            :look-at   [silver]
                            :actions   1}]
            :effect-stack [{:text      "Choose where to put the Silver:"
                            :player-no 0
                            :choice    ::seaside/pearl-diver-choice
                            :source    :special
                            :options   [{:option :top :text "Put it on top of your deck."}
                                        {:option :bottom :text "Leave it at the bottom of your deck."}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [pearl-diver]
                           :deck    [copper estate silver]
                           :actions 1}]}
               (play 0 :pearl-diver)
               (choose :top))
           {:players [{:hand      [copper]
                       :play-area [pearl-diver]
                       :deck      [silver estate]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [pearl-diver]
                           :deck    [copper estate silver]
                           :actions 1}]}
               (play 0 :pearl-diver)
               (choose :bottom))
           {:players [{:hand      [copper]
                       :play-area [pearl-diver]
                       :deck      [estate silver]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [pearl-diver]
                           :deck    [copper]
                           :actions 1}]}
               (play 0 :pearl-diver))
           {:players [{:hand      [copper]
                       :play-area [pearl-diver]
                       :actions   1}]}))))

(deftest pirate-ship-test
  (testing "Pirate Ship"
    (is (= (-> {:players [{:hand    [pirate-ship]
                           :actions 1}]}
               (play 0 :pirate-ship))
           {:players      [{:play-area [pirate-ship]
                            :actions   0}]
            :effect-stack [{:text      "Choose one:"
                            :player-no 0
                            :choice    ::seaside/pirate-ship-choice
                            :source    :special
                            :options   [{:option :coins :text "+$0"}
                                        {:option :attack :text "Attack other players."}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand              [pirate-ship]
                           :actions           1
                           :pirate-ship-coins 4}]}
               (play 0 :pirate-ship))
           {:players      [{:play-area         [pirate-ship]
                            :actions           0
                            :pirate-ship-coins 4}]
            :effect-stack [{:text      "Choose one:"
                            :player-no 0
                            :choice    ::seaside/pirate-ship-choice
                            :source    :special
                            :options   [{:option :coins :text "+$4"}
                                        {:option :attack :text "Attack other players."}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand              [pirate-ship]
                           :actions           1
                           :coins             0
                           :pirate-ship-coins 4}]}
               (play 0 :pirate-ship)
               (choose :coins))
           {:players [{:play-area         [pirate-ship]
                       :actions           0
                       :coins             4
                       :pirate-ship-coins 4}]}))
    (is (= (-> {:players [{:hand    [pirate-ship]
                           :actions 1}
                          {:deck [estate copper copper]}]}
               (play 0 :pirate-ship)
               (choose :attack))
           {:players      [{:play-area [pirate-ship]
                            :actions   0}
                           {:deck     [copper]
                            :revealed [estate copper]}]
            :effect-stack [{:text                "Trash a revealed Treasure (attacker chooses)."
                            :player-no           1
                            :attacking-player-no 0
                            :attacker            0
                            :choice              ::seaside/pirate-ship-trash
                            :source              :revealed
                            :options             [:copper]
                            :min                 1
                            :max                 1}
                           {:player-no 1
                            :effect    [:discard-all-revealed {:attacking-player-no 0}]}
                           {:player-no 0
                            :effect    [::seaside/pirate-ship-add-coin]}
                           {:player-no 1
                            :effect    [:clear-unaffected {:works :once}]}]}))
    (is (= (-> {:players [{:hand    [pirate-ship]
                           :actions 1}
                          {:deck [estate copper copper]}]}
               (play 0 :pirate-ship)
               (choose :attack)
               (choose :copper))
           {:players [{:play-area         [pirate-ship]
                       :actions           0
                       :pirate-ship-coins 1}
                      {:deck           [copper]
                       :discard        [estate]
                       :revealed-cards {:discard 1}}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand              [pirate-ship]
                           :actions           1
                           :pirate-ship-coins 1}
                          {:deck [estate copper copper]}
                          {:deck [gold copper copper]}]}
               (play 0 :pirate-ship)
               (choose :attack)
               (choose :copper)
               (choose :gold))
           {:players [{:play-area         [pirate-ship]
                       :actions           0
                       :pirate-ship-coins 2}
                      {:deck           [copper]
                       :discard        [estate]
                       :revealed-cards {:discard 1}}
                      {:deck           [copper]
                       :discard        [copper]
                       :revealed-cards {:discard 1}}]
            :trash   [copper gold]}))
    (is (= (-> {:players [{:hand    [pirate-ship]
                           :actions 1}
                          {:deck [estate estate copper]}]}
               (play 0 :pirate-ship)
               (choose :attack))
           {:players [{:play-area [pirate-ship]
                       :actions   0}
                      {:deck           [copper]
                       :discard        [estate estate]
                       :revealed-cards {:discard 2}}]}))))

(deftest salvager-test
  (testing "Salvager"
    (is (= (-> {:players [{:hand    [salvager copper estate]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :salvager))
           {:players      [{:hand      [copper estate]
                            :play-area [salvager]
                            :actions   0
                            :coins     0
                            :buys      2}]
            :effect-stack [{:text      "Trash a card from your hand."
                            :player-no 0
                            :choice    ::seaside/salvager-trash
                            :source    :hand
                            :options   [:copper :estate]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [salvager copper estate]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :salvager)
               (choose :copper))
           {:players [{:hand      [estate]
                       :play-area [salvager]
                       :actions   0
                       :coins     0
                       :buys      2}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [salvager copper estate]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :salvager)
               (choose :estate))
           {:players [{:hand      [copper]
                       :play-area [salvager]
                       :actions   0
                       :coins     2
                       :buys      2}]
            :trash   [estate]}))
    (is (= (-> {:cost-reductions [{:reduction 1}]
                :players         [{:hand    [salvager copper estate]
                                   :actions 1
                                   :coins   0
                                   :buys    1}]}
               (play 0 :salvager)
               (choose :estate))
           {:cost-reductions [{:reduction 1}]
            :players         [{:hand      [copper]
                               :play-area [salvager]
                               :actions   0
                               :coins     1
                               :buys      2}]
            :trash           [estate]}))))

(deftest sea-hag-test
  (let [curse (assoc curse :id 1)]
    (testing "Sea Hag"
      (is (= (-> {:supply  [{:card curse :pile-size 20}]
                  :players [{:hand    [sea-hag]
                             :actions 1}
                            {:deck [copper copper]}
                            {}]}
                 (play 0 :sea-hag))
             {:supply  [{:card curse :pile-size 18}]
              :players [{:play-area [sea-hag]
                         :actions   0}
                        {:deck    [curse copper]
                         :discard [copper]}
                        {:deck [curse]}]}))
      (is (= (play {:supply  [{:card curse :pile-size 0}]
                    :players [{:hand    [sea-hag]
                               :actions 1}
                              {:deck [copper copper]}]}
                   0 :sea-hag)
             {:supply  [{:card curse :pile-size 0}]
              :players [{:play-area [sea-hag]
                         :actions   0}
                        {:deck    [copper]
                         :discard [copper]}]})))))

(deftest smugglers-test
  (let [gold  (assoc gold :id 1)
        curse (assoc curse :id 2)]
    (testing "smugglers"
      (is (= (-> {:track-gained-cards? true
                  :current-player      0
                  :supply              [{:card gold :pile-size 30}]
                  :players             [{:coins 6
                                         :buys  1
                                         :phase :action}
                                        {:hand         [smugglers]
                                         :gained-cards [{:name  :silver
                                                         :types #{:treasure}
                                                         :cost  3}]}]}
                 (gain {:player-no 0 :card-name :gold})
                 (end-turn 0))
             {:track-gained-cards? true
              :current-player      1
              :supply              [{:card gold :pile-size 29}]
              :players             [{:hand         [gold]
                                     :actions      0
                                     :coins        0
                                     :buys         0
                                     :phase        :out-of-turn
                                     :gained-cards [{:name  :gold
                                                     :types #{:treasure}
                                                     :cost  6}]}
                                    {:hand    [smugglers]
                                     :actions 1
                                     :coins   0
                                     :buys    1}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [smugglers]
                             :actions 1}
                            {:gained-cards [{:name  :duchy
                                             :types #{:victory}
                                             :cost  5}
                                            {:name  :gold
                                             :types #{:treasure}
                                             :cost  6}
                                            {:name  :province
                                             :types #{:victory}
                                             :cost  8}]}]}
                 (play 0 :smugglers))
             {:supply       (base/supply 2 8)
              :players      [{:play-area [smugglers]
                              :actions   0}
                             {:gained-cards [{:name  :duchy
                                              :types #{:victory}
                                              :cost  5}
                                             {:name  :gold
                                              :types #{:treasure}
                                              :cost  6}
                                             {:name  :province
                                              :types #{:victory}
                                              :cost  8}]}]
              :effect-stack [{:text      "Gain a card costing up to $6 that the player to the right gained on their last turn."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:duchy :gold]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 29}]
                  :players [{:hand    [smugglers]
                             :actions 1}
                            {:gained-cards [{:name  :gold
                                             :types #{:treasure}
                                             :cost  6}]}]}
                 (play 0 :smugglers)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 28}]
              :players [{:play-area [smugglers]
                         :discard   [gold]
                         :actions   0}
                        {:gained-cards [{:name  :gold
                                         :types #{:treasure}
                                         :cost  6}]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 0}]
                  :players [{:hand    [smugglers]
                             :actions 1}
                            {:gained-cards [{:name  :gold
                                             :types #{:treasure}
                                             :cost  6}]}]}
                 (play 0 :smugglers))
             {:supply       [{:card gold :pile-size 0}]
              :players      [{:play-area [smugglers]
                              :actions   0}
                             {:gained-cards [{:name  :gold
                                              :types #{:treasure}
                                              :cost  6}]}]
              :effect-stack [{:text      "Gain a card costing up to $6 that the player to the right gained on their last turn."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:gold]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 0}]
                  :players [{:hand    [smugglers]
                             :actions 1}
                            {:gained-cards [{:name  :gold
                                             :types #{:treasure}
                                             :cost  6}]}]}
                 (play 0 :smugglers)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 0}]
              :players [{:play-area [smugglers]
                         :actions   0}
                        {:gained-cards [{:name  :gold
                                         :types #{:treasure}
                                         :cost  6}]}]}))
      (is (= (-> {:cost-reductions [{:reduction 2}]
                  :supply          (base/supply 2 8)
                  :players         [{:hand    [smugglers]
                                     :actions 1}
                                    {:gained-cards [{:name  :province
                                                     :types #{:victory}
                                                     :cost  8}]}]}
                 (play 0 :smugglers))
             {:cost-reductions [{:reduction 2}]
              :supply          (base/supply 2 8)
              :players         [{:play-area [smugglers]
                                 :actions   0}
                                {:gained-cards [{:name  :province
                                                 :types #{:victory}
                                                 :cost  8}]}]
              :effect-stack    [{:text      "Gain a card costing up to $6 that the player to the right gained on their last turn."
                                 :player-no 0
                                 :choice    :gain
                                 :source    :supply
                                 :options   [:province]
                                 :min       1
                                 :max       1}]}))
      (is (= (-> {:track-gained-cards? true
                  :current-player      0
                  :supply              [{:card curse :pile-size 10}]
                  :players             [{:hand    [sea-hag smugglers]
                                         :actions 2}
                                        {}]}
                 (play 0 :sea-hag)
                 (play 0 :smugglers))
             {:track-gained-cards? true
              :current-player      0
              :supply              [{:card curse :pile-size 9}]
              :players             [{:play-area [sea-hag smugglers]
                                     :actions   0}
                                    {:deck [curse]}]})))))

(deftest tactician-test
  (let [tactician   (assoc tactician :id 1)
        throne-room (assoc throne-room :id 2)]
    (testing "Tactician"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [tactician estate]
                             :actions 1}]}
                 (play 0 :tactician))
             {:players [{:play-area [tactician]
                         :discard   [estate]
                         :actions   0
                         :triggers  [(merge tactician-trigger
                                            {:id      1
                                             :card-id 1
                                             :name    :tactician})]}]}))
      (is (= (-> {:players [{:hand    [tactician]
                             :actions 1}]}
                 (play 0 :tactician))
             {:players [{:play-area [tactician]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [tactician estate]
                             :deck    (repeat 10 copper)
                             :actions 1
                             :phase   :action}]}
                 (play 0 :tactician)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      (repeat 10 copper)
                                :play-area [tactician]
                                :discard   [estate]
                                :actions   2
                                :coins     0
                                :buys      2
                                :phase     :action}]}))
      (is (= (-> {:players [{:hand    [tactician]
                             :deck    (repeat 10 copper)
                             :actions 1
                             :phase   :action}]}
                 (play 0 :tactician)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand    (repeat 5 copper)
                                :deck    (repeat 5 copper)
                                :discard [tactician]
                                :actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [throne-room tactician estate]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :tactician))
             {:players [{:play-area     [throne-room tactician]
                         :discard       [estate]
                         :actions       0
                         :triggers      [(merge tactician-trigger
                                                {:id      1
                                                 :card-id 1
                                                 :name    :tactician})]
                         :repeated-play [{:source 2
                                          :target 1}]}]}))
      (is (= (-> {:players [{:hand    [throne-room tactician estate]
                             :deck    (repeat 10 copper)
                             :actions 1
                             :phase   :action}]}
                 (play 0 :throne-room)
                 (choose :tactician)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      (repeat 10 copper)
                                :play-area [throne-room tactician]
                                :discard   [estate]
                                :actions   2
                                :coins     0
                                :buys      2
                                :phase     :action}]})))))

(deftest treasure-map-test
  (let [treasure-map (assoc treasure-map :id 1)
        gold         (assoc gold :id 2)]
    (testing "Treasure Map"
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [treasure-map]
                             :actions 1}]}
                 (play 0 :treasure-map))
             {:supply  [{:card gold :pile-size 30}]
              :players [{:actions 0}]
              :trash   [treasure-map]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [treasure-map treasure-map]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :treasure-map))
             {:supply  [{:card gold :pile-size 26}]
              :players [{:deck    [gold gold gold gold copper]
                         :actions 0}]
              :trash   [treasure-map treasure-map]}))
      (is (= (-> {:supply  [{:card gold :pile-size 3}]
                  :players [{:hand    [treasure-map treasure-map]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :treasure-map))
             {:supply  [{:card gold :pile-size 0}]
              :players [{:deck    [gold gold gold copper]
                         :actions 0}]
              :trash   [treasure-map treasure-map]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [throne-room treasure-map treasure-map]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :treasure-map))
             {:supply  [{:card gold :pile-size 26}]
              :players [{:play-area [throne-room]
                         :deck      [gold gold gold gold copper]
                         :actions   0}]
              :trash   [treasure-map treasure-map]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [throne-room treasure-map treasure-map treasure-map]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :treasure-map))
             {:supply  [{:card gold :pile-size 26}]
              :players [{:play-area [throne-room]
                         :deck      [gold gold gold gold copper]
                         :actions   0}]
              :trash   [treasure-map treasure-map treasure-map]})))))

(deftest treasury-test
  (let [treasury-1 (assoc treasury :id 1)
        treasury-2 (assoc treasury :id 2)
        province   (assoc province :id 3)]
    (is (= (-> {:track-gained-cards? true
                :current-player      0
                :supply              [{:card province :pile-size 8}]
                :players             [{:coins 8
                                       :buys  1}]}
               (buy-card 0 :province))
           {:track-gained-cards? true
            :current-player      0
            :supply              [{:card province :pile-size 7}]
            :players             [{:discard      [province]
                                   :coins        0
                                   :buys         0
                                   :gained-cards [{:name   :province
                                                   :types  #{:victory}
                                                   :cost   8
                                                   :bought true}]}]}))
    (testing "Treasury"
      (is (= (-> {:players [{:deck    [copper copper copper]
                             :hand    [treasury-1]
                             :actions 1
                             :coins   0}]}
                 (play 0 :treasury))
             {:players [{:deck      [copper copper]
                         :hand      [copper]
                         :play-area [treasury-1]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:players [{:deck         [copper copper copper]
                             :hand         [treasury-1]
                             :gained-cards [{:types #{:treasure} :bought true}
                                            {:types #{:victory}}]
                             :actions      1
                             :coins        0
                             :phase        :action}]}
                 (play 0 :treasury)
                 (clean-up {:player-no 0}))
             {:players      [{:deck         [copper copper]
                              :hand         [copper]
                              :play-area    [treasury-1]
                              :gained-cards [{:types #{:treasure} :bought true}
                                             {:types #{:victory}}]
                              :actions      1
                              :coins        1
                              :phase        :clean-up}]
              :effect-stack [{:player-no 0
                              :text      "You may activate cards, that do something when you discard them from play."
                              :choice    [:simultaneous-effects-choice {:triggers [{:event     :at-clean-up
                                                                                    :name      :treasury
                                                                                    :card-id   1
                                                                                    :mode      :manual
                                                                                    :optional? true
                                                                                    :effects   (:at-clean-up treasury)}]}]
                              :source    :mixed
                              :options   [{:area :play-area :card-name :treasury}]
                              :max       1}
                             {:player-no 0
                              :effect    [:sync-repeated-play]}
                             {:player-no 0
                              :effect    [:do-clean-up {:player-no 0}]}
                             {:player-no 0
                              :effect    [:draw 5]}
                             {:player-no 0
                              :effect    [:set-phase {:phase :out-of-turn}]}
                             {:player-no 0
                              :effect    [:check-game-ended]}]}))
      (is (= (-> {:players [{:deck         (repeat 7 copper)
                             :hand         [treasury-1]
                             :gained-cards [{:types #{:victory} :bought true}]
                             :actions      1
                             :coins        0
                             :phase        :action}]}
                 (play 0 :treasury)
                 (clean-up {:player-no 0}))
             {:players [{:hand         [copper copper copper copper copper]
                         :deck         [copper]
                         :discard      [copper treasury-1]
                         :gained-cards [{:types #{:victory} :bought true}]
                         :actions      0
                         :coins        0
                         :buys         0
                         :phase        :out-of-turn}]}))
      (is (= (-> {:players [{:deck    (repeat 7 copper)
                             :hand    [treasury-1]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :treasury)
                 (clean-up {:player-no 0})
                 (choose {:area :play-area :card-name :treasury}))
             {:players [{:hand    [treasury-1 copper copper copper copper]
                         :deck    [copper copper]
                         :discard [copper]
                         :actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:players [{:deck    (repeat 7 copper)
                             :hand    [treasury-1]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :treasury)
                 (clean-up {:player-no 0})
                 (choose nil))
             {:players [{:hand    [copper copper copper copper copper]
                         :deck    [copper]
                         :discard [copper treasury-1]
                         :actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:players [{:deck    (repeat 7 copper)
                             :hand    [treasury-1 treasury-2]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :treasury)
                 (play 0 :treasury)
                 (clean-up {:player-no 0})
                 (choose {:area :play-area :card-name :treasury}))
             {:players      [{:deck      (concat [treasury-1] (repeat 5 copper))
                              :hand      [copper copper]
                              :play-area [treasury-2]
                              :actions   1
                              :coins     2
                              :phase     :clean-up}]
              :effect-stack [{:player-no 0
                              :text      "You may activate cards, that do something when you discard them from play."
                              :choice    [:simultaneous-effects-choice {:triggers [{:event     :at-clean-up
                                                                                    :name      :treasury
                                                                                    :card-id   2
                                                                                    :mode      :manual
                                                                                    :optional? true
                                                                                    :effects   (:at-clean-up treasury)}]}]
                              :source    :mixed
                              :options   [{:area :play-area :card-name :treasury}]
                              :max       1}
                             {:player-no 0
                              :effect    [:sync-repeated-play]}
                             {:player-no 0
                              :effect    [:do-clean-up {:player-no 0}]}
                             {:player-no 0
                              :effect    [:draw 5]}
                             {:player-no 0
                              :effect    [:set-phase {:phase :out-of-turn}]}
                             {:player-no 0
                              :effect    [:check-game-ended]}]}))
      (is (= (-> {:players [{:deck    (repeat 7 copper)
                             :hand    [treasury-1 treasury-2]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :treasury)
                 (play 0 :treasury)
                 (clean-up {:player-no 0})
                 (choose nil))
             {:players [{:hand    (repeat 5 copper)
                         :discard [copper copper treasury-1 treasury-2]
                         :actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:players [{:deck    (repeat 7 copper)
                             :hand    [treasury-1 throne-room]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :throne-room)
                 (choose :treasury)
                 (clean-up {:player-no 0})
                 (choose {:area :play-area :card-name :treasury}))
             {:players [{:hand    [treasury-1 copper copper copper copper]
                         :deck    [copper]
                         :discard [copper copper throne-room]
                         :actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]}))
      (let [improve (assoc improve :id 4)
            gold    (assoc gold :id 5)]
        (is (= (-> {:supply  [{:card gold :pile-size 30}]
                    :players [{:hand    [treasury-1 treasury-2 improve]
                               :deck    (repeat 7 copper)
                               :actions 1
                               :coins   0
                               :phase   :action}]}
                   (play 0 :treasury)
                   (play 0 :treasury)
                   (play 0 :improve)
                   (clean-up {:player-no 0})
                   (choose {:area :play-area :card-name :improve})
                   (choose :treasury)                       ; improve Treasury-1 => Gold
                   (choose :gold)
                   (choose {:area :play-area :card-name :treasury})) ; topdeck Treasury-2
               {:supply  [{:card gold :pile-size 29}]
                :players [{:hand    [treasury-2 copper copper copper copper]
                           :deck    [copper]
                           :discard [gold copper copper improve]
                           :actions 0
                           :coins   0
                           :buys    0
                           :phase   :out-of-turn}]
                :trash   [treasury-1]}))))))

(deftest warehouse-test
  (testing "Warehouse"
    (is (= (play {:players [{:hand    [warehouse estate copper copper copper]
                             :deck    [estate copper estate copper]
                             :actions 1}]}
                 0 :warehouse)
           {:players      [{:hand      [estate copper copper copper estate copper estate]
                            :play-area [warehouse]
                            :deck      [copper]
                            :actions   1}]
            :effect-stack [{:text      "Discard 3 cards."
                            :player-no 0
                            :choice    :discard-from-hand
                            :source    :hand
                            :options   [:estate :copper :copper :copper :estate :copper :estate]
                            :min       3
                            :max       3}]}))
    (is (= (-> {:players [{:hand    [warehouse estate copper copper copper]
                           :deck    [estate copper estate copper]
                           :actions 1}]}
               (play 0 :warehouse)
               (choose [:estate :estate :estate]))
           {:players [{:hand      [copper copper copper copper]
                       :play-area [warehouse]
                       :deck      [copper]
                       :discard   [estate estate estate]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [warehouse estate copper copper copper]
                           :deck    [estate]
                           :actions 1}]}
               (play 0 :warehouse)
               (choose [:estate :estate :copper]))
           {:players [{:hand      [copper copper]
                       :play-area [warehouse]
                       :discard   [estate estate copper]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [warehouse copper copper]
                           :actions 1}]}
               (play 0 :warehouse)
               (choose [:copper :copper]))
           {:players [{:play-area [warehouse]
                       :discard   [copper copper]
                       :actions   1}]}))))

(deftest wharf-test
  (let [wharf (assoc wharf :id 1)]
    (testing "Wharf"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [wharf estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver]
                             :actions 1
                             :buys    1}]}
                 (play 0 :wharf))
             {:players [{:hand      [estate estate estate copper copper copper]
                         :play-area [wharf]
                         :deck      [copper copper copper copper silver]
                         :actions   0
                         :buys      2
                         :triggers  [(get-trigger wharf)]}]}))
      (is (= (-> {:players [{:hand    [wharf estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver silver silver]
                             :actions 1
                             :buys    1
                             :phase   :action}]}
                 (play 0 :wharf)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper silver silver silver]
                                :play-area [wharf]
                                :discard   [estate estate estate copper copper copper]
                                :actions   1
                                :coins     0
                                :buys      2
                                :phase     :action}]})))))
