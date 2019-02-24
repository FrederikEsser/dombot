(ns dombot.cards.seaside-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :as dominion :refer [moat throne-room witch]]
            [dombot.cards.seaside :as seaside :refer :all]
            [dombot.utils :as ut]))

(deftest bazaar-test
  (testing "Bazaar"
    (is (= (play {:players [{:deck    [copper copper copper]
                             :hand    [bazaar]
                             :actions 1
                             :coins   0}]}
                 0 :bazaar)
           {:players [{:deck      [copper copper]
                       :hand      [copper]
                       :play-area [bazaar]
                       :actions   2
                       :coins     1}]}))))

(deftest caravan-test
  (let [caravan-1 (assoc caravan :id 1)
        caravan-2 (assoc caravan :id 2)
        throne-room (assoc throne-room :id 3)]
    (testing "Caravan"
      (is (= (-> {:players [{:hand    [caravan-1 estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver]
                             :actions 1}]}
                 (play 0 :caravan))
             {:players [{:hand      [estate estate estate copper copper]
                         :play-area [(assoc caravan-1 :at-start-turn [[[:draw 1]]])]
                         :deck      [copper copper copper copper copper silver]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [caravan-1 estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver]
                             :actions 1}]}
                 (play 0 :caravan)
                 (clean-up {:player-no 0}))
             {:players [{:hand           [copper copper copper copper copper]
                         :play-area      [(assoc caravan-1 :at-start-turn [[[:draw 1]]])]
                         :deck           [silver]
                         :discard        [estate estate estate copper copper]
                         :actions        0
                         :coins          0
                         :buys           0
                         :actions-played 0
                         :phase          :out-of-turn}]}))
      (is (= (-> {:players [{:hand    [caravan-1 estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver]
                             :actions 1}]}
                 (play 0 :caravan)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           [copper copper copper copper copper silver]
                                :play-area      [caravan-1]
                                :discard        [estate estate estate copper copper]
                                :actions        1
                                :coins          0
                                :buys           1
                                :actions-played 0
                                :phase          :action}]}))
      (is (= (-> {:players [{:hand    [caravan-1 caravan-2 copper copper copper]
                             :deck    [copper copper estate estate copper copper silver]
                             :actions 1}]}
                 (play 0 :caravan)
                 (play 0 :caravan)
                 (clean-up {:player-no 0}))
             {:players [{:hand           [estate estate copper copper silver]
                         :play-area      [(assoc caravan-1 :at-start-turn [[[:draw 1]]])
                                          (assoc caravan-2 :at-start-turn [[[:draw 1]]])]
                         :discard        [copper copper copper copper copper]
                         :actions        0
                         :coins          0
                         :buys           0
                         :actions-played 0
                         :phase          :out-of-turn}]}))
      (is (= (-> {:players [{:hand    [caravan-1 caravan-2 copper copper copper]
                             :deck    [copper copper estate estate copper copper silver]
                             :actions 1}]}
                 (play 0 :caravan)
                 (play 0 :caravan)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           [estate estate copper copper silver copper copper]
                                :play-area      [caravan-1 caravan-2]
                                :deck           [copper copper copper]
                                :actions        1
                                :coins          0
                                :buys           1
                                :actions-played 0
                                :phase          :action}]}))
      (is (= (-> {:players [{:hand    [caravan-1 throne-room copper copper copper]
                             :deck    [copper copper estate estate copper copper silver]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :caravan)
                 (clean-up {:player-no 0}))
             {:players [{:hand           [estate estate copper copper silver]
                         :play-area      [(assoc throne-room :at-start-turn [[]])
                                          (assoc caravan-1 :at-start-turn [[[:draw 1]] [[:draw 1]]])]
                         :discard        [copper copper copper copper copper]
                         :actions        0
                         :coins          0
                         :buys           0
                         :actions-played 0
                         :phase          :out-of-turn}]}))
      (is (= (-> {:players [{:hand    [caravan-1 throne-room copper copper copper]
                             :deck    [copper copper estate estate copper copper silver]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :caravan)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           [estate estate copper copper silver copper copper]
                                :play-area      [throne-room caravan-1]
                                :deck           [copper copper copper]
                                :actions        1
                                :coins          0
                                :buys           1
                                :actions-played 0
                                :phase          :action}]})))))

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

(deftest fishing-village-test
  (let [fishing-village (assoc fishing-village :id 1)]
    (testing "Fishing Village"
      (is (= (-> {:players [{:hand    [fishing-village]
                             :actions 1
                             :coins   0}]}
                 (play 0 :fishing-village))
             {:players [{:play-area [(assoc fishing-village :at-start-turn [[[:give-actions 1]
                                                                             [:give-coins 1]]])]
                         :actions   2
                         :coins     1}]}))
      (is (= (-> {:players [{:hand    [fishing-village]
                             :actions 1
                             :coins   0}]}
                 (play 0 :fishing-village)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area      [fishing-village]
                                :actions        2
                                :coins          1
                                :buys           1
                                :actions-played 0
                                :phase          :action}]})))))

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
  (let [haven (assoc haven :id 1)
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
      (is (= (-> {:players [{:hand    [haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :haven)
                 (choose :copper))
             {:players [{:hand      [estate]
                         :play-area [(assoc haven :at-start-turn [[[::seaside/haven-put-in-hand {:card-name :copper}]]]
                                                  :set-aside [copper])]
                         :deck      [copper]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :haven)
                 (choose :copper)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           [copper estate copper]
                                :play-area      [(assoc haven :set-aside [])]
                                :actions        1
                                :coins          0
                                :buys           1
                                :actions-played 0
                                :phase          :action}]}))
      (is (= (-> {:players [{:hand    [haven]
                             :actions 1}]}
                 (play 0 :haven))
             {:players [{:play-area [haven]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [throne-room haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :haven)
                 (choose :copper)
                 (choose :estate))
             {:players [{:hand      [copper]
                         :play-area [(assoc throne-room :at-start-turn [[]])
                                     (assoc haven :at-start-turn [[[::seaside/haven-put-in-hand {:card-name :copper}]]
                                                                  [[::seaside/haven-put-in-hand {:card-name :estate}]]]
                                                  :set-aside [copper estate])]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [throne-room haven estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :haven)
                 (choose :copper)
                 (choose :estate)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           [copper copper estate]
                                :play-area      [throne-room
                                                 (assoc haven :set-aside [])]
                                :actions        1
                                :coins          0
                                :buys           1
                                :actions-played 0
                                :phase          :action}]})))))

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
      (is (= (-> {:players [{:hand    [lighthouse-1]
                             :actions 1
                             :coins   0}]}
                 (play 0 :lighthouse))
             {:players [{:play-area  [(assoc lighthouse-1 :at-start-turn [[[:give-coins 1]
                                                                           [:clear-unaffected]]])]
                         :actions    1
                         :coins      1
                         :unaffected [{:card-id 1}]}]}))
      (is (= (-> {:players [{:hand    [lighthouse-1]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :lighthouse)
                 (end-turn 0))
             {:current-player 1
              :players        [{:play-area      [(assoc lighthouse-1 :at-start-turn [[[:give-coins 1]
                                                                                      [:clear-unaffected]]])]
                                :actions        0
                                :coins          0
                                :buys           0
                                :actions-played 0
                                :phase          :out-of-turn
                                :unaffected     [{:card-id 1}]}
                               {:actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}]}))
      (let [curse (assoc curse :id 1)]
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
                :players        [{:hand           [moat]
                                  :play-area      [(assoc lighthouse-1 :at-start-turn [[[:give-coins 1]
                                                                                        [:clear-unaffected]]])]
                                  :actions        0
                                  :coins          0
                                  :buys           0
                                  :actions-played 0
                                  :phase          :out-of-turn
                                  :unaffected     [{:card-id 1}]}
                                 {:hand           [witch]
                                  :discard        [curse]
                                  :actions        0
                                  :coins          0
                                  :buys           0
                                  :actions-played 0
                                  :phase          :out-of-turn}
                                 {:hand      [curse]
                                  :play-area [witch]
                                  :actions   0
                                  :coins     0
                                  :buys      1
                                  :phase     :action}]})))
      (is (= (-> {:players [{:hand    [lighthouse-1]
                             :actions 1
                             :coins   0}]}
                 (play 0 :lighthouse)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area      [lighthouse-1]
                                :actions        1
                                :coins          1
                                :buys           1
                                :actions-played 0
                                :phase          :action}]}))
      (is (= (-> {:players [{:hand    [lighthouse-1 lighthouse-2]
                             :actions 1
                             :coins   0}]}
                 (play 0 :lighthouse)
                 (play 0 :lighthouse)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area      [lighthouse-1 lighthouse-2]
                                :actions        1
                                :coins          2
                                :buys           1
                                :actions-played 0
                                :phase          :action}]})))))

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
      (is (= (-> {:players [{:hand    [merchant-ship]
                             :actions 1
                             :coins   0}]}
                 (play 0 :merchant-ship))
             {:players [{:play-area [(assoc merchant-ship :at-start-turn [[[:give-coins 2]]])]
                         :actions   0
                         :coins     2}]}))
      (is (= (-> {:players [{:hand    [merchant-ship]
                             :actions 1
                             :coins   0}]}
                 (play 0 :merchant-ship)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area      [merchant-ship]
                                :actions        1
                                :coins          2
                                :buys           1
                                :actions-played 0
                                :phase          :action}]})))))

(deftest outpost-test
  (let [outpost (assoc outpost :id 1)]
    (testing "Outpost"
      (is (= (-> {:players [{:hand    [outpost]
                             :actions 1}]}
                 (play 0 :outpost))
             {:players [{:play-area                [(assoc outpost :at-end-turn [[::seaside/outpost-extra-turn]])]
                         :actions                  0
                         :previous-turn-was-yours? true}]}))
      (is (= (-> {:players [{:hand            [outpost]
                             :deck            (repeat 5 copper)
                             :actions         1
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
                                :actions-played           0
                                :phase                    :action
                                :previous-turn-was-yours? true
                                :number-of-turns          1}
                               {}]}))
      (is (= (-> {:players [{:hand                     [outpost]
                             :deck                     (repeat 5 copper)
                             :actions                  1
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
                                :actions-played  0
                                :phase           :out-of-turn
                                :number-of-turns 2}
                               {:actions 1
                                :buys    1
                                :coins   0
                                :phase   :action}]})))))

(deftest sea-hag-test
  (let [curse (assoc curse :id 1)]
    (testing "Sea Hag"
      (is (= (play {:supply  [{:card curse :pile-size 20}]
                    :players [{:hand    [sea-hag]
                               :actions 1}
                              {:deck [copper copper]}
                              {}]}
                   0 :sea-hag)
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

(deftest tactician-test
  (let [tactician (assoc tactician :id 1)
        throne-room (assoc throne-room :id 2)]
    (testing "Tactician"
      (is (= (-> {:players [{:hand    [tactician estate]
                             :actions 1}]}
                 (play 0 :tactician))
             {:players [{:play-area [(assoc tactician :at-start-turn [[[:draw 5]
                                                                       [:give-actions 1]
                                                                       [:give-buys 1]]])]
                         :discard   [estate]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [tactician]
                             :actions 1}]}
                 (play 0 :tactician))
             {:players [{:play-area [(assoc tactician :at-start-turn [])]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [tactician estate]
                             :deck    (repeat 10 copper)
                             :actions 1
                             :coins   0}]}
                 (play 0 :tactician)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           (repeat 10 copper)
                                :play-area      [tactician]
                                :discard        [estate]
                                :actions        2
                                :coins          0
                                :buys           2
                                :actions-played 0
                                :phase          :action}]}))
      (is (= (-> {:players [{:hand    [tactician]
                             :deck    (repeat 10 copper)
                             :actions 1
                             :coins   0}]}
                 (play 0 :tactician)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           (repeat 5 copper)
                                :deck           (repeat 5 copper)
                                :discard        [(assoc tactician :at-start-turn [])]
                                :actions        1
                                :coins          0
                                :buys           1
                                :actions-played 0
                                :phase          :action}]}))
      (is (= (-> {:players [{:hand    [throne-room tactician estate]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :tactician))
             {:players [{:play-area [(assoc throne-room :at-start-turn [[]])
                                     (assoc tactician :at-start-turn [[[:draw 5]
                                                                       [:give-actions 1]
                                                                       [:give-buys 1]]])]
                         :discard   [estate]
                         :actions   0}]})))))

(deftest wharf-test
  (let [wharf (assoc wharf :id 1)]
    (testing "Wharf"
      (is (= (-> {:players [{:hand    [wharf estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver]
                             :actions 1
                             :buys    1}]}
                 (play 0 :wharf))
             {:players [{:hand      [estate estate estate copper copper copper]
                         :play-area [(assoc wharf :at-start-turn [[[:draw 2]
                                                                   [:give-buys 1]]])]
                         :deck      [copper copper copper copper silver]
                         :actions   0
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [wharf estate estate estate copper]
                             :deck    [copper copper copper copper copper copper silver silver silver]
                             :actions 1
                             :buys    1}]}
                 (play 0 :wharf)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           [copper copper copper copper silver silver silver]
                                :play-area      [wharf]
                                :discard        [estate estate estate copper copper copper]
                                :actions        1
                                :coins          0
                                :buys           2
                                :actions-played 0
                                :phase          :action}]})))))
