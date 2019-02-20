(ns dombot.cards.promos-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :refer [moat]]
            [dombot.cards.seaside :refer [caravan]]
            [dombot.cards.promos :as promos :refer :all]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest stash-test
  (let [stash (assoc stash :id 1)
        caravan (assoc caravan :id 2)]
    (testing "Stash"
      (is (= (-> {:players [{:hand  [stash]
                             :coins 0}]}
                 (play 0 :stash))
             {:players [{:play-area [stash]
                         :coins     2}]}))
      (is (= (-> {:players [{:hand    [moat]
                             :discard [stash copper copper copper]
                             :actions 1}]}
                 (play 0 :moat))
             {:players      [{:play-area [moat]
                              :stash     [stash]
                              :deck      [copper copper copper]
                              :actions   0}]
              :effect-stack [{:text      "Put the Stash anywhere in your deck."
                              :player-no 0
                              :choice    ::promos/stash-put
                              :source    :deck-position
                              :options   [0 1 2 3]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:move-card {:player-no     0
                                                      :from          :deck
                                                      :from-position :top
                                                      :to            :hand}]}
                             {:player-no 0
                              :effect    [:move-card {:player-no     0
                                                      :from          :deck
                                                      :from-position :top
                                                      :to            :hand}]}]}))
      (is (= (-> {:players [{:hand    [moat]
                             :discard [stash copper copper copper]
                             :actions 1}]}
                 (play 0 :moat)
                 (choose 0))
             {:players [{:hand      [stash copper]
                         :play-area [moat]
                         :deck      [copper copper]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [moat]
                             :discard [stash copper copper copper]
                             :actions 1}]}
                 (play 0 :moat)
                 (choose 1))
             {:players [{:hand      [copper stash]
                         :play-area [moat]
                         :deck      [copper copper]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [moat]
                             :discard [stash copper copper copper]
                             :actions 1}]}
                 (play 0 :moat)
                 (choose 2))
             {:players [{:hand      [copper copper]
                         :play-area [moat]
                         :deck      [stash copper]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [moat]
                             :discard [stash copper copper copper]
                             :actions 1}]}
                 (play 0 :moat)
                 (choose 3))
             {:players [{:hand      [copper copper]
                         :play-area [moat]
                         :deck      [copper stash]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [moat]
                             :discard [stash copper copper stash copper]
                             :actions 1}]}
                 (play 0 :moat))
             {:players      [{:play-area [moat]
                              :stash     [stash stash]
                              :deck      [copper copper copper]
                              :actions   0}]
              :effect-stack [{:text      "Put the Stash anywhere in your deck."
                              :player-no 0
                              :choice    ::promos/stash-put
                              :source    :deck-position
                              :options   [0 1 2 3]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:give-choice {:text    "Put the Stash anywhere in your deck."
                                                        :choice  ::promos/stash-put
                                                        :options [:deck-position]
                                                        :min     1
                                                        :max     1}]}
                             {:player-no 0
                              :effect    [:move-card {:player-no     0
                                                      :from          :deck
                                                      :from-position :top
                                                      :to            :hand}]}
                             {:player-no 0
                              :effect    [:move-card {:player-no     0
                                                      :from          :deck
                                                      :from-position :top
                                                      :to            :hand}]}]}))
      (is (= (-> {:players [{:play-area       [copper copper copper copper copper]
                             :discard         [stash]
                             :number-of-turns 1}
                            {:play-area       [(assoc caravan :stay-in-play true)]
                             :discard         [stash copper]
                             :number-of-turns 1}]}
                 (end-turn 0))
             {:players      [{:deck            [copper copper copper copper copper]
                              :stash           [stash]
                              :actions         0
                              :coins           0
                              :buys            0
                              :actions-played  0
                              :phase           :out-of-turn
                              :number-of-turns 2}
                             {:play-area       [(assoc caravan :stay-in-play true)]
                              :discard         [stash copper]
                              :number-of-turns 1}]
              :effect-stack [{:text      "Put the Stash anywhere in your deck."
                              :player-no 0
                              :choice    ::promos/stash-put
                              :source    :deck-position
                              :options   [0 1 2 3 4 5]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:move-card {:player-no     0
                                                      :from          :deck
                                                      :from-position :top
                                                      :to            :hand}]}
                             {:player-no 0
                              :effect    [:move-card {:player-no     0
                                                      :from          :deck
                                                      :from-position :top
                                                      :to            :hand}]}
                             {:player-no 0
                              :effect    [:move-card {:player-no     0
                                                      :from          :deck
                                                      :from-position :top
                                                      :to            :hand}]}
                             {:player-no 0
                              :effect    [:move-card {:player-no     0
                                                      :from          :deck
                                                      :from-position :top
                                                      :to            :hand}]}
                             {:player-no 0
                              :effect    [:move-card {:player-no     0
                                                      :from          :deck
                                                      :from-position :top
                                                      :to            :hand}]}
                             {:player-no 1
                              :effect    [:start-turn]}]}))
      (is (= (-> {:players [{:play-area       [copper copper copper copper copper]
                             :discard         [stash]
                             :number-of-turns 1}
                            {:play-area       [(assoc caravan :stay-in-play true)]
                             :discard         [stash copper]
                             :number-of-turns 1}]}
                 (end-turn 0)
                 (choose 0))
             {:current-player 1
              :players        [{:hand            [stash copper copper copper copper]
                                :deck            [copper]
                                :actions         0
                                :coins           0
                                :buys            0
                                :actions-played  0
                                :phase           :out-of-turn
                                :number-of-turns 2}
                               {:play-area       [caravan]
                                :deck            [copper]
                                :stash           [stash]
                                :actions         1
                                :coins           0
                                :buys            1
                                :phase           :action
                                :number-of-turns 1}]
              :effect-stack   [{:text      "Put the Stash anywhere in your deck."
                                :player-no 1
                                :choice    ::promos/stash-put
                                :source    :deck-position
                                :options   [0 1]
                                :min       1
                                :max       1}
                               {:player-no 1
                                :effect    [:move-card {:player-no     1
                                                        :from          :deck
                                                        :from-position :top
                                                        :to            :hand}]}]})))))