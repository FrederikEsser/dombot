(ns dombot.cards.renaissance-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :refer [throne-room chapel]]
            [dombot.cards.intrigue :refer [lurker]]
            [dombot.cards.seaside :refer [merchant-ship]]
            [dombot.cards.renaissance :as renaissance :refer :all]
            [dombot.utils :as ut])
  (:refer-clojure :exclude [key]))

(deftest acting-troupe-test
  (let [acting-troupe (assoc acting-troupe :id 1)]
    (testing "Acting Troupe"
      (is (= (-> {:players [{:hand    [acting-troupe]
                             :actions 1}]}
                 (play 0 :acting-troupe))
             {:players [{:actions   0
                         :villagers 4}]
              :trash   [acting-troupe]}))
      (is (= (-> {:players [{:hand    [acting-troupe throne-room]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :acting-troupe))
             {:players [{:play-area [throne-room]
                         :actions   0
                         :villagers 8}]
              :trash   [acting-troupe]})))))

(deftest border-guard-test
  (let [border-guard (assoc border-guard :id 1)]
    (testing "Border Guard"
      (is (= (-> {:players [{:hand    [border-guard]
                             :deck    [lackeys gold copper]
                             :actions 1}]}
                 (play 0 :border-guard))
             {:players      [{:play-area [border-guard]
                              :deck      [copper]
                              :revealed  [lackeys gold]
                              :actions   1}]
              :effect-stack [{:text      "Put one of the revealed cards into your hand."
                              :player-no 0
                              :card-id   1
                              :choice    ::renaissance/border-guard-take-revealed
                              :source    :revealed
                              :options   [:lackeys :gold]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :card-id   1
                              :effect    [:discard-all-revealed]}]}))
      (is (= (-> {:players [{:hand    [border-guard]
                             :deck    [lackeys gold copper]
                             :actions 1}]}
                 (play 0 :border-guard)
                 (choose :gold))
             {:players [{:hand           [gold]
                         :play-area      [border-guard]
                         :deck           [copper]
                         :discard        [lackeys]
                         :actions        1
                         :revealed-cards {:hand    1
                                          :discard 1}}]}))
      (is (= (-> {:players [{:hand    [border-guard]
                             :deck    [lackeys lackeys copper]
                             :actions 1}]}
                 (play 0 :border-guard)
                 (choose :lackeys))
             {:players      [{:hand           [lackeys]
                              :play-area      [border-guard]
                              :deck           [copper]
                              :revealed       [lackeys]
                              :actions        1
                              :revealed-cards {:hand 1}}]
              :effect-stack [{:text      "Choose one:"
                              :player-no 0
                              :choice    ::renaissance/border-guard-choice
                              :source    :special
                              :options   [{:option :lantern :text "Take the Lantern."}
                                          {:option :horn :text "Take the Horn."}]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :card-id   1
                              :effect    [:discard-all-revealed]}]}))
      (is (= (-> {:artifacts {:horn    horn
                              :lantern lantern}
                  :players   [{:hand    [border-guard]
                               :deck    [lackeys lackeys copper]
                               :actions 1}]}
                 (play 0 :border-guard)
                 (choose :lackeys)
                 (choose :horn))
             {:artifacts {:horn    (assoc horn :owner 0)
                          :lantern lantern}
              :players   [{:hand           [lackeys]
                           :play-area      [border-guard]
                           :deck           [copper]
                           :discard        [lackeys]
                           :actions        1
                           :revealed-cards {:hand    1
                                            :discard 1}
                           :triggers       [{:trigger  :at-clean-up
                                             :duration :horn
                                             :effects  [[::renaissance/horn-at-clean-up]]}]}]}))
      (is (= (-> {:artifacts {:horn    horn
                              :lantern lantern}
                  :players   [{:hand    [border-guard]
                               :deck    [lackeys lackeys copper]
                               :actions 1}]}
                 (play 0 :border-guard)
                 (choose :lackeys)
                 (choose :horn)
                 (clean-up {:player-no 0}))
             {:artifacts    {:horn    (assoc horn :owner 0)
                             :lantern lantern}
              :players      [{:hand           [lackeys]
                              :play-area      [(assoc border-guard :at-clean-up [[:topdeck-this-from-play-area]])]
                              :deck           [copper]
                              :discard        [lackeys]
                              :actions        1
                              :revealed-cards {:hand    1
                                               :discard 1}
                              :triggers       [{:trigger  :at-clean-up
                                                :duration :horn
                                                :effects  [[::renaissance/horn-at-clean-up]]}]}]
              :effect-stack [{:text      "You may activate cards, that do something when you discard them from play."
                              :player-no 0
                              :choice    :at-clean-up-choice
                              :source    :play-area
                              :options   [:border-guard]
                              :max       1}
                             {:player-no 0
                              :effect    [:do-clean-up {:player-no 0}]}
                             {:player-no 0
                              :effect    [:draw 5]}
                             {:player-no 0
                              :effect    [:check-game-ended]}]}))
      (is (= (-> {:artifacts {:horn    horn
                              :lantern lantern}
                  :players   [{:hand    [border-guard]
                               :deck    (concat [lackeys lackeys] (repeat 7 copper))
                               :actions 1}]}
                 (play 0 :border-guard)
                 (choose :lackeys)
                 (choose :horn)
                 (clean-up {:player-no 0})
                 (choose nil))
             {:artifacts {:horn    (assoc horn :owner 0)
                          :lantern lantern}
              :players   [{:hand           (repeat 5 copper)
                           :deck           (repeat 2 copper)
                           :discard        [lackeys lackeys border-guard]
                           :actions        0
                           :coins          0
                           :buys           0
                           :actions-played 0
                           :phase          :out-of-turn
                           :triggers       [{:trigger  :at-clean-up
                                             :duration :horn
                                             :effects  [[::renaissance/horn-at-clean-up]]}]}]}))
      (is (= (-> {:artifacts {:horn    horn
                              :lantern lantern}
                  :players   [{:hand      [border-guard]
                               :play-area [border-guard]
                               :deck      (concat [lackeys lackeys] (repeat 7 copper))
                               :actions   1}]}
                 (play 0 :border-guard)
                 (choose :lackeys)
                 (choose :horn)
                 (clean-up {:player-no 0})
                 (choose :border-guard))
             {:artifacts {:horn    (assoc horn :owner 0)
                          :lantern lantern}
              :players   [{:hand           [border-guard copper copper copper copper]
                           :deck           (repeat 3 copper)
                           :discard        [lackeys lackeys border-guard]
                           :actions        0
                           :coins          0
                           :buys           0
                           :actions-played 0
                           :phase          :out-of-turn
                           :triggers       [{:trigger  :at-clean-up
                                             :duration :horn
                                             :effects  [[::renaissance/horn-at-clean-up]]}]}]}))))
  (is (= (-> {:artifacts {:horn    horn
                          :lantern lantern}
              :players   [{:hand    [border-guard]
                           :deck    [lackeys lackeys copper]
                           :actions 1}]}
             (play 0 :border-guard)
             (choose :lackeys)
             (choose :lantern))
         {:artifacts {:horn    horn
                      :lantern (assoc lantern :owner 0)}
          :players   [{:hand           [lackeys]
                       :play-area      [border-guard]
                       :deck           [copper]
                       :discard        [lackeys]
                       :actions        1
                       :revealed-cards {:hand    1
                                        :discard 1}}]}))
  (is (= (-> {:artifacts {:horn    horn
                          :lantern (assoc lantern :owner 0)}
              :players   [{:hand    [border-guard]
                           :deck    [lackeys gold copper]
                           :actions 1}]}
             (play 0 :border-guard))
         {:artifacts    {:horn    horn
                         :lantern (assoc lantern :owner 0)}
          :players      [{:play-area [border-guard]
                          :revealed  [lackeys gold copper]
                          :actions   1}]
          :effect-stack [{:text      "Put one of the revealed cards into your hand."
                          :player-no 0
                          :choice    ::renaissance/border-guard-take-revealed
                          :source    :revealed
                          :options   [:lackeys :gold :copper]
                          :min       1
                          :max       1}
                         {:player-no 0
                          :effect    [:discard-all-revealed]}]}))
  (is (= (-> {:artifacts {:horn    horn
                          :lantern (assoc lantern :owner 0)}
              :players   [{:hand    [border-guard]
                           :deck    [lackeys gold border-guard]
                           :actions 1}]}
             (play 0 :border-guard)
             (choose :gold))
         {:artifacts {:horn    horn
                      :lantern (assoc lantern :owner 0)}
          :players   [{:hand           [gold]
                       :play-area      [border-guard]
                       :discard        [lackeys border-guard]
                       :actions        1
                       :revealed-cards {:hand    1
                                        :discard 2}}]}))
  (is (= (-> {:artifacts {:horn    horn
                          :lantern (assoc lantern :owner 0)}
              :players   [{:hand    [border-guard]
                           :deck    [lackeys lackeys border-guard]
                           :actions 1}]}
             (play 0 :border-guard)
             (choose :lackeys))
         {:artifacts    {:horn    horn
                         :lantern (assoc lantern :owner 0)}
          :players      [{:hand           [lackeys]
                          :play-area      [border-guard]
                          :revealed       [lackeys border-guard]
                          :actions        1
                          :revealed-cards {:hand 1}}]
          :effect-stack [{:text      "Choose one:"
                          :player-no 0
                          :choice    ::renaissance/border-guard-choice
                          :source    :special
                          :options   [{:option :lantern :text "Take the Lantern."}
                                      {:option :horn :text "Take the Horn."}]
                          :min       1
                          :max       1}
                         {:player-no 0
                          :effect    [:discard-all-revealed]}]}))
  (is (= (-> {:artifacts {:horn    horn
                          :lantern (assoc lantern :owner 0)}
              :players   [{:hand    [border-guard]
                           :deck    [lackeys lackeys]
                           :actions 1}]}
             (play 0 :border-guard)
             (choose :lackeys))
         {:artifacts {:horn    horn
                      :lantern (assoc lantern :owner 0)}
          :players   [{:hand           [lackeys]
                       :play-area      [border-guard]
                       :discard        [lackeys]
                       :actions        1
                       :revealed-cards {:hand    1
                                        :discard 1}}]})))

(deftest ducat-test
  (let [ducat (assoc ducat :id 1)]
    (testing "Ducat"
      (is (= (-> {:players [{:hand  [ducat]
                             :coins 0
                             :buys  1}]}
                 (play 0 :ducat))
             {:players [{:play-area [ducat]
                         :coins     0
                         :buys      2
                         :coffers   1}]}))
      (is (= (-> {:supply  [{:card ducat :pile-size 10}]
                  :players [{:hand [copper copper]}]}
                 (gain {:player-no 0
                        :card-name :ducat}))
             {:supply       [{:card ducat :pile-size 9}]
              :players      [{:hand    [copper copper]
                              :discard [ducat]}]
              :effect-stack [{:text      "You may trash a Copper from your hand."
                              :player-no 0
                              :choice    :trash-from-hand
                              :source    :hand
                              :options   [:copper :copper]
                              :max       1}]}))
      (is (= (-> {:supply  [{:card ducat :pile-size 10}]
                  :players [{:hand [copper copper]}]}
                 (gain {:player-no 0
                        :card-name :ducat})
                 (choose nil))
             {:supply  [{:card ducat :pile-size 9}]
              :players [{:hand    [copper copper]
                         :discard [ducat]}]}))
      (is (= (-> {:supply  [{:card ducat :pile-size 10}]
                  :players [{:hand [copper copper]}]}
                 (gain {:player-no 0
                        :card-name :ducat})
                 (choose :copper))
             {:supply  [{:card ducat :pile-size 9}]
              :players [{:hand    [copper]
                         :discard [ducat]}]
              :trash   [copper]})))))

(deftest experiment-test
  (let [experiment (assoc experiment :id 1)]
    (testing "Experiment"
      (is (= (-> {:supply  [{:card experiment :pile-size 9}]
                  :players [{:deck    [copper copper copper]
                             :hand    [experiment]
                             :actions 1}]}
                 (play 0 :experiment))
             {:supply  [{:card experiment :pile-size 10}]
              :players [{:deck    [copper]
                         :hand    [copper copper]
                         :actions 1}]}))
      (is (= (-> {:supply  [{:card experiment :pile-size 9}]
                  :players [{:deck    [copper copper copper]
                             :hand    [experiment throne-room]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :experiment))
             {:supply  [{:card experiment :pile-size 10}]
              :players [{:hand      [copper copper copper]
                         :play-area [throne-room]
                         :actions   2}]}))
      (is (= (-> {:supply  [{:card experiment :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0
                        :card-name :experiment}))
             {:supply  [{:card experiment :pile-size 8}]
              :players [{:discard [experiment experiment]}]}))
      (is (= (-> {:supply  [{:card experiment :pile-size 1}]
                  :players [{}]}
                 (gain {:player-no 0
                        :card-name :experiment}))
             {:supply  [{:card experiment :pile-size 0}]
              :players [{:discard [experiment]}]}))
      (is (= (-> {:supply  [{:card experiment :pile-size 9}]
                  :players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [experiment]}
                 (play 0 :lurker)
                 (choose :gain)
                 (choose :experiment))
             {:supply  [{:card experiment :pile-size 8}]
              :players [{:play-area [lurker]
                         :discard   [experiment experiment]
                         :actions   1}]
              :trash   []})))))

(deftest flag-bearer-test
  (let [flag-bearer (assoc flag-bearer :id 1)]
    (testing "Flag Bearer"
      (is (= (-> {:players [{:hand    [flag-bearer]
                             :actions 1
                             :coins   0}]}
                 (play 0 :flag-bearer))
             {:players [{:play-area [flag-bearer]
                         :actions   0
                         :coins     2}]}))
      (is (= (-> {:supply    [{:card flag-bearer :pile-size 10}]
                  :artifacts {:flag flag}
                  :players   [{}]}
                 (gain {:player-no 0
                        :card-name :flag-bearer}))
             {:supply    [{:card flag-bearer :pile-size 9}]
              :artifacts {:flag (assoc flag :owner 0)}
              :players   [{:discard  [flag-bearer]
                           :triggers [{:trigger  :at-draw-hand
                                       :duration :flag
                                       :effects  [[:draw 1]]}]}]}))
      (is (= (-> {:supply    [{:card flag-bearer :pile-size 9}]
                  :artifacts {:flag (assoc flag :owner 0)}
                  :players   [{:triggers [{:trigger  :at-draw-hand
                                           :duration :flag
                                           :effects  [[:draw 1]]}]}]}
                 (gain {:player-no 0
                        :card-name :flag-bearer}))
             {:supply    [{:card flag-bearer :pile-size 8}]
              :artifacts {:flag (assoc flag :owner 0)}
              :players   [{:discard  [flag-bearer]
                           :triggers [{:trigger  :at-draw-hand
                                       :duration :flag
                                       :effects  [[:draw 1]]}]}]}))
      (is (= (-> {:supply    [{:card flag-bearer :pile-size 9}]
                  :artifacts {:flag (assoc flag :owner 0)}
                  :players   [{:triggers [{:trigger  :at-draw-hand
                                           :duration :flag
                                           :effects  [[:draw 1]]}]}
                              {}]}
                 (gain {:player-no 1
                        :card-name :flag-bearer}))
             {:supply    [{:card flag-bearer :pile-size 8}]
              :artifacts {:flag (assoc flag :owner 1)}
              :players   [{}
                          {:discard  [flag-bearer]
                           :triggers [{:trigger  :at-draw-hand
                                       :duration :flag
                                       :effects  [[:draw 1]]}]}]}))
      (is (= (-> {:artifacts {:flag flag}
                  :players   [{:hand    [chapel flag-bearer]
                               :actions 1}]}
                 (play 0 :chapel)
                 (choose :flag-bearer))
             {:artifacts {:flag (assoc flag :owner 0)}
              :players   [{:play-area [chapel]
                           :actions   0
                           :triggers  [{:trigger  :at-draw-hand
                                        :duration :flag
                                        :effects  [[:draw 1]]}]}]
              :trash     [flag-bearer]}))
      (is (= (-> {:players [{:deck     (repeat 7 copper)
                             :triggers [{:trigger  :at-draw-hand
                                         :duration :flag
                                         :effects  [[:draw 1]]}]}]}
                 (clean-up {:player-no 0}))
             {:players [{:hand           (repeat 6 copper)
                         :deck           [copper]
                         :triggers       [{:trigger  :at-draw-hand
                                           :duration :flag
                                           :effects  [[:draw 1]]}]
                         :actions        0
                         :coins          0
                         :buys           0
                         :actions-played 0
                         :phase          :out-of-turn}]})))))

(deftest hideout-test
  (let [curse (assoc curse :id 1)]
    (testing "Hideout"
      (is (= (-> {:players [{:hand    [hideout estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :hideout))
             {:players      [{:hand      [estate copper]
                              :play-area [hideout]
                              :deck      [copper]
                              :actions   2}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :choice    ::renaissance/hideout-trash
                              :source    :hand
                              :options   [:estate :copper]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [hideout estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :hideout)
                 (choose :copper))
             {:players [{:hand      [estate]
                         :play-area [hideout]
                         :deck      [copper]
                         :actions   2}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [hideout estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :hideout)
                 (choose :estate))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:hand      [copper]
                         :play-area [hideout]
                         :deck      [copper]
                         :discard   [curse]
                         :actions   2}]
              :trash   [estate]})))))

(deftest improve-test
  (let [improve (assoc improve :id 1)]
    (testing "Improve"
      (is (= (-> {:players [{:hand    [improve]
                             :actions 1
                             :coins   0}]}
                 (play 0 :improve))
             {:players [{:play-area [(assoc improve :at-clean-up [[::renaissance/improve-give-choice]])]
                         :actions   0
                         :coins     2}]}))
      (is (= (-> {:players [{:hand    [improve]
                             :actions 1
                             :coins   0}]}
                 (play 0 :improve)
                 (clean-up {:player-no 0}))
             {:players      [{:play-area [(assoc improve :at-clean-up [[::renaissance/improve-give-choice]])]
                              :actions   0
                              :coins     2}]
              :effect-stack [{:text      "You may activate cards, that do something when you discard them from play."
                              :player-no 0
                              :choice    :at-clean-up-choice
                              :source    :play-area
                              :options   [:improve]
                              :max       1}
                             {:player-no 0
                              :effect    [:do-clean-up {:player-no 0}]}
                             {:player-no 0
                              :effect    [:draw 5]}
                             {:player-no 0
                              :effect    [:check-game-ended]}]}))
      (is (= (-> {:players [{:play-area [(assoc improve :at-clean-up [[::renaissance/improve-give-choice]])
                                         lackeys copper]}]}
                 (clean-up {:player-no 0})
                 (choose :improve))
             {:players      [{:play-area [improve lackeys copper]}]
              :effect-stack [{:text      "You may trash an Action card you would discard this turn to gain a card costing exactly $1 more than it."
                              :player-no 0
                              :choice    ::renaissance/improve-trash
                              :source    :play-area
                              :options   [:improve :lackeys]
                              :max       1}
                             {:player-no 0
                              :card-id   1
                              :effect    [:at-clean-up]}
                             {:player-no 0
                              :effect    [:do-clean-up {:player-no 0}]}
                             {:player-no 0
                              :effect    [:draw 5]}
                             {:player-no 0
                              :effect    [:check-game-ended]}]}))
      (is (= (-> {:players [{:play-area [(assoc researcher :at-start-turn [[:some-effects]])
                                         (assoc improve :at-clean-up [[::renaissance/improve-give-choice]])
                                         copper]}]}
                 (clean-up {:player-no 0})
                 (choose :improve))
             {:players      [{:play-area [(assoc researcher :at-start-turn [[:some-effects]])
                                          improve copper]}]
              :effect-stack [{:text      "You may trash an Action card you would discard this turn to gain a card costing exactly $1 more than it."
                              :player-no 0
                              :choice    ::renaissance/improve-trash
                              :source    :play-area
                              :options   [:improve]
                              :max       1}
                             {:player-no 0
                              :card-id   1
                              :effect    [:at-clean-up]}
                             {:player-no 0
                              :effect    [:do-clean-up {:player-no 0}]}
                             {:player-no 0
                              :effect    [:draw 5]}
                             {:player-no 0
                              :effect    [:check-game-ended]}]}))
      (is (= (-> {:supply  [{:card lackeys :pile-size 9}
                            {:card improve :pile-size 9}]
                  :players [{:play-area [(assoc improve :at-clean-up [[::renaissance/improve-give-choice]])
                                         lackeys copper]}]}
                 (clean-up {:player-no 0})
                 (choose :improve)
                 (choose :lackeys))
             {:supply       [{:card lackeys :pile-size 9}
                             {:card improve :pile-size 9}]
              :players      [{:play-area [improve copper]}]
              :trash        [lackeys]
              :effect-stack [{:text      "Gain a card costing exactly $3."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:improve]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :card-id   1
                              :effect    [:at-clean-up]}
                             {:player-no 0
                              :effect    [:do-clean-up {:player-no 0}]}
                             {:player-no 0
                              :effect    [:draw 5]}
                             {:player-no 0
                              :effect    [:check-game-ended]}]})))))

(deftest inventor-test
  (let [silver (assoc silver :id 1)
        duchy (assoc duchy :id 2)]
    (testing "Inventor"
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [inventor]
                             :actions 1}]}
                 (play 0 :inventor))
             {:supply       (base/supply 2 8)
              :players      [{:play-area [inventor]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing up to $4."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:curse :estate :copper :silver]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:add-cost-reduction 1]}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [inventor]
                             :actions 1}]}
                 (play 0 :inventor)
                 (choose :silver))
             {:cost-reductions [{:reduction 1}]
              :supply          [{:card silver :pile-size 39}]
              :players         [{:play-area [inventor]
                                 :discard   [silver]
                                 :actions   0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card duchy :pile-size 8}]
                  :players [{:hand    [inventor throne-room]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :inventor)
                 (choose :silver)
                 (choose :duchy))
             {:cost-reductions [{:reduction 1} {:reduction 1}]
              :supply          [{:card silver :pile-size 39}
                                {:card duchy :pile-size 7}]
              :players         [{:play-area [throne-room inventor]
                                 :discard   [silver duchy]
                                 :actions   0}]})))))

(deftest lackeys-test
  (let [lackeys (assoc lackeys :id 1)]
    (testing "Lackeys"
      (is (= (-> {:players [{:hand    [lackeys]
                             :deck    [copper copper gold]
                             :actions 1}]}
                 (play 0 :lackeys))
             {:players [{:hand      [copper copper]
                         :play-area [lackeys]
                         :deck      [gold]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card lackeys :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0
                        :card-name :lackeys}))
             {:supply  [{:card lackeys :pile-size 9}]
              :players [{:discard   [lackeys]
                         :villagers 2}]}))
      (is (= (-> {:supply  [{:card lackeys :pile-size 10}]
                  :players [{:hand    [lurker]
                             :actions 1}]}
                 (play 0 :lurker)
                 (choose :trash)
                 (choose :lackeys))
             {:supply  [{:card lackeys :pile-size 9}]
              :players [{:play-area [lurker]
                         :actions   1}]
              :trash   [lackeys]}))
      (is (= (-> {:supply  [{:card lackeys :pile-size 9}]
                  :players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [lackeys]}
                 (play 0 :lurker)
                 (choose :gain)
                 (choose :lackeys))
             {:supply  [{:card lackeys :pile-size 9}]
              :players [{:play-area [lurker]
                         :discard   [lackeys]
                         :actions   1
                         :villagers 2}]
              :trash   []})))))

(deftest mountain-village-test
  (testing "Mountain Village"
    (is (= (-> {:players [{:hand    [mountain-village]
                           :discard [copper gold]
                           :actions 1}]}
               (play 0 :mountain-village))
           {:players      [{:play-area [mountain-village]
                            :discard   [copper gold]
                            :actions   2}]
            :effect-stack [{:text          "Look through your discard pile and put a card from it into your hand."
                            :player-no     0
                            :choice        :take-from-discard
                            :source        :discard
                            :reveal-source true
                            :options       [:copper :gold]
                            :min           1
                            :max           1}]}))
    (is (= (-> {:players [{:hand    [mountain-village]
                           :discard [copper gold]
                           :actions 1}]}
               (play 0 :mountain-village)
               (choose :gold))
           {:players [{:hand      [gold]
                       :play-area [mountain-village]
                       :discard   [copper]
                       :actions   2}]}))
    (is (= (-> {:players [{:hand    [mountain-village]
                           :deck    [copper gold]
                           :actions 1}]}
               (play 0 :mountain-village))
           {:players [{:hand      [copper]
                       :play-area [mountain-village]
                       :deck      [gold]
                       :actions   2}]}))))

(deftest old-witch-test
  (let [curse (assoc curse :id 1)]
    (testing "Old Witch"
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:deck    (repeat 4 copper)
                             :hand    [old-witch]
                             :actions 1}
                            {:discard [copper copper]}]}
                 (play 0 :old-witch))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:deck      [copper]
                         :hand      [copper copper copper]
                         :play-area [old-witch]
                         :actions   0}
                        {:discard [copper copper curse]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 9}]
                  :players [{:deck    (repeat 4 copper)
                             :hand    [old-witch]
                             :actions 1}
                            {:hand    [curse copper copper estate copper]
                             :discard [copper copper]}]}
                 (play 0 :old-witch))
             {:supply       [{:card curse :pile-size 8}]
              :players      [{:deck      [copper]
                              :hand      [copper copper copper]
                              :play-area [old-witch]
                              :actions   0}
                             {:hand    [curse copper copper estate copper]
                              :discard [copper copper curse]}]
              :effect-stack [{:text      "You may trash a Curse from your hand."
                              :player-no 1
                              :choice    :trash-from-hand
                              :source    :hand
                              :options   [:curse]
                              :max       1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 9}]
                  :players [{:deck    (repeat 4 copper)
                             :hand    [old-witch]
                             :actions 1}
                            {:hand    [curse copper copper estate copper]
                             :discard [copper copper]}]}
                 (play 0 :old-witch)
                 (choose :curse))
             {:supply  [{:card curse :pile-size 8}]
              :players [{:deck      [copper]
                         :hand      [copper copper copper]
                         :play-area [old-witch]
                         :actions   0}
                        {:hand    [copper copper estate copper]
                         :discard [copper copper curse]}]
              :trash   [curse]})))))

(deftest patron-test
  (testing "Patron"
    (is (= (-> {:players [{:hand    [patron]
                           :actions 1
                           :coins   0}]}
               (play 0 :patron))
           {:players [{:play-area [patron]
                       :actions   0
                       :coins     2
                       :villagers 1}]}))
    (is (= (-> {:players [{:hand    [seer]
                           :deck    [estate estate estate patron]
                           :actions 1}]}
               (play 0 :seer))
           {:players [{:hand           [estate estate estate patron]
                       :play-area      [seer]
                       :actions        1
                       :coffers        1
                       :revealed-cards {:hand 3}}]}))
    (is (= (-> {:cost-reductions [{:reduction 3}]
                :players         [{:hand    [villain]
                                   :actions 1}
                                  {:hand [copper copper copper patron patron]}]}
               (play 0 :villain))
           {:cost-reductions [{:reduction 3}]
            :players         [{:play-area [villain]
                               :actions   0
                               :coffers   2}
                              {:hand           [copper copper copper patron patron]
                               :coffers        2
                               :revealed-cards {:hand 5}}]}))))

(deftest recruiter-test
  (testing "Recruiter"
    (is (= (-> {:players [{:hand    [recruiter estate estate]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :recruiter))
           {:players      [{:hand      [estate estate copper copper]
                            :play-area [recruiter]
                            :deck      [copper]
                            :actions   0}]
            :effect-stack [{:text      "Trash a cards from your hand."
                            :player-no 0
                            :choice    ::renaissance/recruiter-trash
                            :source    :hand
                            :options   [:estate :estate :copper :copper]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [recruiter estate estate]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :recruiter)
               (choose :estate))
           {:players [{:hand      [estate copper copper]
                       :play-area [recruiter]
                       :deck      [copper]
                       :actions   0
                       :villagers 2}]
            :trash   [estate]}))
    (is (= (-> {:players [{:hand    [recruiter estate estate]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :recruiter)
               (choose :copper))
           {:players [{:hand      [estate estate copper]
                       :play-area [recruiter]
                       :deck      [copper]
                       :actions   0}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [recruiter estate estate]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :recruiter)
               (choose :estate)
               (spend-villager 0))
           {:players [{:hand      [estate copper copper]
                       :play-area [recruiter]
                       :deck      [copper]
                       :actions   1
                       :villagers 1}]
            :trash   [estate]}))
    (is (thrown-with-msg? AssertionError #"You have no Villagers to spend."
                          (-> {:players [{:hand    [recruiter estate estate]
                                          :deck    [copper copper copper]
                                          :actions 1}]}
                              (play 0 :recruiter)
                              (choose :copper)
                              (spend-villager 0))))))

(deftest researcher-test
  (let [researcher (assoc researcher :id 1)
        throne-room (assoc throne-room :id 2)]
    (testing "Researcher"
      (is (= (-> {:players [{:hand    [researcher estate copper copper]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :researcher))
             {:players      [{:hand      [estate copper copper]
                              :play-area [researcher]
                              :deck      [silver silver copper]
                              :actions   1}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :card-id   1
                              :choice    ::renaissance/researcher-trash
                              :source    :hand
                              :options   [:estate :copper :copper]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [researcher estate copper copper]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :researcher)
                 (choose :copper))
             {:players [{:hand      [estate copper]
                         :play-area [researcher]
                         :deck      [silver silver copper]
                         :actions   1}]
              :trash   [copper]}))
      (is (= (-> {:players [{:hand    [researcher estate copper copper]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :researcher)
                 (choose :estate))
             {:players [{:hand      [copper copper]
                         :play-area [(assoc researcher :at-start-turn [[[:put-set-aside-into-hand {:card-name :silver}]
                                                                        [:put-set-aside-into-hand {:card-name :silver}]]]
                                                       :set-aside [silver silver])]
                         :deck      [copper]
                         :actions   1}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand    [researcher estate copper copper]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :researcher)
                 (choose :estate)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           [copper copper copper silver silver]
                                :play-area      [(assoc researcher :set-aside [])]
                                :actions        1
                                :coins          0
                                :buys           1
                                :actions-played 0
                                :phase          :action}]
              :trash          [estate]}))
      (is (= (-> {:players [{:hand    [researcher estate silver copper throne-room]
                             :deck    [silver silver gold estate copper copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :researcher)
                 (choose :estate)
                 (choose :silver)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand           [copper copper silver silver gold estate copper]
                                :play-area      [throne-room
                                                 (assoc researcher :set-aside [])]
                                :actions        1
                                :coins          0
                                :buys           1
                                :actions-played 0
                                :phase          :action}]
              :trash          [estate silver]})))))

(deftest scholar-test
  (testing "Scholar"
    (is (= (-> {:players [{:hand    [scholar estate estate estate]
                           :deck    (repeat 7 copper)
                           :actions 1}]}
               (play 0 :scholar))
           {:players [{:hand      (repeat 7 copper)
                       :play-area [scholar]
                       :discard   [estate estate estate]
                       :actions   0}]}))))

(deftest sculptor-test
  (let [estate (assoc estate :id 1)
        silver (assoc silver :id 2)
        silk-merchant (assoc silk-merchant :id 3)]
    (testing "Sculptor"
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card duchy :pile-size 8}
                            {:card silver :pile-size 40}
                            {:card silk-merchant :pile-size 10}]
                  :players [{:hand    [sculptor]
                             :actions 1}]}
                 (play 0 :sculptor))
             {:supply       [{:card estate :pile-size 8}
                             {:card duchy :pile-size 8}
                             {:card silver :pile-size 40}
                             {:card silk-merchant :pile-size 10}]
              :players      [{:play-area [sculptor]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card to your hand costing up to $4."
                              :player-no 0
                              :choice    ::renaissance/sculptor-gain
                              :source    :supply
                              :options   [:estate :silver :silk-merchant]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [sculptor]
                             :actions 1}]}
                 (play 0 :sculptor)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:hand      [estate]
                         :play-area [sculptor]
                         :actions   0}]}))
      (is (= (-> {:track-gained-cards? true
                  :current-player      0
                  :supply              [{:card silver :pile-size 40}]
                  :players             [{:hand    [sculptor]
                                         :discard [copper]
                                         :actions 1}]}
                 (play 0 :sculptor)
                 (choose :silver))
             {:track-gained-cards? true
              :current-player      0
              :supply              [{:card silver :pile-size 39}]
              :players             [{:hand         [silver]
                                     :play-area    [sculptor]
                                     :discard      [copper]
                                     :actions      0
                                     :villagers    1
                                     :gained-cards [{:name :silver :types #{:treasure} :cost 3}]}]}))
      (is (= (-> {:supply  [{:card silk-merchant :pile-size 10}]
                  :players [{:hand    [sculptor]
                             :actions 1}]}
                 (play 0 :sculptor)
                 (choose :silk-merchant))
             {:supply  [{:card silk-merchant :pile-size 9}]
              :players [{:hand      [silk-merchant]
                         :play-area [sculptor]
                         :actions   0
                         :coffers   1
                         :villagers 1}]})))))

(deftest seer-test
  (testing "Seer"
    (is (= (-> {:players [{:hand    [seer]
                           :deck    [copper estate silver silk-merchant duchy]
                           :actions 1}]}
               (play 0 :seer))
           {:players [{:hand           [copper estate silver silk-merchant]
                       :play-area      [seer]
                       :deck           [duchy]
                       :actions        1
                       :revealed-cards {:hand 3}}]}))
    (is (= (-> {:players [{:hand    [seer]
                           :deck    [estate copper silver duchy silk-merchant]
                           :actions 1}]}
               (play 0 :seer))
           {:players      [{:hand           [estate silver]
                            :play-area      [seer]
                            :deck           [silk-merchant]
                            :revealed       [copper duchy]
                            :actions        1
                            :revealed-cards {:hand 1}}]
            :effect-stack [{:text      "Put the revealed cards back onto your deck in any order."
                            :player-no 0
                            :choice    :topdeck-from-revealed
                            :source    :revealed
                            :options   [:copper :duchy]
                            :min       2
                            :max       2}]}))
    (is (= (-> {:players [{:hand    [seer]
                           :deck    [estate copper silver duchy silk-merchant]
                           :actions 1}]}
               (play 0 :seer)
               (choose [:copper :duchy]))
           {:players [{:hand           [estate silver]
                       :play-area      [seer]
                       :deck           [duchy copper silk-merchant]
                       :actions        1
                       :revealed-cards {:deck 2
                                        :hand 1}}]}))
    (is (= (-> {:cost-reductions [{:reduction 2}]
                :players         [{:hand    [seer]
                                   :deck    [copper gold silver silk-merchant estate]
                                   :actions 1}]}
               (play 0 :seer)
               (choose :silver))
           {:cost-reductions [{:reduction 2}]
            :players         [{:hand           [copper gold silk-merchant]
                               :play-area      [seer]
                               :deck           [silver estate]
                               :actions        1
                               :revealed-cards {:deck 1
                                                :hand 2}}]}))))

(deftest silk-merchant-test
  (let [silk-merchant (assoc silk-merchant :id 1)]
    (testing "Silk Merchant"
      (is (= (-> {:players [{:hand    [silk-merchant]
                             :deck    [copper copper copper]
                             :actions 1
                             :buys    1}]}
                 (play 0 :silk-merchant))
             {:players [{:hand      [copper copper]
                         :play-area [silk-merchant]
                         :deck      [copper]
                         :actions   0
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card silk-merchant :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0
                        :card-name :silk-merchant}))
             {:supply  [{:card silk-merchant :pile-size 9}]
              :players [{:discard   [silk-merchant]
                         :coffers   1
                         :villagers 1}]}))
      (is (= (-> {:players [{:hand    [chapel silk-merchant]
                             :actions 1}]}
                 (play 0 :chapel)
                 (choose :silk-merchant))
             {:players [{:play-area [chapel]
                         :actions   0
                         :coffers   1
                         :villagers 1}]
              :trash   [silk-merchant]}))
      (is (= (-> {:supply  [{:card silk-merchant :pile-size 10}]
                  :players [{:hand    [lurker]
                             :actions 1}]}
                 (play 0 :lurker)
                 (choose :trash)
                 (choose :silk-merchant))
             {:supply  [{:card silk-merchant :pile-size 9}]
              :players [{:play-area [lurker]
                         :actions   1
                         :coffers   1
                         :villagers 1}]
              :trash   [silk-merchant]}))
      (is (= (-> {:supply  [{:card silk-merchant :pile-size 9}]
                  :players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [silk-merchant]}
                 (play 0 :lurker)
                 (choose :gain)
                 (choose :silk-merchant))
             {:supply  [{:card silk-merchant :pile-size 9}]
              :players [{:play-area [lurker]
                         :discard   [silk-merchant]
                         :actions   1
                         :coffers   1
                         :villagers 1}]
              :trash   []})))))

(deftest spices-test
  (let [spices (assoc spices :id 1)]
    (testing "Spices"
      (is (= (-> {:players [{:hand  [spices]
                             :coins 0
                             :buys  1}]}
                 (play 0 :spices))
             {:players [{:play-area [spices]
                         :coins     2
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card spices :pile-size 10}]
                  :players [{:hand [copper copper]}]}
                 (gain {:player-no 0
                        :card-name :spices}))
             {:supply  [{:card spices :pile-size 9}]
              :players [{:hand    [copper copper]
                         :discard [spices]
                         :coffers 2}]})))))

(deftest treasurer-test
  (testing "Treasurer"
    (is (= (-> {:players [{:hand    [treasurer]
                           :actions 1
                           :coins   0}]}
               (play 0 :treasurer))
           {:players      [{:play-area [treasurer]
                            :actions   0
                            :coins     3}]
            :effect-stack [{:text      "Choose one:"
                            :player-no 0
                            :choice    ::renaissance/treasurer-choice
                            :source    :special
                            :options   [{:option :trash :text "Trash a Treasure from your hand."}
                                        {:option :gain :text "Gain a Treasure from the trash to your hand."}
                                        {:option :key :text "Take the Key."}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [treasurer copper estate silver]
                           :actions 1
                           :coins   0}]}
               (play 0 :treasurer)
               (choose :trash))
           {:players      [{:hand      [copper estate silver]
                            :play-area [treasurer]
                            :actions   0
                            :coins     3}]
            :effect-stack [{:text      "Trash a Treasure from your hand."
                            :player-no 0
                            :choice    :trash-from-hand
                            :source    :hand
                            :options   [:copper :silver]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [treasurer]
                           :actions 1
                           :coins   0}]
                :trash   [copper estate gold]}
               (play 0 :treasurer)
               (choose :gain))
           {:players      [{:play-area [treasurer]
                            :actions   0
                            :coins     3}]
            :trash        [copper estate gold]
            :effect-stack [{:text      "Gain a Treasure from the trash to your hand."
                            :player-no 0
                            :choice    :gain-from-trash-to-hand
                            :source    :trash
                            :options   [:copper :gold]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  [{:card ducat :pile-size 9}]
                :players [{:hand    [treasurer copper]
                           :actions 1
                           :coins   0}]
                :trash   [copper estate ducat]}
               (play 0 :treasurer)
               (choose :gain)
               (choose :ducat)
               (choose :copper))
           {:supply  [{:card ducat :pile-size 9}]
            :players [{:hand      [ducat]
                       :play-area [treasurer]
                       :actions   0
                       :coins     3}]
            :trash   [copper estate copper]}))
    (is (= (-> {:artifacts {:key key}
                :players   [{:hand    [treasurer]
                             :actions 1
                             :coins   0}]}
               (play 0 :treasurer)
               (choose :key))
           {:artifacts {:key (assoc key :owner 0)}
            :players   [{:play-area [treasurer]
                         :actions   0
                         :coins     3
                         :triggers  [{:trigger  :at-start-turn
                                      :duration :key
                                      :effects  [[:give-coins 1]]}]}]}))
    (is (= (-> {:artifacts {:key (assoc key :owner 0)}
                :players   [{:triggers [{:trigger  :at-start-turn
                                         :duration :key
                                         :effects  [[:give-coins 1]]}]}]}
               (end-turn 0))
           {:current-player 0
            :artifacts      {:key (assoc key :owner 0)}
            :players        [{:actions        1
                              :coins          1
                              :buys           1
                              :actions-played 0
                              :phase          :action
                              :triggers       [{:trigger  :at-start-turn
                                                :duration :key
                                                :effects  [[:give-coins 1]]}]}]}))
    (let [merchant-ship (assoc merchant-ship :id 1)]
      (is (= (-> {:players [{:hand     [merchant-ship]
                             :actions  1
                             :coins    0
                             :triggers [{:trigger  :at-start-turn
                                         :duration :key
                                         :effects  [[:give-coins 1]]}]}]}
                 (play 0 :merchant-ship)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area      [merchant-ship]
                                :actions        1
                                :coins          3
                                :buys           1
                                :actions-played 0
                                :phase          :action
                                :triggers       [{:trigger  :at-start-turn
                                                  :duration :key
                                                  :effects  [[:give-coins 1]]}]}]})))))

(deftest villain-test
  (testing "Villain"
    (is (= (-> {:players [{:hand    [villain]
                           :actions 1}
                          {:hand [copper copper copper copper silver]}]}
               (play 0 :villain))
           {:players      [{:play-area [villain]
                            :actions   0
                            :coffers   2}
                           {:hand [copper copper copper copper silver]}]
            :effect-stack [{:text      "Discard a card costing $2 or more."
                            :player-no 1
                            :choice    :discard-from-hand
                            :source    :hand
                            :options   [:silver]
                            :min       1
                            :max       1}
                           {:player-no 1
                            :effect    [:clear-unaffected {:works :once}]}]}))
    (is (= (-> {:players [{:hand    [villain]
                           :actions 1}
                          {:hand [copper copper copper copper silver]}]}
               (play 0 :villain)
               (choose :silver))
           {:players [{:play-area [villain]
                       :actions   0
                       :coffers   2}
                      {:hand    [copper copper copper copper]
                       :discard [silver]}]}))
    (is (= (-> {:players [{:hand    [villain]
                           :actions 1}
                          {:hand [copper copper copper silver]}]}
               (play 0 :villain))
           {:players [{:play-area [villain]
                       :actions   0
                       :coffers   2}
                      {:hand [copper copper copper silver]}]}))
    (is (= (-> {:players [{:hand    [villain]
                           :actions 1}
                          {:hand [copper copper copper copper copper]}]}
               (play 0 :villain))
           {:players [{:play-area [villain]
                       :actions   0
                       :coffers   2}
                      {:hand           [copper copper copper copper copper]
                       :revealed-cards {:hand 5}}]}))))