(ns dombot.cards.adventures-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.adventures :as adventures :refer :all]
            [dombot.cards.dominion :refer [militia]]
            [dombot.cards.intrigue :refer [harem]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest amulet-test
  (let [amulet (assoc amulet :id 0)
        silver (assoc silver :id 1)]
    (testing "Amulet"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [amulet]
                             :actions 1
                             :coins   0}]}
                 (play 0 :amulet)
                 (choose :coin))
             {:players [{:play-area [amulet]
                         :actions   0
                         :coins     1
                         :triggers  [(get-trigger amulet)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [amulet estate]
                             :actions 1}]}
                 (play 0 :amulet)
                 (choose :trash)
                 (choose :estate))
             {:players [{:play-area [amulet]
                         :actions   0
                         :triggers  [(get-trigger amulet)]}]
              :trash   [estate]}))
      (ut/reset-ids!)
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [amulet]
                             :actions 1}]}
                 (play 0 :amulet)
                 (choose :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [amulet]
                         :discard   [silver]
                         :actions   0
                         :triggers  [(get-trigger amulet)]}]}))
      (is (= (-> {:players [{:play-area [amulet]
                             :phase     :buy
                             :triggers  [(get-trigger amulet)]}]}
                 (end-turn 0)
                 (choose :coin))
             {:current-player 0
              :players        [{:play-area [amulet]
                                :actions   1
                                :coins     1
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:players [{:play-area [amulet]
                             :deck      (repeat 5 copper)
                             :phase     :buy
                             :triggers  [(get-trigger amulet)]}]}
                 (end-turn 0)
                 (choose :trash)
                 (choose :copper))
             {:current-player 0
              :players        [{:hand      (repeat 4 copper)
                                :play-area [amulet]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]
              :trash          [copper]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:play-area [amulet]
                             :phase     :buy
                             :triggers  [(get-trigger amulet)]}]}
                 (end-turn 0)
                 (choose :silver))
             {:current-player 0
              :supply         [{:card silver :pile-size 39}]
              :players        [{:play-area [amulet]
                                :discard   [silver]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]})))))

(deftest artificer-test
  (let [artificer (assoc artificer :id 0)
        copper    (assoc copper :id 1)
        silver    (assoc silver :id 2)]
    (testing "Artificer"
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}]
                  :players [{:hand    [artificer estate estate]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :artificer)
                 (choose nil)
                 (choose nil))
             {:supply  [{:card copper :pile-size 46}
                        {:card silver :pile-size 40}]
              :players [{:hand      [estate estate copper]
                         :play-area [artificer]
                         :deck      [copper]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}]
                  :players [{:hand    [artificer estate estate]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :artificer)
                 (choose nil)
                 (choose :copper))
             {:supply  [{:card copper :pile-size 45}
                        {:card silver :pile-size 40}]
              :players [{:hand      [estate estate copper]
                         :play-area [artificer]
                         :deck      [copper copper]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}]
                  :players [{:hand    [artificer estate estate]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :artificer)
                 (choose [:estate :estate :copper])
                 (choose :silver))
             {:supply  [{:card copper :pile-size 46}
                        {:card silver :pile-size 39}]
              :players [{:play-area [artificer]
                         :deck      [silver copper]
                         :discard   [estate estate copper]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}]
                  :players [{:hand    [artificer]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :artificer)
                 (choose nil)
                 (choose nil))
             {:supply  [{:card copper :pile-size 46}
                        {:card silver :pile-size 40}]
              :players [{:hand      [copper]
                         :play-area [artificer]
                         :deck      [copper]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}]
                  :players [{:hand    [artificer]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :artificer)
                 (choose :copper))
             {:supply  [{:card copper :pile-size 46}
                        {:card silver :pile-size 40}]
              :players [{:play-area [artificer]
                         :deck      [copper]
                         :discard   [copper]
                         :actions   1
                         :coins     1}]})))))

(deftest caravan-guard-test
  (let [caravan-guard (assoc caravan-guard :id 0)]
    (testing "Caravan Guard"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [caravan-guard]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :caravan-guard))
             {:players [{:hand      [copper]
                         :play-area [caravan-guard]
                         :deck      [copper]
                         :actions   1
                         :triggers  [(get-trigger caravan-guard)]}]}))
      (is (= (-> {:players [{:play-area [caravan-guard]
                             :phase     :buy
                             :triggers  [(get-trigger caravan-guard)]}]}
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [caravan-guard]
                                :actions   1
                                :coins     1
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:players [{:hand    [militia]
                             :actions 1
                             :coins   0}
                            {:hand [caravan-guard copper copper copper copper]}]}
                 (play 0 :militia)
                 (choose nil)
                 (choose [:copper :copper]))
             {:players [{:play-area [militia]
                         :actions   0
                         :coins     2}
                        {:hand    [caravan-guard copper copper]
                         :discard [copper copper]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [militia]
                             :actions 1
                             :coins   0}
                            {:hand    [caravan-guard copper copper copper copper]
                             :deck    [copper copper]
                             :actions 0
                             :phase   :out-of-turn}]}
                 (play 0 :militia)
                 (choose :caravan-guard)
                 (choose [:copper :copper]))
             {:players [{:play-area [militia]
                         :actions   0
                         :coins     2}
                        {:hand      [copper copper copper]
                         :play-area [caravan-guard]
                         :deck      [copper]
                         :discard   [copper copper]
                         :actions   1
                         :phase     :out-of-turn
                         :triggers  [(get-trigger caravan-guard)]}]}))
      (let [caravan-guard-1 (assoc caravan-guard :id 1)]
        (ut/reset-ids!)
        (is (= (-> {:players [{:hand    [militia]
                               :actions 1
                               :coins   0}
                              {:hand    [caravan-guard copper copper copper copper]
                               :deck    [caravan-guard-1 copper]
                               :actions 0
                               :phase   :out-of-turn}]}
                   (play 0 :militia)
                   (choose :caravan-guard)
                   (choose :caravan-guard)
                   (choose [:copper :copper]))
               {:players [{:play-area [militia]
                           :actions   0
                           :coins     2}
                          {:hand      [copper copper copper]
                           :play-area [caravan-guard caravan-guard-1]
                           :discard   [copper copper]
                           :actions   2
                           :phase     :out-of-turn
                           :triggers  [(get-trigger caravan-guard)
                                       (assoc (get-trigger caravan-guard-1) :id 2)]}]})))
      (is (= (-> {:players [{:hand    [militia]
                             :actions 1
                             :coins   0
                             :phase   :action}
                            {:hand    [caravan-guard copper copper copper copper]
                             :deck    [copper copper]
                             :actions 0
                             :phase   :out-of-turn}]}
                 (play 0 :militia)
                 (choose :caravan-guard)
                 (choose [:copper :copper])
                 (end-turn 0))
             {:current-player 1
              :players        [{:hand    [militia]
                                :actions 0
                                :coins   0
                                :buys    0
                                :phase   :out-of-turn}
                               {:hand      [copper copper copper]
                                :play-area [caravan-guard]
                                :deck      [copper]
                                :discard   [copper copper]
                                :actions   1
                                :coins     1
                                :buys      1
                                :phase     :action}]})))))

(deftest distant-lands-test
  (let [distant-lands (assoc distant-lands :id 0)]
    (testing "Distant Lands"
      (is (= (-> {:players [{:hand    [distant-lands]
                             :actions 1}]}
                 (play 0 :distant-lands))
             {:players [{:tavern-mat [(assoc distant-lands :victory-points 4)]
                         :actions    0}]}))
      (is (= (calc-victory-points {:deck [distant-lands]})
             0))
      (is (= (calc-victory-points {:tavern-mat [(assoc distant-lands :victory-points 4)]})
             4)))))

(deftest dungeon-test
  (let [dungeon (assoc dungeon :id 0)]
    (testing "Dungeon"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [dungeon estate copper copper copper]
                             :deck    [estate copper estate copper]
                             :actions 1}]}
                 (play 0 :dungeon)
                 (choose [:estate :estate]))
             {:players [{:hand      [copper copper copper copper]
                         :play-area [dungeon]
                         :deck      [estate copper]
                         :discard   [estate estate]
                         :actions   1
                         :triggers  [(get-trigger dungeon)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [dungeon estate copper copper copper]
                             :deck    [estate]
                             :actions 1}]}
                 (play 0 :dungeon)
                 (choose [:estate :estate]))
             {:players [{:hand      [copper copper copper]
                         :play-area [dungeon]
                         :discard   [estate estate]
                         :actions   1
                         :triggers  [(get-trigger dungeon)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [dungeon copper]
                             :actions 1}]}
                 (play 0 :dungeon)
                 (choose :copper))
             {:players [{:play-area [dungeon]
                         :discard   [copper]
                         :actions   1
                         :triggers  [(get-trigger dungeon)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [dungeon]
                             :actions 1}]}
                 (play 0 :dungeon))
             {:players [{:play-area [dungeon]
                         :actions   1
                         :triggers  [(get-trigger dungeon)]}]}))
      (is (= (-> {:players [{:play-area [dungeon]
                             :deck      [estate estate copper copper copper copper copper]
                             :actions   1
                             :phase     :buy
                             :triggers  [(get-trigger dungeon)]}]}
                 (end-turn 0)
                 (choose [:estate :estate]))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper]
                                :play-area [dungeon]
                                :discard   [estate estate]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]})))))

(deftest gear-test
  (let [gear (assoc gear :id 0)]
    (testing "Dungeon"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [gear estate copper copper copper]
                             :deck    [estate copper estate copper]
                             :actions 1}]}
                 (play 0 :gear)
                 (choose [:estate :estate]))
             {:players [{:hand      [copper copper copper copper]
                         :play-area [gear]
                         :deck      [estate copper]
                         :actions   0
                         :triggers  [(merge set-aside=>hand-trigger
                                            {:id        1
                                             :card-id   0
                                             :name      :gear
                                             :set-aside [estate estate]})]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [gear estate copper copper copper]
                             :deck    [estate copper estate copper]
                             :actions 1}]}
                 (play 0 :gear)
                 (choose :copper))
             {:players [{:hand      [estate copper copper estate copper]
                         :play-area [gear]
                         :deck      [estate copper]
                         :actions   0
                         :triggers  [(merge set-aside=>hand-trigger
                                            {:id        1
                                             :card-id   0
                                             :name      :gear
                                             :set-aside [copper]})]}]}))
      (is (= (-> {:players [{:hand    [gear estate copper copper copper]
                             :deck    [estate copper estate copper]
                             :actions 1}]}
                 (play 0 :gear)
                 (choose nil))
             {:players [{:hand      [estate copper copper copper estate copper]
                         :play-area [gear]
                         :deck      [estate copper]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [gear estate copper copper copper]
                             :deck    [estate copper estate copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :gear)
                 (choose [:estate :estate])
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [estate copper copper copper copper estate estate]
                                :play-area [gear]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]})))))

(deftest giant-test
  (let [giant (assoc giant :id 0)
        curse (assoc curse :id 1)]
    (testing "Giant"
      (is (= (-> {:players [{:hand          [giant]
                             :actions       1
                             :coins         0
                             :journey-token :face-up}]}
                 (play 0 :giant))
             {:players [{:play-area     [giant]
                         :actions       0
                         :coins         1
                         :journey-token :face-down}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand          [giant]
                             :actions       1
                             :coins         0
                             :journey-token :face-down}
                            {:deck [estate]}]}
                 (play 0 :giant))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:play-area     [giant]
                         :actions       0
                         :coins         5
                         :journey-token :face-up}
                        {:discard [estate curse]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand          [giant]
                             :actions       1
                             :coins         0
                             :journey-token :face-down}
                            {:deck [silver]}]}
                 (play 0 :giant))
             {:supply  [{:card curse :pile-size 10}]
              :players [{:play-area     [giant]
                         :actions       0
                         :coins         5
                         :journey-token :face-up}
                        {}]
              :trash   [silver]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand          [giant]
                             :actions       1
                             :coins         0
                             :journey-token :face-down}
                            {:deck [gold]}]}
                 (play 0 :giant))
             {:supply  [{:card curse :pile-size 10}]
              :players [{:play-area     [giant]
                         :actions       0
                         :coins         5
                         :journey-token :face-up}
                        {}]
              :trash   [gold]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand          [giant]
                             :actions       1
                             :coins         0
                             :journey-token :face-down}
                            {:deck [province]}]}
                 (play 0 :giant))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:play-area     [giant]
                         :actions       0
                         :coins         5
                         :journey-token :face-up}
                        {:discard [province curse]}]})))))

(deftest haunted-woods-test
  (let [haunted-woods (assoc haunted-woods :id 0)]
    (testing "Haunted Woods"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [haunted-woods]
                             :actions 1}]}
                 (play 0 :haunted-woods))
             {:players [{:play-area [haunted-woods]
                         :actions   0
                         :triggers  [(get-trigger haunted-woods)]}]}))
      (is (= (-> {:players [{:play-area [haunted-woods]
                             :deck      (repeat 10 copper)
                             :phase     :buy
                             :triggers  [(get-trigger haunted-woods)]}]}
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      (repeat 8 copper)
                                :play-area [haunted-woods]
                                :deck      [copper copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (let [silver (assoc silver :id 1)]
        (ut/reset-ids!)
        (is (= (-> {:supply  [{:card silver :pile-size 46}]
                    :players [{:hand    [haunted-woods]
                               :actions 1}
                              {:hand [gold estate estate copper]}]}
                   (play 0 :haunted-woods)
                   (end-turn 0)
                   (play 1 :gold)
                   (buy-card 1 :silver)
                   (choose [:copper :estate :estate]))
               {:current-player 1
                :supply         [{:card silver :pile-size 45}]
                :players        [{:play-area [haunted-woods]
                                  :actions   0
                                  :coins     0
                                  :buys      0
                                  :triggers  [(get-trigger haunted-woods)]}
                                 {:play-area [gold]
                                  :deck      [estate estate copper]
                                  :discard   [silver]
                                  :actions   1
                                  :coins     0
                                  :buys      0
                                  :triggers  [(assoc haunted-woods-trigger :id 2
                                                                           :card-id 0)]}]}))
        (is (= (-> {:players [{:hand    [haunted-woods]
                               :actions 1
                               :phase   :action}
                              {}]}
                   (play 0 :haunted-woods)
                   (end-turn 0)
                   (end-turn 1))
               {:current-player 0
                :players        [{:play-area [haunted-woods]
                                  :actions   1
                                  :coins     0
                                  :buys      1
                                  :phase     :action}
                                 {:actions 0
                                  :coins   0
                                  :buys    0}]}))))))

(deftest hireling-test
  (let [hireling (assoc hireling :id 0)]
    (testing "Hireling"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [hireling]
                             :actions 1}]}
                 (play 0 :hireling))
             {:players [{:play-area [hireling]
                         :actions   0
                         :triggers  [(get-trigger hireling)]}]}))
      (is (= (-> {:players [{:play-area [hireling]
                             :deck      (repeat 7 copper)
                             :phase     :buy
                             :triggers  [(get-trigger hireling)]}]}
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      (repeat 6 copper)
                                :play-area [hireling]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action
                                :triggers  [(get-trigger hireling)]}]})))))

(deftest lost-city-test
  (let [lost-city (assoc lost-city :id 0)]
    (testing "Lost City"
      (is (= (-> {:players [{:hand    [lost-city]
                             :deck    [estate silver copper]
                             :actions 1}]}
                 (play 0 :lost-city))
             {:players [{:hand      [estate silver]
                         :play-area [lost-city]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:supply  [{:card lost-city :pile-size 10}]
                  :players [{}
                            {:deck [copper copper]}]}
                 (gain {:player-no 0 :card-name :lost-city}))
             {:supply  [{:card lost-city :pile-size 9}]
              :players [{:discard [lost-city]}
                        {:hand [copper]
                         :deck [copper]}]})))))

(deftest magpie-test
  (let [magpie (assoc magpie :id 0)]
    (testing "Magpie"
      (is (= (-> {:supply  [{:card magpie :pile-size 9}]
                  :players [{:hand    [magpie]
                             :deck    [estate silver copper]
                             :actions 1}]}
                 (play 0 :magpie))
             {:supply  [{:card magpie :pile-size 9}]
              :players [{:hand           [estate silver]
                         :play-area      [magpie]
                         :deck           [copper]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:supply  [{:card magpie :pile-size 9}]
                  :players [{:hand    [magpie]
                             :deck    [estate estate]
                             :actions 1}]}
                 (play 0 :magpie))
             {:supply  [{:card magpie :pile-size 8}]
              :players [{:hand           [estate]
                         :play-area      [magpie]
                         :deck           [estate]
                         :discard        [magpie]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:supply  [{:card magpie :pile-size 8}]
                  :players [{:hand    [magpie]
                             :deck    [estate magpie]
                             :actions 1}]}
                 (play 0 :magpie))
             {:supply  [{:card magpie :pile-size 7}]
              :players [{:hand           [estate]
                         :play-area      [magpie]
                         :deck           [magpie]
                         :discard        [magpie]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:supply  [{:card magpie :pile-size 9}]
                  :players [{:hand    [magpie]
                             :deck    [estate harem]
                             :actions 1}]}
                 (play 0 :magpie))
             {:supply  [{:card magpie :pile-size 8}]
              :players [{:hand           [estate harem]
                         :play-area      [magpie]
                         :discard        [magpie]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:supply  [{:card magpie :pile-size 9}]
                  :players [{:hand    [magpie]
                             :deck    [estate curse]
                             :actions 1}]}
                 (play 0 :magpie))
             {:supply  [{:card magpie :pile-size 9}]
              :players [{:hand           [estate]
                         :play-area      [magpie]
                         :deck           [curse]
                         :revealed-cards {:deck 1}
                         :actions        1}]})))))

(deftest miser-test
  (let [miser (assoc miser :id 0)]
    (testing "Miser"
      (is (= (-> {:players [{:hand    [miser copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :miser)
                 (choose :copper))
             {:players [{:play-area  [miser]
                         :tavern-mat [copper]
                         :actions    0
                         :coins      0}]}))
      (is (= (-> {:players [{:hand    [miser]
                             :actions 1
                             :coins   0}]}
                 (play 0 :miser)
                 (choose :coins))
             {:players [{:play-area [miser]
                         :actions   0
                         :coins     0}]}))
      (is (= (-> {:players [{:hand       [miser]
                             :tavern-mat [copper]
                             :actions    1
                             :coins      0}]}
                 (play 0 :miser)
                 (choose :coins))
             {:players [{:play-area  [miser]
                         :tavern-mat [copper]
                         :actions    0
                         :coins      1}]}))
      (is (= (-> {:players [{:hand       [miser]
                             :tavern-mat (repeat 7 copper)
                             :actions    1
                             :coins      0}]}
                 (play 0 :miser)
                 (choose :coins))
             {:players [{:play-area  [miser]
                         :tavern-mat (repeat 7 copper)
                         :actions    0
                         :coins      7}]})))))

(deftest page-test
  (let [page            (assoc page :id 0)
        treasure-hunter (assoc treasure-hunter :id 1)]
    (testing "Page"
      (is (= (-> {:players [{:hand    [page]
                             :deck    [silver copper]
                             :actions 1}]}
                 (play 0 :page))
             {:players [{:hand      [silver]
                         :play-area [page]
                         :deck      [copper]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [page]
                             :deck    [silver copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :page)
                 (clean-up {:player-no 0}))
             {:players      [{:hand      [silver]
                              :play-area [page]
                              :deck      [copper]
                              :actions   1
                              :phase     :clean-up}]
              :effect-stack [{:player-no 0
                              :text      "You may activate cards, that do something when you discard them from play."
                              :choice    [:simultaneous-effects-choice {:triggers [{:event     :at-clean-up
                                                                                    :name      :page
                                                                                    :card-id   0
                                                                                    :mode      :manual
                                                                                    :optional? true
                                                                                    :effects   (:at-clean-up page)}]}]
                              :source    :mixed
                              :options   [{:area :play-area :card-name :page}]
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
      (is (= (-> {:players [{:hand    [page]
                             :deck    (repeat 7 copper)
                             :actions 1
                             :phase   :action}]}
                 (play 0 :page)
                 (clean-up {:player-no 0})
                 (choose nil))
             {:players [{:hand    (repeat 5 copper)
                         :deck    [copper]
                         :discard [copper page]
                         :actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:extra-cards [{:card treasure-hunter :pile-size 5}]
                  :supply      [{:card page :pile-size 9}]
                  :players     [{:hand    [page]
                                 :deck    (repeat 7 copper)
                                 :actions 1
                                 :phase   :action}]}
                 (play 0 :page)
                 (clean-up {:player-no 0})
                 (choose {:area :play-area :card-name :page}))
             {:extra-cards [{:card treasure-hunter :pile-size 4}]
              :supply      [{:card page :pile-size 10}]
              :players     [{:hand    (repeat 5 copper)
                             :deck    [copper]
                             :discard [treasure-hunter copper]
                             :actions 0
                             :coins   0
                             :buys    0
                             :phase   :out-of-turn}]})))))

(deftest treasure-hunter-test
  (let [treasure-hunter (assoc treasure-hunter :id 0)
        silver          (assoc silver :id 1)
        warrior         (assoc warrior :id 2)]
    (testing "Treasure Hunter"
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [treasure-hunter]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :treasure-hunter))
             {:supply  [{:card silver :pile-size 40}]
              :players [{:play-area [treasure-hunter]
                         :actions   1
                         :coins     1}
                        {}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [treasure-hunter]
                             :actions 1
                             :coins   0}
                            {:gained-cards [{:name  :gold
                                             :types #{:treasure}
                                             :cost  6}]}]}
                 (play 0 :treasure-hunter))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [treasure-hunter]
                         :discard   [silver]
                         :actions   1
                         :coins     1}
                        {:gained-cards [{:name  :gold
                                         :types #{:treasure}
                                         :cost  6}]}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [treasure-hunter]
                             :actions 1
                             :coins   0}
                            {:gained-cards [{:name  :silver
                                             :types #{:treasure}
                                             :cost  3}
                                            {:name  :gold
                                             :types #{:treasure}
                                             :cost  6}]}]}
                 (play 0 :treasure-hunter))
             {:supply  [{:card silver :pile-size 38}]
              :players [{:play-area [treasure-hunter]
                         :discard   [silver silver]
                         :actions   1
                         :coins     1}
                        {:gained-cards [{:name  :silver
                                         :types #{:treasure}
                                         :cost  3}
                                        {:name  :gold
                                         :types #{:treasure}
                                         :cost  6}]}]}))
      (is (= (-> {:players [{:hand    [treasure-hunter]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :treasure-hunter)
                 (clean-up {:player-no 0})
                 (choose nil))
             {:players [{:hand    [treasure-hunter]
                         :actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:extra-cards [{:card treasure-hunter :pile-size 4}
                                {:card warrior :pile-size 5}]
                  :players     [{:hand    [treasure-hunter]
                                 :deck    (repeat 7 copper)
                                 :actions 1
                                 :coins   0
                                 :phase   :action}]}
                 (play 0 :treasure-hunter)
                 (clean-up {:player-no 0})
                 (choose {:area :play-area :card-name :treasure-hunter}))
             {:extra-cards [{:card treasure-hunter :pile-size 5}
                            {:card warrior :pile-size 4}]
              :players     [{:hand    (repeat 5 copper)
                             :deck    [copper copper]
                             :discard [warrior]
                             :actions 0
                             :coins   0
                             :buys    0
                             :phase   :out-of-turn}]})))))

(deftest warrior-test
  (let [warrior (assoc warrior :id 0)
        hero    (assoc hero :id 1)]
    (testing "Warrior"
      (is (= (-> {:players [{:hand    [warrior]
                             :deck    [copper copper]
                             :actions 1}
                            {}]}
                 (play 0 :warrior))
             {:players [{:hand      [copper copper]
                         :play-area [warrior]
                         :actions   0}
                        {}]}))
      (is (= (-> {:players [{:hand      [warrior]
                             :play-area [page magpie]
                             :deck      [copper copper]
                             :actions   1}
                            {:deck [estate silver]}]}
                 (play 0 :warrior))
             {:players [{:hand      [copper copper]
                         :play-area [page magpie warrior]
                         :actions   0}
                        {:discard [estate]}]
              :trash   [silver]}))
      (is (= (-> {:players [{:hand      [warrior]
                             :play-area [page page]
                             :deck      [copper copper]
                             :actions   1}
                            {:deck [copper warrior duchy copper]}]}
                 (play 0 :warrior))
             {:players [{:hand      [copper copper]
                         :play-area [page page warrior]
                         :actions   0}
                        {:deck    [copper]
                         :discard [copper duchy]}]
              :trash   [warrior]}))
      (is (= (-> {:extra-cards [{:card warrior :pile-size 4}
                                {:card hero :pile-size 5}]
                  :players     [{:hand    [warrior]
                                 :actions 1
                                 :phase   :action}]}
                 (play 0 :warrior)
                 (clean-up {:player-no 0})
                 (choose {:area :play-area :card-name :warrior}))
             {:extra-cards [{:card warrior :pile-size 5}
                            {:card hero :pile-size 4}]
              :players     [{:hand    [hero]
                             :actions 0
                             :coins   0
                             :buys    0
                             :phase   :out-of-turn}]})))))

(deftest hero-test
  (let [hero     (assoc hero :id 0)
        champion (assoc champion :id 1)
        gold     (assoc gold :id 2)]
    (testing "Hero"
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [hero]
                             :actions 1
                             :coins   0}]}
                 (play 0 :hero)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [hero]
                         :discard   [gold]
                         :actions   0
                         :coins     2}]}))
      (is (= (-> {:extra-cards [{:card hero :pile-size 4}
                                {:card champion :pile-size 5}]
                  :players     [{:play-area [hero]
                                 :phase     :action}]}
                 (clean-up {:player-no 0})
                 (choose {:area :play-area :card-name :hero}))
             {:extra-cards [{:card hero :pile-size 5}
                            {:card champion :pile-size 4}]
              :players     [{:hand    [champion]
                             :actions 0
                             :coins   0
                             :buys    0
                             :phase   :out-of-turn}]})))))

(deftest champion-test
  (let [champion (assoc champion :id 0)]
    (testing "Champion"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [champion]
                             :actions 1}]}
                 (play 0 :champion))
             {:players [{:play-area  [champion]
                         :actions    1
                         :unaffected [{:card-id 0}]
                         :triggers   [(get-trigger champion)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [champion]
                             :actions 1}]}
                 (play 0 :champion)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area  [champion]
                                :actions    1
                                :coins      0
                                :buys       1
                                :unaffected [{:card-id 0}]
                                :triggers   [(get-trigger champion)]}]}))
      (is (= (-> {:players [{:hand       [page]
                             :play-area  [champion]
                             :deck       [copper]
                             :actions    1
                             :unaffected [{:card-id 0}]
                             :triggers   [(get-trigger champion)]}]}
                 (play 0 :page))
             {:players [{:hand       [copper]
                         :play-area  [champion page]
                         :actions    2
                         :unaffected [{:card-id 0}]
                         :triggers   [(get-trigger champion)]}]}))
      (is (= (-> {:players [{:hand    [warrior]
                             :deck    [copper copper]
                             :actions 1}
                            {:play-area  [champion]
                             :deck       [silver]
                             :unaffected [{:card-id 0}]
                             :triggers   [(get-trigger champion)]}]}
                 (play 0 :warrior))
             {:players [{:hand      [copper copper]
                         :play-area [warrior]
                         :actions   0}
                        {:play-area  [champion]
                         :deck       [silver]
                         :unaffected [{:card-id 0}]
                         :triggers   [(get-trigger champion)]}]}))
      (is (= (-> {:players [{:hand      [copper]
                             :play-area [champion]
                             :actions   1
                             :coins     0
                             :triggers  [(get-trigger champion)]}]}
                 (play 0 :copper))
             {:players [{:play-area [champion copper]
                         :actions   1
                         :coins     1
                         :triggers  [(get-trigger champion)]}]})))))

(deftest port-test
  (let [port (assoc port :id 0)]
    (testing "Port"
      (is (= (-> {:players [{:hand    [port]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :port))
             {:players [{:hand      [copper]
                         :play-area [port]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:supply  [{:card port :pile-size 12}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-card 0 :port))
             {:supply  [{:card port :pile-size 10}]
              :players [{:discard [port port]
                         :coins   0
                         :buys    0}]})))))

(deftest ranger-test
  (let [ranger (assoc ranger :id 0)]
    (testing "Ranger"
      (is (= (-> {:players [{:hand          [ranger]
                             :deck          (repeat 5 copper)
                             :actions       1
                             :buys          1
                             :journey-token :face-up}]}
                 (play 0 :ranger))
             {:players [{:play-area     [ranger]
                         :deck          (repeat 5 copper)
                         :actions       0
                         :buys          2
                         :journey-token :face-down}]}))
      (is (= (-> {:players [{:hand          [ranger]
                             :deck          (repeat 5 copper)
                             :actions       1
                             :buys          1
                             :journey-token :face-down}]}
                 (play 0 :ranger))
             {:players [{:hand          (repeat 5 copper)
                         :play-area     [ranger]
                         :actions       0
                         :buys          2
                         :journey-token :face-up}]})))))

(deftest ratcatcher-test
  (let [ratcatcher (assoc ratcatcher :id 0)]
    (testing "Ratcatcher"
      (is (= (-> {:players [{:hand    [ratcatcher]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :ratcatcher))
             {:players [{:hand       [copper]
                         :deck       [copper]
                         :tavern-mat [ratcatcher]
                         :actions    1}]}))
      (is (= (-> {:players [{:deck       [copper copper copper estate estate]
                             :tavern-mat [ratcatcher]
                             :phase      :action}]}
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand       [copper copper copper estate estate]
                                :tavern-mat [ratcatcher]
                                :actions    1
                                :coins      0
                                :buys       1
                                :phase      :action}]
              :effect-stack   [{:text      "One things happen at the start of your turn. Select which one happens next."
                                :player-no 0
                                :choice    [:simultaneous-effects-choice {:triggers [(get-call-trigger ratcatcher)]}]
                                :source    :mixed
                                :options   [{:area :tavern-mat :card-name :ratcatcher}]
                                :max       1}
                               {:player-no 0
                                :effect    [:sync-repeated-play]}]}))
      (is (= (-> {:players [{:deck       [copper copper copper estate estate]
                             :tavern-mat [ratcatcher]
                             :phase      :action}]}
                 (end-turn 0)
                 (choose nil))
             {:current-player 0
              :players        [{:hand       [copper copper copper estate estate]
                                :tavern-mat [ratcatcher]
                                :actions    1
                                :coins      0
                                :buys       1
                                :phase      :action}]}))
      (is (= (-> {:players [{:deck       [copper copper copper estate estate]
                             :tavern-mat [ratcatcher]
                             :phase      :action}]}
                 (end-turn 0)
                 (choose {:area :tavern-mat :card-name :ratcatcher})
                 (choose :estate))
             {:current-player 0
              :players        [{:hand      [copper copper copper estate]
                                :play-area [ratcatcher]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]
              :trash          [estate]})))))

(deftest raze-test
  (let [raze (assoc raze :id 0)]
    (testing "Raze"
      (is (= (-> {:players [{:hand    [raze estate]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 (play 0 :raze))
             {:players      [{:hand      [estate]
                              :play-area [raze]
                              :deck      [copper copper copper]
                              :actions   1}]
              :effect-stack [{:text      "Trash Raze or a card from your hand."
                              :player-no 0
                              :card-id   0
                              :choice    ::adventures/raze-trash-from-area
                              :source    :mixed
                              :options   [{:area :play-area :card-name :raze}
                                          {:area :hand :card-name :estate}]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [raze silver]
                             :deck    [copper copper gold copper]
                             :actions 1}]}
                 (play 0 :raze)
                 (choose {:area :play-area :card-name :raze}))
             {:players      [{:hand    [silver]
                              :look-at [copper copper]
                              :deck    [gold copper]
                              :actions 1}]
              :effect-stack [{:text      "Look at 2 cards from the top of your deck. Put one of them into your hand and discard the rest."
                              :player-no 0
                              :choice    :take-from-look-at
                              :source    :look-at
                              :options   [:copper :copper]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:discard-all-look-at]}]
              :trash        [raze]}))
      (is (= (-> {:players [{:hand    [raze silver]
                             :deck    [copper copper gold copper]
                             :actions 1}]}
                 (play 0 :raze)
                 (choose {:area :hand :card-name :silver}))
             {:players      [{:play-area [raze]
                              :look-at   [copper copper gold]
                              :deck      [copper]
                              :actions   1}]
              :effect-stack [{:text      "Look at 3 cards from the top of your deck. Put one of them into your hand and discard the rest."
                              :player-no 0
                              :choice    :take-from-look-at
                              :source    :look-at
                              :options   [:copper :copper :gold]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:discard-all-look-at]}]
              :trash        [silver]}))
      (is (= (-> {:players [{:hand    [raze silver]
                             :deck    [copper copper gold copper]
                             :actions 1}]}
                 (play 0 :raze)
                 (choose {:area :hand :card-name :silver})
                 (choose :gold))
             {:players [{:hand      [gold]
                         :play-area [raze]
                         :deck      [copper]
                         :discard   [copper copper]
                         :actions   1}]
              :trash   [silver]}))
      (is (= (-> {:players [{:hand    [raze]
                             :actions 1}]}
                 (play 0 :raze)
                 (choose {:area :play-area :card-name :raze}))
             {:players [{:actions 1}]
              :trash   [raze]}))
      (is (= (-> {:players [{:hand    [raze copper]
                             :deck    [gold]
                             :actions 1}]}
                 (play 0 :raze)
                 (choose {:area :hand :card-name :copper}))
             {:players [{:play-area [raze]
                         :deck      [gold]
                         :actions   1}]
              :trash   [copper]})))))

(deftest swamp-hag-test
  (let [swamp-hag (assoc swamp-hag :id 0)]
    (testing "Swamp Hag"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [swamp-hag]
                             :actions 1}]}
                 (play 0 :swamp-hag))
             {:players [{:play-area [swamp-hag]
                         :actions   0
                         :triggers  [(get-trigger swamp-hag)]}]}))
      (is (= (-> {:players [{:play-area [swamp-hag]
                             :phase     :buy
                             :triggers  [(get-trigger swamp-hag)]}]}
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [swamp-hag]
                                :actions   1
                                :coins     3
                                :buys      1
                                :phase     :action}]}))
      (let [silver (assoc silver :id 1)
            curse  (assoc curse :id 2)]
        (ut/reset-ids!)
        (is (= (-> {:supply  [{:card silver :pile-size 46}
                              {:card curse :pile-size 10}]
                    :players [{:hand    [swamp-hag]
                               :actions 1}
                              {:hand [gold]}]}
                   (play 0 :swamp-hag)
                   (end-turn 0)
                   (play 1 :gold)
                   (buy-card 1 :silver))
               {:current-player 1
                :supply         [{:card silver :pile-size 45}
                                 {:card curse :pile-size 9}]
                :players        [{:play-area [swamp-hag]
                                  :actions   0
                                  :coins     0
                                  :buys      0
                                  :triggers  [(get-trigger swamp-hag)]}
                                 {:play-area [gold]
                                  :discard   [curse silver]
                                  :actions   1
                                  :coins     0
                                  :buys      0
                                  :triggers  [(assoc swamp-hag-trigger :id 2
                                                                       :card-id 0)]}]}))
        (is (= (-> {:players [{:hand    [swamp-hag]
                               :actions 1
                               :phase   :action}
                              {}]}
                   (play 0 :swamp-hag)
                   (end-turn 0)
                   (end-turn 1))
               {:current-player 0
                :players        [{:play-area [swamp-hag]
                                  :actions   1
                                  :coins     3
                                  :buys      1
                                  :phase     :action}
                                 {:actions 0
                                  :coins   0
                                  :buys    0}]}))))))

(deftest treasure-trove-test
  (let [treasure-trove (assoc treasure-trove :id 0)
        copper         (assoc copper :id 1)
        gold           (assoc gold :id 2)]
    (testing "Treasure Trove"
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card gold :pile-size 30}]
                  :players [{:hand  [treasure-trove]
                             :coins 0}]}
                 (play 0 :treasure-trove))
             {:supply  [{:card copper :pile-size 45}
                        {:card gold :pile-size 29}]
              :players [{:play-area [treasure-trove]
                         :discard   [gold copper]
                         :coins     2}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 16}
                            {:card gold :pile-size 0}]
                  :players [{:hand  [treasure-trove]
                             :coins 0}]}
                 (play 0 :treasure-trove))
             {:supply  [{:card copper :pile-size 15}
                        {:card gold :pile-size 0}]
              :players [{:play-area [treasure-trove]
                         :discard   [copper]
                         :coins     2}]})))))

(deftest alms-test
  (let [silver (assoc silver :id 1)]
    (testing "Alms"
      (ut/reset-ids!)
      (is (= (-> {:events  {:alms alms}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:coins 0
                             :buys  1}]}
                 (buy-event 0 :alms)
                 (choose :silver))
             {:events  {:alms alms}
              :supply  [{:card silver :pile-size 39}]
              :players [{:discard       [silver]
                         :coins         0
                         :buys          0
                         :bought-events #{:alms}}]}))
      (is (= (-> {:events  {:alms alms}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:play-area [copper]
                             :coins     0
                             :buys      1}]}
                 (buy-event 0 :alms))
             {:events  {:alms alms}
              :supply  [{:card silver :pile-size 40}]
              :players [{:play-area     [copper]
                         :coins         0
                         :buys          0
                         :bought-events #{:alms}}]}))
      (is (thrown-with-msg? AssertionError #"Buy error: Event Alms can only be bought once per turn."
                            (-> {:events  {:alms alms}
                                 :supply  [{:card silver :pile-size 40}]
                                 :players [{:coins 0
                                            :buys  2}]}
                                (buy-event 0 :alms)
                                (choose :silver)
                                (buy-event 0 :alms)))))))

(deftest bonfire-test
  (testing "Bonfire"
    (is (= (-> {:events  {:bonfire bonfire}
                :players [{:hand      [estate]
                           :play-area [copper copper copper]
                           :coins     3
                           :buys      1}]}
               (buy-event 0 :bonfire))
           {:events       {:bonfire bonfire}
            :players      [{:hand      [estate]
                            :play-area [copper copper copper]
                            :coins     0
                            :buys      0}]
            :effect-stack [{:text      "Trash up to 2 cards you have in play."
                            :player-no 0
                            :choice    :trash-from-play-area
                            :source    :play-area
                            :options   [:copper :copper :copper]
                            :max       2}]}))
    (is (= (-> {:events  {:bonfire bonfire}
                :players [{:hand      [estate]
                           :play-area [copper copper copper]
                           :coins     3
                           :buys      1}]}
               (buy-event 0 :bonfire)
               (choose nil))
           {:events  {:bonfire bonfire}
            :players [{:hand      [estate]
                       :play-area [copper copper copper]
                       :coins     0
                       :buys      0}]}))
    (is (= (-> {:events  {:bonfire bonfire}
                :players [{:hand      [estate]
                           :play-area [copper copper copper]
                           :coins     3
                           :buys      1}]}
               (buy-event 0 :bonfire)
               (choose :copper))
           {:events  {:bonfire bonfire}
            :players [{:hand      [estate]
                       :play-area [copper copper]
                       :coins     0
                       :buys      0}]
            :trash   [copper]}))
    (is (= (-> {:events  {:bonfire bonfire}
                :players [{:hand      [estate]
                           :play-area [copper copper copper]
                           :coins     3
                           :buys      1}]}
               (buy-event 0 :bonfire)
               (choose [:copper :copper]))
           {:events  {:bonfire bonfire}
            :players [{:hand      [estate]
                       :play-area [copper]
                       :coins     0
                       :buys      0}]
            :trash   [copper copper]}))))

(deftest expedition-test
  (testing "Expedition"
    (ut/reset-ids!)
    (is (= (-> {:events  {:expedition expedition}
                :players [{:deck  (repeat 8 copper)
                           :coins 3
                           :buys  1
                           :phase :buy}]}
               (buy-event 0 :expedition)
               (clean-up {:player-no 0}))
           {:events  {:expedition expedition}
            :players [{:hand    (repeat 7 copper)
                       :deck    [copper]
                       :actions 0
                       :coins   0
                       :buys    0
                       :phase   :out-of-turn}]}))
    (is (= (-> {:events  {:expedition expedition}
                :players [{:deck  (repeat 10 copper)
                           :coins 6
                           :buys  2
                           :phase :buy}]}
               (buy-event 0 :expedition)
               (buy-event 0 :expedition)
               (clean-up {:player-no 0}))
           {:events  {:expedition expedition}
            :players [{:hand    (repeat 9 copper)
                       :deck    [copper]
                       :actions 0
                       :coins   0
                       :buys    0
                       :phase   :out-of-turn}]}))))

(deftest pilgrimage-test
  (let [gold   (assoc gold :id 1)
        copper (assoc copper :id 2)
        ranger (assoc ranger :id 3)]
    (testing "Pilgrimage"
      (is (= (-> {:events  {:pilgrimage pilgrimage}
                  :players [{:coins         4
                             :buys          1
                             :journey-token :face-up}]}
                 (buy-event 0 :pilgrimage))
             {:events  {:pilgrimage pilgrimage}
              :players [{:coins         0
                         :buys          0
                         :bought-events #{:pilgrimage}
                         :journey-token :face-down}]}))
      (is (= (-> {:events  {:pilgrimage pilgrimage}
                  :players [{:coins         4
                             :buys          1
                             :journey-token :face-down}]}
                 (buy-event 0 :pilgrimage))
             {:events  {:pilgrimage pilgrimage}
              :players [{:coins         0
                         :buys          0
                         :bought-events #{:pilgrimage}
                         :journey-token :face-up}]}))
      (is (= (-> {:events  {:pilgrimage pilgrimage}
                  :supply  [{:card gold :pile-size 29}]
                  :players [{:play-area     [gold]
                             :coins         4
                             :buys          1
                             :journey-token :face-down}]}
                 (buy-event 0 :pilgrimage)
                 (choose nil))
             {:events  {:pilgrimage pilgrimage}
              :supply  [{:card gold :pile-size 29}]
              :players [{:play-area     [gold]
                         :coins         0
                         :buys          0
                         :bought-events #{:pilgrimage}
                         :journey-token :face-up}]}))
      (is (= (-> {:events  {:pilgrimage pilgrimage}
                  :supply  [{:card gold :pile-size 29}]
                  :players [{:play-area     [gold]
                             :coins         4
                             :buys          1
                             :journey-token :face-down}]}
                 (buy-event 0 :pilgrimage)
                 (choose :gold))
             {:events  {:pilgrimage pilgrimage}
              :supply  [{:card gold :pile-size 28}]
              :players [{:play-area     [gold]
                         :discard       [gold]
                         :coins         0
                         :buys          0
                         :bought-events #{:pilgrimage}
                         :journey-token :face-up}]}))
      (is (= (-> {:events  {:pilgrimage pilgrimage}
                  :supply  [{:card gold :pile-size 28}
                            {:card ranger :pile-size 9}]
                  :players [{:play-area     [gold gold ranger copper]
                             :coins         7
                             :buys          2
                             :journey-token :face-down}]}
                 (buy-event 0 :pilgrimage)
                 (choose [:gold :ranger]))
             {:events  {:pilgrimage pilgrimage}
              :supply  [{:card gold :pile-size 27}
                        {:card ranger :pile-size 8}]
              :players [{:play-area     [gold gold ranger copper]
                         :discard       [gold ranger]
                         :coins         3
                         :buys          1
                         :bought-events #{:pilgrimage}
                         :journey-token :face-up}]}))
      (is (= (-> {:events  {:pilgrimage pilgrimage}
                  :supply  [{:card copper :pile-size 46}
                            {:card gold :pile-size 28}
                            {:card ranger :pile-size 9}]
                  :players [{:play-area     [gold gold ranger copper]
                             :coins         7
                             :buys          2
                             :journey-token :face-down}]}
                 (buy-event 0 :pilgrimage)
                 (choose [:gold :ranger :copper]))
             {:events  {:pilgrimage pilgrimage}
              :supply  [{:card copper :pile-size 45}
                        {:card gold :pile-size 27}
                        {:card ranger :pile-size 8}]
              :players [{:play-area     [gold gold ranger copper]
                         :discard       [gold ranger copper]
                         :coins         3
                         :buys          1
                         :bought-events #{:pilgrimage}
                         :journey-token :face-up}]}))
      (is (thrown-with-msg? AssertionError #"Pilgrimage error: All choices must be different: Gold, Ranger, Gold"
                            (-> {:events  {:pilgrimage pilgrimage}
                                 :supply  [{:card gold :pile-size 28}
                                           {:card ranger :pile-size 9}]
                                 :players [{:play-area     [gold gold ranger copper]
                                            :coins         7
                                            :buys          2
                                            :journey-token :face-down}]}
                                (buy-event 0 :pilgrimage)
                                (choose [:gold :ranger :gold]))))
      (is (thrown-with-msg? AssertionError #"Buy error: Event Pilgrimage can only be bought once per turn."
                            (-> {:events  {:pilgrimage pilgrimage}
                                 :supply  [{:card gold :pile-size 28}
                                           {:card ranger :pile-size 9}]
                                 :players [{:play-area     [gold gold ranger silver]
                                            :coins         8
                                            :buys          2
                                            :journey-token :face-up}]}
                                (buy-event 0 :pilgrimage)
                                (buy-event 0 :pilgrimage)))))))

(deftest quest-test
  (let [gold (assoc gold :id 1)]
    (testing "Trade"
      (is (= (-> {:events  {:quest quest}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:hand  (repeat 5 copper)
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :quest)
                 (choose :attack))
             {:events  {:quest quest}
              :supply  [{:card gold :pile-size 30}]
              :players [{:hand  (repeat 5 copper)
                         :coins 0
                         :buys  0}]}))
      (is (= (-> {:events  {:quest quest}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:hand  [warrior]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :quest)
                 (choose :attack)
                 (choose :warrior))
             {:events  {:quest quest}
              :supply  [{:card gold :pile-size 29}]
              :players [{:discard [warrior gold]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:events  {:quest quest}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:hand  [curse]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :quest)
                 (choose :curses)
                 (choose :curse))
             {:events  {:quest quest}
              :supply  [{:card gold :pile-size 30}]
              :players [{:discard [curse]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:events  {:quest quest}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:hand  [curse curse]
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :quest)
                 (choose :curses)
                 (choose [:curse :curse]))
             {:events  {:quest quest}
              :supply  [{:card gold :pile-size 29}]
              :players [{:discard [curse curse gold]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:events  {:quest quest}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:hand  (repeat 5 copper)
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :quest)
                 (choose :six-cards)
                 (choose (repeat 5 :copper)))
             {:events  {:quest quest}
              :supply  [{:card gold :pile-size 30}]
              :players [{:discard (repeat 5 copper)
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:events  {:quest quest}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:hand  (repeat 6 copper)
                             :coins 0
                             :buys  1}]}
                 (buy-event 0 :quest)
                 (choose :six-cards)
                 (choose (repeat 6 :copper)))
             {:events  {:quest quest}
              :supply  [{:card gold :pile-size 29}]
              :players [{:discard [copper copper copper copper copper copper gold]
                         :coins   0
                         :buys    0}]})))))

(deftest save-test
  (testing "Save"
    (ut/reset-ids!)
    (is (= (-> {:events  {:save save}
                :players [{:hand  [copper]
                           :coins 1
                           :buys  1}]}
               (buy-event 0 :save)
               (choose :copper))
           {:events  {:save save}
            :players [{:hand          []
                       :coins         0
                       :buys          1
                       :bought-events #{:save}
                       :triggers      [(assoc save-trigger :id 1
                                                           :set-aside [copper])]}]}))
    (is (thrown-with-msg? AssertionError #"Buy error: Event Save can only be bought once per turn."
                          (-> {:events  {:save save}
                               :players [{:hand  [copper copper]
                                          :coins 2
                                          :buys  1}]}
                              (buy-event 0 :save)
                              (choose :copper)
                              (buy-event 0 :save))))
    (is (= (-> {:events  {:save save}
                :players [{:hand  [copper]
                           :deck  (repeat 5 copper)
                           :coins 1
                           :buys  1
                           :phase :buy}]}
               (buy-event 0 :save)
               (choose :copper)
               (clean-up {:player-no 0}))
           {:events  {:save save}
            :players [{:hand    (repeat 6 copper)
                       :actions 0
                       :coins   0
                       :buys    0
                       :phase   :out-of-turn}]}))))

(deftest scouting-party-test
  (testing "Scouting Party"
    (ut/reset-ids!)
    (is (= (-> {:events  {:scouting-party scouting-party}
                :players [{:deck  (repeat 7 copper)
                           :coins 5
                           :buys  1}]}
               (buy-event 0 :scouting-party)
               (choose [:copper :copper :copper]))
           {:events  {:scouting-party scouting-party}
            :players [{:deck    (repeat 4 copper)
                       :discard [copper copper copper]
                       :coins   3
                       :buys    1}]}))
    (is (= (-> {:events  {:scouting-party scouting-party}
                :players [{:deck  (repeat 4 copper)
                           :coins 5
                           :buys  1}]}
               (buy-event 0 :scouting-party)
               (choose [:copper :copper :copper]))
           {:events  {:scouting-party scouting-party}
            :players [{:deck    [copper]
                       :discard [copper copper copper]
                       :coins   3
                       :buys    1}]}))
    (is (= (-> {:events  {:scouting-party scouting-party}
                :players [{:deck  [copper copper]
                           :coins 5
                           :buys  1}]}
               (buy-event 0 :scouting-party)
               (choose [:copper :copper]))
           {:events  {:scouting-party scouting-party}
            :players [{:discard [copper copper]
                       :coins   3
                       :buys    1}]}))))

(deftest trade-test
  (let [silver (assoc silver :id 1)]
    (testing "Trade"
      (is (= (-> {:events  {:trade trade}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:hand  [copper estate]
                             :coins 5
                             :buys  1}]}
                 (buy-event 0 :trade)
                 (choose nil))
             {:events  {:trade trade}
              :supply  [{:card silver :pile-size 40}]
              :players [{:hand  [copper estate]
                         :coins 0
                         :buys  0}]}))
      (is (= (-> {:events  {:trade trade}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:hand  [copper estate]
                             :coins 5
                             :buys  1}]}
                 (buy-event 0 :trade)
                 (choose :estate))
             {:events  {:trade trade}
              :supply  [{:card silver :pile-size 39}]
              :players [{:hand    [copper]
                         :discard [silver]
                         :coins   0
                         :buys    0}]
              :trash   [estate]}))
      (is (= (-> {:events  {:trade trade}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:hand  [copper estate]
                             :coins 5
                             :buys  1}]}
                 (buy-event 0 :trade)
                 (choose [:estate :copper]))
             {:events  {:trade trade}
              :supply  [{:card silver :pile-size 38}]
              :players [{:discard [silver silver]
                         :coins   0
                         :buys    0}]
              :trash   [estate copper]}))
      (is (= (-> {:events  {:trade trade}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:hand  [estate]
                             :coins 5
                             :buys  1}]}
                 (buy-event 0 :trade)
                 (choose nil))
             {:events  {:trade trade}
              :supply  [{:card silver :pile-size 40}]
              :players [{:hand  [estate]
                         :coins 0
                         :buys  0}]}))
      (is (= (-> {:events  {:trade trade}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:hand  [estate]
                             :coins 5
                             :buys  1}]}
                 (buy-event 0 :trade)
                 (choose :estate))
             {:events  {:trade trade}
              :supply  [{:card silver :pile-size 39}]
              :players [{:discard [silver]
                         :coins   0
                         :buys    0}]
              :trash   [estate]})))))

(deftest travelling-fair-test
  (let [silver (assoc silver :id 1)]
    (testing "Travelling Fair"
      (ut/reset-ids!)
      (is (= (-> {:events  {:travelling-fair travelling-fair}
                  :players [{:coins 5
                             :buys  1}]}
                 (buy-event 0 :travelling-fair))
             {:events  {:travelling-fair travelling-fair}
              :players [{:coins    3
                         :buys     2
                         :triggers [(assoc travelling-fair-trigger :id 1)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:events  {:travelling-fair travelling-fair}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:coins 5
                             :buys  1}]}
                 (buy-event 0 :travelling-fair)
                 (buy-card 0 :silver)
                 (choose nil))
             {:events  {:travelling-fair travelling-fair}
              :supply  [{:card silver :pile-size 39}]
              :players [{:discard  [silver]
                         :coins    0
                         :buys     1
                         :triggers [(assoc travelling-fair-trigger :id 1)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:events  {:travelling-fair travelling-fair}
                  :supply  [{:card silver :pile-size 40}]
                  :players [{:coins 5
                             :buys  1}]}
                 (buy-event 0 :travelling-fair)
                 (buy-card 0 :silver)
                 (choose :silver))
             {:events  {:travelling-fair travelling-fair}
              :supply  [{:card silver :pile-size 39}]
              :players [{:deck     [silver]
                         :coins    0
                         :buys     1
                         :triggers [(assoc travelling-fair-trigger :id 1)]}]})))))
