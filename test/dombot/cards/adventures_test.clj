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
                             :coins   0}
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
                              :choice    [:simultaneous-effects-choice {:triggers [{:event   :at-clean-up
                                                                                    :name    :page
                                                                                    :card-id 0
                                                                                    :mode    :optional
                                                                                    :effects (:at-clean-up page)}]}]
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
                              :effect    [:remove-triggers {:event :at-draw-hand}]}
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
                                :phase      :action
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
                         :triggers   [(get-trigger champion)]}]})))))

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
