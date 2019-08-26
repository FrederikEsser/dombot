(ns dombot.cards.renaissance-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :refer [mine throne-room chapel]]
            [dombot.cards.intrigue :as intrigue :refer [courtier ironworks lurker swindler]]
            [dombot.cards.seaside :refer [merchant-ship]]
            [dombot.cards.prosperity :refer [bank loan mint peddler venture watchtower]]
            [dombot.cards.promos :refer [stash]]
            [dombot.cards.renaissance :as renaissance :refer :all]
            [dombot.utils :as ut])
  (:refer-clojure :exclude [key]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(defn get-project-trigger [{:keys [name type trigger]}]
  (merge (if (= :project type)
           {:name     name
            :duration :game}
           {:duration name})
         trigger))

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
                           :triggers       [(get-project-trigger horn)]}]}))
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
                              :triggers       [(get-project-trigger horn)]}]
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
                              :effect    [:remove-triggers {:event :at-draw-hand}]}
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
              :players   [{:hand     (repeat 5 copper)
                           :deck     (repeat 2 copper)
                           :discard  [lackeys lackeys border-guard]
                           :actions  0
                           :coins    0
                           :buys     0
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger horn)]}]}))
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
              :players   [{:hand     [border-guard copper copper copper copper]
                           :deck     (repeat 3 copper)
                           :discard  [lackeys lackeys border-guard]
                           :actions  0
                           :coins    0
                           :buys     0
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger horn)]}]}))))
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
                                        :discard 1}}]}))
  (is (= (-> {:artifacts {:horn    (assoc horn :owner 0)
                          :lantern lantern}
              :players   [{:triggers [(get-project-trigger horn)]}]}
             (clean-up {:player-no 0}))
         {:artifacts {:horn    (assoc horn :owner 0)
                      :lantern lantern}
          :players   [{:actions  0
                       :coins    0
                       :buys     0
                       :phase    :out-of-turn
                       :triggers [(get-project-trigger horn)]}]})))

(deftest cargo-ship-test
  (let [cargo-ship     (assoc cargo-ship :id 1)
        cargo-ship-2   (assoc cargo-ship :id 2)
        border-guard   (assoc border-guard :id 3)
        border-guard-4 (assoc border-guard :id 4)
        gold           (assoc gold :id 5)
        inventor       (assoc inventor :id 6)
        improve        (assoc improve :id 7)
        throne-room    (assoc throne-room :id 8)]
    (testing "Cargo Ship"
      (is (= (-> {:players [{:hand    [cargo-ship]
                             :actions 1
                             :coins   0}]}
                 (play 0 :cargo-ship))
             {:players [{:play-area [cargo-ship]
                         :actions   0
                         :coins     2
                         :triggers  [(assoc cargo-ship-trigger :card-id 1)]}]}))
      (is (= (-> {:players [{:hand    [cargo-ship]
                             :deck    (repeat 5 copper)
                             :actions 1
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand    (repeat 5 copper)
                                :discard [cargo-ship]
                                :actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [cargo-ship]
                             :discard [copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (gain {:player-no 0 :card-name :gold}))
             {:supply       [{:card gold :pile-size 29}]
              :players      [{:play-area [cargo-ship]
                              :gaining   [gold]
                              :discard   [copper]
                              :actions   0
                              :coins     2
                              :triggers  [(assoc cargo-ship-trigger :card-id 1)]}]
              :effect-stack [{:text      "You may set the gained Gold aside on Cargo Ship."
                              :player-no 0
                              :card-id   1
                              :choice    [::renaissance/cargo-ship-set-aside {:gained-card-id 5}]
                              :source    :gaining
                              :options   [:gold]
                              :max       1}
                             {:player-no 0
                              :effect    [:remove-triggers {:event :on-gain}]}
                             {:player-no 0
                              :effect    [:finalize-gain {:player-no      0
                                                          :card-name      :gold
                                                          :gained-card-id 5}]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [cargo-ship]
                             :discard [copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (gain {:player-no 0 :card-name :gold})
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [cargo-ship]
                         :discard   [copper]
                         :actions   0
                         :coins     2
                         :triggers  [(merge set-aside=>hand-trigger {:card-id   1
                                                                     :set-aside [gold]})]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [cargo-ship]
                             :discard [copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (gain {:player-no 0 :card-name :gold})
                 (choose nil))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [cargo-ship]
                         :discard   [copper gold]
                         :actions   0
                         :coins     2
                         :triggers  [(assoc cargo-ship-trigger :card-id 1)]}]}))
      (is (= (-> {:supply  [{:card border-guard :pile-size 10}]
                  :players [{:hand    [cargo-ship sculptor]
                             :discard [copper]
                             :actions 2
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (play 0 :sculptor)
                 (choose :border-guard))
             {:supply       [{:card border-guard :pile-size 9}]
              :players      [{:gaining   [border-guard]
                              :play-area [cargo-ship sculptor]
                              :discard   [copper]
                              :actions   0
                              :coins     2
                              :triggers  [(assoc cargo-ship-trigger :card-id 1)]}]
              :effect-stack [{:text      "You may set the gained Border Guard aside on Cargo Ship."
                              :player-no 0
                              :card-id   1
                              :choice    [::renaissance/cargo-ship-set-aside {:gained-card-id 3}]
                              :source    :gaining
                              :options   [:border-guard]
                              :max       1}
                             {:player-no 0
                              :effect    [:remove-triggers {:event :on-gain}]}
                             {:player-no 0
                              :effect    [:finalize-gain {:player-no      0
                                                          :card-name      :border-guard
                                                          :gained-card-id 3
                                                          :to             :hand}]}]}))
      (is (= (-> {:supply  [{:card border-guard :pile-size 10}]
                  :players [{:hand    [cargo-ship sculptor]
                             :discard [copper]
                             :actions 2
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (play 0 :sculptor)
                 (choose :border-guard)
                 (choose :border-guard))
             {:supply  [{:card border-guard :pile-size 9}]
              :players [{:play-area [cargo-ship
                                     sculptor]
                         :discard   [copper]
                         :actions   0
                         :coins     2
                         :triggers  [(merge set-aside=>hand-trigger {:card-id   1
                                                                     :set-aside [border-guard]})]}]}))
      (is (= (-> {:supply  [{:card border-guard :pile-size 10}]
                  :players [{:hand    [cargo-ship sculptor border-guard-4]
                             :discard [copper]
                             :actions 2
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (play 0 :sculptor)
                 (choose :border-guard)
                 (choose :border-guard))
             {:supply  [{:card border-guard :pile-size 9}]
              :players [{:hand      [border-guard-4]
                         :play-area [cargo-ship
                                     sculptor]
                         :discard   [copper]
                         :actions   0
                         :coins     2
                         :triggers  [(merge set-aside=>hand-trigger {:card-id   1
                                                                     :set-aside [border-guard]})]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [cargo-ship cargo-ship-2]
                             :discard [copper]
                             :actions 2
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (play 0 :cargo-ship)
                 (gain {:player-no 0 :card-name :gold})
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [cargo-ship cargo-ship-2]
                         :discard   [copper]
                         :actions   0
                         :coins     4
                         :triggers  [(assoc cargo-ship-trigger :card-id 2)
                                     (merge set-aside=>hand-trigger {:card-id   1
                                                                     :set-aside [gold]})]}]}))
      (is (= (-> {:supply  [{:card border-guard :pile-size 10}]
                  :players [{:hand    [cargo-ship inventor]
                             :discard [copper]
                             :actions 2
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (play 0 :inventor)
                 (choose :border-guard)
                 (choose :border-guard))
             {:cost-reductions [{:reduction 1}]
              :supply          [{:card border-guard :pile-size 9}]
              :players         [{:play-area [cargo-ship inventor]
                                 :discard   [copper]
                                 :actions   0
                                 :coins     2
                                 :triggers  [(merge set-aside=>hand-trigger {:card-id   1
                                                                             :set-aside [border-guard]})]}]}))
      (is (= (-> {:supply  [{:card inventor :pile-size 10}]
                  :players [{:hand    [cargo-ship improve]
                             :deck    (repeat 5 copper)
                             :actions 2
                             :coins   0}]}
                 (play 0 :cargo-ship)
                 (play 0 :improve)
                 (clean-up {:player-no 0})
                 (choose :improve)
                 (choose :cargo-ship)                       ; improve Cargo Ship
                 (choose :inventor)                         ; gain Inventor
                 (choose :inventor))                        ; put Inventor on vanished Cargo Ship
             {:supply  [{:card inventor :pile-size 9}]
              :players [{:hand     (repeat 5 copper)
                         :discard  [improve]
                         :actions  0
                         :coins    0
                         :buys     0
                         :phase    :out-of-turn
                         :triggers [(merge set-aside=>hand-trigger {:card-id   1
                                                                    :set-aside [inventor]})]}]
              :trash   [cargo-ship]}))
      (is (= (-> {:supply  [{:card inventor :pile-size 10}]
                  :players [{:hand    [throne-room cargo-ship improve]
                             :deck    (repeat 5 copper)
                             :actions 2
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :cargo-ship)
                 (play 0 :improve)
                 (end-turn 0)
                 (choose :improve)
                 (choose :cargo-ship)                       ; improve Cargo Ship
                 (choose :inventor)                         ; gain Inventor
                 (choose :inventor))                        ; put Inventor on vanished Cargo Ship
             {:current-player 0
              :supply         [{:card inventor :pile-size 9}]
              :players        [{:hand    [copper copper copper copper copper inventor]
                                :discard [throne-room improve]
                                :actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}]
              :trash          [cargo-ship]})))
    (is (= (-> {:players [{:hand    [cargo-ship throne-room]
                           :discard [copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :cargo-ship))
           {:players [{:play-area     [throne-room cargo-ship]
                       :discard       [copper]
                       :actions       0
                       :coins         4
                       :triggers      [(assoc cargo-ship-trigger :card-id 1) (assoc cargo-ship-trigger :card-id 1)]
                       :repeated-play [{:source 8
                                        :target 1}]}]}))
    (is (= (-> {:players [{:hand    [cargo-ship throne-room]
                           :deck    (repeat 5 copper)
                           :actions 1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :cargo-ship)
               (end-turn 0))
           {:current-player 0
            :players        [{:hand    (repeat 5 copper)
                              :discard [throne-room cargo-ship]
                              :actions 1
                              :coins   0
                              :buys    1
                              :phase   :action}]}))
    (is (= (-> {:supply  [{:card gold :pile-size 30}]
                :players [{:hand    [cargo-ship throne-room]
                           :discard [copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :cargo-ship)
               (gain {:player-no 0 :card-name :gold}))
           {:supply       [{:card gold :pile-size 29}]
            :players      [{:play-area     [throne-room cargo-ship]
                            :gaining       [gold]
                            :discard       [copper]
                            :actions       0
                            :coins         4
                            :triggers      [(assoc cargo-ship-trigger :card-id 1)
                                            (assoc cargo-ship-trigger :card-id 1)]
                            :repeated-play [{:source 8
                                             :target 1}]}]
            :effect-stack [{:text      "You may set the gained Gold aside on Cargo Ship."
                            :player-no 0
                            :card-id   1
                            :choice    [::renaissance/cargo-ship-set-aside {:gained-card-id 5}]
                            :source    :gaining
                            :options   [:gold]
                            :max       1}
                           {:player-no 0
                            :effect    [:dombot.cards.renaissance/cargo-ship-give-choice {:player-no      0
                                                                                          :card-id        1
                                                                                          :card-name      :gold
                                                                                          :gained-card-id 5}]}
                           {:player-no 0
                            :effect    [:remove-triggers {:event :on-gain}]}
                           {:player-no 0
                            :effect    [:finalize-gain {:player-no      0
                                                        :card-name      :gold
                                                        :gained-card-id 5}]}]}))
    (is (= (-> {:supply  [{:card gold :pile-size 30}]
                :players [{:hand    [cargo-ship throne-room]
                           :discard [copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :cargo-ship)
               (gain {:player-no 0 :card-name :gold})
               (choose :gold))
           {:supply  [{:card gold :pile-size 29}]
            :players [{:play-area     [throne-room cargo-ship]
                       :discard       [copper]
                       :actions       0
                       :coins         4
                       :triggers      [(assoc cargo-ship-trigger :card-id 1)
                                       (merge set-aside=>hand-trigger {:card-id   1
                                                                       :set-aside [gold]})]
                       :repeated-play [{:source 8
                                        :target 1}]}]}))
    (is (= (-> {:supply  [{:card gold :pile-size 30}]
                :players [{:hand    [cargo-ship throne-room]
                           :deck    (repeat 5 copper)
                           :actions 1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :cargo-ship)
               (gain {:player-no 0 :card-name :gold})
               (choose :gold)
               (end-turn 0))
           {:current-player 0
            :supply         [{:card gold :pile-size 29}]
            :players        [{:hand      [copper copper copper copper copper gold]
                              :play-area [throne-room cargo-ship]
                              :actions   1
                              :coins     0
                              :buys      1
                              :phase     :action}]}))
    (is (= (-> {:supply  [{:card gold :pile-size 30}
                          {:card border-guard :pile-size 10}]
                :players [{:hand    [cargo-ship throne-room]
                           :discard [copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :cargo-ship)
               (gain {:player-no 0 :card-name :gold})
               (choose :gold)
               (gain {:player-no 0 :card-name :border-guard})
               (choose :border-guard))
           {:supply  [{:card gold :pile-size 29}
                      {:card border-guard :pile-size 9}]
            :players [{:play-area     [throne-room cargo-ship]
                       :discard       [copper]
                       :actions       0
                       :coins         4
                       :triggers      [(merge set-aside=>hand-trigger {:card-id   1
                                                                       :set-aside [gold]})
                                       (merge set-aside=>hand-trigger {:card-id   1
                                                                       :set-aside [border-guard]})]
                       :repeated-play [{:source 8
                                        :target 1}]}]}))
    (is (= (-> {:supply  [{:card experiment :pile-size 10}]
                :players [{:hand    [cargo-ship throne-room]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :throne-room)
               (choose :cargo-ship)
               (buy-card 0 :experiment)
               (choose :experiment)
               (choose :experiment))
           {:supply  [{:card experiment :pile-size 8}]
            :players [{:play-area     [throne-room cargo-ship]
                       :actions       0
                       :coins         1
                       :buys          0
                       :triggers      [(merge set-aside=>hand-trigger {:card-id   1
                                                                       :set-aside [(assoc experiment :id 2)]})
                                       (merge set-aside=>hand-trigger {:card-id   1
                                                                       :set-aside [(assoc experiment :id 1)]})]
                       :repeated-play [{:source 8
                                        :target 1}]}]}))))

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
                              :gaining [ducat]}]
              :effect-stack [{:text           "You may trash a Copper from your hand."
                              :player-no      0
                              :card-name      :ducat
                              :gained-card-id 1
                              :choice         :trash-from-hand
                              :source         :hand
                              :options        [:copper :copper]
                              :max            1}
                             {:player-no 0
                              :effect    [:remove-triggers {:event :on-gain}]}
                             {:player-no 0
                              :effect    [:finalize-gain {:player-no      0
                                                          :card-name      :ducat
                                                          :gained-card-id 1}]}]}))
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
                           :triggers [(get-project-trigger flag)]}]}))
      (is (= (-> {:supply    [{:card flag-bearer :pile-size 9}]
                  :artifacts {:flag (assoc flag :owner 0)}
                  :players   [{:triggers [(get-project-trigger flag)]}]}
                 (gain {:player-no 0
                        :card-name :flag-bearer}))
             {:supply    [{:card flag-bearer :pile-size 8}]
              :artifacts {:flag (assoc flag :owner 0)}
              :players   [{:discard  [flag-bearer]
                           :triggers [(get-project-trigger flag)]}]}))
      (is (= (-> {:supply    [{:card flag-bearer :pile-size 9}]
                  :artifacts {:flag (assoc flag :owner 0)}
                  :players   [{:triggers [(get-project-trigger flag)]}
                              {}]}
                 (gain {:player-no 1
                        :card-name :flag-bearer}))
             {:supply    [{:card flag-bearer :pile-size 8}]
              :artifacts {:flag (assoc flag :owner 1)}
              :players   [{}
                          {:discard  [flag-bearer]
                           :triggers [(get-project-trigger flag)]}]}))
      (is (= (-> {:artifacts {:flag flag}
                  :players   [{:hand    [chapel flag-bearer]
                               :actions 1}]}
                 (play 0 :chapel)
                 (choose :flag-bearer))
             {:artifacts {:flag (assoc flag :owner 0)}
              :players   [{:play-area [chapel]
                           :actions   0
                           :triggers  [(get-project-trigger flag)]}]
              :trash     [flag-bearer]}))
      (is (= (-> {:players [{:deck     (repeat 7 copper)
                             :triggers [(get-project-trigger flag)]}]}
                 (clean-up {:player-no 0}))
             {:players [{:hand     (repeat 6 copper)
                         :deck     [copper]
                         :triggers [(get-project-trigger flag)]
                         :actions  0
                         :coins    0
                         :buys     0
                         :phase    :out-of-turn}]})))))

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
  (let [improve  (assoc improve :id 1)
        research (assoc research :id 2)]
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
                              :effect    [:remove-triggers {:event :at-draw-hand}]}
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
                              :effect    [:remove-triggers {:event :at-draw-hand}]}
                             {:player-no 0
                              :effect    [:check-game-ended]}]}))
      (is (= (-> {:players [{:play-area [research
                                         (assoc improve :at-clean-up [[::renaissance/improve-give-choice]])
                                         copper]
                             :triggers  [(merge set-aside=>hand-trigger {:card-id   2
                                                                         :set-aside [silver silver]})]}]}
                 (clean-up {:player-no 0})
                 (choose :improve))
             {:players      [{:play-area [research improve copper]
                              :triggers  [(merge set-aside=>hand-trigger {:card-id   2
                                                                          :set-aside [silver silver]})]}]
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
                              :effect    [:remove-triggers {:event :at-draw-hand}]}
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
                              :effect    [:remove-triggers {:event :at-draw-hand}]}
                             {:player-no 0
                              :effect    [:check-game-ended]}]})))))

(deftest inventor-test
  (let [silver (assoc silver :id 1)
        duchy  (assoc duchy :id 2)]
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
           {:players      [{:play-area      [mountain-village]
                            :discard        [copper gold]
                            :revealed-cards {:discard 2}
                            :actions        2}]
            :effect-stack [{:text      "Look through your discard pile and put a card from it into your hand."
                            :player-no 0
                            :choice    :take-from-discard
                            :source    :discard
                            :options   [:copper :gold]
                            :min       1
                            :max       1}]}))
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
              :effect-stack [{:text                "You may trash a Curse from your hand."
                              :player-no           1
                              :attacking-player-no 0
                              :choice              :trash-from-hand
                              :source              :hand
                              :options             [:curse]
                              :max                 1}
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

(deftest priest-test
  (testing "Priest"
    (is (= (-> {:players [{:hand    [priest copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :priest)
               (choose :copper))
           {:players [{:play-area [priest]
                       :actions   0
                       :coins     2
                       :triggers  [priest-trigger]}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand     [priest copper]
                           :actions  1
                           :coins    0
                           :triggers [priest-trigger]}]}
               (play 0 :priest)
               (choose :copper))
           {:players [{:play-area [priest]
                       :actions   0
                       :coins     4
                       :triggers  [priest-trigger priest-trigger]}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand     [priest copper]
                           :actions  1
                           :coins    0
                           :triggers [priest-trigger priest-trigger]}]}
               (play 0 :priest)
               (choose :copper))
           {:players [{:play-area [priest]
                       :actions   0
                       :coins     6
                       :triggers  [priest-trigger priest-trigger priest-trigger]}]
            :trash   [copper]}))
    (is (= (-> {:players [{:hand    [throne-room priest copper copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :priest)
               (choose :copper)
               (choose :copper))
           {:players [{:play-area [throne-room priest]
                       :actions   0
                       :coins     6
                       :triggers  [priest-trigger priest-trigger]}]
            :trash   [copper copper]}))
    (is (= (-> {:players [{:hand    [throne-room priest copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :priest)
               (choose :copper))
           {:players [{:play-area [throne-room priest]
                       :actions   0
                       :coins     4
                       :triggers  [priest-trigger priest-trigger]}]
            :trash   [copper]}))))

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

(deftest research-test
  (let [research    (assoc research :id 1)
        throne-room (assoc throne-room :id 2)]
    (testing "research"
      (is (= (-> {:players [{:hand    [research estate copper copper]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :research))
             {:players      [{:hand      [estate copper copper]
                              :play-area [research]
                              :deck      [silver silver copper]
                              :actions   1}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :card-id   1
                              :choice    ::renaissance/research-trash
                              :source    :hand
                              :options   [:estate :copper :copper]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [research estate copper copper]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :research)
                 (choose :copper))
             {:players [{:hand      [estate copper]
                         :play-area [research]
                         :deck      [silver silver copper]
                         :actions   1}]
              :trash   [copper]}))
      (is (= (-> {:players [{:hand    [research estate copper copper]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :research)
                 (choose :estate))
             {:players [{:hand      [copper copper]
                         :play-area [research]
                         :deck      [copper]
                         :actions   1
                         :triggers  [(merge set-aside=>hand-trigger {:card-id   1
                                                                     :set-aside [silver silver]})]}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand    [research estate copper copper]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :research)
                 (choose :estate)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper copper silver silver]
                                :play-area [research]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]
              :trash          [estate]}))
      (is (= (-> {:players [{:hand    [research estate silver copper throne-room]
                             :deck    [silver silver gold estate copper copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :research)
                 (choose :estate)
                 (choose :silver)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper silver silver gold estate copper]
                                :play-area [throne-room
                                            research]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]
              :trash          [estate silver]})))))

(deftest scepter-test
  (let [patron         (assoc patron :id 0)
        swindler       (assoc swindler :id 1)
        curse          (assoc curse :id 2)
        peddler        (assoc peddler :id 3)
        villain        (assoc villain :id 4)
        priest         (assoc priest :id 5)
        priest-trigger (assoc priest-trigger :card-id 5)
        merchant-ship  (assoc merchant-ship :id 6)
        research       (assoc research :id 7)
        scepter        (assoc scepter :id 8)]
    (testing "Scepter"
      (is (= (-> {:players [{:hand  [scepter]
                             :coins 0}]}
                 (play 0 :scepter)
                 (choose :coins))
             {:players [{:play-area [scepter]
                         :coins     2}]}))
      (is (= (-> {:players [{:hand  [scepter]
                             :coins 0}]}
                 (play 0 :scepter)
                 (choose :replay-action))
             {:players [{:play-area [scepter]
                         :coins     0}]}))
      (is (= (-> {:players [{:hand  [scepter]
                             :coins 0}]}
                 (play 0 :scepter)
                 (choose :replay-action))
             {:players [{:play-area [scepter]
                         :coins     0}]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [patron scepter]
                                           :actions 1
                                           :coins   0}]}
                 (play 0 :patron)
                 (play 0 :scepter)
                 (choose :replay-action)
                 (choose :patron))
             {:track-played-actions? true
              :players               [{:play-area      [patron scepter]
                                       :actions-played [0 0]
                                       :actions        0
                                       :coins          4
                                       :villagers      2}]}))
      (is (= (-> {:players [{:hand      [scepter]
                             :play-area [cargo-ship]
                             :coins     0}]}
                 (play 0 :scepter)
                 (choose :replay-action))
             {:players [{:play-area [cargo-ship scepter]
                         :coins     0}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card peddler :pile-size 6}]
                  :players [{:hand           [scepter]
                             :play-area      [peddler peddler peddler swindler]
                             :actions-played [1]
                             :coins          5
                             :phase          :action}
                            {:deck [peddler]}]}
                 (play 0 :scepter)
                 (choose :replay-action)
                 (choose :swindler)
                 (choose :curse))
             {:supply  [{:card curse :pile-size 9}
                        {:card peddler :pile-size 6}]
              :players [{:play-area      [peddler peddler peddler swindler scepter]
                         :actions-played [1]
                         :coins          7
                         :phase          :pay}
                        {:discard [curse]}]
              :trash   [peddler]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card peddler :pile-size 6}]
                  :players [{:hand           [scepter]
                             :play-area      [peddler peddler peddler swindler]
                             :actions-played [1]
                             :coins          5
                             :phase          :action}
                            {:deck [peddler]}]}
                 (play 0 :scepter)
                 (choose :replay-action)
                 (choose :swindler)
                 (choose :peddler))
             {:supply  [{:card curse :pile-size 10}
                        {:card peddler :pile-size 5}]
              :players [{:play-area      [peddler peddler peddler swindler scepter]
                         :actions-played [1]
                         :coins          7
                         :phase          :pay}
                        {:discard [peddler]}]
              :trash   [peddler]}))
      (is (= (-> {:players [{:hand           [scepter]
                             :play-area      [peddler peddler peddler villain]
                             :actions-played [4]
                             :coins          5
                             :phase          :action}
                            {:hand [copper copper copper copper peddler]}]}
                 (play 0 :scepter)
                 (choose :replay-action)
                 (choose :villain))
             {:players [{:play-area      [peddler peddler peddler villain scepter]
                         :actions-played [4]
                         :coins          5
                         :coffers        2
                         :phase          :pay}
                        {:hand           [copper copper copper copper peddler]
                         :revealed-cards {:hand 5}}]}))
      (is (= (-> {:players [{:hand           [scepter copper]
                             :play-area      [priest]
                             :actions-played [5]
                             :coins          2
                             :phase          :action
                             :triggers       [priest-trigger]}]}
                 (play-treasures {:player-no 0})
                 (choose :replay-action)
                 (choose :priest)
                 (choose :copper))
             {:players [{:play-area      [priest scepter]
                         :actions-played [5]
                         :coins          6
                         :phase          :pay
                         :triggers       [priest-trigger priest-trigger]}]
              :trash   [copper]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [merchant-ship scepter]
                                           :deck    (repeat 5 copper)
                                           :actions 1
                                           :coins   2}]}
                 (play 0 :merchant-ship)
                 (play 0 :scepter)
                 (choose :replay-action)
                 (choose :merchant-ship)
                 (end-turn 0))
             {:track-played-actions? true
              :current-player        0
              :players               [{:hand      (repeat 5 copper)
                                       :play-area [merchant-ship scepter]
                                       :actions   1
                                       :coins     4
                                       :buys      1
                                       :phase     :action}]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [research scepter estate copper copper]
                                           :deck    (repeat 7 copper)
                                           :actions 1}]}
                 (play 0 :research)
                 (choose :estate)
                 (play 0 :scepter)
                 (choose :replay-action)
                 (choose :research)
                 (choose :copper)
                 (end-turn 0))
             {:track-played-actions? true
              :current-player        0
              :players               [{:hand      (repeat 7 copper)
                                       :play-area [research scepter]
                                       :discard   [copper]
                                       :actions   1
                                       :coins     0
                                       :buys      1
                                       :phase     :action}]
              :trash                 [estate copper]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [research scepter estate copper copper]
                                           :deck    (repeat 7 copper)
                                           :actions 1}]}
                 (play 0 :research)
                 (choose :copper)
                 (play 0 :scepter)
                 (choose :replay-action)
                 (choose :research)
                 (choose :estate)
                 (end-turn 0))
             {:track-played-actions? true
              :current-player        0
              :players               [{:hand      (repeat 7 copper)
                                       :play-area [research scepter]
                                       :discard   [copper]
                                       :actions   1
                                       :coins     0
                                       :buys      1
                                       :phase     :action}]
              :trash                 [copper estate]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [research scepter estate copper copper]
                                           :deck    (repeat 7 copper)
                                           :actions 1}]}
                 (play 0 :research)
                 (choose :copper)
                 (play 0 :scepter)
                 (choose :replay-action)
                 (choose :research)
                 (choose :copper)
                 (end-turn 0))
             {:track-played-actions? true
              :current-player        0
              :players               [{:hand    (repeat 5 copper)
                                       :deck    [copper copper]
                                       :discard [estate research scepter]
                                       :actions 1
                                       :coins   0
                                       :buys    1
                                       :phase   :action}]
              :trash                 [copper copper]})))))

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
  (let [estate     (assoc estate :id 1)
        silver     (assoc silver :id 2)
        watchtower (assoc watchtower :id 3)]
    (testing "Sculptor"
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card duchy :pile-size 8}
                            {:card silver :pile-size 40}
                            {:card watchtower :pile-size 10}]
                  :players [{:hand    [sculptor]
                             :actions 1}]}
                 (play 0 :sculptor))
             {:supply       [{:card estate :pile-size 8}
                             {:card duchy :pile-size 8}
                             {:card silver :pile-size 40}
                             {:card watchtower :pile-size 10}]
              :players      [{:play-area [sculptor]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card to your hand costing up to $4."
                              :player-no 0
                              :choice    ::renaissance/sculptor-gain
                              :source    :supply
                              :options   [:estate :silver :watchtower]
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
      (is (= (-> {:supply  [{:card watchtower :pile-size 10}]
                  :players [{:hand    [sculptor]
                             :actions 1}]}
                 (play 0 :sculptor)
                 (choose :watchtower))
             {:supply  [{:card watchtower :pile-size 9}]
              :players [{:hand      [watchtower]
                         :play-area [sculptor]
                         :actions   0}]})))))

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

(deftest swashbuckler-test
  (let [gold (assoc gold :id 0)]
    (testing "Swashbuckler"
      (is (= (-> {:artifacts {:treasure-chest treasure-chest}
                  :players   [{:hand    [swashbuckler]
                               :deck    [copper copper]
                               :discard [copper copper]
                               :actions 1}]}
                 (play 0 :swashbuckler))
             {:artifacts {:treasure-chest treasure-chest}
              :players   [{:hand      [copper copper copper]
                           :play-area [swashbuckler]
                           :deck      [copper]
                           :actions   0}]}))
      (is (= (-> {:artifacts {:treasure-chest treasure-chest}
                  :players   [{:hand    [swashbuckler]
                               :deck    [copper copper copper]
                               :discard [estate]
                               :actions 1
                               :coffers 2}]}
                 (play 0 :swashbuckler))
             {:artifacts {:treasure-chest treasure-chest}
              :players   [{:hand      [copper copper copper]
                           :play-area [swashbuckler]
                           :discard   [estate]
                           :actions   0
                           :coffers   3}]}))
      (is (= (-> {:artifacts {:treasure-chest treasure-chest}
                  :players   [{:hand    [swashbuckler]
                               :deck    [copper copper copper copper]
                               :discard [estate]
                               :actions 1
                               :coffers 3}]}
                 (play 0 :swashbuckler))
             {:artifacts {:treasure-chest (assoc treasure-chest :owner 0)}
              :players   [{:hand      [copper copper copper]
                           :play-area [swashbuckler]
                           :deck      [copper]
                           :discard   [estate]
                           :actions   0
                           :coffers   4
                           :triggers  [(get-project-trigger treasure-chest)]}]}))
      (is (= (-> {:artifacts {:treasure-chest (assoc treasure-chest :owner 0)}
                  :supply    [{:card gold :pile-size 30}]
                  :players   [{:hand     [copper]
                               :phase    :action
                               :coins    0
                               :triggers [(get-project-trigger treasure-chest)]}]}
                 (play 0 :copper))
             {:artifacts {:treasure-chest (assoc treasure-chest :owner 0)}
              :supply    [{:card gold :pile-size 29}]
              :players   [{:play-area [copper]
                           :discard   [gold]
                           :phase     :pay
                           :coins     1
                           :triggers  [(get-project-trigger treasure-chest)]}]}))
      (is (= (-> {:artifacts {:treasure-chest (assoc treasure-chest :owner 0)}
                  :supply    [{:card gold :pile-size 30}]
                  :players   [{:phase    :action
                               :coins    6
                               :buys     1
                               :triggers [(get-project-trigger treasure-chest)]}]}
                 (buy-card 0 :gold))
             {:artifacts {:treasure-chest (assoc treasure-chest :owner 0)}
              :supply    [{:card gold :pile-size 28}]
              :players   [{:discard  [gold gold]
                           :phase    :buy
                           :coins    0
                           :buys     0
                           :triggers [(get-project-trigger treasure-chest)]}]}))
      (is (= (-> {:artifacts {:treasure-chest (assoc treasure-chest :owner 0)}
                  :supply    [{:card gold :pile-size 30}]
                  :players   [{:phase    :action
                               :triggers [(get-project-trigger treasure-chest)]}]}
                 (end-turn 0))
             {:artifacts      {:treasure-chest (assoc treasure-chest :owner 0)}
              :supply         [{:card gold :pile-size 29}]
              :current-player 0
              :players        [{:hand     [gold]
                                :actions  1
                                :coins    0
                                :buys     1
                                :phase    :action
                                :triggers [(get-project-trigger treasure-chest)]}]})))))

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
    (let [ducat (assoc ducat :id 0)]
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
              :trash   [copper estate copper]})))
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
                         :triggers  [(get-project-trigger key)]}]}))
    (is (= (-> {:artifacts {:key (assoc key :owner 0)}
                :players   [{:triggers [(get-project-trigger key)]}]}
               (end-turn 0))
           {:current-player 0
            :artifacts      {:key (assoc key :owner 0)}
            :players        [{:actions  1
                              :coins    1
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger key)]}]}))
    (let [merchant-ship (assoc merchant-ship :id 1)]
      (is (= (-> {:players [{:hand     [merchant-ship]
                             :actions  1
                             :coins    0
                             :triggers [(get-project-trigger key)]}]}
                 (play 0 :merchant-ship)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [merchant-ship]
                                :actions   1
                                :coins     3
                                :buys      1
                                :phase     :action
                                :triggers  [(get-project-trigger key)]}]})))))

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

(deftest academy-test
  (let [silver           (assoc silver :id 0)
        mountain-village (assoc mountain-village :id 1)
        experiment       (assoc experiment :id 2)]
    (testing "Academy"
      (is (= (-> {:projects {:academy academy}
                  :players  [{:coins 5
                              :buys  1
                              :phase :pay}]}
                 (buy-project 0 :academy))
             {:projects {:academy (assoc academy :participants [{:player-no 0}])}
              :players  [{:coins    0
                          :buys     0
                          :phase    :buy
                          :triggers [(get-project-trigger academy)]}]}))
      (is (= (-> {:supply  [{:card mountain-village :pile-size 10}]
                  :players [{:coins    4
                             :buys     1
                             :triggers [(get-project-trigger academy)]}]}
                 (buy-card 0 :mountain-village))
             {:supply  [{:card mountain-village :pile-size 9}]
              :players [{:discard   [mountain-village]
                         :coins     0
                         :buys      0
                         :villagers 1
                         :triggers  [(get-project-trigger academy)]}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:coins    3
                             :buys     1
                             :triggers [(get-project-trigger academy)]}]}
                 (buy-card 0 :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:discard  [silver]
                         :coins    0
                         :buys     0
                         :triggers [(get-project-trigger academy)]}]}))
      (is (= (-> {:supply  [{:card experiment :pile-size 10}]
                  :players [{:coins    3
                             :buys     1
                             :triggers [(get-project-trigger academy)]}]}
                 (buy-card 0 :experiment))
             {:supply  [{:card experiment :pile-size 8}]
              :players [{:discard   [experiment experiment]
                         :coins     0
                         :buys      0
                         :villagers 2
                         :triggers  [(get-project-trigger academy)]}]})))))

(deftest barracks-test
  (testing "Barracks"
    (is (= (-> {:players [{:phase    :out-of-turn
                           :triggers [(get-project-trigger barracks)]}]}
               (start-turn {:player-no 0}))
           {:current-player 0
            :players        [{:actions  2
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger barracks)]}]}))))

(deftest canal-test
  (testing "Canal"
    (is (= (-> {:projects {:canal canal}
                :players  [{:coins 7
                            :buys  1}]}
               (buy-project 0 :canal))
           {:projects {:canal (assoc canal :participants [{:player-no 0}])}
            :players  [{:coins           0
                        :buys            0
                        :cost-reductions [{:reduction 1}]}]}))))

(deftest capitalism-test
  (let [patron (assoc patron :id 0)
        mint   (assoc mint :id 1)]
    (testing "Capitalism"
      (testing "get-types"
        (is (= (-> {:projects {:capitalism capitalism}
                    :players  [{:coins 7
                                :buys  1}]}
                   (ut/get-types priest))
               #{:action}))
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:coins 7
                                :buys  1}]}
                   (ut/get-types priest))
               #{:action :treasure}))
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:coins 7
                                :buys  1}]}
                   (ut/get-types villain))
               #{:action :attack})))
      (testing "playing Actions as Treasures"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [patron]
                                :actions 1
                                :coins   0
                                :phase   :action}]}
                   (play 0 :patron))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area [patron]
                            :actions   0
                            :coins     2
                            :villagers 1
                            :phase     :action}]}))
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [patron]
                                :actions 1
                                :coins   0
                                :phase   :pay}]}
                   (play 0 :patron))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area [patron]
                            :actions   1
                            :coins     2
                            :villagers 1
                            :phase     :pay}]}))
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [patron]
                                :actions 0
                                :coins   0
                                :phase   :action}]}
                   (play 0 :patron))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area [patron]
                            :actions   0
                            :coins     2
                            :villagers 1
                            :phase     :pay}]}))
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [patron copper]
                                :actions 1
                                :coins   0
                                :phase   :action}]}
                   (play-treasures {:player-no 0}))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area [patron copper]
                            :actions   1
                            :coins     3
                            :villagers 1
                            :phase     :pay}]}))
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [priest copper]
                                :actions 0
                                :coins   0
                                :phase   :action}]}
                   (play-treasures {:player-no 0})
                   (choose :copper))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area [priest]
                            :actions   0
                            :coins     2
                            :phase     :pay
                            :triggers  [priest-trigger]}]
                :trash    [copper]})))
      (testing "with Sculptor"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :supply   [{:card patron :pile-size 10}]
                    :players  [{:hand    [sculptor]
                                :actions 1}]}
                   (play 0 :sculptor)
                   (choose :patron))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :supply   [{:card patron :pile-size 9}]
                :players  [{:hand      [patron]
                            :play-area [sculptor]
                            :actions   0
                            :villagers 1}]})))
      (testing "with Guildhall"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :supply   [{:card patron :pile-size 10}]
                    :players  [{:coins    4
                                :buys     1
                                :triggers [(get-project-trigger guildhall)]}]}
                   (buy-card 0 :patron))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :supply   [{:card patron :pile-size 9}]
                :players  [{:discard  [patron]
                            :coins    0
                            :buys     0
                            :coffers  1
                            :triggers [(get-project-trigger guildhall)]}]})))
      (testing "with Loan"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand  [loan]
                                :deck  [patron estate]
                                :coins 0}]}
                   (play 0 :loan)
                   (choose nil))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area      [loan]
                            :deck           [estate]
                            :discard        [patron]
                            :revealed-cards {:discard 1}
                            :coins          1
                            :coffers        1}]})))
      (testing "with Venture"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand  [venture]
                                :deck  [patron estate]
                                :coins 0}]}
                   (play 0 :venture))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area      [venture patron]
                            :deck           [estate]
                            :revealed-cards {:play-area 1}
                            :coins          3
                            :coffers        1
                            :villagers      1}]})))
      (testing "with Courtier"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [courtier copper patron]
                                :actions 1}]}
                   (play 0 :courtier)
                   (choose :patron))
               {:projects     {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players      [{:hand           [copper patron]
                                :play-area      [courtier]
                                :revealed-cards {:hand 1}
                                :actions        0
                                :coffers        1}]
                :effect-stack [{:text      "Choose three:"
                                :player-no 0
                                :choice    ::intrigue/courtier-choices
                                :source    :special
                                :options   [{:option :action :text "+1 Action"}
                                            {:option :buy :text "+1 Buy"}
                                            {:option :coins :text "+$3"}
                                            {:option :gold :text "Gain a Gold."}]
                                :min       3
                                :max       3}]})))
      (testing "with Ironworks"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :supply   [{:card patron :pile-size 10}]
                    :players  [{:hand    [ironworks]
                                :actions 1
                                :coins   0}]}
                   (play 0 :ironworks)
                   (choose :patron))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :supply   [{:card patron :pile-size 9}]
                :players  [{:play-area [ironworks]
                            :discard   [patron]
                            :actions   1
                            :coins     1}]})))
      (testing "with Mine"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :supply   [{:card patron :pile-size 10}]
                    :players  [{:hand    [mine silver estate]
                                :actions 1}]}
                   (play 0 :mine)
                   (choose :silver)
                   (choose :patron))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :supply   [{:card patron :pile-size 9}]
                :players  [{:hand      [estate patron]
                            :play-area [mine]
                            :actions   0}]
                :trash    [silver]})))
      (testing "with Treasurer"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [treasurer patron]
                                :actions 1
                                :coins   0}]}
                   (play 0 :treasurer)
                   (choose :trash)
                   (choose :patron))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area [treasurer]
                            :actions   0
                            :coins     3}]
                :trash    [patron]}))
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand    [treasurer]
                                :actions 1
                                :coins   0}]
                    :trash    [patron]}
                   (play 0 :treasurer)
                   (choose :gain)
                   (choose :patron))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:hand      [patron]
                            :play-area [treasurer]
                            :actions   0
                            :coins     3}]
                :trash    []})))
      (testing "with Mint"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :supply   [{:card mint :pile-size 10}]
                    :players  [{:play-area [patron copper copper copper]
                                :coins     5
                                :buys      1}]}
                   (buy-card 0 :mint))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :supply   [{:card mint :pile-size 9}]
                :players  [{:discard [mint]
                            :coins   0
                            :buys    0}]
                :trash    [patron copper copper copper]})))
      (testing "with Bank"
        (is (= (-> {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                    :players  [{:hand      [bank]
                                :play-area [patron]
                                :coins     2}]}
                   (play 0 :bank))
               {:projects {:capitalism (assoc capitalism :participants [{:player-no 0}])}
                :players  [{:play-area [patron bank]
                            :coins     4}]}))))))

(deftest cathedral-test
  (testing "Cathedral"
    (is (= (-> {:players [{:hand     [copper copper copper copper copper]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger cathedral)]}]}
               (start-turn {:player-no 0})
               (choose :copper))
           {:current-player 0
            :players        [{:hand     [copper copper copper copper]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger cathedral)]}]
            :trash          [copper]}))))

(deftest citadel-test
  (let [patron      (assoc patron :id 0)
        throne-room (assoc throne-room :id 1)
        flag-bearer (assoc flag-bearer :id 2)]
    (testing "Citadel"
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand     [patron]
                                           :actions  1
                                           :coins    0
                                           :triggers [(get-project-trigger citadel)]}]}
                 (play 0 :patron))
             {:track-played-actions? true
              :players               [{:play-area      [patron]
                                       :actions        0
                                       :coins          4
                                       :villagers      2
                                       :actions-played [0 0]
                                       :triggers       [(get-project-trigger citadel)]}]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand           [patron]
                                           :actions        1
                                           :coins          0
                                           :actions-played [0]
                                           :triggers       [(get-project-trigger citadel)]}]}
                 (play 0 :patron))
             {:track-played-actions? true
              :players               [{:play-area      [patron]
                                       :actions        0
                                       :coins          2
                                       :villagers      1
                                       :actions-played [0 0]
                                       :triggers       [(get-project-trigger citadel)]}]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand     [throne-room patron flag-bearer]
                                           :actions  1
                                           :coins    0
                                           :triggers [(get-project-trigger citadel)]}]}
                 (play 0 :throne-room)
                 (choose :patron)
                 (choose :flag-bearer))
             {:track-played-actions? true
              :players               [{:play-area      [throne-room patron flag-bearer]
                                       :actions        0
                                       :coins          8
                                       :villagers      2
                                       :actions-played [1 0 0 1 2 2]
                                       :triggers       [(get-project-trigger citadel)]}]})))))

(deftest city-gate-test
  (testing "City Gate"
    (is (= (-> {:players [{:hand     [copper copper copper copper copper]
                           :deck     [silver estate]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger city-gate)]}]}
               (start-turn {:player-no 0})
               (choose :copper))
           {:current-player 0
            :players        [{:hand     [copper copper copper copper silver]
                              :deck     [copper estate]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger city-gate)]}]}))))

(deftest crop-rotation-test
  (testing "Crop Rotation"
    (is (= (-> {:players [{:hand     [copper copper copper copper estate]
                           :deck     [gold copper silver]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger crop-rotation)]}]}
               (start-turn {:player-no 0})
               (choose :estate))
           {:current-player 0
            :players        [{:hand     [copper copper copper copper gold copper]
                              :deck     [silver]
                              :discard  [estate]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger crop-rotation)]}]}))
    (is (= (-> {:players [{:hand     [copper copper copper copper gold]
                           :deck     [estate copper silver]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger crop-rotation)]}]}
               (start-turn {:player-no 0}))
           {:current-player 0
            :players        [{:hand     [copper copper copper copper gold]
                              :deck     [estate copper silver]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger crop-rotation)]}]}))))

(deftest exploration-test
  (testing "Exploration"
    (is (= (-> {:players [{:phase    :buy
                           :triggers [(get-project-trigger exploration)]}]}
               (clean-up {:player-no 0}))
           {:players [{:actions   0
                       :coins     0
                       :buys      0
                       :villagers 1
                       :coffers   1
                       :phase     :out-of-turn
                       :triggers  [(get-project-trigger exploration)]}]}))
    (is (= (-> {:players [{:gained-cards [{:name :silver :bought false}]
                           :phase        :buy
                           :triggers     [(get-project-trigger exploration)]}]}
               (clean-up {:player-no 0}))
           {:players [{:gained-cards [{:name :silver :bought false}]
                       :actions      0
                       :coins        0
                       :buys         0
                       :villagers    1
                       :coffers      1
                       :phase        :out-of-turn
                       :triggers     [(get-project-trigger exploration)]}]}))
    (is (= (-> {:players [{:gained-cards [{:name :silver :bought true}]
                           :phase        :buy
                           :triggers     [(get-project-trigger exploration)]}]}
               (clean-up {:player-no 0}))
           {:players [{:gained-cards [{:name :silver :bought true}]
                       :actions      0
                       :coins        0
                       :buys         0
                       :phase        :out-of-turn
                       :triggers     [(get-project-trigger exploration)]}]}))))

(deftest fair-test
  (testing "Fair"
    (is (= (-> {:players [{:phase    :out-of-turn
                           :triggers [(get-project-trigger fair)]}]}
               (start-turn {:player-no 0}))
           {:current-player 0
            :players        [{:actions  1
                              :coins    0
                              :buys     2
                              :phase    :action
                              :triggers [(get-project-trigger fair)]}]}))))

(deftest fleet-test
  (testing "Fleet"
    (is (= (-> {:supply  [{:card province :pile-size 0}]
                :players [{:triggers [(get-project-trigger fleet)]}]}
               (end-turn 0))
           {:supply         [{:card province :pile-size 0}]
            :current-player 0
            :game-ending?   true
            :players        [{:actions 1
                              :coins   0
                              :buys    1
                              :phase   :action}]}))
    (is (= (-> {:supply  [{:card province :pile-size 0}]
                :players [{:triggers [(get-project-trigger fleet)]}
                          {:phase    :out-of-turn
                           :triggers [(get-project-trigger fleet)]}]}
               (end-turn 0))
           {:supply         [{:card province :pile-size 0}]
            :current-player 1
            :game-ending?   true
            :players        [{:actions  0
                              :coins    0
                              :buys     0
                              :phase    :out-of-turn
                              :triggers [(get-project-trigger fleet)]}
                             {:actions 1
                              :coins   0
                              :buys    1
                              :phase   :action}]}))
    (is (= (-> {:supply  [{:card province :pile-size 0}]
                :players [{:triggers [(get-project-trigger fleet)]}
                          {}]}
               (end-turn 0))
           {:supply         [{:card province :pile-size 0}]
            :current-player 0
            :game-ending?   true
            :players        [{:actions 1
                              :coins   0
                              :buys    1
                              :phase   :action}
                             {:actions 0
                              :coins   0
                              :buys    0
                              :phase   :out-of-turn}]}))))

(deftest guildhall-test
  (let [silver           (assoc silver :id 0)
        mountain-village (assoc mountain-village :id 1)]
    (testing "Guildhall"
      (is (= (-> {:supply  [{:card mountain-village :pile-size 10}]
                  :players [{:coins    4
                             :buys     1
                             :triggers [(get-project-trigger guildhall)]}]}
                 (buy-card 0 :mountain-village))
             {:supply  [{:card mountain-village :pile-size 9}]
              :players [{:discard  [mountain-village]
                         :coins    0
                         :buys     0
                         :triggers [(get-project-trigger guildhall)]}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:coins    3
                             :buys     1
                             :triggers [(get-project-trigger guildhall)]}]}
                 (buy-card 0 :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:discard  [silver]
                         :coins    0
                         :buys     0
                         :coffers  1
                         :triggers [(get-project-trigger guildhall)]}]})))))

(deftest innovation-test
  (let [silver           (assoc silver :id 0)
        mountain-village (assoc mountain-village :id 1)]
    (testing "Innovation"
      (is (= (-> {:current-player      0
                  :track-gained-cards? true
                  :supply              [{:card mountain-village :pile-size 10}]
                  :players             [{:deck     [silver silver]
                                         :actions  0
                                         :triggers [(get-project-trigger innovation)]}]}
                 (gain {:player-no 0 :card-name :mountain-village})
                 (choose :mountain-village))
             {:current-player      0
              :track-gained-cards? true
              :supply              [{:card mountain-village :pile-size 9}]
              :players             [{:hand         [silver]
                                     :play-area    [mountain-village]
                                     :deck         [silver]
                                     :actions      2
                                     :gained-cards [{:cost  4
                                                     :name  :mountain-village
                                                     :types #{:action}}]
                                     :triggers     [(get-project-trigger innovation)]}]}))
      (is (= (-> {:current-player      0
                  :track-gained-cards? true
                  :supply              [{:card mountain-village :pile-size 9}]
                  :players             [{:hand         [silver]
                                         :play-area    [mountain-village]
                                         :deck         [silver]
                                         :gained-cards [{:cost  4
                                                         :name  :mountain-village
                                                         :types #{:action}}]
                                         :triggers     [(get-project-trigger innovation)]}]}
                 (gain {:player-no 0 :card-name :mountain-village}))
             {:current-player      0
              :track-gained-cards? true
              :supply              [{:card mountain-village :pile-size 8}]
              :players             [{:hand         [silver]
                                     :play-area    [mountain-village]
                                     :deck         [silver]
                                     :discard      [mountain-village]
                                     :gained-cards [{:cost  4
                                                     :name  :mountain-village
                                                     :types #{:action}}
                                                    {:cost  4
                                                     :name  :mountain-village
                                                     :types #{:action}}]
                                     :triggers     [(get-project-trigger innovation)]}]}))
      (is (= (-> {:current-player      0
                  :track-gained-cards? true
                  :supply              [{:card mountain-village :pile-size 10}]
                  :players             [{:deck     [silver silver]
                                         :triggers [(get-project-trigger innovation)]}]}
                 (gain {:player-no 0 :card-name :mountain-village})
                 (choose nil))
             {:current-player      0
              :track-gained-cards? true
              :supply              [{:card mountain-village :pile-size 9}]
              :players             [{:deck         [silver silver]
                                     :discard      [mountain-village]
                                     :gained-cards [{:cost  4
                                                     :name  :mountain-village
                                                     :types #{:action}}]
                                     :triggers     [(get-project-trigger innovation)]}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:deck     [silver silver]
                             :triggers [(get-project-trigger innovation)]}]}
                 (gain {:player-no 0 :card-name :silver}))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:deck     [silver silver]
                         :discard  [silver]
                         :triggers [(get-project-trigger innovation)]}]})))))

(deftest pageant-test
  (testing "Pageant"
    (is (= (-> {:players [{:coins    1
                           :phase    :buy
                           :triggers [(get-project-trigger pageant)]}]}
               (clean-up {:player-no 0})
               (choose :get-coffers))
           {:players [{:actions  0
                       :coins    0
                       :buys     0
                       :coffers  1
                       :phase    :out-of-turn
                       :triggers [(get-project-trigger pageant)]}]}))
    (is (= (-> {:players [{:coins    1
                           :phase    :action
                           :triggers [(get-project-trigger pageant)]}]}
               (clean-up {:player-no 0})
               (choose :get-coffers))
           {:players [{:actions  0
                       :coins    0
                       :buys     0
                       :coffers  1
                       :phase    :out-of-turn
                       :triggers [(get-project-trigger pageant)]}]}))
    (is (= (-> {:players [{:coins    1
                           :phase    :buy
                           :triggers [(get-project-trigger pageant)]}]}
               (clean-up {:player-no 0})
               (choose :decline))
           {:players [{:actions  0
                       :coins    0
                       :buys     0
                       :phase    :out-of-turn
                       :triggers [(get-project-trigger pageant)]}]}))))

(deftest piazza-test
  (testing "Piazza"
    (is (= (-> {:players [{:hand     [copper copper estate silver estate]
                           :deck     [patron gold]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger piazza)]}]}
               (start-turn {:player-no 0}))
           {:current-player 0
            :players        [{:hand           [copper copper estate silver estate]
                              :play-area      [patron]
                              :deck           [gold]
                              :revealed-cards {:play-area 1}
                              :actions        1
                              :coins          2
                              :buys           1
                              :coffers        1
                              :villagers      1
                              :phase          :action
                              :triggers       [(get-project-trigger piazza)]}]}))
    (is (= (-> {:players [{:hand     [copper copper estate silver estate]
                           :deck     [gold patron]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger piazza)]}]}
               (start-turn {:player-no 0}))
           {:current-player 0
            :players        [{:hand           [copper copper estate silver estate]
                              :deck           [gold patron]
                              :revealed-cards {:deck 1}
                              :actions        1
                              :coins          0
                              :buys           1
                              :phase          :action
                              :triggers       [(get-project-trigger piazza)]}]}))))

(deftest road-network-test
  (let [duchy  (assoc duchy :id 0)
        silver (assoc silver :id 1)]
    (testing "Road Network"
      (is (= (-> {:projects {:road-network road-network}
                  :players  [{:coins 5
                              :buys  1}
                             {}]}
                 (buy-project 0 :road-network))
             {:projects {:road-network (assoc road-network :participants [{:player-no 0}])}
              :players  [{:coins 0
                          :buys  0}
                         {:triggers [{:name     :road-network
                                      :duration :game
                                      :event    :on-gain
                                      :effects  [[::renaissance/road-network-on-gain {:player-no 0}]]}]}]}))
      (is (= (-> {:projects {:road-network (assoc road-network :participants [{:player-no 1}])}
                  :players  [{:coins    5
                              :buys     1
                              :triggers [{:name     :road-network
                                          :duration :game
                                          :event    :on-gain
                                          :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}]}
                             {}
                             {:triggers [{:name     :road-network
                                          :duration :game
                                          :event    :on-gain
                                          :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}]}]}
                 (buy-project 0 :road-network))
             {:projects {:road-network (assoc road-network :participants [{:player-no 1} {:player-no 0}])}
              :players  [{:coins    0
                          :buys     0
                          :triggers [{:name     :road-network
                                      :duration :game
                                      :event    :on-gain
                                      :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}]}
                         {:triggers [{:name     :road-network
                                      :duration :game
                                      :event    :on-gain
                                      :effects  [[::renaissance/road-network-on-gain {:player-no 0}]]}]}
                         {:triggers [{:name     :road-network
                                      :duration :game
                                      :event    :on-gain
                                      :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}
                                     {:name     :road-network
                                      :duration :game
                                      :event    :on-gain
                                      :effects  [[::renaissance/road-network-on-gain {:player-no 0}]]}]}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:coins    3
                             :buys     1
                             :triggers [{:name     :road-network
                                         :duration :game
                                         :event    :on-gain
                                         :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}]}
                            {:hand [copper copper copper copper copper]
                             :deck [silver silver]}]}
                 (buy-card 0 :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:discard  [silver]
                         :coins    0
                         :buys     0
                         :triggers [{:name     :road-network
                                     :duration :game
                                     :event    :on-gain
                                     :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}]}
                        {:hand [copper copper copper copper copper]
                         :deck [silver silver]}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:coins    5
                             :buys     1
                             :triggers [{:name     :road-network
                                         :duration :game
                                         :event    :on-gain
                                         :effects  [[::renaissance/road-network-on-gain {:player-no 2}]]}]}
                            {:hand [copper copper copper copper copper]
                             :deck [silver silver]}
                            {:hand [copper copper copper copper copper]
                             :deck [silver silver]}]}
                 (buy-card 0 :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:discard  [duchy]
                         :coins    0
                         :buys     0
                         :triggers [{:name     :road-network
                                     :duration :game
                                     :event    :on-gain
                                     :effects  [[::renaissance/road-network-on-gain {:player-no 2}]]}]}
                        {:hand [copper copper copper copper copper]
                         :deck [silver silver]}
                        {:hand [copper copper copper copper copper silver]
                         :deck [silver]}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:coins    5
                             :buys     1
                             :triggers [{:name     :road-network
                                         :duration :game
                                         :event    :on-gain
                                         :effects  [[::renaissance/road-network-on-gain {:player-no 2}]]}
                                        {:name     :road-network
                                         :duration :game
                                         :event    :on-gain
                                         :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}]}
                            {:hand [copper copper copper copper copper]
                             :deck [silver silver]}
                            {:hand [copper copper copper copper copper]
                             :deck [silver silver]}]}
                 (buy-card 0 :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:discard  [duchy]
                         :coins    0
                         :buys     0
                         :triggers [{:name     :road-network
                                     :duration :game
                                     :event    :on-gain
                                     :effects  [[::renaissance/road-network-on-gain {:player-no 2}]]}
                                    {:name     :road-network
                                     :duration :game
                                     :event    :on-gain
                                     :effects  [[::renaissance/road-network-on-gain {:player-no 1}]]}]}
                        {:hand [copper copper copper copper copper silver]
                         :deck [silver]}
                        {:hand [copper copper copper copper copper silver]
                         :deck [silver]}]})))))

(deftest silos-test
  (testing "Silos"
    (is (= (-> {:players [{:hand     [copper copper estate silver estate]
                           :deck     [gold gold duchy]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger silos)]}]}
               (start-turn {:player-no 0})
               (choose [:copper :copper]))
           {:current-player 0
            :players        [{:hand     [estate silver estate gold gold]
                              :deck     [duchy]
                              :discard  [copper copper]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger silos)]}]}))))

(deftest sewers-test
  (testing "Sewers"
    (is (= (-> {:players [{:hand     [research copper estate copper]
                           :actions  1
                           :triggers [(get-project-trigger sewers)]}]}
               (play 0 :research)
               (choose :copper)
               (choose :estate))
           {:players [{:hand      [copper]
                       :play-area [research]
                       :actions   1
                       :triggers  [(get-project-trigger sewers)]}]
            :trash   [copper estate]}))
    (is (= (-> {:players [{:hand     [priest copper estate copper]
                           :actions  1
                           :coins    0
                           :triggers [(get-project-trigger sewers)]}]}
               (play 0 :priest)
               (choose :copper)
               (choose :estate))
           {:players [{:hand      [copper]
                       :play-area [priest]
                       :actions   0
                       :coins     2                         ; up for discussion; does Priest's on-trash apply to Sewers trash?
                       :triggers  [(get-project-trigger sewers)
                                   priest-trigger]}]
            :trash   [copper estate]}))))

(deftest sinister-plot-test
  (testing "Sinister Plot"
    (is (= (-> {:projects {:sinister-plot (assoc fair :participants [{:player-no 0}])}
                :players  [{:hand     [copper copper estate silver estate]
                            :deck     [gold gold duchy]
                            :phase    :out-of-turn
                            :triggers [(get-project-trigger sinister-plot)]}]}
               (start-turn {:player-no 0})
               (choose :add-token))
           {:projects       {:sinister-plot (assoc fair :participants [{:player-no 0
                                                                        :tokens    1}])}
            :current-player 0
            :players        [{:hand     [copper copper estate silver estate]
                              :deck     [gold gold duchy]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger sinister-plot)]}]}))
    (is (= (-> {:projects {:sinister-plot (assoc fair :participants [{:player-no 0
                                                                      :tokens    1}])}
                :players  [{:hand     [copper copper estate silver estate]
                            :deck     [gold gold duchy]
                            :phase    :out-of-turn
                            :triggers [(get-project-trigger sinister-plot)]}]}
               (start-turn {:player-no 0})
               (choose :add-token))
           {:projects       {:sinister-plot (assoc fair :participants [{:player-no 0
                                                                        :tokens    2}])}
            :current-player 0
            :players        [{:hand     [copper copper estate silver estate]
                              :deck     [gold gold duchy]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger sinister-plot)]}]}))
    (is (= (-> {:projects {:sinister-plot (assoc fair :participants [{:player-no 0
                                                                      :tokens    2}])}
                :players  [{:hand     [copper copper estate silver estate]
                            :deck     [gold gold duchy]
                            :phase    :out-of-turn
                            :triggers [(get-project-trigger sinister-plot)]}]}
               (start-turn {:player-no 0})
               (choose :remove-tokens))
           {:projects       {:sinister-plot (assoc fair :participants [{:player-no 0}])}
            :current-player 0
            :players        [{:hand     [copper copper estate silver estate gold gold]
                              :deck     [duchy]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger sinister-plot)]}]}))
    (is (= (-> {:projects {:sinister-plot (assoc fair :participants [{:player-no 0}])}
                :players  [{:hand     [copper copper estate silver estate]
                            :deck     [gold gold duchy]
                            :phase    :out-of-turn
                            :triggers [(get-project-trigger sinister-plot)]}]}
               (start-turn {:player-no 0})
               (choose :remove-tokens))
           {:projects       {:sinister-plot (assoc fair :participants [{:player-no 0}])}
            :current-player 0
            :players        [{:hand     [copper copper estate silver estate]
                              :deck     [gold gold duchy]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger sinister-plot)]}]}))))

(deftest star-chart-test
  (testing "Star Chart"
    (is (= (-> {:players [{:hand     [lackeys]
                           :discard  [silver copper copper copper]
                           :actions  1
                           :triggers [(get-project-trigger star-chart)]}]}
               (play 0 :lackeys)
               (choose :silver))
           {:players [{:hand      [silver copper]
                       :play-area [lackeys]
                       :deck      [copper copper]
                       :actions   0
                       :triggers  [(get-project-trigger star-chart)]}]}))
    (is (= (-> {:players [{:hand     [lackeys]
                           :discard  [silver copper copper copper]
                           :actions  1
                           :triggers [(get-project-trigger star-chart)]}]}
               (play 0 :lackeys)
               (choose :copper))
           {:players [{:hand      [copper copper]
                       :play-area [lackeys]
                       :deck      [copper silver]
                       :actions   0
                       :triggers  [(get-project-trigger star-chart)]}]}))
    (is (= (-> {:players [{:hand     [lackeys]
                           :discard  [silver copper copper copper]
                           :actions  1
                           :triggers [(get-project-trigger star-chart)]}]}
               (play 0 :lackeys)
               (choose nil))
           {:players [{:hand      [copper silver]
                       :play-area [lackeys]
                       :deck      [copper copper]
                       :actions   0
                       :triggers  [(get-project-trigger star-chart)]}]}))
    (is (= (-> {:players [{:hand     [lackeys]
                           :discard  [silver copper stash copper copper]
                           :actions  1
                           :triggers [(get-project-trigger star-chart)]}]}
               (play 0 :lackeys)
               (choose :silver)
               (choose 1))
           {:players [{:hand      [silver stash]
                       :play-area [lackeys]
                       :deck      [copper copper copper]
                       :actions   0
                       :triggers  [(get-project-trigger star-chart)]}]}))))

(deftest simultaneous-effects-test
  (testing "Simultaneous effects"
    (is (= (-> {:players [{:hand     [copper copper silver]
                           :deck     [estate gold copper copper]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger crop-rotation)
                                      (get-project-trigger silos)]}]}
               (start-turn {:player-no 0}))
           {:current-player 0
            :players        [{:hand     [copper copper silver]
                              :deck     [estate gold copper copper]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger crop-rotation)
                                         (get-project-trigger silos)]}]
            :effect-stack   [{:text      "Multiple things happen at the start of your turn. Select which one happens next."
                              :player-no 0
                              :choice    [:simultaneous-effects-choice {:triggers [(get-project-trigger crop-rotation)
                                                                                   (get-project-trigger silos)]}]
                              :source    :mixed
                              :options   [:crop-rotation :silos]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:remove-triggers {:event :at-start-turn}]}
                             {:player-no 0
                              :effect    [:sync-repeated-play]}]}))
    (is (= (-> {:players [{:hand     [copper copper silver]
                           :deck     [estate gold copper copper]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger crop-rotation)
                                      (get-project-trigger silos)]}]}
               (start-turn {:player-no 0})
               (choose :silos)
               (choose [:copper :copper])
               (choose :estate))
           {:current-player 0
            :players        [{:hand     [silver gold copper copper]
                              :discard  [copper copper estate]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger crop-rotation)
                                         (get-project-trigger silos)]}]}))
    (is (= (-> {:players [{:hand     [copper copper silver estate]
                           :deck     [gold copper copper silver silver silver]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger crop-rotation)
                                      (get-project-trigger silos)]}]}
               (start-turn {:player-no 0})
               (choose :crop-rotation)
               (choose :estate)
               (choose [:copper :copper :copper]))
           {:current-player 0
            :players        [{:hand     [silver gold copper silver silver]
                              :deck     [silver]
                              :discard  [estate copper copper copper]
                              :actions  1
                              :coins    0
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger crop-rotation)
                                         (get-project-trigger silos)]}]}))
    (is (= (-> {:players [{:hand     [copper copper silver]
                           :deck     [estate gold copper copper]
                           :phase    :out-of-turn
                           :triggers [(get-project-trigger key)
                                      (get-project-trigger silos)]}]}
               (start-turn {:player-no 0})
               (choose [:copper :copper]))
           {:current-player 0
            :players        [{:hand     [silver estate gold]
                              :deck     [copper copper]
                              :discard  [copper copper]
                              :actions  1
                              :coins    1
                              :buys     1
                              :phase    :action
                              :triggers [(get-project-trigger key)
                                         (get-project-trigger silos)]}]}))))

