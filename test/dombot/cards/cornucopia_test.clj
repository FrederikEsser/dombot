(ns dombot.cards.cornucopia-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.cornucopia :as cornucopia :refer :all]
            [dombot.cards.dominion :refer [militia]]
            [dombot.cards.intrigue :refer [nobles]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest fairgrounds-test
  (testing "Fairgrounds"
    (is (= (calc-victory-points {:deck [fairgrounds copper silver gold]})
           0))
    (is (= (calc-victory-points {:deck [fairgrounds copper silver gold hamlet]})
           2))
    (is (= (calc-victory-points {:deck [fairgrounds copper silver gold hamlet fairgrounds]})
           4))
    (is (= (calc-victory-points {:deck cornucopia/kingdom-cards})
           4))))

(deftest farming-village-test
  (testing "Farming Village"
    (is (= (-> {:players [{:hand    [farming-village]
                           :deck    [copper copper]
                           :actions 1}]}
               (play 0 :farming-village))
           {:players [{:hand           [copper]
                       :play-area      [farming-village]
                       :deck           [copper]
                       :revealed-cards {:hand 1}
                       :actions        2}]}))
    (is (= (-> {:players [{:hand    [farming-village]
                           :deck    [farming-village copper]
                           :actions 1}]}
               (play 0 :farming-village))
           {:players [{:hand           [farming-village]
                       :play-area      [farming-village]
                       :deck           [copper]
                       :revealed-cards {:hand 1}
                       :actions        2}]}))
    (is (= (-> {:players [{:hand    [farming-village]
                           :deck    [estate estate copper copper]
                           :actions 1}]}
               (play 0 :farming-village))
           {:players [{:hand           [copper]
                       :play-area      [farming-village]
                       :deck           [copper]
                       :discard        [estate estate]
                       :revealed-cards {:hand    1
                                        :discard 2}
                       :actions        2}]}))))

(deftest fortune-teller-test
  (testing "Fortune Teller"
    (is (= (-> {:players [{:hand    [fortune-teller]
                           :actions 1
                           :coins   0}
                          {:deck [curse]}]}
               (play 0 :fortune-teller))
           {:players [{:play-area [fortune-teller]
                       :actions   0
                       :coins     2}
                      {:deck           [curse]
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:players [{:hand    [fortune-teller]
                           :actions 1
                           :coins   0}
                          {:deck [nobles]}]}
               (play 0 :fortune-teller))
           {:players [{:play-area [fortune-teller]
                       :actions   0
                       :coins     2}
                      {:deck           [nobles]
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:players [{:hand    [fortune-teller]
                           :actions 1
                           :coins   0}
                          {:deck [copper curse]}]}
               (play 0 :fortune-teller))
           {:players [{:play-area [fortune-teller]
                       :actions   0
                       :coins     2}
                      {:deck           [curse]
                       :discard        [copper]
                       :revealed-cards {:deck    1
                                        :discard 1}}]}))
    (is (= (-> {:players [{:hand    [fortune-teller]
                           :actions 1
                           :coins   0}
                          {:deck [copper silver fortune-teller estate copper curse]}]}
               (play 0 :fortune-teller))
           {:players [{:play-area [fortune-teller]
                       :actions   0
                       :coins     2}
                      {:deck           [estate copper curse]
                       :discard        [copper silver fortune-teller]
                       :revealed-cards {:deck    1
                                        :discard 3}}]}))))

(deftest hamlet-test
  (testing "Hamlet"
    (is (= (-> {:players [{:hand    [hamlet copper copper estate estate]
                           :deck    [silver silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :hamlet)
               (choose :estate)
               (choose :estate))
           {:players [{:hand      [copper copper silver]
                       :play-area [hamlet]
                       :deck      [silver]
                       :discard   [estate estate]
                       :actions   2
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [hamlet copper copper estate estate]
                           :deck    [silver silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :hamlet)
               (choose :estate)
               (choose nil))
           {:players [{:hand      [copper copper estate silver]
                       :play-area [hamlet]
                       :deck      [silver]
                       :discard   [estate]
                       :actions   2
                       :buys      1}]}))
    (is (= (-> {:players [{:hand    [hamlet copper copper estate estate]
                           :deck    [silver silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :hamlet)
               (choose nil)
               (choose :estate))
           {:players [{:hand      [copper copper estate silver]
                       :play-area [hamlet]
                       :deck      [silver]
                       :discard   [estate]
                       :actions   1
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [hamlet copper copper estate estate]
                           :deck    [silver silver]
                           :actions 1
                           :buys    1}]}
               (play 0 :hamlet)
               (choose nil)
               (choose nil))
           {:players [{:hand      [copper copper estate estate silver]
                       :play-area [hamlet]
                       :deck      [silver]
                       :actions   1
                       :buys      1}]}))))

(deftest harvest-test
  (testing "Harvest"
    (is (= (-> {:players [{:hand    [harvest]
                           :deck    (repeat 5 copper)
                           :actions 1
                           :coins   0}]}
               (play 0 :harvest))
           {:players [{:play-area      [harvest]
                       :deck           [copper]
                       :discard        [copper copper copper copper]
                       :revealed-cards {:discard 4}
                       :actions        0
                       :coins          1}]}))
    (is (= (-> {:players [{:hand    [harvest]
                           :deck    [copper estate silver estate copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :harvest))
           {:players [{:play-area      [harvest]
                       :deck           [copper]
                       :discard        [copper estate silver estate]
                       :revealed-cards {:discard 4}
                       :actions        0
                       :coins          3}]}))))

(deftest horn-of-plenty-test
  (let [copper (assoc copper :id 0)
        silver (assoc silver :id 1)
        estate (assoc estate :id 2)
        horn-of-plenty (assoc horn-of-plenty :id 3)]
    (testing "Horn of Plenty"
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand  [horn-of-plenty]
                             :coins 0}]}
                 (play 0 :horn-of-plenty)
                 (choose :copper))
             {:supply  [{:card copper :pile-size 45}]
              :players [{:play-area [horn-of-plenty]
                         :discard   [copper]
                         :coins     0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 39}]
                  :players [{:hand      [horn-of-plenty]
                             :play-area [copper silver]
                             :coins     0}]}
                 (play 0 :horn-of-plenty)
                 (choose :silver))
             {:supply  [{:card silver :pile-size 38}]
              :players [{:play-area [copper silver horn-of-plenty]
                         :discard   [silver]
                         :coins     0}]}))
      (is (thrown-with-msg? AssertionError #"Choose error: Silver is not a valid option."
                            (-> {:supply  [{:card copper :pile-size 46}
                                           {:card silver :pile-size 39}]
                                 :players [{:hand      [horn-of-plenty]
                                            :play-area [gold]
                                            :coins     0}]}
                                (play 0 :horn-of-plenty)
                                (choose :silver))))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand      [horn-of-plenty]
                             :play-area [silver]
                             :coins     0}]}
                 (play 0 :horn-of-plenty)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:play-area [silver]
                         :discard   [estate]
                         :coins     0}]
              :trash   [horn-of-plenty]})))))

(deftest horse-traders-test
  (let [horse-traders (assoc horse-traders :id 0)]
    (testing "Horse Traders"
      (is (= (-> {:players [{:hand    [horse-traders copper copper copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :horse-traders)
                 (choose [:copper :copper]))
             {:players [{:hand      [copper copper]
                         :play-area [horse-traders]
                         :discard   [copper copper]
                         :actions   0
                         :coins     3
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [horse-traders copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :horse-traders)
                 (choose :copper))
             {:players [{:play-area [horse-traders]
                         :discard   [copper]
                         :actions   0
                         :coins     3
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [horse-traders]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :horse-traders))
             {:players [{:play-area [horse-traders]
                         :actions   0
                         :coins     3
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [militia]
                             :actions 1
                             :coins   0}
                            {:hand [horse-traders copper copper copper copper]}]}
                 (play 0 :militia)
                 (choose :horse-traders)
                 (choose :copper))
             {:players [{:play-area [militia]
                         :actions   0
                         :coins     2}
                        {:hand      [copper copper copper]
                         :play-area [(assoc horse-traders :at-start-turn [[[::cornucopia/horse-traders-return-to-hand {:card-id 0}]
                                                                           [:draw 1]]])]
                         :discard   [copper]}]}))
      (is (= (-> {:players [{:hand    [militia]
                             :actions 1
                             :coins   0}
                            {:hand [horse-traders copper copper copper copper]}]}
                 (play 0 :militia)
                 (choose nil)
                 (choose [:copper :copper]))
             {:players [{:play-area [militia]
                         :actions   0
                         :coins     2}
                        {:hand    [horse-traders copper copper]
                         :discard [copper copper]}]}))
      (is (= (-> {:players [{:hand    [militia]
                             :actions 1
                             :coins   0}
                            {:hand [horse-traders copper copper copper copper]
                             :deck [silver silver]}]}
                 (play 0 :militia)
                 (choose :horse-traders)
                 (choose :copper)
                 (end-turn 0))
             {:current-player 1
              :players        [{:hand    [militia]
                                :actions 0
                                :coins   0
                                :buys    0
                                :phase   :out-of-turn}
                               {:hand    [copper copper copper horse-traders silver]
                                :deck    [silver]
                                :discard [copper]
                                :actions 1
                                :coins   0
                                :buys    1}]})))))

(deftest hunting-party-test
  (testing "Hunting Party"
    (is (= (-> {:players [{:hand    [hunting-party copper copper copper copper]
                           :deck    [copper silver estate]
                           :actions 1}]}
               (play 0 :hunting-party))
           {:players [{:hand           [copper copper copper copper copper silver]
                       :play-area      [hunting-party]
                       :deck           [estate]
                       :revealed-cards {:hand 6}
                       :actions        1}]}))
    (is (= (-> {:players [{:hand    [hunting-party copper copper copper copper]
                           :deck    [silver copper estate]
                           :actions 1}]}
               (play 0 :hunting-party))
           {:players [{:hand           [copper copper copper copper silver estate]
                       :play-area      [hunting-party]
                       :discard        [copper]
                       :revealed-cards {:hand    6
                                        :discard 1}
                       :actions        1}]}))
    (is (= (-> {:players [{:hand    [hunting-party]
                           :deck    [silver copper estate]
                           :actions 1}]}
               (play 0 :hunting-party))
           {:players [{:hand           [silver copper]
                       :play-area      [hunting-party]
                       :deck           [estate]
                       :revealed-cards {:hand 2}
                       :actions        1}]}))
    (is (= (-> {:players [{:hand    [hunting-party]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :hunting-party))
           {:players [{:hand           [copper]
                       :play-area      [hunting-party]
                       :discard        [copper copper]
                       :revealed-cards {:hand    1
                                        :discard 2}
                       :actions        1}]}))))

(deftest jester-test
  (let [curse (assoc curse :id 0)
        copper (assoc copper :id 1)
        gold (assoc gold :id 2)]
    (testing "Jester"
      (is (= (-> {:players [{:name    :p1
                             :hand    [jester]
                             :actions 1
                             :coins   0}
                            {:name :p2
                             :deck [copper copper]}]}
                 (play 0 :jester))
             {:players      [{:name      :p1
                              :play-area [jester]
                              :actions   0
                              :coins     2}
                             {:name    :p2
                              :deck    [copper]
                              :discard [copper]}]
              :effect-stack [{:text      "Who gains a Copper?"
                              :player-no 0
                              :choice    [::cornucopia/jester-gain-copy {:card-name :copper}]
                              :source    :special
                              :options   [{:option 1 :text "P2"}
                                          {:option 0 :text "P1"}]
                              :min       1
                              :max       1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:name    :p1
                             :hand    [jester]
                             :actions 1
                             :coins   0}
                            {:name    :p2
                             :discard [copper copper]}]}
                 (play 0 :jester)
                 (choose 1))
             {:supply  [{:card copper :pile-size 45}]
              :players [{:name      :p1
                         :play-area [jester]
                         :actions   0
                         :coins     2}
                        {:name    :p2
                         :deck    [copper]
                         :discard [copper copper]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 29}]
                  :players [{:name    :p1
                             :hand    [jester]
                             :actions 1
                             :coins   0}
                            {:name :p2
                             :deck [gold copper]}]}
                 (play 0 :jester)
                 (choose 0))
             {:supply  [{:card gold :pile-size 28}]
              :players [{:name      :p1
                         :play-area [jester]
                         :discard   [gold]
                         :actions   0
                         :coins     2}
                        {:name    :p2
                         :deck    [copper]
                         :discard [gold]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:name    :p1
                             :hand    [jester]
                             :actions 1
                             :coins   0}
                            {:name :p2
                             :deck [estate copper]}]}
                 (play 0 :jester))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:name      :p1
                         :play-area [jester]
                         :actions   0
                         :coins     2}
                        {:name    :p2
                         :deck    [copper]
                         :discard [estate curse]}]})))))

(deftest menagerie-test
  (testing "Menagerie"
    (is (= (-> {:players [{:hand    [menagerie estate copper silver estate]
                           :deck    [copper copper copper copper]
                           :actions 1}]}
               (play 0 :menagerie))
           {:players [{:hand      [estate copper silver estate copper]
                       :play-area [menagerie]
                       :deck      [copper copper copper]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [menagerie estate copper silver gold]
                           :deck    [copper copper copper copper]
                           :actions 1}]}
               (play 0 :menagerie))
           {:players [{:hand      [estate copper silver gold copper copper copper]
                       :play-area [menagerie]
                       :deck      [copper]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [menagerie]
                           :deck    [copper copper copper copper]
                           :actions 1}]}
               (play 0 :menagerie))
           {:players [{:hand      [copper copper copper]
                       :play-area [menagerie]
                       :deck      [copper]
                       :actions   1}]}))))

(deftest remake-test
  (let [silver (assoc silver :id 0)]
    (testing "Remake"
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card estate :pile-size 8}
                            {:card silver :pile-size 40}]
                  :players [{:hand    [remake copper copper estate estate]
                             :actions 1}]}
                 (play 0 :remake)
                 (choose :copper)
                 (choose :estate)
                 (choose :silver))
             {:supply  [{:card copper :pile-size 46}
                        {:card estate :pile-size 8}
                        {:card silver :pile-size 39}]
              :players [{:hand      [copper estate]
                         :play-area [remake]
                         :discard   [silver]
                         :actions   0}]
              :trash   [copper estate]}))
      (is (thrown-with-msg? AssertionError #"Choose error: You must pick an option"
                            (-> {:supply  [{:card copper :pile-size 46}
                                           {:card estate :pile-size 8}
                                           {:card silver :pile-size 40}]
                                 :players [{:hand    [remake copper copper estate estate]
                                            :actions 1}]}
                                (play 0 :remake)
                                (choose :copper)
                                (choose nil))))
      (is (thrown-with-msg? AssertionError #"Choose error: You must pick an option"
                            (-> {:supply  [{:card copper :pile-size 46}
                                           {:card estate :pile-size 8}
                                           {:card silver :pile-size 40}]
                                 :players [{:hand    [remake copper copper estate estate]
                                            :actions 1}]}
                                (play 0 :remake)
                                (choose nil)))))))

(deftest tournament-test
  (let [duchy (assoc duchy :id 0)
        bag-of-gold (assoc bag-of-gold :id 1)
        gold (assoc gold :id 2)
        curse (assoc curse :id 3)
        estate (assoc estate :id 4)
        silver (assoc silver :id 5)]
    (testing "Tournament"
      (is (= (-> {:players [{:hand    [tournament]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :tournament))
             {:players [{:hand      [copper]
                         :play-area [tournament]
                         :deck      [copper]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:players [{:hand    [tournament province]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :tournament)
                 (choose nil))
             {:players [{:hand      [province copper]
                         :play-area [tournament]
                         :deck      [copper]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [tournament province]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :tournament)
                 (choose :province)
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:hand      [duchy]
                         :play-area [tournament]
                         :deck      [copper copper]
                         :discard   [province]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:players [{:hand    [tournament province]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}
                            {:hand [province]}]}
                 (play 0 :tournament)
                 (choose nil)
                 (choose :province))
             {:players [{:hand      [province]
                         :play-area [tournament]
                         :deck      [copper copper]
                         :actions   1
                         :coins     0}
                        {:hand [province]}]}))
      (is (= (-> {:players [{:hand    [tournament province]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}
                            {:hand [province]}]}
                 (play 0 :tournament)
                 (choose :province)
                 (choose :province)
                 (choose :nothing))
             {:players [{:play-area [tournament]
                         :deck      [copper copper]
                         :discard   [province]
                         :actions   1
                         :coins     0}
                        {:hand [province]}]}))
      (is (= (-> {:supply      [{:card duchy :pile-size 8}]
                  :extra-cards [{:card bag-of-gold :pile-size 1}]
                  :players     [{:hand    [tournament province]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :tournament)
                 (choose :province)
                 (choose :bag-of-gold))
             {:supply      [{:card duchy :pile-size 8}]
              :extra-cards [{:card bag-of-gold :pile-size 0}]
              :players     [{:hand      [bag-of-gold]
                             :play-area [tournament]
                             :deck      [copper copper]
                             :discard   [province]
                             :actions   1
                             :coins     1}]}))
      (testing "Prizes"
        (is (= (-> {:supply  [{:card gold :pile-size 30}]
                    :players [{:hand    [bag-of-gold]
                               :deck    [copper]
                               :actions 1}]}
                   (play 0 :bag-of-gold))
               {:supply  [{:card gold :pile-size 29}]
                :players [{:play-area [bag-of-gold]
                           :deck      [gold copper]
                           :actions   1}]}))
        (is (= (-> {:players [{:hand    [diadem]
                               :actions 0
                               :coins   0}]}
                   (play 0 :diadem))
               {:players [{:play-area [diadem]
                           :actions   0
                           :coins     2}]}))
        (is (= (-> {:players [{:hand    [diadem]
                               :actions 1
                               :coins   0}]}
                   (play 0 :diadem))
               {:players [{:play-area [diadem]
                           :actions   1
                           :coins     3}]}))
        (is (= (-> {:players [{:hand    [diadem]
                               :actions 3
                               :coins   0}]}
                   (play 0 :diadem))
               {:players [{:play-area [diadem]
                           :actions   3
                           :coins     5}]}))
        (is (= (-> {:supply  [{:card curse :pile-size 10}
                              {:card estate :pile-size 8}]
                    :players [{:hand    [followers]
                               :deck    [copper copper copper]
                               :actions 1}
                              {:hand [copper copper copper estate estate]}]}
                   (play 0 :followers)
                   (choose [:estate :estate]))
               {:supply  [{:card curse :pile-size 9}
                          {:card estate :pile-size 7}]
                :players [{:hand      [copper copper]
                           :play-area [followers]
                           :deck      [copper]
                           :discard   [estate]
                           :actions   0}
                          {:hand    [copper copper copper]
                           :discard [curse estate estate]}]}))
        (is (= (-> {:supply  [{:card estate :pile-size 8}
                              {:card gold :pile-size 30}]
                    :players [{:hand    [princess]
                               :actions 1
                               :coins   4
                               :buys    1}]}
                   (play 0 :princess)
                   (buy-card 0 :gold)
                   (buy-card 0 :estate))
               {:supply  [{:card estate :pile-size 7}
                          {:card gold :pile-size 29}]
                :players [{:play-area [princess]
                           :discard   [gold estate]
                           :actions   0
                           :coins     0
                           :buys      0}]}))
        (is (= (-> {:players [{:hand    [trusty-steed]
                               :actions 1
                               :coins   0}]}
                   (play 0 :trusty-steed)
                   (choose [:coins :actions]))
               {:players [{:play-area [trusty-steed]
                           :actions   2
                           :coins     2}]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [trusty-steed]
                               :deck    [copper copper copper]
                               :actions 1}]}
                   (play 0 :trusty-steed)
                   (choose [:silvers :cards]))
               {:supply  [{:card silver :pile-size 36}]
                :players [{:hand      [copper copper]
                           :play-area [trusty-steed]
                           :discard   [silver silver silver silver copper]
                           :actions   0}]}))))))

(deftest young-witch-test
  (let [curse (assoc curse :id 0)
        hamlet (assoc hamlet :id 1 :bane? true)]
    (testing "Young Witch"
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [young-witch estate copper silver estate]
                             :deck    [copper copper copper]
                             :actions 1}
                            {:hand [copper]}]}
                 (play 0 :young-witch)
                 (choose [:estate :estate]))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:hand      [copper silver copper copper]
                         :play-area [young-witch]
                         :deck      [copper]
                         :discard   [estate estate]
                         :actions   0}
                        {:hand    [copper]
                         :discard [curse]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [young-witch estate copper silver estate]
                             :deck    [copper copper copper]
                             :actions 1}
                            {:hand [copper hamlet]}]}
                 (play 0 :young-witch)
                 (choose [:estate :estate])
                 (choose nil))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:hand      [copper silver copper copper]
                         :play-area [young-witch]
                         :deck      [copper]
                         :discard   [estate estate]
                         :actions   0}
                        {:hand    [copper hamlet]
                         :discard [curse]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [young-witch estate copper silver estate]
                             :deck    [copper copper copper]
                             :actions 1}
                            {:hand [copper hamlet]}]}
                 (play 0 :young-witch)
                 (choose [:estate :estate])
                 (choose :hamlet))
             {:supply  [{:card curse :pile-size 10}]
              :players [{:hand      [copper silver copper copper]
                         :play-area [young-witch]
                         :deck      [copper]
                         :discard   [estate estate]
                         :actions   0}
                        {:hand [copper hamlet]}]})))))