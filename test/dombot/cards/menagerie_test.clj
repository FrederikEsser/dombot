(ns dombot.cards.menagerie-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.menagerie :as menagerie :refer :all]
            [dombot.cards.dominion :refer [throne-room vassal workshop]]
            [dombot.cards.intrigue :refer [mill nobles]]
            [dombot.cards.prosperity :refer [royal-seal colony platinum]]
            [dombot.cards.empires :refer [crown]]
            [dombot.cards.nocturne :refer [exorcist]]
            [dombot.cards.kingdom :refer [setup-game]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 234 (f)))

(use-fixtures :each fixture)

(deftest exile-test
  (let [gold (assoc gold :id 1)]
    (testing "Exile"
      (is (= (-> {:supply  [{:card gold :pile-size 29}]
                  :players [{:exile    [gold]
                             :triggers [(assoc exile-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :gold})
                 (choose :gold))
             {:supply  [{:card gold :pile-size 28}]
              :players [{:discard  [gold gold]
                         :triggers [(assoc exile-trigger :id 1)]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 28}]
                  :players [{:exile    [gold gold]
                             :triggers [(assoc exile-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :gold})
                 (choose :gold))
             {:supply  [{:card gold :pile-size 27}]
              :players [{:discard  [gold gold gold]
                         :triggers [(assoc exile-trigger :id 1)]}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 29}]
                  :players [{:exile    [gold]
                             :triggers [(assoc exile-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :gold})
                 (choose nil))
             {:supply  [{:card gold :pile-size 28}]
              :players [{:discard  [gold]
                         :exile    [gold]
                         :triggers [(assoc exile-trigger :id 1)]}]})))))

(deftest barge-test
  (let [barge (assoc barge :id 1)]
    (testing "Barge"
      (is (= (-> {:players [{:hand    [barge copper copper gold estate]
                             :deck    [copper copper copper copper]
                             :actions 1
                             :buys    1}]}
                 (play 0 :barge)
                 (choose :now))
             {:players [{:hand      [copper copper gold estate copper copper copper]
                         :play-area [barge]
                         :deck      [copper]
                         :actions   0
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [barge]
                             :deck    [copper copper copper copper copper silver silver silver silver]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :barge)
                 (choose :next-turn)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper silver silver silver]
                                :play-area [barge]
                                :deck      [silver]
                                :actions   1
                                :coins     0
                                :buys      2
                                :phase     :action}]})))))

(deftest bounty-hunter-test
  (let [bounty-hunter (assoc bounty-hunter :id 0)]
    (testing "Bounty Huunter"
      (is (= (-> {:players [{:hand    [bounty-hunter copper]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :bounty-hunter)
                 (choose :copper))
             {:players [{:play-area [bounty-hunter]
                         :exile     [copper]
                         :actions   1
                         :coins     3
                         :phase     :action}]}))
      (is (= (-> {:players [{:hand    [bounty-hunter copper]
                             :exile   [copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :bounty-hunter)
                 (choose :copper))
             {:players [{:play-area [bounty-hunter]
                         :exile     [copper copper]
                         :actions   1
                         :coins     0}]}))
      (is (= (-> {:players [{:hand    [bounty-hunter estate]
                             :exile   [copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :bounty-hunter)
                 (choose :estate))
             {:players [{:play-area [bounty-hunter]
                         :exile     [copper estate]
                         :actions   1
                         :coins     3}]}))
      (is (= (-> {:players [{:hand    [bounty-hunter]
                             :actions 1
                             :coins   0}]}
                 (play 0 :bounty-hunter))
             {:players [{:play-area [bounty-hunter]
                         :actions   1
                         :coins     0}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:players [{:hand    [bounty-hunter gold]
                                            :actions 1
                                            :coins   0
                                            :buys    1}]}
                                (play 0 :bounty-hunter)
                                (choose nil)))))))

(deftest camel-train-test
  (let [camel-train (assoc camel-train :id 0)
        gold        (assoc gold :id 1)]
    (testing "Camel Train"
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [camel-train]
                             :actions 1}]}
                 (play 0 :camel-train)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [camel-train]
                         :exile     [gold]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [camel-train]
                             :actions 1}]}
                 (play 0 :camel-train))
             {:supply  [{:card duchy :pile-size 8}]
              :players [{:play-area [camel-train]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}
                            {:card camel-train :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0 :card-name :camel-train}))
             {:supply  [{:card gold :pile-size 29}
                        {:card camel-train :pile-size 9}]
              :players [{:discard [camel-train]
                         :exile   [gold]}]})))))

(deftest cardinal-test
  (let [cardinal (assoc cardinal :id 0)]
    (testing "Cardinal"
      (is (= (-> {:players [{:hand    [cardinal]
                             :actions 1
                             :coins   0}
                            {:deck [silver estate]}]}
                 (play 0 :cardinal)
                 (choose :silver))
             {:players [{:play-area [cardinal]
                         :actions   0
                         :coins     2}
                        {:exile          [silver]
                         :discard        [estate]
                         :revealed-cards {:discard 1
                                          :exile   1}}]}))
      (is (= (-> {:players [{:hand    [cardinal]
                             :actions 1
                             :coins   0}
                            {:deck [gold province]}]}
                 (play 0 :cardinal)
                 (choose :gold))
             {:players [{:play-area [cardinal]
                         :actions   0
                         :coins     2}
                        {:exile          [gold]
                         :discard        [province]
                         :revealed-cards {:discard 1
                                          :exile   1}}]}))
      (is (= (-> {:players [{:hand    [cardinal]
                             :actions 1
                             :coins   0}
                            {:deck [estate province]}]}
                 (play 0 :cardinal))
             {:players [{:play-area [cardinal]
                         :actions   0
                         :coins     2}
                        {:discard        [estate province]
                         :revealed-cards {:discard 2}}]}))
      (is (= (-> {:players [{:hand    [cardinal]
                             :actions 1
                             :coins   0}
                            {:deck [silver]}]}
                 (play 0 :cardinal)
                 (choose :silver))
             {:players [{:play-area [cardinal]
                         :actions   0
                         :coins     2}
                        {:exile          [silver]
                         :revealed-cards {:exile 1}}]})))))

(deftest cavalry-test
  (let [cavalry (assoc cavalry :id 0)
        horse   (assoc horse :id 1)]
    (testing "Cavalry"
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [cavalry]
                                 :actions 1}]}
                 (play 0 :cavalry))
             {:extra-cards [{:card horse :pile-size 28}]
              :players     [{:play-area [cavalry]
                             :discard   [horse horse]
                             :actions   0}]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 1}]
                  :players     [{:hand    [cavalry]
                                 :actions 1}]}
                 (play 0 :cavalry))
             {:extra-cards [{:card horse :pile-size 0}]
              :players     [{:play-area [cavalry]
                             :discard   [horse]
                             :actions   0}]}))
      (testing "on buy"
        (is (= (-> {:supply  [{:card cavalry :pile-size 10}]
                    :players [{:deck    [silver silver copper]
                               :actions 0
                               :coins   4
                               :buys    1
                               :phase   :pay}]}
                   (buy-card 0 :cavalry))
               {:supply  [{:card cavalry :pile-size 9}]
                :players [{:hand    [silver silver]
                           :deck    [copper]
                           :discard [cavalry]
                           :actions 0
                           :coins   0
                           :buys    1
                           :phase   :action}]}))
        (is (= (-> {:supply  [{:card cavalry :pile-size 10}]
                    :players [{:deck    [copper]
                               :actions 0
                               :coins   4
                               :buys    1
                               :phase   :pay}]}
                   (buy-card 0 :cavalry))
               {:supply  [{:card cavalry :pile-size 9}]
                :players [{:hand    [copper]
                           :discard [cavalry]
                           :actions 0
                           :coins   0
                           :buys    1
                           :phase   :action}]}))
        (is (= (-> {:supply  [{:card cavalry :pile-size 10}]
                    :players [{:hand    [workshop]
                               :deck    [horse silver silver]
                               :actions 1
                               :coins   0
                               :buys    1
                               :phase   :action}]}
                   (play 0 :workshop)
                   (choose :cavalry))
               {:supply  [{:card cavalry :pile-size 9}]
                :players [{:hand      [horse silver]
                           :play-area [workshop]
                           :deck      [silver]
                           :discard   [cavalry]
                           :actions   0
                           :coins     0
                           :buys      2
                           :phase     :action}]}))))))

(deftest coven-test
  (let [coven (assoc coven :id 0)
        curse (assoc curse :id 1)]
    (testing "Coven"
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [coven]
                             :actions 1
                             :coins   0}
                            {}]}
                 (play 0 :coven))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:play-area [coven]
                         :actions   1
                         :coins     2}
                        {:exile [curse]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 1}]
                  :players [{:hand    [coven]
                             :actions 1
                             :coins   0}
                            {:exile (repeat 5 curse)}]}
                 (play 0 :coven))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:play-area [coven]
                         :actions   1
                         :coins     2}
                        {:exile (repeat 6 curse)}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 0}]
                  :players [{:hand    [coven]
                             :actions 1
                             :coins   0}
                            {:exile (repeat 6 curse)}]}
                 (play 0 :coven))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:play-area [coven]
                         :actions   1
                         :coins     2}
                        {:discard (repeat 6 curse)}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 1}]
                  :players [{:hand    [coven]
                             :actions 1
                             :coins   0}
                            {:exile (repeat 6 curse)}
                            {:exile (repeat 6 curse)}]}
                 (play 0 :coven))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:play-area [coven]
                         :actions   1
                         :coins     2}
                        {:exile (repeat 7 curse)}
                        {:discard (repeat 6 curse)}]})))))

(deftest groom-test
  (let [groom  (assoc groom :id 0)
        estate (assoc estate :id 1)
        silver (assoc silver :id 2)
        mill   (assoc mill :id 3)
        horse  (assoc horse :id 4)]
    (testing "Groom"
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card estate :pile-size 8}
                                {:card silver :pile-size 40}
                                {:card groom :pile-size 9}
                                {:card mill :pile-size 8}]
                  :players     [{:hand    [groom]
                                 :actions 1}]}
                 (play 0 :groom)
                 (choose :groom))
             {:extra-cards [{:card horse :pile-size 29}]
              :supply      [{:card estate :pile-size 8}
                            {:card silver :pile-size 40}
                            {:card groom :pile-size 8}
                            {:card mill :pile-size 8}]
              :players     [{:play-area [groom]
                             :discard   [groom horse]
                             :actions   0}]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card estate :pile-size 8}
                                {:card silver :pile-size 40}
                                {:card groom :pile-size 9}
                                {:card mill :pile-size 8}]
                  :players     [{:hand    [groom]
                                 :actions 1}]}
                 (play 0 :groom)
                 (choose :silver))
             {:extra-cards [{:card horse :pile-size 30}]
              :supply      [{:card estate :pile-size 8}
                            {:card silver :pile-size 38}
                            {:card groom :pile-size 9}
                            {:card mill :pile-size 8}]
              :players     [{:play-area [groom]
                             :discard   [silver silver]
                             :actions   0}]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card estate :pile-size 8}
                                {:card silver :pile-size 40}
                                {:card groom :pile-size 9}
                                {:card mill :pile-size 8}]
                  :players     [{:hand    [groom]
                                 :deck    [copper copper]
                                 :actions 1}]}
                 (play 0 :groom)
                 (choose :estate))
             {:extra-cards [{:card horse :pile-size 30}]
              :supply      [{:card estate :pile-size 7}
                            {:card silver :pile-size 40}
                            {:card groom :pile-size 9}
                            {:card mill :pile-size 8}]
              :players     [{:hand      [copper]
                             :play-area [groom]
                             :deck      [copper]
                             :discard   [estate]
                             :actions   1}]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card estate :pile-size 8}
                                {:card silver :pile-size 40}
                                {:card groom :pile-size 9}
                                {:card mill :pile-size 8}]
                  :players     [{:hand    [groom]
                                 :actions 1}]}
                 (play 0 :groom)
                 (choose :mill))
             {:extra-cards [{:card horse :pile-size 29}]
              :supply      [{:card estate :pile-size 8}
                            {:card silver :pile-size 40}
                            {:card groom :pile-size 9}
                            {:card mill :pile-size 7}]
              :players     [{:hand      [horse]
                             :play-area [groom]
                             :deck      [mill]
                             :actions   1}]})))))

(deftest horse-test
  (let [horse (assoc horse :id 1)]
    (testing "Horse"
      (is (= (-> {:extra-cards [{:card horse :pile-size 29}]
                  :players     [{:hand    [horse]
                                 :deck    [copper copper copper]
                                 :actions 1}]}
                 (play 0 :horse))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:hand    [copper copper]
                             :deck    [copper]
                             :actions 1}]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 29}]
                  :players     [{:hand    [throne-room horse]
                                 :deck    [copper copper copper copper copper]
                                 :actions 1}]}
                 (play 0 :throne-room)
                 (choose :horse))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:hand      [copper copper copper copper]
                             :play-area [throne-room]
                             :deck      [copper]
                             :actions   2}]})))))

(deftest hunting-lodge-test
  (let [hunting-lodge (assoc hunting-lodge :id 1)]
    (testing "Hunting Lodge"
      (is (= (-> {:players [{:hand    [hunting-lodge copper copper gold barge]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :hunting-lodge)
                 (choose :no))
             {:players [{:hand      [copper copper gold barge copper]
                         :play-area [hunting-lodge]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [hunting-lodge copper copper estate estate]
                             :deck    [copper copper gold barge silver estate estate]
                             :actions 1}]}
                 (play 0 :hunting-lodge)
                 (choose :yes))
             {:players [{:hand      [copper gold barge silver estate]
                         :play-area [hunting-lodge]
                         :deck      [estate]
                         :discard   [copper copper estate estate copper]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [hunting-lodge copper copper estate village-green]
                             :deck    [copper copper gold barge silver estate estate]
                             :actions 1}]}
                 (play 0 :hunting-lodge)
                 (choose :yes)                              ; discard hand
                 (choose :village-green)                    ; play Village Green on discard
                 (choose :now))
             {:players [{:hand      [copper gold barge silver estate estate]
                         :play-area [hunting-lodge village-green]
                         :discard   [copper copper estate copper]
                         :actions   4}]})))))

(deftest kiln-test
  (let [kiln (assoc kiln :id 0)]
    (testing "Kiln"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [kiln]
                             :actions 1
                             :coins   0}]}
                 (play 0 :kiln))
             {:players [{:play-area [kiln]
                         :actions   0
                         :coins     2
                         :triggers  [(get-trigger kiln)]}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:supply  [{:card gold :pile-size 30}]
                    :players [{:hand    [kiln gold]
                               :actions 1
                               :coins   0}]}
                   (play 0 :kiln)
                   (play 0 :gold)
                   (choose :gold))
               {:supply  [{:card gold :pile-size 29}]
                :players [{:play-area [kiln gold]
                           :discard   [gold]
                           :actions   0
                           :coins     5}]})))
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [kiln copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :kiln)
                 (play 0 :copper)
                 (choose nil))
             {:supply  [{:card copper :pile-size 46}]
              :players [{:play-area [kiln copper]
                         :actions   0
                         :coins     3}]}))
      (ut/reset-ids!)
      (is (= (-> {:supply  [{:card kiln :pile-size 8}]
                  :players [{:hand    [kiln kiln]
                             :actions 2
                             :coins   0}]}
                 (play 0 :kiln)
                 (play 0 :kiln)
                 (choose :kiln))
             {:supply  [{:card kiln :pile-size 7}]
              :players [{:play-area [kiln kiln]
                         :discard   [kiln]
                         :actions   0
                         :coins     4
                         :triggers  [(get-trigger kiln 2)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:supply  [{:card kiln :pile-size 9}]
                  :players [{:hand    [throne-room kiln]
                             :actions 1
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :kiln)                             ; Throne Room the Kiln
                 (choose :kiln))                            ; gain Kiln on second play
             {:supply  [{:card kiln :pile-size 8}]
              :players [{:play-area [throne-room kiln]
                         :discard   [kiln]
                         :actions   0
                         :coins     4
                         :triggers  [(get-trigger kiln 2)]}]}))
      (let [horse (assoc horse :id 1)]
        (is (= (-> {:extra-cards [{:card horse :pile-size 29}]
                    :players     [{:hand    [kiln horse]
                                   :actions 2
                                   :coins   0}]}
                   (play 0 :kiln)
                   (play 0 :horse))
               {:extra-cards [{:card horse :pile-size 30}]
                :players     [{:play-area [kiln]
                               :actions   1
                               :coins     2}]})))
      (let [exorcist (assoc exorcist :id 1)]
        (is (= (-> {:supply  [{:card exorcist :pile-size 9}]
                    :players [{:hand    [kiln exorcist]
                               :actions 1
                               :coins   0
                               :phase   :action}]}
                   (play 0 :kiln)
                   (play 0 :exorcist)
                   (choose :exorcist))
               {:supply  [{:card exorcist :pile-size 8}]
                :players [{:play-area [kiln exorcist]
                           :discard   [exorcist]
                           :actions   0
                           :coins     2
                           :phase     :night}]})))
      (is (= (-> {:players [{:hand    [kiln]
                             :actions 1
                             :coins   0
                             :phase   :action}]}
                 (play 0 :kiln)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand    [kiln]
                                :actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}]})))))

(deftest livery-test
  (let [livery (assoc livery :id 0)
        horse  (assoc horse :id 1)
        silver (assoc silver :id 2)]
    (testing "Livery"
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [livery]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :livery))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:play-area [livery]
                             :actions   0
                             :coins     3
                             :triggers  [(get-trigger livery)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 9}]
                  :players     [{:hand    [livery]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :livery)
                 (gain {:player-no 0 :card-name :livery}))
             {:extra-cards [{:card horse :pile-size 29}]
              :supply      [{:card livery :pile-size 8}]
              :players     [{:play-area [livery]
                             :discard   [horse livery]
                             :actions   0
                             :coins     3
                             :triggers  [(get-trigger livery)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card silver :pile-size 40}]
                  :players     [{:hand    [livery]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :livery)
                 (gain {:player-no 0 :card-name :silver}))
             {:extra-cards [{:card horse :pile-size 30}]
              :supply      [{:card silver :pile-size 39}]
              :players     [{:play-area [livery]
                             :discard   [silver]
                             :actions   0
                             :coins     3
                             :triggers  [(get-trigger livery)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 9}]
                  :players     [{:hand    [livery]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :livery)
                 (gain {:player-no 0 :card-name :livery})
                 (gain {:player-no 0 :card-name :livery}))
             {:extra-cards [{:card horse :pile-size 28}]
              :supply      [{:card livery :pile-size 7}]
              :players     [{:play-area [livery]
                             :discard   [horse livery horse livery]
                             :actions   0
                             :coins     3
                             :triggers  [(get-trigger livery)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 8}]
                  :players     [{:hand    [livery livery]
                                 :actions 2
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :livery)
                 (play 0 :livery)
                 (buy-card 0 :livery))
             {:extra-cards [{:card horse :pile-size 28}]
              :supply      [{:card livery :pile-size 7}]
              :players     [{:play-area [livery livery]
                             :discard   [horse horse livery]
                             :actions   0
                             :coins     1
                             :buys      0
                             :triggers  [(get-trigger livery)
                                         (get-trigger livery 2)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 9}]
                  :players     [{:hand    [throne-room livery]
                                 :actions 1
                                 :coins   0}]}
                 (play 0 :throne-room)
                 (choose :livery)
                 (gain {:player-no 0 :card-name :livery}))
             {:extra-cards [{:card horse :pile-size 28}]
              :supply      [{:card livery :pile-size 8}]
              :players     [{:play-area [throne-room livery]
                             :discard   [horse horse livery]
                             :actions   0
                             :coins     6
                             :triggers  [(get-trigger livery)
                                         (get-trigger livery 2)]}]})))))

(deftest mastermind-test
  (let [mastermind (assoc mastermind :id 0)
        livery     (assoc livery :id 1)]
    (testing "Mastermind"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [mastermind]
                             :deck    [livery copper copper copper copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :mastermind))
             {:players [{:play-area [mastermind]
                         :deck      [livery copper copper copper copper]
                         :actions   0
                         :phase     :action
                         :triggers  [(get-trigger mastermind)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [mastermind]
                             :deck    [livery copper copper copper copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :mastermind)
                 (end-turn 0)
                 (choose :livery))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper]
                                :play-area [mastermind livery]
                                :actions   1
                                :coins     9
                                :buys      1
                                :phase     :action
                                :triggers  [(get-trigger livery 2)
                                            (get-trigger livery 3)
                                            (get-trigger livery 4)]}]}))
      (is (= (-> {:players [{:hand    [mastermind]
                             :deck    [scrap province province gold gold]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :mastermind)
                 (end-turn 0)
                 (choose nil))
             {:current-player 0
              :players        [{:hand      [scrap province province gold gold]
                                :play-area [mastermind]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:players [{:hand    [mastermind]
                             :deck    [copper copper copper copper copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :mastermind)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper]
                                :play-area [mastermind]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]})))))

(deftest sanctuary-test
  (let [sanctuary (assoc sanctuary :id 0)]
    (testing "Sanctuary"
      (is (= (-> {:players [{:hand    [sanctuary]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :sanctuary)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [sanctuary]
                         :deck      [copper]
                         :actions   1
                         :coins     0
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [sanctuary]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :sanctuary)
                 (choose :copper))
             {:players [{:play-area [sanctuary]
                         :deck      [copper]
                         :exile     [copper]
                         :actions   1
                         :coins     0
                         :buys      2}]}))
      (ut/reset-ids!)
      (is (= (-> {:supply  [{:card sanctuary :pile-size 10}]
                  :players [{}]}
                 setup-game)
             {:supply  [{:card sanctuary :pile-size 10}]
              :players [{:triggers [(assoc exile-trigger :id 1)]}]})))))

(deftest scrap-test
  (let [scrap  (assoc scrap :id 0)
        horse  (assoc horse :id 1)
        silver (assoc silver :id 2)]
    (testing "Scrap"
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [scrap copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :scrap)
                 (choose :copper))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:play-area [scrap]
                             :actions   0
                             :coins     0
                             :buys      1}]
              :trash       [copper]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [scrap estate]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :scrap)
                 (choose :estate)
                 (choose [:card :action]))
             {:extra-cards [{:card horse :pile-size 30}]
              :players     [{:hand      [copper]
                             :play-area [scrap]
                             :deck      [copper]
                             :actions   1
                             :coins     0
                             :buys      1}]
              :trash       [estate]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [scrap silver]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :scrap)
                 (choose :silver)
                 (choose [:buy :coin :horse]))
             {:extra-cards [{:card horse :pile-size 29}]
              :players     [{:play-area [scrap]
                             :deck      [copper copper]
                             :discard   [horse]
                             :actions   0
                             :coins     1
                             :buys      2}]
              :trash       [silver]}))
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card silver :pile-size 40}]
                  :players     [{:hand    [scrap gold]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :scrap)
                 (choose :gold)
                 (choose [:card :action :buy :coin :silver :horse]))
             {:extra-cards [{:card horse :pile-size 29}]
              :supply      [{:card silver :pile-size 39}]
              :players     [{:play-area [scrap]
                             :discard   [silver horse]      ; cards are gained after +1 Card
                             :actions   1
                             :coins     1
                             :buys      2}]
              :trash       [gold]})))))

(deftest snowy-village-test
  (let [snowy-village (assoc snowy-village :id 0)]
    (testing "Snowy Village"
      (is (= (-> {:players [{:hand    [snowy-village]
                             :deck    [snowy-village copper]
                             :actions 1
                             :buys    1}]}
                 (play 0 :snowy-village))
             {:players [{:hand            [snowy-village]
                         :play-area       [snowy-village]
                         :deck            [copper]
                         :actions         4
                         :buys            2
                         :ignore-actions? true}]}))
      (is (= (-> {:players [{:hand            [snowy-village]
                             :play-area       [snowy-village]
                             :deck            [copper]
                             :actions         4
                             :buys            2
                             :ignore-actions? true}]}
                 (play 0 :snowy-village))
             {:players [{:hand            [copper]
                         :play-area       [snowy-village snowy-village]
                         :actions         3
                         :buys            3
                         :ignore-actions? true}]}))
      (is (= (-> {:players [{:actions         0
                             :villagers       1
                             :ignore-actions? true}]}
                 (spend-villager 0))
             {:players [{:actions         0
                         :villagers       0
                         :ignore-actions? true}]})))))

(deftest stockpile-test
  (let [stockpile (assoc stockpile :id 0)]
    (testing "Stockpile"
      (is (= (-> {:players [{:hand    [stockpile]
                             :actions 0
                             :coins   0
                             :buys    1
                             :phase   :action}]}
                 (play 0 :stockpile))
             {:players [{:exile   [stockpile]
                         :actions 0
                         :coins   3
                         :buys    2
                         :phase   :pay}]})))))

(deftest supplies-test
  (let [supplies (assoc supplies :id 0)
        horse    (assoc horse :id 1)]
    (testing "Supplies"
      (is (= (-> {:extra-cards [{:card horse :pile-size 30}]
                  :players     [{:hand    [supplies]
                                 :actions 0
                                 :coins   0}]}
                 (play 0 :supplies))
             {:extra-cards [{:card horse :pile-size 29}]
              :players     [{:play-area [supplies]
                             :deck      [horse]
                             :actions   0
                             :coins     1}]})))))

(deftest village-green-test
  (let [village-green (assoc village-green :id 0)]
    (testing "Village Green"
      (is (= (-> {:players [{:hand    [village-green copper copper gold barge]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :village-green)
                 (choose :now))
             {:players [{:hand      [copper copper gold barge copper]
                         :play-area [village-green]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [village-green copper copper estate estate]
                             :deck    [copper copper gold barge estate silver silver]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :village-green)
                 (choose :next-turn)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper gold barge estate silver]
                                :play-area [village-green]
                                :deck      [silver]
                                :discard   [copper copper estate estate]
                                :actions   3
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:supply  [{:card village-green :pile-size 9}]
                  :players [{:deck     [copper copper]
                             :exile    [village-green]
                             :actions  0
                             :triggers [(assoc exile-trigger :id 1)]}]}
                 (gain {:player-no 0 :card-name :village-green})
                 (choose :village-green)                    ; discard from Exile
                 (choose :village-green)                    ; play when discarded
                 (choose :now))
             {:supply  [{:card village-green :pile-size 8}]
              :players [{:hand      [copper]
                         :play-area [village-green]
                         :deck      [copper]
                         :discard   [village-green]
                         :actions   2
                         :triggers  [(assoc exile-trigger :id 1)]}]}))
      (is (= (-> {:players [{:hand    [cardinal]
                             :actions 1
                             :coins   0
                             :phase   :action}
                            {:hand  [copper copper copper copper copper]
                             :deck  [village-green silver gold]
                             :phase :out-of-turn}]}
                 (play 0 :cardinal)
                 (choose :silver)                           ; exile Silver
                 (choose :village-green)                    ; play when discarded
                 (choose :next-turn)
                 (end-turn 0))
             {:current-player 1
              :players        [{:hand    [cardinal]
                                :actions 0
                                :coins   0
                                :buys    0
                                :phase   :out-of-turn}
                               {:hand      [copper copper copper copper copper gold]
                                :play-area [village-green]
                                :exile     [silver]
                                :actions   3
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (let [vassal (assoc vassal :id 1)]
        (is (= (-> {:players [{:hand    [vassal]
                               :deck    [village-green silver]
                               :actions 1
                               :coins   0}]}
                   (play 0 :vassal)
                   (choose :village-green)                  ; play when discarded
                   (choose :now))                           ; problem: should still be able to play through Vassal, but lost track
               {:players [{:hand      [silver]
                           :play-area [vassal village-green]
                           :actions   2
                           :coins     2}]}))))))

(deftest alliance-test
  (let [estate   (assoc estate :id 1)
        duchy    (assoc duchy :id 2)
        province (assoc province :id 3)
        copper   (assoc copper :id 4)
        silver   (assoc silver :id 5)
        gold     (assoc gold :id 6)]
    (testing "Alliance"
      (is (= (-> {:events      {:alliance alliance}
                  :extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card curse :pile-size 10}
                                {:card estate :pile-size 8}
                                {:card duchy :pile-size 0}
                                {:card province :pile-size 8}
                                {:card copper :pile-size 46}
                                {:card silver :pile-size 40}
                                {:card gold :pile-size 30}
                                {:card colony :pile-size 8}
                                {:card platinum :pile-size 12}
                                {:card supplies :pile-size 10}
                                {:card livery :pile-size 10}]
                  :players     [{:coins 10
                                 :buys  1}]}
                 (buy-event 0 :alliance))
             {:events      {:alliance alliance}
              :extra-cards [{:card horse :pile-size 30}]
              :supply      [{:card curse :pile-size 10}
                            {:card estate :pile-size 7}
                            {:card duchy :pile-size 0}
                            {:card province :pile-size 7}
                            {:card copper :pile-size 45}
                            {:card silver :pile-size 39}
                            {:card gold :pile-size 29}
                            {:card colony :pile-size 8}
                            {:card platinum :pile-size 12}
                            {:card supplies :pile-size 10}
                            {:card livery :pile-size 10}]
              :players     [{:discard [province estate gold silver copper]
                             :coins   0
                             :buys    0}]})))))

(deftest banish-test
  (testing "Banish"
    (is (= (-> {:events  {:banish banish}
                :players [{:hand  [copper copper estate]
                           :coins 4
                           :buys  1}]}
               (buy-event 0 :banish)
               (choose [:copper :copper]))
           {:events  {:banish banish}
            :players [{:hand  [estate]
                       :exile [copper copper]
                       :coins 0
                       :buys  0}]}))
    (is (= (-> {:events  {:banish banish}
                :players [{:hand  [copper copper estate]
                           :coins 4
                           :buys  1}]}
               (buy-event 0 :banish)
               (choose [:copper]))
           {:events  {:banish banish}
            :players [{:hand  [copper estate]
                       :exile [copper]
                       :coins 0
                       :buys  0}]}))
    (is (= (-> {:events  {:banish banish}
                :players [{:hand  [copper copper estate]
                           :coins 4
                           :buys  1}]}
               (buy-event 0 :banish)
               (choose :estate))
           {:events  {:banish banish}
            :players [{:hand  [copper copper]
                       :exile [estate]
                       :coins 0
                       :buys  0}]}))
    (is (= (-> {:events  {:banish banish}
                :players [{:hand  [copper copper estate]
                           :coins 4
                           :buys  1}]}
               (buy-event 0 :banish)
               (choose nil))
           {:events  {:banish banish}
            :players [{:hand  [copper copper estate]
                       :coins 0
                       :buys  0}]}))
    (is (= (-> {:events  {:banish banish}
                :players [{:coins 4
                           :buys  1}]}
               (buy-event 0 :banish))
           {:events  {:banish banish}
            :players [{:coins 0
                       :buys  0}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:events  {:banish banish}
                               :players [{:hand  [copper copper estate]
                                          :coins 4
                                          :buys  1}]}
                              (buy-event 0 :banish)
                              (choose [:copper :copper :estate]))))))

(deftest bargain-test
  (let [horse  (assoc horse :id 1)
        livery (assoc livery :id 2)]
    (testing "Bargain"
      (is (= (-> {:events      {:bargain bargain}
                  :extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card livery :pile-size 10}]
                  :players     [{:coins 4
                                 :buys  1}
                                {}
                                {}]}
                 (buy-event 0 :bargain)
                 (choose :livery))
             {:events      {:bargain bargain}
              :extra-cards [{:card horse :pile-size 28}]
              :supply      [{:card livery :pile-size 9}]
              :players     [{:discard [livery]
                             :coins   0
                             :buys    0}
                            {:discard [horse]}
                            {:discard [horse]}]}))
      (is (= (-> {:events      {:bargain bargain}
                  :extra-cards [{:card horse :pile-size 1}]
                  :supply      [{:card duchy :pile-size 8}]
                  :players     [{:coins 4
                                 :buys  1}
                                {}
                                {}]}
                 (buy-event 0 :bargain))
             {:events      {:bargain bargain}
              :extra-cards [{:card horse :pile-size 0}]
              :supply      [{:card duchy :pile-size 8}]
              :players     [{:coins 0
                             :buys  0}
                            {:discard [horse]}
                            {}]})))))

(deftest commerce-test
  (let [gold (assoc gold :id 1)]
    (testing "Commerce"
      (is (= (-> {:events  {:commerce commerce}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:coins 5
                             :buys  1}]}
                 (buy-event 0 :commerce))
             {:events  {:commerce commerce}
              :supply  [{:card gold :pile-size 30}]
              :players [{:coins 0
                         :buys  0}]}))
      (is (= (-> {:track-gained-cards? true
                  :events              {:commerce commerce}
                  :supply              [{:card gold :pile-size 30}]
                  :players             [{:gained-cards [{:name  :silver
                                                         :types #{:treasure}
                                                         :cost  3}]
                                         :coins        5
                                         :buys         1}]}
                 (buy-event 0 :commerce))
             {:track-gained-cards? true
              :events              {:commerce commerce}
              :supply              [{:card gold :pile-size 29}]
              :players             [{:discard      [gold]
                                     :gained-cards [{:name  :silver
                                                     :types #{:treasure}
                                                     :cost  3}
                                                    {:name  :gold
                                                     :types #{:treasure}
                                                     :cost  6}]
                                     :coins        0
                                     :buys         0}]}))
      (is (= (-> {:track-gained-cards? true
                  :events              {:commerce commerce}
                  :supply              [{:card gold :pile-size 30}]
                  :players             [{:gained-cards [{:name  :silver
                                                         :types #{:treasure}
                                                         :cost  3}
                                                        {:name  :silver
                                                         :types #{:treasure}
                                                         :cost  3}]
                                         :coins        5
                                         :buys         1}]}
                 (buy-event 0 :commerce))
             {:track-gained-cards? true
              :events              {:commerce commerce}
              :supply              [{:card gold :pile-size 29}]
              :players             [{:discard      [gold]
                                     :gained-cards [{:name  :silver
                                                     :types #{:treasure}
                                                     :cost  3}
                                                    {:name  :silver
                                                     :types #{:treasure}
                                                     :cost  3}
                                                    {:name  :gold
                                                     :types #{:treasure}
                                                     :cost  6}]
                                     :coins        0
                                     :buys         0}]}))
      (is (= (-> {:track-gained-cards? true
                  :events              {:commerce commerce}
                  :supply              [{:card gold :pile-size 29}]
                  :players             [{:gained-cards [{:name  :silver
                                                         :types #{:treasure}
                                                         :cost  3}
                                                        {:name  :gold
                                                         :types #{:treasure}
                                                         :cost  6}]
                                         :coins        5
                                         :buys         1}]}
                 (buy-event 0 :commerce))
             {:track-gained-cards? true
              :events              {:commerce commerce}
              :supply              [{:card gold :pile-size 27}]
              :players             [{:discard      [gold gold]
                                     :gained-cards [{:name  :silver
                                                     :types #{:treasure}
                                                     :cost  3}
                                                    {:name  :gold
                                                     :types #{:treasure}
                                                     :cost  6}
                                                    {:name  :gold
                                                     :types #{:treasure}
                                                     :cost  6}
                                                    {:name  :gold
                                                     :types #{:treasure}
                                                     :cost  6}]
                                     :coins        0
                                     :buys         0}]})))))

(deftest demand-test
  (let [horse    (assoc horse :id 1)
        cardinal (assoc cardinal :id 2)]
    (testing "Demand"
      (is (= (-> {:events      {:demand demand}
                  :extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card cardinal :pile-size 10}]
                  :players     [{:deck  [copper]
                                 :coins 5
                                 :buys  1}]}
                 (buy-event 0 :demand)
                 (choose :cardinal))
             {:events      {:demand demand}
              :extra-cards [{:card horse :pile-size 29}]
              :supply      [{:card cardinal :pile-size 9}]
              :players     [{:deck  [cardinal horse copper]
                             :coins 0
                             :buys  0}]})))))

(deftest desperation-test
  (let [curse (assoc curse :id 1)]
    (testing "Desperation"
      (is (= (-> {:events  {:desperation desperation}
                  :supply  [{:card curse :pile-size 10}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :desperation))
             {:events  {:desperation desperation}
              :supply  [{:card curse :pile-size 9}]
              :players [{:discard       [curse]
                         :bought-events #{:desperation}
                         :coins         6
                         :buys          1}]}))
      (is (= (-> {:events  {:desperation desperation}
                  :supply  [{:card curse :pile-size 0}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-event 0 :desperation))
             {:events  {:desperation desperation}
              :supply  [{:card curse :pile-size 0}]
              :players [{:bought-events #{:desperation}
                         :coins         4
                         :buys          0}]}))
      (is (thrown-with-msg? AssertionError #"Buy error:"
                            (-> {:events  {:desperation desperation}
                                 :supply  [{:card curse :pile-size 9}]
                                 :players [{:bought-events #{:desperation}
                                            :coins         6
                                            :buys          1}]}
                                (buy-event 0 :desperation)))))))

(deftest enclave-test
  (let [gold  (assoc gold :id 1)
        duchy (assoc duchy :id 2)]
    (testing "Enclave"
      (is (= (-> {:events  {:enclave enclave}
                  :supply  [{:card duchy :pile-size 8}
                            {:card gold :pile-size 30}]
                  :players [{:coins 8
                             :buys  1}]}
                 (buy-event 0 :enclave))
             {:events  {:enclave enclave}
              :supply  [{:card duchy :pile-size 7}
                        {:card gold :pile-size 29}]
              :players [{:discard [gold]
                         :exile   [duchy]
                         :coins   0
                         :buys    0}]})))))

(deftest gamble-test
  (let [livery (assoc livery :id 1)]
    (testing "Gamble"
      (ut/reset-ids!)
      (is (= (-> {:events  {:gamble gamble}
                  :players [{:deck    [livery]
                             :actions 0
                             :coins   5
                             :buys    1}]}
                 (buy-event 0 :gamble)
                 (choose :livery))
             {:events  {:gamble gamble}
              :players [{:play-area      [livery]
                         :revealed-cards {:play-area 1}
                         :actions        0
                         :coins          6
                         :buys           1
                         :triggers       [(get-trigger livery)]}]}))
      (is (= (-> {:events  {:gamble gamble}
                  :players [{:deck    [gold]
                             :actions 0
                             :coins   5
                             :buys    1}]}
                 (buy-event 0 :gamble)
                 (choose :gold))
             {:events  {:gamble gamble}
              :players [{:play-area      [gold]
                         :revealed-cards {:play-area 1}
                         :actions        0
                         :coins          6
                         :buys           1}]}))
      (is (= (-> {:events  {:gamble gamble}
                  :players [{:deck    [livery]
                             :actions 0
                             :coins   5
                             :buys    1}]}
                 (buy-event 0 :gamble)
                 (choose nil))
             {:events  {:gamble gamble}
              :players [{:discard        [livery]
                         :revealed-cards {:discard 1}
                         :actions        0
                         :coins          3
                         :buys           1}]}))
      (is (= (-> {:events  {:gamble gamble}
                  :players [{:deck    [estate]
                             :actions 0
                             :coins   5
                             :buys    1}]}
                 (buy-event 0 :gamble))
             {:events  {:gamble gamble}
              :players [{:discard        [estate]
                         :revealed-cards {:discard 1}
                         :actions        0
                         :coins          3
                         :buys           1}]}))
      (is (= (-> {:events  {:gamble gamble}
                  :players [{:coins 5
                             :buys  1}]}
                 (buy-event 0 :gamble))
             {:events  {:gamble gamble}
              :players [{:coins 3
                         :buys  1}]})))))

(deftest populate-test
  (let [livery        (assoc livery :id 1)
        crown         (assoc crown :id 2)
        nobles        (assoc nobles :id 3)
        village-green (assoc village-green :id 4)]
    (testing "Populate"
      (is (= (-> {:events      {:populate populate}
                  :extra-cards [{:card horse :pile-size 30}]
                  :supply      [{:card curse :pile-size 10}
                                {:card estate :pile-size 8}
                                {:card duchy :pile-size 8}
                                {:card province :pile-size 8}
                                {:card copper :pile-size 46}
                                {:card silver :pile-size 40}
                                {:card gold :pile-size 30}
                                {:card supplies :pile-size 10}
                                {:card livery :pile-size 10}
                                {:card crown :pile-size 10}
                                {:card nobles :pile-size 8}
                                {:card village-green :pile-size 10}
                                {:card groom :pile-size 0}
                                {:card exorcist :pile-size 10}]
                  :players     [{:coins 10
                                 :buys  1}]}
                 (buy-event 0 :populate))
             {:events      {:populate populate}
              :extra-cards [{:card horse :pile-size 30}]
              :supply      [{:card curse :pile-size 10}
                            {:card estate :pile-size 8}
                            {:card duchy :pile-size 8}
                            {:card province :pile-size 8}
                            {:card copper :pile-size 46}
                            {:card silver :pile-size 40}
                            {:card gold :pile-size 30}
                            {:card supplies :pile-size 10}
                            {:card livery :pile-size 9}
                            {:card crown :pile-size 9}
                            {:card nobles :pile-size 7}
                            {:card village-green :pile-size 9}
                            {:card groom :pile-size 0}
                            {:card exorcist :pile-size 10}]
              :players     [{:discard [livery crown nobles village-green]
                             :coins   0
                             :buys    0}]})))))

(deftest pursue-test
  (testing "Pursue"
    (is (= (-> {:events      {:pursue pursue}
                :extra-cards [{:card horse :pile-size 28}]
                :players     [{:deck  [horse copper horse copper copper]
                               :coins 2
                               :buys  1}]}
               (buy-event 0 :pursue)
               (choose {:area :extra-cards :card-name :horse}))
           {:events      {:pursue pursue}
            :extra-cards [{:card horse :pile-size 28}]
            :players     [{:deck           [horse horse copper]
                           :discard        [copper copper]
                           :revealed-cards {:deck    2
                                            :discard 2}
                           :coins          0
                           :buys           1}]}))
    (is (= (-> {:events      {:pursue pursue}
                :extra-cards [{:card horse :pile-size 0}]
                :players     [{:deck  [horse copper horse copper copper]
                               :coins 2
                               :buys  1}]}
               (buy-event 0 :pursue)
               (choose {:area :extra-cards :card-name :horse}))
           {:events      {:pursue pursue}
            :extra-cards [{:card horse :pile-size 0}]
            :players     [{:deck           [horse horse copper]
                           :discard        [copper copper]
                           :revealed-cards {:deck    2
                                            :discard 2}
                           :coins          0
                           :buys           1}]}))
    (is (= (-> {:events      {:pursue pursue}
                :extra-cards [{:card horse :pile-size 28}]
                :supply      [{:card silver :pile-size 43}]
                :players     [{:deck  [horse silver silver silver horse]
                               :coins 2
                               :buys  1}]}
               (buy-event 0 :pursue)
               (choose {:area :supply :card-name :silver}))
           {:events      {:pursue pursue}
            :extra-cards [{:card horse :pile-size 28}]
            :supply      [{:card silver :pile-size 43}]
            :players     [{:deck           [silver silver silver horse]
                           :discard        [horse]
                           :revealed-cards {:deck    3
                                            :discard 1}
                           :coins          0
                           :buys           1}]}))
    (is (= (-> {:events  {:pursue pursue}
                :supply  [{:card silver :pile-size 43}]
                :players [{:deck  [copper copper copper copper copper]
                           :coins 2
                           :buys  1}]}
               (buy-event 0 :pursue)
               (choose {:area :supply :card-name :silver}))
           {:events  {:pursue pursue}
            :supply  [{:card silver :pile-size 43}]
            :players [{:deck           [copper]
                       :discard        [copper copper copper copper]
                       :revealed-cards {:discard 4}
                       :coins          0
                       :buys           1}]}))
    (is (= (-> {:events  {:pursue pursue}
                :supply  [{:card silver :pile-size 43}]
                :players [{:deck  [silver silver silver silver silver]
                           :coins 2
                           :buys  1}]}
               (buy-event 0 :pursue)
               (choose {:area :supply :card-name :silver}))
           {:events  {:pursue pursue}
            :supply  [{:card silver :pile-size 43}]
            :players [{:deck           [silver silver silver silver silver]
                       :revealed-cards {:deck 4}
                       :coins          0
                       :buys           1}]}))
    (is (= (-> {:events  {:pursue pursue}
                :supply  [{:card silver :pile-size 43}]
                :players [{:discard  [silver silver silver silver silver]
                           :coins 2
                           :buys  1}]}
               (buy-event 0 :pursue)
               (choose {:area :supply :card-name :silver}))
           {:events  {:pursue pursue}
            :supply  [{:card silver :pile-size 43}]
            :players [{:deck           [silver silver silver silver silver]
                       :revealed-cards {:deck 4}
                       :coins          0
                       :buys           1}]}))))

(deftest reap-test
  (let [gold (assoc gold :id 1)]
    (testing "Reap"
      (ut/reset-ids!)
      (is (= (-> {:events  {:reap reap}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:coins 7
                             :buys  1}]}
                 (buy-event 0 :reap))
             {:events  {:reap reap}
              :supply  [{:card gold :pile-size 29}]
              :players [{:coins    0
                         :buys     0
                         :triggers [(merge reap-trigger {:id        1
                                                         :set-aside [gold]})]}]}))
      (is (= (-> {:events  {:reap reap}
                  :supply  [{:card gold :pile-size 30}]
                  :players [{:play-area [royal-seal]
                             :coins     7
                             :buys      1}]}
                 (buy-event 0 :reap)
                 (choose :gold))                            ; topdeck from Royal Seal
             {:events  {:reap reap}
              :supply  [{:card gold :pile-size 29}]
              :players [{:play-area [royal-seal]
                         :deck      [gold]
                         :coins     0
                         :buys      0}]})))))

(deftest ride-test
  (let [horse (assoc horse :id 1)]
    (testing "Ride"
      (is (= (-> {:events      {:ride ride}
                  :extra-cards [{:card horse :pile-size 30}]
                  :players     [{:coins 2
                                 :buys  1}]}
                 (buy-event 0 :ride))
             {:events      {:ride ride}
              :extra-cards [{:card horse :pile-size 29}]
              :players     [{:discard [horse]
                             :coins   0
                             :buys    0}]})))))

(deftest toil-test
  (let [livery (assoc livery :id 1)]
    (testing "Toil"
      (ut/reset-ids!)
      (is (= (-> {:events  {:toil toil}
                  :players [{:hand    [livery]
                             :actions 0
                             :coins   5
                             :buys    1}]}
                 (buy-event 0 :toil)
                 (choose :livery))
             {:events  {:toil toil}
              :players [{:play-area [livery]
                         :actions   0
                         :coins     6
                         :buys      1
                         :triggers  [(get-trigger livery)]}]}))
      (is (= (-> {:events  {:toil toil}
                  :players [{:hand    [livery]
                             :actions 0
                             :coins   5
                             :buys    1}]}
                 (buy-event 0 :toil)
                 (choose nil))
             {:events  {:toil toil}
              :players [{:hand    [livery]
                         :actions 0
                         :coins   3
                         :buys    1}]}))
      (is (= (-> {:events  {:toil toil}
                  :players [{:coins 5
                             :buys  1}]}
                 (buy-event 0 :toil))
             {:events  {:toil toil}
              :players [{:coins 3
                         :buys  1}]})))))

(deftest stampede-test
  (let [horse (assoc horse :id 1)]
    (testing "Stampede"
      (is (= (-> {:events      {:stampede stampede}
                  :extra-cards [{:card horse :pile-size 30}]
                  :players     [{:coins 5
                                 :buys  1}]}
                 (buy-event 0 :stampede))
             {:events      {:stampede stampede}
              :extra-cards [{:card horse :pile-size 25}]
              :players     [{:deck  (repeat 5 horse)
                             :coins 0
                             :buys  0}]}))
      (is (= (-> {:events      {:stampede stampede}
                  :extra-cards [{:card horse :pile-size 3}]
                  :players     [{:play-area (repeat 5 copper)
                                 :deck      [silver]
                                 :coins     5
                                 :buys      1}]}
                 (buy-event 0 :stampede))
             {:events      {:stampede stampede}
              :extra-cards [{:card horse :pile-size 0}]
              :players     [{:play-area (repeat 5 copper)
                             :deck      [horse horse horse silver]
                             :coins     0
                             :buys      0}]}))
      (is (= (-> {:events      {:stampede stampede}
                  :extra-cards [{:card horse :pile-size 30}]
                  :players     [{:play-area (repeat 6 copper)
                                 :coins     5
                                 :buys      1}]}
                 (buy-event 0 :stampede))
             {:events      {:stampede stampede}
              :extra-cards [{:card horse :pile-size 30}]
              :players     [{:play-area (repeat 6 copper)
                             :coins     0
                             :buys      0}]})))))

(deftest transport-test
  (let [livery (assoc livery :id 1)
        nobles (assoc nobles :id 2)]
    (testing "Transport"
      (is (= (-> {:events  {:transport transport}
                  :supply  [{:card livery :pile-size 10}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-event 0 :transport)
                 (choose :exile)
                 (choose :livery))
             {:events  {:transport transport}
              :supply  [{:card livery :pile-size 9}]
              :players [{:exile [livery]
                         :coins 0
                         :buys  0}]}))
      (is (= (-> {:events  {:transport transport}
                  :supply  [{:card nobles :pile-size 10}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-event 0 :transport)
                 (choose :exile)
                 (choose :nobles))
             {:events  {:transport transport}
              :supply  [{:card nobles :pile-size 9}]
              :players [{:exile [nobles]
                         :coins 0
                         :buys  0}]}))
      (is (= (-> {:events  {:transport transport}
                  :supply  [{:card duchy :pile-size 8}]
                  :players [{:coins 3
                             :buys  1}]}
                 (buy-event 0 :transport)
                 (choose :exile))
             {:events  {:transport transport}
              :supply  [{:card duchy :pile-size 8}]
              :players [{:coins 0
                         :buys  0}]}))
      (is (= (-> {:events  {:transport transport}
                  :supply  [{:card livery :pile-size 9}]
                  :players [{:exile [livery]
                             :coins 3
                             :buys  1}]}
                 (buy-event 0 :transport)
                 (choose :deliver)
                 (choose :livery))
             {:events  {:transport transport}
              :supply  [{:card livery :pile-size 9}]
              :players [{:deck  [livery]
                         :coins 0
                         :buys  0}]}))
      (is (= (-> {:events  {:transport transport}
                  :supply  [{:card livery :pile-size 9}]
                  :players [{:exile [livery livery]
                             :coins 3
                             :buys  1}]}
                 (buy-event 0 :transport)
                 (choose :deliver)
                 (choose :livery))
             {:events  {:transport transport}
              :supply  [{:card livery :pile-size 9}]
              :players [{:deck  [livery]
                         :exile [livery]
                         :coins 0
                         :buys  0}]})))))
