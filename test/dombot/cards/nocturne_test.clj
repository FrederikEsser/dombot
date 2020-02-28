(ns dombot.cards.nocturne-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dominion :refer [militia throne-room witch]]
            [dombot.cards.intrigue :refer [lurker]]
            [dombot.cards.seaside :refer [fishing-village]]
            [dombot.cards.prosperity :as prosperity :refer [hoard mint talisman]]
            [dombot.cards.cornucopia :refer [trusty-steed]]
            [dombot.cards.nocturne :as nocturne :refer :all]
            [dombot.cards.renaissance :refer [patron silk-merchant academy]]
            [dombot.cards.kingdom :refer [setup-game]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest boons-test
  (testing "Boons"
    (testing "The Earth's Gift"
      (is (= (-> {:boons   {:deck [earth-gift]}
                  :players [{:hand [estate]}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [earth-gift]}
              :players [{:hand [estate]}]}))
      (is (= (-> {:boons   {:deck [earth-gift]}
                  :players [{:hand [estate copper]}]}
                 (receive-boon {:player-no 0})
                 (choose nil))
             {:boons   {:discard [earth-gift]}
              :players [{:hand [estate copper]}]}))
      (let [bard (assoc bard :id 1)]
        (is (= (-> {:boons   {:deck [earth-gift]}
                    :supply  [{:card bard :pile-size 9}]
                    :players [{:hand [estate copper]}]}
                   (receive-boon {:player-no 0})
                   (choose :copper)
                   (choose :bard))
               {:boons   {:discard [earth-gift]}
                :supply  [{:card bard :pile-size 8}]
                :players [{:hand    [estate]
                           :discard [copper bard]}]}))))
    (testing "The Field's Gift"
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [field-gift]}
                  :players [{:actions 0
                             :coins   0}]}
                 (receive-boon {:player-no 0}))
             {:boons   {}
              :players [{:actions  1
                         :coins    1
                         :boons    [field-gift]
                         :triggers [{:id       1
                                     :name     :the-field's-gift
                                     :event    :at-clean-up
                                     :duration :once
                                     :effects  [[:return-boon {:boon-name :the-field's-gift}]]}]}]}))
      (is (= (-> {:boons   {:deck [field-gift]}
                  :players [{:actions 0
                             :coins   0
                             :phase   :buy}]}
                 (receive-boon {:player-no 0})
                 (clean-up {:player-no 0}))
             {:boons   {:discard [field-gift]}
              :players [{:actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]})))
    (testing "The Flame's Gift"
      (is (= (-> {:boons   {:deck [flame-gift]}
                  :players [{:hand [estate]}]}
                 (receive-boon {:player-no 0})
                 (choose nil))
             {:boons   {:discard [flame-gift]}
              :players [{:hand [estate]}]}))
      (is (= (-> {:boons   {:deck [flame-gift]}
                  :players [{:hand [estate]}]}
                 (receive-boon {:player-no 0})
                 (choose :estate))
             {:boons   {:discard [flame-gift]}
              :players [{}]
              :trash   [estate]})))
    (testing "The Forest's Gift"
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [forest-gift]}
                  :players [{:coins 0
                             :buys  1}]}
                 (receive-boon {:player-no 0}))
             {:boons   {}
              :players [{:coins    1
                         :buys     2
                         :boons    [forest-gift]
                         :triggers [{:id       1
                                     :name     :the-forest's-gift
                                     :event    :at-clean-up
                                     :duration :once
                                     :effects  [[:return-boon {:boon-name :the-forest's-gift}]]}]}]}))
      (is (= (-> {:boons   {:deck [forest-gift]}
                  :players [{:coins 0
                             :buys  1
                             :phase :buy}]}
                 (receive-boon {:player-no 0})
                 (clean-up {:player-no 0}))
             {:boons   {:discard [forest-gift]}
              :players [{:actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]})))
    (testing "The Moon's Gift"
      (is (= (-> {:boons   {:deck [moon-gift]}
                  :players [{}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [moon-gift]}
              :players [{}]}))
      (is (= (-> {:boons   {:deck [moon-gift]}
                  :players [{:deck    [bard]
                             :discard [estate gold copper]}]}
                 (receive-boon {:player-no 0})
                 (choose :gold))
             {:boons   {:discard [moon-gift]}
              :players [{:deck    [gold bard]
                         :discard [estate copper]}]})))
    (testing "The Mountain's Gift"
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:boons   {:deck [mountain-gift]}
                    :supply  [{:card silver :pile-size 40}]
                    :players [{}]}
                   (receive-boon {:player-no 0}))
               {:boons   {:discard [mountain-gift]}
                :supply  [{:card silver :pile-size 39}]
                :players [{:discard [silver]}]}))))
    (testing "The River's Gift"
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [river-gift]}
                  :players [{}]}
                 (receive-boon {:player-no 0}))
             {:boons   {}
              :players [{:boons    [river-gift]
                         :triggers [{:id       1
                                     :name     :the-river's-gift
                                     :event    :at-draw-hand
                                     :duration :once
                                     :effects  [[:draw {:arg 1 :player-no 0}]]}
                                    {:id       2
                                     :name     :the-river's-gift
                                     :event    :at-clean-up
                                     :duration :once
                                     :effects  [[:return-boon {:boon-name :the-river's-gift}]]}]}]}))
      (is (= (-> {:boons   {:deck [river-gift]}
                  :players [{:deck  (repeat 7 copper)
                             :phase :buy}
                            {:hand (repeat 5 copper)}]}
                 (receive-boon {:player-no 0})
                 (end-turn 0))
             {:current-player 1
              :boons          {:discard [river-gift]}
              :players        [{:hand    (repeat 6 copper)
                                :deck    [copper]
                                :actions 0
                                :coins   0
                                :buys    0
                                :phase   :out-of-turn}
                               {:hand    (repeat 5 copper)
                                :actions 1
                                :coins   0
                                :buys    1}]})))
    (testing "The Sea's Gift"
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:deck [copper copper]}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [sea-gift]}
              :players [{:hand [copper]
                         :deck [copper]}]}))
      (is (= (-> {:boons   {:discard [sea-gift]}
                  :players [{:deck [copper copper]}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [sea-gift]}
              :players [{:hand [copper]
                         :deck [copper]}]})))
    (testing "The Sky's Gift"
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [sky-gift]}
              :players [{}]}))
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{:hand [copper]}]}
                 (receive-boon {:player-no 0})
                 (choose :copper))
             {:boons   {:discard [sky-gift]}
              :players [{:discard [copper]}]}))
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{:hand [copper]}]}
                 (receive-boon {:player-no 0})
                 (choose nil))
             {:boons   {:discard [sky-gift]}
              :players [{:hand [copper]}]}))
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{:hand [copper copper]}]}
                 (receive-boon {:player-no 0})
                 (choose [:copper :copper]))
             {:boons   {:discard [sky-gift]}
              :players [{:discard [copper copper]}]}))
      (is (= (-> {:boons   {:deck [sky-gift]}
                  :players [{:hand [copper copper copper]}]}
                 (receive-boon {:player-no 0})
                 (choose nil))
             {:boons   {:discard [sky-gift]}
              :players [{:hand [copper copper copper]}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:boons   {:deck [sky-gift]}
                    :supply  [{:card gold :pile-size 30}]
                    :players [{:hand [copper copper copper]}]}
                   (receive-boon {:player-no 0})
                   (choose [:copper :copper :copper]))
               {:boons   {:discard [sky-gift]}
                :supply  [{:card gold :pile-size 29}]
                :players [{:discard [copper copper copper gold]}]}))))
    (testing "The Sun's Gift"
      (is (= (-> {:boons   {:deck [sun-gift]}
                  :players [{:deck [copper copper estate silver estate]}]}
                 (receive-boon {:player-no 0})
                 (choose nil)
                 (choose [:copper :copper :estate :silver]))
             {:boons   {:discard [sun-gift]}
              :players [{:deck [silver estate copper copper estate]}]}))
      (is (= (-> {:boons   {:deck [sun-gift]}
                  :players [{:deck [copper copper estate silver estate]}]}
                 (receive-boon {:player-no 0})
                 (choose [:estate])
                 (choose [:copper :copper :silver]))
             {:boons   {:discard [sun-gift]}
              :players [{:deck    [silver copper copper estate]
                         :discard [estate]}]}))
      (is (= (-> {:boons   {:deck [sun-gift]}
                  :players [{:deck [copper copper estate silver estate]}]}
                 (receive-boon {:player-no 0})
                 (choose [:estate :copper :copper :silver]))
             {:boons   {:discard [sun-gift]}
              :players [{:deck    [estate]
                         :discard [estate copper copper silver]}]})))
    (testing "The Swamp's Gift"
      (let [will-o-wisp (assoc will-o-wisp :id 1)]
        (is (= (-> {:boons       {:deck [swamp-gift]}
                    :extra-cards [{:card will-o-wisp :pile-size 12}]
                    :players     [{}]}
                   (receive-boon {:player-no 0}))
               {:boons       {:discard [swamp-gift]}
                :extra-cards [{:card will-o-wisp :pile-size 11}]
                :players     [{:discard [will-o-wisp]}]}))))
    (testing "The Wind's Gift"
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{}]}
                 (receive-boon {:player-no 0}))
             {:boons   {:discard [wind-gift]}
              :players [{}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [copper]}]}
                 (receive-boon {:player-no 0})
                 (choose :copper))
             {:boons   {:discard [wind-gift]}
              :players [{:discard [copper]}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [copper copper copper]}]}
                 (receive-boon {:player-no 0})
                 (choose [:copper :copper]))
             {:boons   {:discard [wind-gift]}
              :players [{:hand    [copper]
                         :discard [copper copper]}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [estate]
                             :deck [copper]}]}
                 (receive-boon {:player-no 0})
                 (choose [:estate :copper]))
             {:boons   {:discard [wind-gift]}
              :players [{:discard [estate copper]}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [estate copper estate gold]
                             :deck [copper silver]}]}
                 (receive-boon {:player-no 0})
                 (choose [:estate :estate]))
             {:boons   {:discard [wind-gift]}
              :players [{:hand    [copper gold copper silver]
                         :discard [estate estate]}]})))))

(deftest setup-boons-test
  (testing "Setup"
    (is (= (-> {:supply [{:card druid :pile-size 10}]}
               setup-game)
           {:supply      [{:card druid :pile-size 10}]
            :druid-boons [field-gift mountain-gift swamp-gift]
            :extra-cards [{:card will-o-wisp :pile-size 12}]}))
    (is (= (-> {:supply [{:card druid :pile-size 10}]}
               setup-game)
           {:supply      [{:card druid :pile-size 10}]
            :druid-boons [field-gift flame-gift wind-gift]}))
    (is (= (-> {:extra-cards [{:card will-o-wisp :pile-size 12}]
                :supply      [{:card druid :pile-size 10}]
                :boons       {:deck all-boons}}
               setup-game)
           {:extra-cards [{:card will-o-wisp :pile-size 12}]
            :supply      [{:card druid :pile-size 10}]
            :boons       {:deck [sun-gift earth-gift mountain-gift
                                 sea-gift river-gift field-gift
                                 moon-gift wind-gift sky-gift]}
            :druid-boons [flame-gift forest-gift swamp-gift]}))
    (is (= (-> {:supply      [{:card blessed-village :pile-size 10}]
                :druid-boons [flame-gift moon-gift wind-gift]}
               setup-game)
           {:extra-cards [{:card will-o-wisp :pile-size 12}]
            :supply      [{:card blessed-village :pile-size 10}]
            :boons       {:deck [mountain-gift sea-gift earth-gift
                                 river-gift sky-gift swamp-gift
                                 sun-gift field-gift forest-gift]}
            :druid-boons [flame-gift moon-gift wind-gift]}))))

(deftest hexes-test
  (testing "Hexes"
    (testing "Bad Omens"
      (is (= (-> {:hexes   {:deck [bad-omens]}
                  :players [{:deck [faithful-hound patron copper copper copper]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [bad-omens]}
              :players [{:deck           [copper copper]
                         :discard        [faithful-hound patron copper]
                         :revealed-cards {:deck 2}}]}))
      (is (= (-> {:hexes   {:deck [bad-omens]}
                  :players [{:deck    [copper faithful-hound patron]
                             :discard [gold copper]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [bad-omens]}
              :players [{:deck           [copper copper]
                         :discard        [gold faithful-hound patron]
                         :revealed-cards {:deck 2}}]}))
      (is (= (-> {:hexes   {:deck [bad-omens]}
                  :players [{:deck [copper faithful-hound patron gold]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [bad-omens]}
              :players [{:deck           [copper]
                         :discard        [faithful-hound patron gold]
                         :revealed-cards {:deck    1
                                          :discard 3}
                         :coffers        1}]}))
      (is (= (-> {:hexes   {:deck [bad-omens]}
                  :players [{:deck [faithful-hound patron gold]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [bad-omens]}
              :players [{:discard        [faithful-hound patron gold]
                         :revealed-cards {:discard 3}
                         :coffers        1}]})))
    (testing "Delusion"
      (ut/reset-ids!)
      (is (= (-> {:hexes   {:deck [delusion]}
                  :players [{}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [delusion]}
              :players [{:states   [deluded]
                         :triggers [(assoc (:trigger deluded) :id 1
                                                              :name :deluded)]}]}))
      (is (= (-> {:players [{:hand     [copper]
                             :coins    0
                             :phase    :action
                             :states   [deluded]
                             :triggers [(assoc (:trigger deluded) :name :deluded)]}]}
                 (play 0 :copper))
             {:players        [{:play-area [copper]
                                :coins     1
                                :phase     :pay}]
              :unbuyable-type :action}))
      (let [conclave (assoc conclave :id 1)]
        (is (thrown-with-msg? AssertionError #"Conclave can't be bought."
                              (-> {:hexes   {:deck [delusion]}
                                   :supply  [{:card conclave :pile-size 10}]
                                   :players [{:hand  [copper]
                                              :coins 3
                                              :buys  1
                                              :phase :action}]}
                                  (receive-hex {:player-no 0})
                                  (play 0 :copper)
                                  (buy-card 0 :conclave))))
        (is (thrown-with-msg? AssertionError #"Conclave can't be bought."
                              (-> {:hexes   {:deck [delusion]}
                                   :supply  [{:card conclave :pile-size 10}]
                                   :players [{:coins 4
                                              :buys  1
                                              :phase :action}]}
                                  (receive-hex {:player-no 0})
                                  (buy-card 0 :conclave))))
        (ut/reset-ids!)
        (is (= (-> {:hexes   {:deck [delusion]}
                    :supply  [{:card conclave :pile-size 10}]
                    :players [{:coins 4
                               :buys  1
                               :phase :pay}]}
                   (receive-hex {:player-no 0})
                   (buy-card 0 :conclave))
               {:hexes   {:discard [delusion]}
                :supply  [{:card conclave :pile-size 9}]
                :players [{:discard  [conclave]
                           :coins    0
                           :buys     0
                           :phase    :buy
                           :states   [deluded]
                           :triggers [(assoc (:trigger deluded) :id 1
                                                                :name :deluded)]}]})))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:supply         [{:card silver :pile-size 40}]
                    :players        [{:coins 3
                                      :buys  1}]
                    :unbuyable-type :action}
                   (buy-card 0 :silver))
               {:supply         [{:card silver :pile-size 39}]
                :players        [{:discard [silver]
                                  :coins   0
                                  :buys    0}]
                :unbuyable-type :action})))
      (is (= (-> {:hexes   {:deck [delusion]}
                  :players [{:phase :action}]}
                 (receive-hex {:player-no 0})
                 (end-turn 0))
             {:hexes          {:discard [delusion]}
              :current-player 0
              :players        [{:actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}]}))
      (ut/reset-ids!)
      (is (= (-> {:hexes   {:deck [delusion]}
                  :players [{:phase :buy}]}
                 (receive-hex {:player-no 0})
                 (end-turn 0))
             {:hexes          {:discard [delusion]}
              :current-player 0
              :players        [{:actions  1
                                :coins    0
                                :buys     1
                                :phase    :action
                                :states   [deluded]
                                :triggers [(assoc (:trigger deluded) :id 1
                                                                     :name :deluded)]}]})))
    (testing "Envy"
      (ut/reset-ids!)
      (is (= (-> {:hexes   {:deck [envy]}
                  :players [{}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [envy]}
              :players [{:states   [envious]
                         :triggers [(assoc (:trigger envious) :id 1
                                                              :name :envious)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:hexes   {:deck [envy]}
                  :players [{:hand  [copper]
                             :coins 0
                             :phase :action}]}
                 (receive-hex {:player-no 0})
                 (play 0 :copper))
             {:hexes   {:discard [envy]}
              :players [{:play-area [copper]
                         :coins     1
                         :phase     :pay
                         :triggers  [(assoc envious-silver-trigger :id 2)
                                     (assoc envious-gold-trigger :id 3)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:hexes   {:deck [envy]}
                  :players [{:hand  [silver]
                             :coins 0
                             :phase :action}]}
                 (receive-hex {:player-no 0})
                 (play 0 :silver))
             {:hexes   {:discard [envy]}
              :players [{:play-area [silver]
                         :coins     1
                         :phase     :pay
                         :triggers  [(assoc envious-silver-trigger :id 2)
                                     (assoc envious-gold-trigger :id 3)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:hexes   {:deck [envy]}
                  :players [{:hand  [silver silver]
                             :coins 0
                             :phase :action}]}
                 (receive-hex {:player-no 0})
                 (play 0 :silver)
                 (play 0 :silver))
             {:hexes   {:discard [envy]}
              :players [{:play-area [silver silver]
                         :coins     2
                         :phase     :pay
                         :triggers  [(assoc envious-silver-trigger :id 2)
                                     (assoc envious-gold-trigger :id 3)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:hexes   {:deck [envy]}
                  :players [{:hand  [gold]
                             :coins 0
                             :phase :action}]}
                 (receive-hex {:player-no 0})
                 (play 0 :gold))
             {:hexes   {:discard [envy]}
              :players [{:play-area [gold]
                         :coins     1
                         :phase     :pay
                         :triggers  [(assoc envious-silver-trigger :id 2)
                                     (assoc envious-gold-trigger :id 3)]}]}))
      (is (= (-> {:hexes   {:deck [envy]}
                  :players [{:phase :action}]}
                 (receive-hex {:player-no 0})
                 (end-turn 0))
             {:hexes          {:discard [envy]}
              :current-player 0
              :players        [{:actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}]})))
    (testing "Famine"
      (is (= (-> {:hexes   {:deck [famine]}
                  :players [{:deck [estate copper copper copper]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [famine]}
              :players [{:deck [copper copper copper estate]}]}))
      (is (= (-> {:hexes   {:deck [famine]}
                  :players [{:deck [estate skulk copper copper]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [famine]}
              :players [{:deck           [estate copper copper]
                         :discard        [skulk]
                         :revealed-cards {:discard 1}}]}))
      (is (= (-> {:hexes   {:deck [famine]}
                  :players [{:deck [skulk skulk monastery skulk]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [famine]}
              :players [{:deck           [skulk monastery]
                         :discard        [skulk skulk]
                         :revealed-cards {:discard 2}}]}))
      (is (= (-> {:hexes   {:deck [famine]}
                  :players [{:deck [skulk skulk skulk skulk]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [famine]}
              :players [{:deck           [skulk]
                         :discard        [skulk skulk skulk]
                         :revealed-cards {:discard 3}}]})))
    (testing "Fear"
      (is (= (-> {:hexes   {:deck [fear]}
                  :players [{:hand [estate copper copper copper]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [fear]}
              :players [{:hand [estate copper copper copper]}]}))
      (is (= (-> {:hexes   {:deck [fear]}
                  :players [{:hand [estate copper copper copper skulk]}]}
                 (receive-hex {:player-no 0})
                 (choose :copper))
             {:hexes   {:discard [fear]}
              :players [{:hand    [estate copper copper skulk]
                         :discard [copper]}]}))
      (is (= (-> {:hexes   {:deck [fear]}
                  :players [{:hand [estate copper copper copper skulk]}]}
                 (receive-hex {:player-no 0})
                 (choose :skulk))
             {:hexes   {:discard [fear]}
              :players [{:hand    [estate copper copper copper]
                         :discard [skulk]}]}))
      (is (thrown-with-msg? AssertionError #"Choose error: Estate is not a valid option."
                            (-> {:hexes   {:deck [fear]}
                                 :players [{:hand [estate copper copper copper skulk]}]}
                                (receive-hex {:player-no 0})
                                (choose :estate))))
      (is (= (-> {:hexes   {:deck [fear]}
                  :players [{:hand (repeat 5 estate)}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [fear]}
              :players [{:hand           (repeat 5 estate)
                         :revealed-cards {:hand 5}}]})))
    (testing "Greed"
      (let [copper (assoc copper :id 0)]
        (is (= (-> {:hexes   {:deck [greed]}
                    :supply  [{:card copper :pile-size 46}]
                    :players [{:deck [silver]}]}
                   (receive-hex {:player-no 0}))
               {:hexes   {:discard [greed]}
                :supply  [{:card copper :pile-size 45}]
                :players [{:deck [copper silver]}]}))))
    (testing "Haunting"
      (is (= (-> {:hexes   {:deck [haunting]}
                  :players [{:hand [copper copper copper copper copper]
                             :deck [silver]}]}
                 (receive-hex {:player-no 0})
                 (choose :copper))
             {:hexes   {:discard [haunting]}
              :players [{:hand [copper copper copper copper]
                         :deck [copper silver]}]}))
      (is (= (-> {:hexes   {:deck [haunting]}
                  :players [{:hand [copper copper copper copper]
                             :deck [silver]}]}
                 (receive-hex {:player-no 0})
                 (choose :copper))
             {:hexes   {:discard [haunting]}
              :players [{:hand [copper copper copper]
                         :deck [copper silver]}]}))
      (is (= (-> {:hexes   {:deck [haunting]}
                  :players [{:hand [copper copper copper]
                             :deck [silver]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [haunting]}
              :players [{:hand [copper copper copper]
                         :deck [silver]}]})))
    (testing "Locusts"
      (let [curse  (assoc curse :id 0)
            copper (assoc copper :id 1)]
        (is (= (-> {:hexes   {:deck [locusts]}
                    :players [{}]}
                   (receive-hex {:player-no 0}))
               {:hexes   {:discard [locusts]}
                :players [{}]}))
        (is (= (-> {:hexes   {:deck [locusts]}
                    :players [{:deck [curse]}]}
                   (receive-hex {:player-no 0}))
               {:hexes   {:discard [locusts]}
                :players [{}]
                :trash   [curse]}))
        (is (= (-> {:hexes   {:deck [locusts]}
                    :supply  [{:card curse :pile-size 10}]
                    :players [{:deck [copper]}]}
                   (receive-hex {:player-no 0}))
               {:hexes   {:discard [locusts]}
                :supply  [{:card curse :pile-size 9}]
                :players [{:discard [curse]}]
                :trash   [copper]}))
        (is (= (-> {:hexes   {:deck [locusts]}
                    :supply  [{:card curse :pile-size 10}]
                    :players [{:deck [estate]}]}
                   (receive-hex {:player-no 0}))
               {:hexes   {:discard [locusts]}
                :supply  [{:card curse :pile-size 9}]
                :players [{:discard [curse]}]
                :trash   [estate]}))
        (is (= (-> {:hexes   {:deck [locusts]}
                    :supply  (base/supply 2 8)
                    :players [{:deck [silver]}]}
                   (receive-hex {:player-no 0}))
               {:hexes        {:discard [locusts]}
                :supply       (base/supply 2 8)
                :players      [{}]
                :effect-stack [{:text      (str "Gain a Treasure card costing up to $2.")
                                :player-no 0
                                :choice    :gain
                                :source    :supply
                                :options   [:copper]
                                :min       1
                                :max       1}]
                :trash        [silver]}))
        (is (= (-> {:hexes   {:deck [locusts]}
                    :supply  [{:card copper :pile-size 46}]
                    :players [{:deck [silver]}]}
                   (receive-hex {:player-no 0})
                   (choose :copper))
               {:hexes   {:discard [locusts]}
                :supply  [{:card copper :pile-size 45}]
                :players [{:discard [copper]}]
                :trash   [silver]}))
        (is (= (-> {:hexes   {:deck [locusts]}
                    :supply  (base/supply 2 8)
                    :players [{:deck [silk-merchant]}]}
                   (receive-hex {:player-no 0}))
               {:hexes   {:discard [locusts]}
                :supply  (base/supply 2 8)
                :players [{:coffers   1
                           :villagers 1}]
                :trash   [silk-merchant]}))))
    (testing "Misery"
      (is (= (-> {:hexes   {:deck [misery]}
                  :players [{}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [misery]}
              :players [{:states [miserable]}]}))
      (is (= (-> {:hexes   {:deck [misery]}
                  :players [{:states [miserable]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [misery]}
              :players [{:states [twice-miserable]}]}))
      (is (= (-> {:hexes   {:deck [misery]}
                  :players [{:states [twice-miserable]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [misery]}
              :players [{:states [twice-miserable]}]}))
      (is (= (calc-victory-points {:states [miserable]})
             -2))
      (is (= (calc-victory-points {:states [twice-miserable]})
             -4)))
    (testing "Plague"
      (let [curse (assoc curse :id 0)]
        (is (= (-> {:hexes   {:deck [plague]}
                    :supply  [{:card curse :pile-size 10}]
                    :players [{}]}
                   (receive-hex {:player-no 0}))
               {:hexes   {:discard [plague]}
                :supply  [{:card curse :pile-size 9}]
                :players [{:hand [curse]}]}))))
    (testing "Poverty"
      (is (= (-> {:hexes   {:deck [poverty]}
                  :players [{:hand [copper copper copper]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [poverty]}
              :players [{:hand [copper copper copper]}]}))
      (is (= (-> {:hexes   {:deck [poverty]}
                  :players [{:hand [copper copper copper copper]}]}
                 (receive-hex {:player-no 0})
                 (choose :copper))
             {:hexes   {:discard [poverty]}
              :players [{:hand    [copper copper copper]
                         :discard [copper]}]}))
      (is (= (-> {:hexes   {:deck [poverty]}
                  :players [{:hand [copper copper copper copper copper]}]}
                 (receive-hex {:player-no 0})
                 (choose [:copper :copper]))
             {:hexes   {:discard [poverty]}
              :players [{:hand    [copper copper copper]
                         :discard [copper copper]}]})))
    (testing "War"
      (is (= (-> {:hexes   {:deck [war]}
                  :players [{:deck [silver]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [war]}
              :players [{}]
              :trash   [silver]}))
      (is (= (-> {:hexes   {:deck [war]}
                  :players [{:deck [patron]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [war]}
              :players [{:coffers 1}]
              :trash   [patron]}))
      (is (= (-> {:hexes   {:deck [war]}
                  :players [{:deck [copper estate silver copper]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [war]}
              :players [{:deck           [copper]
                         :discard        [copper estate]
                         :revealed-cards {:discard 2}}]
              :trash   [silver]}))
      (is (= (-> {:hexes   {:deck [war]}
                  :players [{:deck    [copper estate]
                             :discard [silver]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [war]}
              :players [{:discard        [copper estate]
                         :revealed-cards {:discard 2}}]
              :trash   [silver]}))
      (is (= (-> {:hexes   {:deck [war]}
                  :players [{:deck [copper estate duchy gold]}]}
                 (receive-hex {:player-no 0}))
             {:hexes   {:discard [war]}
              :players [{:discard        [copper estate duchy gold]
                         :revealed-cards {:discard 4}}]})))))

(deftest bard-test
  (let [bard (assoc bard :id 0)]
    (testing "Bard"
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [bard]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :bard))
             {:boons   {:discard [sea-gift]}
              :players [{:hand      [copper]
                         :play-area [bard]
                         :deck      [copper]
                         :actions   0
                         :coins     2}]})))))

(deftest blessed-village-test
  (let [blessed-village (assoc blessed-village :id 0)]
    (testing "blessed-village"
      (is (= (-> {:players [{:hand    [blessed-village]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :blessed-village))
             {:players [{:hand      [copper]
                         :play-area [blessed-village]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :supply  [{:card blessed-village :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0 :card-name :blessed-village}))
             {:boons        {}
              :supply       [{:card blessed-village :pile-size 9}]
              :players      [{:gaining [blessed-village]
                              :boons   [sea-gift]}]
              :effect-stack [{:player-no 0
                              :text      "Receive The Sea's Gift now or at the start of your next turn."
                              :choice    [::nocturne/blessed-village-choice {:boon-name :the-sea's-gift}]
                              :source    :special
                              :options   [{:option :now :text "Now"}
                                          {:option :at-start-turn :text "Next turn"}]
                              :min       1
                              :max       1}
                             {:player-no 0
                              :effect    [:remove-triggers {:event :on-gain}]}
                             {:player-no 0
                              :effect    [:finalize-gain {:player-no      0
                                                          :card-name      :blessed-village
                                                          :gained-card-id 0}]}]}))
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :supply  [{:card blessed-village :pile-size 10}]
                  :players [{:deck [copper copper]}]}
                 (gain {:player-no 0 :card-name :blessed-village})
                 (choose :now))
             {:boons   {:discard [sea-gift]}
              :supply  [{:card blessed-village :pile-size 9}]
              :players [{:hand    [copper]
                         :deck    [copper]
                         :discard [blessed-village]}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :supply  [{:card blessed-village :pile-size 10}]
                  :players [{:deck [copper copper]}]}
                 (gain {:player-no 0 :card-name :blessed-village})
                 (choose :at-start-turn))
             {:boons   {}
              :supply  [{:card blessed-village :pile-size 9}]
              :players [{:deck     [copper copper]
                         :discard  [blessed-village]
                         :boons    [sea-gift]
                         :triggers [{:id       1
                                     :event    :at-start-turn
                                     :name     :the-sea's-gift
                                     :duration :once
                                     :mode     :complex
                                     :effects  [[:return-boon {:boon-name :the-sea's-gift}]
                                                [:receive-boon {:boon sea-gift}]]}]}]}))
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :supply  [{:card blessed-village :pile-size 10}]
                  :players [{:deck  (repeat 7 copper)
                             :phase :action}]}
                 (gain {:player-no 0 :card-name :blessed-village})
                 (choose :at-start-turn)
                 (end-turn 0))
             {:current-player 0
              :boons          {:discard [sea-gift]}
              :supply         [{:card blessed-village :pile-size 9}]
              :players        [{:hand    (repeat 6 copper)
                                :deck    [copper]
                                :discard [blessed-village]
                                :actions 1
                                :coins   0
                                :buys    1
                                :phase   :action}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [forest-gift]}
                  :supply  [{:card blessed-village :pile-size 10}]
                  :players [{:coins 0
                             :buys  1}]}
                 (gain {:player-no 0 :card-name :blessed-village})
                 (choose :now))
             {:boons   {}
              :supply  [{:card blessed-village :pile-size 9}]
              :players [{:discard  [blessed-village]
                         :coins    1
                         :buys     2
                         :boons    [forest-gift]
                         :triggers [{:id       1
                                     :name     :the-forest's-gift
                                     :event    :at-clean-up
                                     :duration :once
                                     :effects  [[:return-boon {:boon-name :the-forest's-gift}]]}]}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [forest-gift]}
                  :supply  [{:card blessed-village :pile-size 10}]
                  :players [{:deck [copper copper]}]}
                 (gain {:player-no 0 :card-name :blessed-village})
                 (choose :at-start-turn))
             {:boons   {}
              :supply  [{:card blessed-village :pile-size 9}]
              :players [{:deck     [copper copper]
                         :discard  [blessed-village]
                         :boons    [forest-gift]
                         :triggers [{:id       1
                                     :event    :at-start-turn
                                     :name     :the-forest's-gift
                                     :duration :once
                                     :mode     :complex
                                     :effects  [[:receive-boon {:boon forest-gift}]]}]}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [forest-gift]}
                  :supply  [{:card blessed-village :pile-size 10}]
                  :players [{:deck  (repeat 5 copper)
                             :phase :action}]}
                 (gain {:player-no 0 :card-name :blessed-village})
                 (choose :at-start-turn)
                 (end-turn 0))
             {:current-player 0
              :boons          {}
              :supply         [{:card blessed-village :pile-size 9}]
              :players        [{:hand     (repeat 5 copper)
                                :discard  [blessed-village]
                                :actions  1
                                :coins    1
                                :buys     2
                                :phase    :action
                                :boons    [forest-gift]
                                :triggers [{:id       2
                                            :name     :the-forest's-gift
                                            :event    :at-clean-up
                                            :duration :once
                                            :effects  [[:return-boon {:boon-name :the-forest's-gift}]]}]}]})))))

(deftest cemetery-test
  (let [cemetery (assoc cemetery :id 0)]
    (testing "Cemetery"
      (is (= (calc-victory-points {:deck [cemetery]})
             2))
      (is (= (-> {:supply  [{:card cemetery :pile-size 8}]
                  :players [{}]}
                 (gain {:player-no 0
                        :card-name :cemetery}))
             {:supply  [{:card cemetery :pile-size 7}]
              :players [{:discard [cemetery]}]}))
      (is (= (-> {:supply  [{:card cemetery :pile-size 8}]
                  :players [{:hand [estate]}]}
                 (gain {:player-no 0
                        :card-name :cemetery})
                 (choose nil))
             {:supply  [{:card cemetery :pile-size 7}]
              :players [{:hand    [estate]
                         :discard [cemetery]}]}))
      (is (= (-> {:supply  [{:card cemetery :pile-size 8}]
                  :players [{:hand [estate]}]}
                 (gain {:player-no 0
                        :card-name :cemetery})
                 (choose :estate))
             {:supply  [{:card cemetery :pile-size 7}]
              :players [{:discard [cemetery]}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card cemetery :pile-size 8}]
                  :players [{:hand [estate estate estate copper copper]}]}
                 (gain {:player-no 0
                        :card-name :cemetery})
                 (choose [:estate :estate :estate :copper]))
             {:supply  [{:card cemetery :pile-size 7}]
              :players [{:hand    [copper]
                         :discard [cemetery]}]
              :trash   [estate estate estate copper]})))
    (let [haunted-mirror (assoc haunted-mirror :id 1)
          ghost          (assoc ghost :id 2)]
      (testing "Haunted Mirror"
        (is (= (-> {:players [{:hand  [haunted-mirror]
                               :coins 0}]}
                   (play 0 :haunted-mirror))
               {:players [{:play-area [haunted-mirror]
                           :coins     1}]}))
        (is (= (-> {:supply      [{:card cemetery :pile-size 8}]
                    :extra-cards [{:card ghost :pile-size 6}]
                    :players     [{:hand [haunted-mirror]}]}
                   (gain {:player-no 0
                          :card-name :cemetery})
                   (choose :haunted-mirror))
               {:supply      [{:card cemetery :pile-size 7}]
                :extra-cards [{:card ghost :pile-size 6}]
                :players     [{:discard [cemetery]}]
                :trash       [haunted-mirror]}))
        (is (= (-> {:supply      [{:card cemetery :pile-size 8}]
                    :extra-cards [{:card ghost :pile-size 6}]
                    :players     [{:hand [haunted-mirror conclave]}]}
                   (gain {:player-no 0
                          :card-name :cemetery})
                   (choose :haunted-mirror)
                   (choose nil))
               {:supply      [{:card cemetery :pile-size 7}]
                :extra-cards [{:card ghost :pile-size 6}]
                :players     [{:hand    [conclave]
                               :discard [cemetery]}]
                :trash       [haunted-mirror]}))
        (is (= (-> {:supply      [{:card cemetery :pile-size 8}]
                    :extra-cards [{:card ghost :pile-size 6}]
                    :players     [{:hand [haunted-mirror conclave]}]}
                   (gain {:player-no 0
                          :card-name :cemetery})
                   (choose :haunted-mirror)
                   (choose :conclave))
               {:supply      [{:card cemetery :pile-size 7}]
                :extra-cards [{:card ghost :pile-size 5}]
                :players     [{:discard [conclave ghost cemetery]}]
                :trash       [haunted-mirror]}))))))

(deftest changeling-test
  (let [changeling (assoc changeling :id 0)]
    (testing "Changeling"
      (is (= (-> {:players [{:hand      [changeling copper]
                             :play-area [silver conclave]}]}
                 (play 0 :changeling))
             {:players      [{:hand      [copper]
                              :play-area [silver conclave]}]
              :trash        [changeling]
              :effect-stack [{:text      "Gain a copy of a card you have in play."
                              :player-no 0
                              :card-id   0
                              :choice    :gain
                              :source    :play-area
                              :options   [:silver :conclave]
                              :min       1
                              :max       1}]}))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:supply  [{:card silver :pile-size 40}
                              {:card conclave :pile-size 0}]
                    :players [{:hand      [changeling copper]
                               :play-area [silver conclave]}]}
                   (play 0 :changeling)
                   (choose :silver))
               {:supply  [{:card silver :pile-size 39}
                          {:card conclave :pile-size 0}]
                :players [{:hand      [copper]
                           :play-area [silver conclave]
                           :discard   [silver]}]
                :trash   [changeling]})))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card conclave :pile-size 0}]
                  :players [{:hand      [changeling copper]
                             :play-area [silver conclave]}]}
                 (play 0 :changeling)
                 (choose :conclave))
             {:supply  [{:card silver :pile-size 40}
                        {:card conclave :pile-size 0}]
              :players [{:hand      [copper]
                         :play-area [silver conclave]}]
              :trash   [changeling]}))
      (let [conclave (assoc conclave :id 1)]
        (is (= (-> {:supply  [{:card changeling :pile-size 10}
                              {:card conclave :pile-size 10}]
                    :players [{:triggers [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :conclave})
                   (choose nil))
               {:supply  [{:card changeling :pile-size 10}
                          {:card conclave :pile-size 9}]
                :players [{:discard  [conclave]
                           :triggers [changeling-trigger]}]}))
        (is (= (-> {:supply  [{:card changeling :pile-size 10}
                              {:card conclave :pile-size 10}]
                    :players [{:triggers [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :conclave})
                   (choose :changeling))
               {:supply  [{:card changeling :pile-size 9}
                          {:card conclave :pile-size 10}]
                :players [{:discard  [changeling]
                           :triggers [changeling-trigger]}]}))
        (is (= (-> {:track-gained-cards? true
                    :supply              [{:card changeling :pile-size 10}
                                          {:card conclave :pile-size 10}]
                    :players             [{:gained-cards []
                                           :triggers     [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :conclave})
                   (choose :changeling))
               {:track-gained-cards? true
                :supply              [{:card changeling :pile-size 9}
                                      {:card conclave :pile-size 10}]
                :players             [{:discard      [changeling]
                                       :gained-cards [{:name :conclave :cost 4 :types #{:action}}]
                                       :triggers     [changeling-trigger]}]}))
        (is (= (-> {:supply  [{:card changeling :pile-size 0}
                              {:card conclave :pile-size 10}]
                    :players [{:triggers [changeling-trigger]}]}
                   (gain {:player-no 0 :card-name :conclave}))
               {:supply  [{:card changeling :pile-size 0}
                          {:card conclave :pile-size 9}]
                :players [{:discard  [conclave]
                           :triggers [changeling-trigger]}]})))
      (testing "ignoring gains"
        (let [conclave (assoc conclave :id 1)]
          (is (= (-> {:supply  [{:card changeling :pile-size 9}
                                {:card conclave :pile-size 9}]
                      :players [{:hand      [changeling]
                                 :play-area [conclave]
                                 :triggers  [changeling-trigger]}]}
                     (play 0 :changeling)
                     (choose :conclave)
                     (choose nil))
                 {:supply  [{:card changeling :pile-size 9}
                            {:card conclave :pile-size 8}]
                  :players [{:play-area [conclave]
                             :discard   [conclave]
                             :triggers  [changeling-trigger]}]
                  :trash   [changeling]}))
          (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                {:card conclave :pile-size 10}]
                      :players [{:triggers [changeling-trigger]}]}
                     (gain {:player-no 0 :card-name :changeling}))
                 {:supply  [{:card changeling :pile-size 9}
                            {:card conclave :pile-size 10}]
                  :players [{:discard  [changeling]
                             :triggers [changeling-trigger]}]}))
          (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                {:card conclave :pile-size 10}]
                      :players [{:coins    4
                                 :buys     1
                                 :triggers [changeling-trigger]}]}
                     (buy-card 0 :conclave))
                 {:supply  [{:card changeling :pile-size 10}
                            {:card conclave :pile-size 9}]
                  :players [{:discard  [conclave]
                             :coins    0
                             :buys     0
                             :triggers [changeling-trigger]}]}))
          (let [mint (assoc mint :id 2)]
            (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                  {:card mint :pile-size 10}]
                        :players [{:play-area (repeat 5 copper)
                                   :coins     5
                                   :buys      1
                                   :triggers  [changeling-trigger]}]}
                       (buy-card 0 :mint)
                       (choose :changeling))
                   {:supply  [{:card changeling :pile-size 9}
                              {:card mint :pile-size 10}]
                    :players [{:discard  [changeling]
                               :coins    0
                               :buys     0
                               :triggers [changeling-trigger]}]
                    :trash   (repeat 5 copper)})))
          (let [duchy (assoc duchy :id 2)
                gold  (assoc gold :id 3)]
            (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                  {:card duchy :pile-size 8}
                                  {:card gold :pile-size 30}]
                        :players [{:play-area [hoard]
                                   :coins     5
                                   :buys      1
                                   :triggers  [changeling-trigger]}]}
                       (buy-card 0 :duchy)
                       (choose nil)                         ; don't exchange gained Gold
                       (choose :changeling))                ;exchange bought Duchy
                   {:supply  [{:card changeling :pile-size 9}
                              {:card duchy :pile-size 8}
                              {:card gold :pile-size 29}]
                    :players [{:play-area [hoard]
                               :discard   [gold changeling]
                               :coins     0
                               :buys      0
                               :triggers  [changeling-trigger]}]})))
          (let [curse (assoc curse :id 2)]
            (is (= (-> {:supply  [{:card curse :pile-size 10}
                                  {:card changeling :pile-size 10}
                                  {:card   conclave :pile-size 10
                                   :tokens [{:token-type :embargo
                                             :on-buy     [[:gain {:card-name :curse}]]}]}]
                        :players [{:coins    4
                                   :buys     1
                                   :triggers [changeling-trigger]}]}
                       (buy-card 0 :conclave)
                       (choose :changeling))
                   {:supply  [{:card curse :pile-size 9}
                              {:card changeling :pile-size 9}
                              {:card   conclave :pile-size 10
                               :tokens [{:token-type :embargo
                                         :on-buy     [[:gain {:card-name :curse}]]}]}]
                    :players [{:discard  [curse changeling]
                               :coins    0
                               :buys     0
                               :triggers [changeling-trigger]}]}))
            (is (= (-> {:supply  [{:card curse :pile-size 10}
                                  {:card   changeling :pile-size 10
                                   :tokens [{:token-type :embargo
                                             :on-buy     [[:gain {:card-name :curse}]]}]}
                                  {:card conclave :pile-size 10}]
                        :players [{:coins    4
                                   :buys     1
                                   :triggers [changeling-trigger]}]}
                       (buy-card 0 :conclave)
                       (choose :changeling))
                   {:supply  [{:card curse :pile-size 10}
                              {:card   changeling :pile-size 9
                               :tokens [{:token-type :embargo
                                         :on-buy     [[:gain {:card-name :curse}]]}]}
                              {:card conclave :pile-size 10}]
                    :players [{:discard  [changeling]
                               :coins    0
                               :buys     0
                               :triggers [changeling-trigger]}]})))
          (let [silk-merchant (assoc silk-merchant :id 2)]
            (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                  {:card silk-merchant :pile-size 10}]
                        :players [{:coins    4
                                   :buys     1
                                   :triggers [changeling-trigger]}]}
                       (buy-card 0 :silk-merchant)
                       (choose :changeling))
                   {:supply  [{:card changeling :pile-size 9}
                              {:card silk-merchant :pile-size 10}]
                    :players [{:discard   [changeling]
                               :coins     0
                               :buys      0
                               :villagers 1
                               :coffers   1
                               :triggers  [changeling-trigger]}]})))
          (let [duchy (assoc duchy :id 2)]
            (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                  {:card   duchy :pile-size 8
                                   :tokens [{:token-type :trade-route
                                             :on-gain    [[::prosperity/trade-route-move-token]]}]}]
                        :players [{:coins    5
                                   :buys     1
                                   :triggers [changeling-trigger]}]}
                       (buy-card 0 :duchy)
                       (choose :changeling))
                   {:trade-route-mat 1
                    :supply          [{:card changeling :pile-size 9}
                                      {:card duchy :pile-size 8}]
                    :players         [{:discard  [changeling]
                                       :coins    0
                                       :buys     0
                                       :triggers [changeling-trigger]}]})))
          (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                {:card conclave :pile-size 10}]
                      :players [{:play-area [talisman]
                                 :coins     4
                                 :buys      1
                                 :triggers  [changeling-trigger]}]}
                     (buy-card 0 :conclave)
                     (choose nil)                           ; keep bought Conclave
                     (choose :changeling))                  ; exchange gained Conclave
                 {:supply  [{:card changeling :pile-size 9}
                            {:card conclave :pile-size 9}]
                  :players [{:play-area [talisman]
                             :discard   [conclave changeling]
                             :coins     0
                             :buys      0
                             :triggers  [changeling-trigger]}]}))
          (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                {:card conclave :pile-size 10}]
                      :players [{:play-area [talisman]
                                 :coins     4
                                 :buys      1
                                 :triggers  [changeling-trigger]}]}
                     (buy-card 0 :conclave)
                     (choose :changeling)                   ; exchange bought Conclave
                     (choose nil))                          ; keep gained Conclave
                 {:supply  [{:card changeling :pile-size 9}
                            {:card conclave :pile-size 9}]
                  :players [{:play-area [talisman]
                             :discard   [changeling conclave]
                             :coins     0
                             :buys      0
                             :triggers  [changeling-trigger]}]}))
          (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                {:card conclave :pile-size 10}]
                      :players [{:play-area [tracker]
                                 :coins     4
                                 :buys      1
                                 :triggers  [changeling-trigger]}]}
                     (buy-card 0 :conclave)
                     (choose :conclave))                    ; topdeck Conclave from Tracker
                 {:supply  [{:card changeling :pile-size 10}
                            {:card conclave :pile-size 9}]
                  :players [{:play-area [tracker]
                             :deck      [conclave]
                             :coins     0
                             :buys      0
                             :triggers  [changeling-trigger]}]}))
          (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                {:card conclave :pile-size 10}]
                      :players [{:coins    4
                                 :buys     1
                                 :triggers [changeling-trigger (:trigger academy)]}]}
                     (buy-card 0 :conclave)
                     (choose :changeling))
                 {:supply  [{:card changeling :pile-size 9}
                            {:card conclave :pile-size 10}]
                  :players [{:discard   [changeling]
                             :coins     0
                             :buys      0
                             :villagers 1
                             :triggers  [changeling-trigger (:trigger academy)]}]}))))
      (testing "gaining from Extra Cards"
        (let [ghost (assoc ghost :id 1)]
          (is (= (-> {:extra-cards [{:card ghost :pile-size 6}]
                      :supply      [{:card changeling :pile-size 10}]
                      :players     [{:triggers [changeling-trigger]}]}
                     (gain {:player-no 0 :card-name :ghost :from :extra-cards})
                     (choose :changeling))
                 {:extra-cards [{:card ghost :pile-size 6}]
                  :supply      [{:card changeling :pile-size 9}]
                  :players     [{:discard  [changeling]
                                 :triggers [changeling-trigger]}]}))))
      (testing "gaining from Trash"
        (let [conclave (assoc conclave :id 1)]
          (is (= (-> {:supply  [{:card changeling :pile-size 10}
                                {:card conclave :pile-size 9}]
                      :players [{:triggers [changeling-trigger]}]
                      :trash   [conclave]}
                     (gain {:player-no 0 :card-name :conclave :from :trash})
                     (choose :changeling))
                 {:supply  [{:card changeling :pile-size 9}
                            {:card conclave :pile-size 10}]
                  :players [{:discard  [changeling]
                             :triggers [changeling-trigger]}]
                  :trash   []})))
        (let [ghost (assoc ghost :id 1)]
          (is (= (-> {:extra-cards [{:card ghost :pile-size 5}]
                      :supply      [{:card changeling :pile-size 10}]
                      :players     [{:triggers [changeling-trigger]}]
                      :trash       [ghost]}
                     (gain {:player-no 0 :card-name :ghost :from :trash})
                     (choose :changeling))
                 {:extra-cards [{:card ghost :pile-size 6}]
                  :supply      [{:card changeling :pile-size 9}]
                  :players     [{:discard  [changeling]
                                 :triggers [changeling-trigger]}]
                  :trash       []})))
        (let [zombie-apprentice (assoc zombie-apprentice :id 1)]
          (is (= (-> {:supply  [{:card changeling :pile-size 10}]
                      :players [{:triggers [changeling-trigger]}]
                      :trash   [zombie-apprentice]}
                     (gain {:player-no 0 :card-name :zombie-apprentice :from :trash}))
                 {:supply  [{:card changeling :pile-size 10}]
                  :players [{:discard  [zombie-apprentice]
                             :triggers [changeling-trigger]}]
                  :trash   []})))))))

(deftest cobbler-test
  (let [cobbler (assoc cobbler :id 0)]
    (testing "Cobbler"
      (ut/reset-ids!)
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand  [cobbler]
                             :phase :night}]}
                 (play 0 :cobbler)
                 (end-turn 0))
             {:supply         (base/supply 2 8)
              :current-player 0
              :players        [{:play-area [cobbler]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action
                                :triggers  [(get-trigger cobbler)]}]
              :effect-stack   [{:text      "Gain a card to your hand costing up to $4."
                                :player-no 0
                                :card-id   0
                                :choice    :gain-to-hand
                                :source    :supply
                                :options   [:curse :estate :copper :silver]
                                :min       1
                                :max       1}
                               {:player-no 0
                                :effect    [:remove-trigger {:trigger-id 1}]}
                               {:player-no 0
                                :effect    [:sync-repeated-play]}]}))
      (let [blessed-village (assoc blessed-village :id 1)]
        (ut/reset-ids!)
        (is (= (-> {:supply  [{:card blessed-village :pile-size 10}]
                    :boons   {:deck [sea-gift]}
                    :players [{:hand  [cobbler]
                               :phase :night}]}
                   (play 0 :cobbler)
                   (end-turn 0)
                   (choose :blessed-village)
                   (choose :at-start-turn))
               {:supply         [{:card blessed-village :pile-size 9}]
                :boons          {}
                :current-player 0
                :players        [{:hand      [blessed-village]
                                  :play-area [cobbler]
                                  :boons     [sea-gift]
                                  :actions   1
                                  :coins     0
                                  :buys      1
                                  :phase     :action
                                  :triggers  [{:id       2
                                               :event    :at-start-turn
                                               :name     :the-sea's-gift
                                               :duration :once
                                               :mode     :complex
                                               :effects  [[:return-boon {:boon-name :the-sea's-gift}]
                                                          [:receive-boon {:boon sea-gift}]]}]}]}))))))

(deftest conclave-test
  (testing "Conclave"
    (is (= (-> {:players [{:hand    [conclave]
                           :actions 1
                           :coins   0}]}
               (play 0 :conclave))
           {:players [{:play-area [conclave]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [conclave conclave]
                           :actions 1
                           :coins   0}]}
               (play 0 :conclave))
           {:players [{:hand      [conclave]
                       :play-area [conclave]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [conclave tragic-hero]
                           :actions 1
                           :coins   0}]}
               (play 0 :conclave))
           {:players      [{:hand      [tragic-hero]
                            :play-area [conclave]
                            :actions   0
                            :coins     2}]
            :effect-stack [{:text      "You may play an Action card from your hand that you don't have a copy of in play."
                            :player-no 0
                            :choice    ::nocturne/conclave-play-action
                            :source    :hand
                            :options   [:tragic-hero]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [conclave tragic-hero]
                           :deck    [copper copper copper]
                           :actions 1
                           :coins   0
                           :buys    1}]}
               (play 0 :conclave)
               (choose :tragic-hero))
           {:players [{:hand      [copper copper copper]
                       :play-area [conclave tragic-hero]
                       :actions   1
                       :coins     2
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [conclave tragic-hero]
                           :deck    [copper copper copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :conclave)
               (choose nil))
           {:players [{:hand      [tragic-hero]
                       :deck      [copper copper copper]
                       :play-area [conclave]
                       :actions   0
                       :coins     2}]}))))

(deftest crypt-test
  (let [crypt (assoc crypt :id 0)]
    (testing "Crypt"
      (is (= (-> {:players [{:hand [crypt]}]}
                 (play 0 :crypt))
             {:players [{:play-area [crypt]}]}))
      (is (= (-> {:players [{:hand      [crypt]
                             :play-area [conclave guardian]}]}
                 (play 0 :crypt))
             {:players [{:play-area [conclave guardian crypt]}]}))
      (is (= (-> {:players [{:hand      [crypt]
                             :play-area [copper]}]}
                 (play 0 :crypt)
                 (choose nil))
             {:players [{:play-area [copper crypt]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand      [crypt]
                             :play-area [gold]}]}
                 (play 0 :crypt)
                 (choose :gold))
             {:players [{:play-area [crypt]
                         :triggers  [(merge crypt-trigger
                                            {:id        1
                                             :card-id   0
                                             :name      :crypt
                                             :set-aside [gold]})]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand      [crypt]
                             :play-area [copper silver gold]}]}
                 (play 0 :crypt)
                 (choose [:copper :silver :gold]))
             {:players [{:play-area [crypt]
                         :triggers  [(merge crypt-trigger
                                            {:id        1
                                             :card-id   0
                                             :name      :crypt
                                             :set-aside [copper silver gold]})]}]}))
      (is (= (-> {:players [{:play-area [crypt]
                             :deck      (repeat 6 copper)
                             :phase     :night
                             :triggers  [(merge crypt-trigger
                                                {:card-id   0
                                                 :name      :crypt
                                                 :set-aside [copper silver gold]})]}]}
                 (end-turn 0)
                 (choose :gold))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper gold]
                                :play-area [crypt]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action
                                :triggers  [(merge crypt-trigger
                                                   {:card-id   0
                                                    :name      :crypt
                                                    :set-aside [copper silver]})]}]}))
      (is (= (-> {:players [{:play-area [crypt]
                             :deck      (repeat 6 copper)
                             :phase     :night
                             :triggers  [(merge crypt-trigger
                                                {:card-id   0
                                                 :name      :crypt
                                                 :set-aside [copper silver]})]}]}
                 (end-turn 0)
                 (choose :copper))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper copper]
                                :play-area [crypt]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action
                                :triggers  [(merge crypt-trigger
                                                   {:card-id   0
                                                    :name      :crypt
                                                    :set-aside [silver]})]}]}))
      (is (= (-> {:players [{:play-area [crypt]
                             :deck      (repeat 6 copper)
                             :phase     :night
                             :triggers  [(merge crypt-trigger
                                                {:card-id   0
                                                 :name      :crypt
                                                 :set-aside [silver]})]}]}
                 (end-turn 0)
                 (choose :silver))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper silver]
                                :play-area [crypt]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:mode    :swift
                  :players [{:play-area [crypt]
                             :deck      (repeat 6 copper)
                             :phase     :night
                             :triggers  [(merge crypt-trigger
                                                {:card-id   0
                                                 :name      :crypt
                                                 :set-aside [silver]})]}]}
                 (end-turn 0))
             {:mode           :swift
              :current-player 0
              :players        [{:hand      [copper copper copper copper copper silver]
                                :play-area [crypt]
                                :deck      [copper]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]})))))

(deftest cursed-village-test
  (let [cursed-village (assoc cursed-village :id 0)]
    (testing "Cursed Village"
      (is (= (-> {:players [{:hand    [cursed-village]
                             :deck    (repeat 7 copper)
                             :actions 1}]}
                 (play 0 :cursed-village))
             {:players [{:hand      (repeat 6 copper)
                         :play-area [cursed-village]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [cursed-village copper copper copper copper]
                             :deck    (repeat 3 copper)
                             :actions 1}]}
                 (play 0 :cursed-village))
             {:players [{:hand      (repeat 6 copper)
                         :play-area [cursed-village]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:hexes   {:deck [poverty]}
                  :supply  [{:card cursed-village :pile-size 10}]
                  :players [{:hand [copper copper copper copper copper]}]}
                 (gain {:player-no 0 :card-name :cursed-village})
                 (choose [:copper :copper]))
             {:hexes   {:discard [poverty]}
              :supply  [{:card cursed-village :pile-size 9}]
              :players [{:hand    [copper copper copper]
                         :discard [copper copper cursed-village]}]})))))

(deftest den-of-sin-test
  (let [den-of-sin (assoc den-of-sin :id 0)]
    (testing "Den of Sin"
      (is (= (-> {:players [{:hand  [den-of-sin]
                             :deck  [copper copper copper copper copper copper silver silver]
                             :phase :night}]}
                 (play 0 :den-of-sin)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper copper silver]
                                :play-area [den-of-sin]
                                :deck      [silver]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:supply  [{:card den-of-sin :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0 :card-name :den-of-sin}))
             {:supply  [{:card den-of-sin :pile-size 9}]
              :players [{:hand [den-of-sin]}]})))))

(deftest devils-workshop-test
  (let [devils-workshop (assoc devils-workshop :id 0)]
    (testing "Devil's Workshop"
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:track-gained-cards? true
                    :supply              [{:card gold :pile-size 30}]
                    :players             [{:hand [devils-workshop]}]}
                   (play 0 :devil's-workshop))
               {:track-gained-cards? true
                :supply              [{:card gold :pile-size 29}]
                :players             [{:play-area    [devils-workshop]
                                       :discard      [gold]
                                       :gained-cards [{:name :gold :cost 6 :types #{:treasure}}]}]})))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:track-gained-cards? true
                    :supply              [{:card silver :pile-size 40}]
                    :players             [{:hand         [devils-workshop]
                                           :gained-cards [{:name :gold :cost 6 :types #{:treasure}}]}]}
                   (play 0 :devil's-workshop)
                   (choose :silver))
               {:track-gained-cards? true
                :supply              [{:card silver :pile-size 39}]
                :players             [{:play-area    [devils-workshop]
                                       :discard      [silver]
                                       :gained-cards [{:name :gold :cost 6 :types #{:treasure}}
                                                      {:name :silver :cost 3 :types #{:treasure}}]}]})))
      (let [imp (assoc imp :id 1)]
        (is (= (-> {:track-gained-cards? true
                    :extra-cards         [{:card imp :pile-size 13}]
                    :players             [{:hand         [devils-workshop]
                                           :gained-cards [{:name :gold :cost 6 :types #{:treasure}}
                                                          {:name :silver :cost 3 :types #{:treasure}}]}]}
                   (play 0 :devil's-workshop))
               {:track-gained-cards? true
                :extra-cards         [{:card imp :pile-size 12}]
                :players             [{:play-area    [devils-workshop]
                                       :discard      [imp]
                                       :gained-cards [{:name :gold :cost 6 :types #{:treasure}}
                                                      {:name :silver :cost 3 :types #{:treasure}}
                                                      {:name :imp :cost 2 :types #{:action :spirit}}]}]}))))))

(deftest druid-test
  (let [druid       (assoc druid :id 0)
        will-o-wisp (assoc will-o-wisp :id 1)]
    (testing "Druid"
      (is (= (-> {:druid-boons [field-gift sea-gift swamp-gift]
                  :extra-cards [{:card will-o-wisp :pile-size 12}]
                  :players     [{:hand    [druid]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :druid))
             {:druid-boons  [field-gift sea-gift swamp-gift]
              :extra-cards  [{:card will-o-wisp :pile-size 12}]
              :players      [{:play-area [druid]
                              :deck      [copper copper]
                              :actions   0
                              :coins     0
                              :buys      2}]
              :effect-stack [{:text      "Receive one of the Druid Boons."
                              :player-no 0
                              :card-id   0
                              :choice    ::nocturne/druid-receive-boon
                              :source    :druid-boons
                              :options   [:the-field's-gift :the-sea's-gift :the-swamp's-gift]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:druid-boons [field-gift sea-gift swamp-gift]
                  :extra-cards [{:card will-o-wisp :pile-size 12}]
                  :players     [{:hand    [druid]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :druid)
                 (choose :the-field's-gift))
             {:druid-boons [field-gift sea-gift swamp-gift]
              :extra-cards [{:card will-o-wisp :pile-size 12}]
              :players     [{:play-area [druid]
                             :deck      [copper copper]
                             :actions   1
                             :coins     1
                             :buys      2}]}))
      (is (= (-> {:druid-boons [field-gift sea-gift swamp-gift]
                  :extra-cards [{:card will-o-wisp :pile-size 12}]
                  :players     [{:hand    [druid]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :druid)
                 (choose :the-sea's-gift))
             {:druid-boons [field-gift sea-gift swamp-gift]
              :extra-cards [{:card will-o-wisp :pile-size 12}]
              :players     [{:hand      [copper]
                             :play-area [druid]
                             :deck      [copper]
                             :actions   0
                             :coins     0
                             :buys      2}]}))
      (is (= (-> {:druid-boons [field-gift sea-gift swamp-gift]
                  :extra-cards [{:card will-o-wisp :pile-size 12}]
                  :players     [{:hand    [druid]
                                 :deck    [copper copper]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                 (play 0 :druid)
                 (choose :the-swamp's-gift))
             {:druid-boons [field-gift sea-gift swamp-gift]
              :extra-cards [{:card will-o-wisp :pile-size 11}]
              :players     [{:play-area [druid]
                             :deck      [copper copper]
                             :discard   [will-o-wisp]
                             :actions   0
                             :coins     0
                             :buys      2}]})))))

(deftest exorcist-test
  (let [exorcist    (assoc exorcist :id 0)
        will-o-wisp (assoc will-o-wisp :id 1)
        imp         (assoc imp :id 2)
        ghost       (assoc ghost :id 3)]
    (testing "Exorcist"
      (is (= (-> {:players [{:hand [exorcist]}]}
                 (play 0 :exorcist))
             {:players [{:play-area [exorcist]}]}))
      (is (= (-> {:extra-cards (vals spirit-piles)
                  :players     [{:hand [exorcist copper]}]}
                 (play 0 :exorcist)
                 (choose :copper))
             {:extra-cards (vals spirit-piles)
              :players     [{:play-area [exorcist]}]
              :trash       [copper]}))
      (is (= (-> {:extra-cards [{:card will-o-wisp :pile-size 12}]
                  :players     [{:hand [exorcist estate]}]}
                 (play 0 :exorcist)
                 (choose :estate)
                 (choose :will-o'-wisp))
             {:extra-cards [{:card will-o-wisp :pile-size 11}]
              :players     [{:play-area [exorcist]
                             :discard   [will-o-wisp]}]
              :trash       [estate]}))
      (is (thrown-with-msg? AssertionError #"Choose error: Imp is not a valid option."
                            (-> {:extra-cards (vals spirit-piles)
                                 :players     [{:hand [exorcist estate]}]}
                                (play 0 :exorcist)
                                (choose :estate)
                                (choose :imp))))
      (is (thrown-with-msg? AssertionError #"Choose error: Wish is not a valid option."
                            (-> {:extra-cards [{:card wish :pile-size 12}
                                               {:card will-o-wisp :pile-size 12}]
                                 :players     [{:hand [exorcist estate]}]}
                                (play 0 :exorcist)
                                (choose :estate)
                                (choose :wish))))
      (is (= (-> {:extra-cards [{:card imp :pile-size 13}]
                  :players     [{:hand [exorcist silver]}]}
                 (play 0 :exorcist)
                 (choose :silver)
                 (choose :imp))
             {:extra-cards [{:card imp :pile-size 12}]
              :players     [{:play-area [exorcist]
                             :discard   [imp]}]
              :trash       [silver]}))
      (is (= (-> {:extra-cards [{:card ghost :pile-size 6}]
                  :players     [{:hand [exorcist duchy]}]}
                 (play 0 :exorcist)
                 (choose :duchy)
                 (choose :ghost))
             {:extra-cards [{:card ghost :pile-size 5}]
              :players     [{:play-area [exorcist]
                             :discard   [ghost]}]
              :trash       [duchy]})))))

(deftest faithful-hound-test
  (let [faithful-hound (assoc faithful-hound :id 0)]
    (testing "Faithful Hound"
      (is (= (-> {:players [{:hand    [faithful-hound]
                             :deck    (repeat 7 copper)
                             :actions 1}]}
                 (play 0 :faithful-hound))
             {:players [{:hand      [copper copper]
                         :play-area [faithful-hound]
                         :deck      (repeat 5 copper)
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [faithful-hound]
                             :deck    (repeat 7 copper)
                             :actions 1}]}
                 (play 0 :faithful-hound)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand    (repeat 5 copper)
                                :discard [copper copper faithful-hound]
                                :actions 1
                                :coins   0
                                :buys    1}]}))
      (is (= (-> {:players [{:hand    [faithful-hound]
                             :deck    (repeat 7 copper)
                             :actions 1}]}
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand    (repeat 5 copper)
                                :deck    [copper copper]
                                :discard [faithful-hound]
                                :actions 1
                                :coins   0
                                :buys    1}]}))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [trusty-steed]
                               :deck    [faithful-hound]
                               :actions 1}]}
                   (play 0 :trusty-steed)
                   (choose [:actions :silvers]))
               {:supply  [{:card silver :pile-size 36}]
                :players [{:play-area [trusty-steed]
                           :discard   [silver silver silver silver faithful-hound]
                           :actions   2}]})))
      (is (= (-> {:supply  [{:card faithful-hound :pile-size 10}]
                  :players [{:coins 2
                             :buys  1}]}
                 (buy-card 0 :faithful-hound))
             {:supply  [{:card faithful-hound :pile-size 9}]
              :players [{:discard [faithful-hound]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand    [faithful-hound copper]
                             :deck    [estate copper]
                             :discard [silver]}]}
                 (receive-boon {:player-no 0})
                 (choose [:faithful-hound :estate]))
             {:boons        {:discard [wind-gift]}
              :players      [{:hand    [copper estate copper]
                              :discard [silver faithful-hound]}]
              :effect-stack [{:text      "You may set Faithful Hound aside, and put it into your hand at end of this turn."
                              :player-no 0
                              :choice    ::nocturne/faithful-hound-set-aside
                              :source    :discard
                              :options   [:faithful-hound]
                              :max       1}
                             {:player-no 0
                              :effect    [:move-card {:player-no 0
                                                      :card-id   nil
                                                      :card-name :estate
                                                      :from      :hand
                                                      :to        :discard}]}]}))
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [faithful-hound copper]
                             :deck [estate copper]}]}
                 (receive-boon {:player-no 0})
                 (choose [:faithful-hound :estate])
                 (choose nil))
             {:boons   {:discard [wind-gift]}
              :players [{:hand    [copper copper]
                         :discard [faithful-hound estate]}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [wind-gift]}
                  :players [{:hand [faithful-hound copper]
                             :deck [estate copper]}]}
                 (receive-boon {:player-no 0})
                 (choose [:faithful-hound :estate])
                 (choose :faithful-hound))
             {:boons   {:discard [wind-gift]}
              :players [{:hand      [copper copper]
                         :set-aside [faithful-hound]
                         :discard   [estate]
                         :triggers  [{:id       1
                                      :name     :faithful-hound
                                      :event    :at-draw-hand
                                      :duration :once
                                      :effects  [[:move-card {:player-no 0
                                                              :card-name :faithful-hound
                                                              :from      :set-aside
                                                              :to        :hand}]]}]}]}))
      (is (= (-> {:players [{:set-aside [faithful-hound]
                             :deck      (repeat 7 copper)
                             :phase     :buy
                             :triggers  [{:id       1
                                          :name     :faithful-hound
                                          :event    :at-draw-hand
                                          :duration :once
                                          :effects  [[:move-card {:player-no 0
                                                                  :card-name :faithful-hound
                                                                  :from      :set-aside
                                                                  :to        :hand}]]}]}]}
                 (clean-up {:player-no 0}))
             {:players [{:hand    [copper copper copper copper copper faithful-hound]
                         :deck    [copper copper]
                         :actions 0
                         :coins   0
                         :buys    0
                         :phase   :out-of-turn}]}))
      (is (= (-> {:players [{:hand    [militia]
                             :actions 1
                             :coins   0
                             :phase   :action}
                            {:hand [copper copper copper copper faithful-hound]}]}
                 (play 0 :militia)
                 (choose [:copper :faithful-hound])
                 (choose :faithful-hound)
                 (end-turn 0))
             {:current-player 1
              :players        [{:hand    [militia]
                                :actions 0
                                :coins   0
                                :buys    0
                                :phase   :out-of-turn}
                               {:hand    [copper copper copper faithful-hound]
                                :discard [copper]
                                :actions 1
                                :coins   0
                                :buys    1}]})))))

(deftest fool-test
  (let [fool (assoc fool :id 0)
        gold (assoc gold :id 1)]
    (testing "Fool"
      (ut/reset-ids!)
      (is (= (-> {:artifacts {:lost-in-the-woods (assoc lost-in-the-woods :owner 0)}
                  :players   [{:hand     [fool]
                               :actions  1
                               :triggers [(merge (get-trigger lost-in-the-woods)
                                                 {:duration :lost-in-the-woods})]}]}
                 (play 0 :fool))
             {:artifacts {:lost-in-the-woods (assoc lost-in-the-woods :owner 0)}
              :players   [{:play-area [fool]
                           :actions   0
                           :triggers  [(merge (get-trigger lost-in-the-woods)
                                              {:duration :lost-in-the-woods})]}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons     {:deck    [field-gift sea-gift]
                              :discard [sky-gift river-gift]}
                  :artifacts {:lost-in-the-woods lost-in-the-woods}
                  :players   [{:hand    [fool]
                               :actions 1}]}
                 (play 0 :fool))
             {:boons        {:deck [river-gift]}
              :artifacts    {:lost-in-the-woods (assoc lost-in-the-woods :owner 0)}
              :players      [{:play-area [fool]
                              :actions   0
                              :boons     [field-gift sea-gift sky-gift]
                              :triggers  [(merge (get-trigger lost-in-the-woods)
                                                 {:duration :lost-in-the-woods})]}]
              :effect-stack [{:text      "Receive the Boons in any order."
                              :player-no 0
                              :choice    [::nocturne/fool-receive-boon {:boons [field-gift sea-gift sky-gift]}]
                              :source    :boons
                              :options   [:the-field's-gift :the-sea's-gift :the-sky's-gift]
                              :min       1
                              :max       1}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons     {:deck [field-gift sea-gift sky-gift river-gift]}
                  :artifacts {:lost-in-the-woods lost-in-the-woods}
                  :players   [{:hand    [fool gold gold copper copper]
                               :deck    [estate estate]
                               :actions 1}]}
                 (play 0 :fool)
                 (choose :the-sea's-gift))
             {:boons        {:deck    [river-gift]
                             :discard [sea-gift]}
              :artifacts    {:lost-in-the-woods (assoc lost-in-the-woods :owner 0)}
              :players      [{:hand      [gold gold copper copper estate]
                              :play-area [fool]
                              :deck      [estate]
                              :actions   0
                              :boons     [field-gift sky-gift]
                              :triggers  [(merge (get-trigger lost-in-the-woods)
                                                 {:duration :lost-in-the-woods})]}]
              :effect-stack [{:text      "Receive the Boons in any order."
                              :player-no 0
                              :choice    [::nocturne/fool-receive-boon {:boons [field-gift sky-gift]}]
                              :source    :boons
                              :options   [:the-field's-gift :the-sky's-gift]
                              :min       1
                              :max       1}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons     {:deck [field-gift sea-gift sky-gift river-gift]}
                  :artifacts {:lost-in-the-woods lost-in-the-woods}
                  :players   [{:hand    [fool gold gold copper copper]
                               :deck    [estate estate]
                               :actions 1
                               :coins   0}]}
                 (play 0 :fool)
                 (choose :the-sea's-gift)
                 (choose :the-field's-gift))
             {:boons        {:deck    [river-gift]
                             :discard [sea-gift]}
              :artifacts    {:lost-in-the-woods (assoc lost-in-the-woods :owner 0)}
              :players      [{:hand      [gold gold copper copper estate]
                              :play-area [fool]
                              :deck      [estate]
                              :actions   1
                              :coins     1
                              :boons     [field-gift sky-gift]
                              :triggers  [(merge (get-trigger lost-in-the-woods)
                                                 {:duration :lost-in-the-woods})
                                          {:id       2
                                           :name     :the-field's-gift
                                           :event    :at-clean-up
                                           :duration :once
                                           :effects  [[:return-boon {:boon-name :the-field's-gift}]]}]}]
              :effect-stack [{:text      "Receive the Boons in any order."
                              :player-no 0
                              :choice    [::nocturne/fool-receive-boon {:boons [sky-gift]}]
                              :source    :boons
                              :options   [:the-sky's-gift]
                              :min       1
                              :max       1}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons     {:deck [field-gift sea-gift sky-gift river-gift]}
                  :artifacts {:lost-in-the-woods lost-in-the-woods}
                  :supply    [{:card gold :pile-size 30}]
                  :players   [{:hand    [fool gold gold copper copper]
                               :deck    [estate estate]
                               :actions 1
                               :coins   0}]}
                 (play 0 :fool)
                 (choose :the-sea's-gift)
                 (choose :the-field's-gift)
                 (choose :the-sky's-gift)
                 (choose [:copper :copper :estate]))
             {:boons     {:deck    [river-gift]
                          :discard [sea-gift sky-gift]}
              :artifacts {:lost-in-the-woods (assoc lost-in-the-woods :owner 0)}
              :supply    [{:card gold :pile-size 29}]
              :players   [{:hand      [gold gold]
                           :play-area [fool]
                           :discard   [copper copper estate gold]
                           :deck      [estate]
                           :actions   1
                           :coins     1
                           :boons     [field-gift]
                           :triggers  [(merge (get-trigger lost-in-the-woods)
                                              {:duration :lost-in-the-woods})
                                       {:id       2
                                        :name     :the-field's-gift
                                        :event    :at-clean-up
                                        :duration :once
                                        :effects  [[:return-boon {:boon-name :the-field's-gift}]]}]}]}))
      (testing "Lost in the Woods"
        (is (= (-> {:boons   {:deck [sea-gift]}
                    :players [{:deck     (repeat 7 copper)
                               :phase    :buy
                               :triggers [(merge (get-trigger lost-in-the-woods)
                                                 {:duration :lost-in-the-woods})]}]}
                   (end-turn 0)
                   (choose nil))
               {:current-player 0
                :boons          {:deck [sea-gift]}
                :players        [{:hand     (repeat 5 copper)
                                  :deck     [copper copper]
                                  :actions  1
                                  :coins    0
                                  :buys     1
                                  :phase    :action
                                  :triggers [(merge (get-trigger lost-in-the-woods)
                                                    {:duration :lost-in-the-woods})]}]}))
        (is (= (-> {:boons   {:deck [sea-gift]}
                    :players [{:deck     (repeat 7 copper)
                               :phase    :buy
                               :triggers [(merge (get-trigger lost-in-the-woods)
                                                 {:duration :lost-in-the-woods})]}]}
                   (end-turn 0)
                   (choose :copper))
               {:current-player 0
                :boons          {:discard [sea-gift]}
                :players        [{:hand     (repeat 5 copper)
                                  :deck     [copper]
                                  :discard  [copper]
                                  :actions  1
                                  :coins    0
                                  :buys     1
                                  :phase    :action
                                  :triggers [(merge (get-trigger lost-in-the-woods)
                                                    {:duration :lost-in-the-woods})]}]}))))
    (testing "Lucky Coin"
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand  [lucky-coin]
                               :coins 0}]}
                   (play 0 :lucky-coin))
               {:supply  [{:card silver :pile-size 39}]
                :players [{:play-area [lucky-coin]
                           :discard   [silver]
                           :coins     1}]}))))))

(deftest ghost-test
  (let [ghost (assoc ghost :id 0)]
    (testing "Ghost"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand [ghost]
                             :deck [conclave estate]}]}
                 (play 0 :ghost))
             {:players [{:play-area      [ghost]
                         :deck           [estate]
                         :revealed-cards {:ghost 1}
                         :triggers       [(merge ghost-trigger
                                                 {:id        1
                                                  :card-id   0
                                                  :name      :ghost
                                                  :set-aside [conclave]})]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand [ghost]
                             :deck [estate conclave estate]}]}
                 (play 0 :ghost))
             {:players [{:play-area      [ghost]
                         :deck           [estate]
                         :discard        [estate]
                         :revealed-cards {:discard 1
                                          :ghost   1}
                         :triggers       [(merge ghost-trigger
                                                 {:id        1
                                                  :card-id   0
                                                  :name      :ghost
                                                  :set-aside [conclave]})]}]}))
      (is (= (-> {:players [{:hand    [ghost]
                             :deck    [estate]
                             :discard [copper]}]}
                 (play 0 :ghost))
             {:players [{:play-area      [ghost]
                         :discard        [estate copper]
                         :revealed-cards {:discard 2}}]}))
      (is (= (-> {:players [{:play-area [ghost]
                             :phase     :night
                             :triggers  [(merge ghost-trigger
                                                {:card-id   0
                                                 :name      :ghost
                                                 :set-aside [conclave]})]}]}
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [ghost conclave]
                                :actions   1
                                :coins     4
                                :buys      1
                                :phase     :action}]}))
      (let [fishing-village (assoc fishing-village :id 1)]
        (ut/reset-ids!)
        (is (= (-> {:players [{:hand  [ghost]
                               :deck  [fishing-village]
                               :phase :night}]}
                   (play 0 :ghost)
                   (end-turn 0))
               {:current-player 0
                :players        [{:play-area     [ghost fishing-village]
                                  :actions       5
                                  :coins         2
                                  :buys          1
                                  :phase         :action
                                  :triggers      [(get-trigger fishing-village 2)
                                                  (get-trigger fishing-village 3)]
                                  :repeated-play [{:source 0
                                                   :target 1}]}]}))
        (is (= (-> {:players [{:hand  [ghost]
                               :deck  [fishing-village]
                               :phase :night}]}
                   (play 0 :ghost)
                   (end-turn 0)
                   (end-turn 0))
               {:current-player 0
                :players        [{:play-area [ghost fishing-village]
                                  :actions   3
                                  :coins     2
                                  :buys      1
                                  :phase     :action}]}))))))

(deftest ghost-town-test
  (let [ghost-town (assoc ghost-town :id 0)]
    (testing "Ghost Town"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :action}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :pay}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :buy}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :night}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (is (= (-> {:players [{:hand  [ghost-town]
                             :deck  [copper copper copper copper copper copper silver]
                             :phase :night}]}
                 (play 0 :ghost-town)
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      [copper copper copper copper copper copper]
                                :play-area [ghost-town]
                                :deck      [silver]
                                :actions   2
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:supply  [{:card ghost-town :pile-size 10}]
                  :players [{}]}
                 (gain {:player-no 0 :card-name :ghost-town}))
             {:supply  [{:card ghost-town :pile-size 9}]
              :players [{:hand [ghost-town]}]})))))

(deftest guardian-test
  (let [guardian (assoc guardian :id 0)]
    (testing "guardian"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand [guardian]}]}
                 (play 0 :guardian))
             {:players [{:play-area  [guardian]
                         :unaffected [{:card-id 0}]
                         :triggers   [(get-trigger guardian)]}]}))
      (let [curse (assoc curse :id 1)]
        (is (= (-> {:supply  [{:card curse :pile-size 10}]
                    :players [{:hand    [witch]
                               :actions 1}
                              {:play-area  [guardian]
                               :unaffected [{:card-id 0}]
                               :triggers   [(get-trigger guardian)]}]}
                   (play 0 :witch))
               {:supply  [{:card curse :pile-size 10}]
                :players [{:play-area [witch]
                           :actions   0}
                          {:play-area  [guardian]
                           :unaffected [{:card-id 0}]
                           :triggers   [(get-trigger guardian)]}]})))
      (is (= (-> {:players [{:hand  [guardian]
                             :phase :night}]}
                 (play 0 :guardian)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [guardian]
                                :actions   1
                                :coins     1
                                :buys      1
                                :phase     :action}]})))))

(deftest idol-test
  (let [idol  (assoc idol :id 0)
        curse (assoc curse :id 1)]
    (testing "Idol"
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :supply  [{:card curse :pile-size 10}]
                  :players [{:hand  [idol]
                             :deck  [copper copper]
                             :coins 0}
                            {}]}
                 (play 0 :idol))
             {:boons   {:discard [sea-gift]}
              :supply  [{:card curse :pile-size 10}]
              :players [{:hand      [copper]
                         :play-area [idol]
                         :deck      [copper]
                         :coins     2}
                        {}]}))
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :supply  [{:card curse :pile-size 10}]
                  :players [{:hand      [idol]
                             :play-area [idol]
                             :deck      [copper copper]
                             :coins     2}
                            {}]}
                 (play 0 :idol))
             {:boons   {:deck [sea-gift]}
              :supply  [{:card curse :pile-size 9}]
              :players [{:play-area [idol idol]
                         :deck      [copper copper]
                         :coins     4}
                        {:discard [curse]}]})))))

(deftest imp-test
  (testing "Imp"
    (is (= (-> {:players [{:hand    [imp]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :imp))
           {:players [{:hand      [copper copper]
                       :play-area [imp]
                       :deck      [copper]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [imp imp]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :imp))
           {:players [{:hand      [imp copper copper]
                       :play-area [imp]
                       :deck      [copper]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [imp]
                           :deck    [conclave copper copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :imp)
               (choose :conclave))
           {:players [{:hand      [copper]
                       :play-area [imp conclave]
                       :deck      [copper]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [imp]
                           :deck    [conclave copper copper]
                           :actions 1}]}
               (play 0 :imp)
               (choose nil))
           {:players [{:hand      [conclave copper]
                       :play-area [imp]
                       :deck      [copper]
                       :actions   0}]}))))

(deftest leprechaun-test
  (let [leprechaun (assoc leprechaun :id 0)
        gold       (assoc gold :id 1)
        wish       (assoc wish :id 2)]
    (testing "Leprechaun"
      (is (= (-> {:hexes       {:deck [poverty]}
                  :extra-cards [{:card wish :pile-size 12}]
                  :supply      [{:card gold :pile-size 30}]
                  :players     [{:hand      [leprechaun copper copper copper copper]
                                 :play-area (repeat 5 secret-cave)
                                 :actions   1}]}
                 (play 0 :leprechaun)
                 (choose :copper))
             {:hexes       {:discard [poverty]}
              :extra-cards [{:card wish :pile-size 12}]
              :supply      [{:card gold :pile-size 29}]
              :players     [{:hand      [copper copper copper]
                             :play-area (concat (repeat 5 secret-cave) [leprechaun])
                             :discard   [gold copper]
                             :actions   0}]}))
      (is (= (-> {:hexes       {:deck [poverty]}
                  :extra-cards [{:card wish :pile-size 12}]
                  :supply      [{:card gold :pile-size 30}]
                  :players     [{:hand      [leprechaun copper copper copper copper]
                                 :play-area (repeat 6 secret-cave)
                                 :actions   1}]}
                 (play 0 :leprechaun))
             {:hexes       {:deck [poverty]}
              :extra-cards [{:card wish :pile-size 11}]
              :supply      [{:card gold :pile-size 29}]
              :players     [{:hand      [copper copper copper copper]
                             :play-area (concat (repeat 6 secret-cave) [leprechaun])
                             :discard   [gold wish]
                             :actions   0}]}))
      (is (= (-> {:hexes       {:deck [poverty]}
                  :extra-cards [{:card wish :pile-size 12}]
                  :supply      [{:card gold :pile-size 30}]
                  :players     [{:hand      [leprechaun copper copper copper copper]
                                 :play-area (repeat 7 secret-cave)
                                 :actions   1}]}
                 (play 0 :leprechaun)
                 (choose :copper))
             {:hexes       {:discard [poverty]}
              :extra-cards [{:card wish :pile-size 12}]
              :supply      [{:card gold :pile-size 29}]
              :players     [{:hand      [copper copper copper]
                             :play-area (concat (repeat 7 secret-cave) [leprechaun])
                             :discard   [gold copper]
                             :actions   0}]})))))

(deftest monastery-test
  (let [monastery (assoc monastery :id 0)]
    (testing "Monastery"
      (is (= (-> {:players [{:hand         [monastery estate copper]
                             :gained-cards [{:name :silver :types #{:treasure} :cost 3}]}]}
                 (play 0 :monastery))
             {:players      [{:hand         [estate copper]
                              :play-area    [monastery]
                              :gained-cards [{:name :silver :types #{:treasure} :cost 3}]}]
              :effect-stack [{:text      "Trash up to 1 card from your hand or Coppers you have in play."
                              :player-no 0
                              :choice    :trash-from-area
                              :source    :mixed
                              :options   [{:area :hand :card-name :estate}
                                          {:area :hand :card-name :copper}]
                              :max       1}]}))
      (is (= (-> {:players [{:hand         [monastery estate copper]
                             :play-area    [silver copper]
                             :gained-cards [{:name :silver :types #{:treasure} :cost 3}
                                            {:name :silver :types #{:treasure} :cost 3}]}]}
                 (play 0 :monastery))
             {:players      [{:hand         [estate copper]
                              :play-area    [silver copper monastery]
                              :gained-cards [{:name :silver :types #{:treasure} :cost 3}
                                             {:name :silver :types #{:treasure} :cost 3}]}]
              :effect-stack [{:text      "Trash up to 2 cards from your hand or Coppers you have in play."
                              :player-no 0
                              :choice    :trash-from-area
                              :source    :mixed
                              :options   [{:area :hand :card-name :estate}
                                          {:area :hand :card-name :copper}
                                          {:area :play-area :card-name :copper}]
                              :max       2}]}))
      (is (= (-> {:players [{:hand         [monastery estate copper]
                             :play-area    [silver copper]
                             :gained-cards [{:name :silver :types #{:treasure} :cost 3}]}]}
                 (play 0 :monastery)
                 (choose {:area :hand :card-name :estate}))
             {:players [{:hand         [copper]
                         :play-area    [silver copper monastery]
                         :gained-cards [{:name :silver :types #{:treasure} :cost 3}]}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand         [monastery estate copper]
                             :play-area    [silver copper]
                             :gained-cards [{:name :silver :types #{:treasure} :cost 3}
                                            {:name :silver :types #{:treasure} :cost 3}]}]}
                 (play 0 :monastery)
                 (choose [{:area :hand :card-name :copper}
                          {:area :play-area :card-name :copper}]))
             {:players [{:hand         [estate]
                         :play-area    [silver monastery]
                         :gained-cards [{:name :silver :types #{:treasure} :cost 3}
                                        {:name :silver :types #{:treasure} :cost 3}]}]
              :trash   [copper copper]})))))

(deftest necromancer-test
  (testing "Necromancer"
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1
                           :coins   0}]
                :trash   [conclave]}
               (play 0 :necromancer)
               (choose :conclave))
           {:players [{:play-area [necromancer]
                       :actions   0
                       :coins     2}]
            :trash   [(assoc conclave :face :down)]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1
                           :coins   0}]
                :trash   [conclave conclave]}
               (play 0 :necromancer)
               (choose :conclave))
           {:players [{:play-area [necromancer]
                       :actions   0
                       :coins     2}]
            :trash   [(assoc conclave :face :down) conclave]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1
                           :coins   2}]
                :trash   [(assoc conclave :face :down) conclave]}
               (play 0 :necromancer)
               (choose :conclave))
           {:players [{:play-area [necromancer]
                       :actions   0
                       :coins     4}]
            :trash   [(assoc conclave :face :down) (assoc conclave :face :down)]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1
                           :coins   4}]
                :trash   [(assoc conclave :face :down) (assoc conclave :face :down)]}
               (play 0 :necromancer))
           {:players [{:play-area [necromancer]
                       :actions   0
                       :coins     4}]
            :trash   [(assoc conclave :face :down) (assoc conclave :face :down)]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1}]
                :trash   [secret-cave]}
               (play 0 :necromancer))
           {:players [{:play-area [necromancer]
                       :actions   0}]
            :trash   [secret-cave]}))
    (is (= (-> {:players [{:hand    [necromancer]
                           :actions 1}]
                :trash   [(assoc zombie-apprentice :face :down)]}
               (clean-up {:player-no 0}))
           {:players [{:hand    [necromancer]
                       :actions 0
                       :coins   0
                       :buys    0}]
            :trash   [zombie-apprentice]}))
    (let [zombie-apprentice (assoc zombie-apprentice :id 1)]
      (is (= (-> {:players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [(assoc zombie-apprentice :face :down)]}
                 (play 0 :lurker)
                 (choose :gain)
                 (choose :zombie-apprentice))
             {:players [{:play-area [lurker]
                         :discard   [zombie-apprentice]
                         :actions   1}]
              :trash   []})))
    (testing "Zombie Apprentice"
      (is (= (-> {:players [{:hand    [necromancer]
                             :actions 1}]
                  :trash   [zombie-apprentice]}
                 (play 0 :necromancer)
                 (choose :zombie-apprentice))
             {:players [{:play-area [necromancer]
                         :actions   0}]
              :trash   [(assoc zombie-apprentice :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer conclave]
                             :actions 1}]
                  :trash   [zombie-apprentice]}
                 (play 0 :necromancer)
                 (choose :zombie-apprentice)
                 (choose nil))
             {:players [{:hand      [conclave]
                         :play-area [necromancer]
                         :actions   0}]
              :trash   [(assoc zombie-apprentice :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer conclave]
                             :deck    [copper copper copper copper]
                             :actions 1}]
                  :trash   [zombie-apprentice]}
                 (play 0 :necromancer)
                 (choose :zombie-apprentice)
                 (choose :conclave))
             {:players [{:hand      [copper copper copper]
                         :play-area [necromancer]
                         :deck      [copper]
                         :actions   1}]
              :trash   [(assoc zombie-apprentice :face :down)
                        conclave]})))
    (testing "Zombie Mason"
      (is (= (-> {:players [{:hand    [necromancer]
                             :actions 1}]
                  :trash   [zombie-mason]}
                 (play 0 :necromancer)
                 (choose :zombie-mason))
             {:players [{:play-area [necromancer]
                         :actions   0}]
              :trash   [(assoc zombie-mason :face :down)]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [necromancer]
                             :deck    [copper]
                             :actions 1}]
                  :trash   [zombie-mason]}
                 (play 0 :necromancer)
                 (choose :zombie-mason)
                 (choose nil))
             {:supply  (base/supply 2 8)
              :players [{:play-area [necromancer]
                         :actions   0}]
              :trash   [(assoc zombie-mason :face :down)
                        copper]}))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [necromancer]
                               :discard [estate estate]
                               :actions 1}]
                    :trash   [zombie-mason]}
                   (play 0 :necromancer)
                   (choose :zombie-mason)
                   (choose :silver))
               {:supply  [{:card silver :pile-size 39}]
                :players [{:play-area [necromancer]
                           :deck      [estate]
                           :discard   [silver]
                           :actions   0}]
                :trash   [(assoc zombie-mason :face :down)
                          estate]}))))
    (testing "Zombie Spy"
      (is (= (-> {:players [{:hand    [necromancer]
                             :actions 1}]
                  :trash   [zombie-spy]}
                 (play 0 :necromancer)
                 (choose :zombie-spy))
             {:players [{:play-area [necromancer]
                         :actions   1}]
              :trash   [(assoc zombie-spy :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer]
                             :deck    [copper]
                             :actions 1}]
                  :trash   [zombie-spy]}
                 (play 0 :necromancer)
                 (choose :zombie-spy))
             {:players [{:hand      [copper]
                         :play-area [necromancer]
                         :actions   1}]
              :trash   [(assoc zombie-spy :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer]
                             :deck    [copper silver estate]
                             :actions 1}]
                  :trash   [zombie-spy]}
                 (play 0 :necromancer)
                 (choose :zombie-spy)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [necromancer]
                         :deck      [silver estate]
                         :actions   1}]
              :trash   [(assoc zombie-spy :face :down)]}))
      (is (= (-> {:players [{:hand    [necromancer]
                             :deck    [copper estate silver]
                             :actions 1}]
                  :trash   [zombie-spy]}
                 (play 0 :necromancer)
                 (choose :zombie-spy)
                 (choose :estate))
             {:players [{:hand      [copper]
                         :play-area [necromancer]
                         :deck      [silver]
                         :discard   [estate]
                         :actions   1}]
              :trash   [(assoc zombie-spy :face :down)]})))))

(deftest night-watchman-test
  (testing "Night Watchman"
    (is (= (-> {:players [{:hand [night-watchman]
                           :deck [copper copper estate estate silver estate]}]}
               (play 0 :night-watchman)
               (choose nil))
           {:players [{:play-area [night-watchman]
                       :deck      [copper copper estate estate silver estate]}]}))
    (is (= (-> {:players [{:hand [night-watchman]
                           :deck [copper copper estate estate silver estate]}]}
               (play 0 :night-watchman)
               (choose [:estate :estate]))
           {:players [{:play-area [night-watchman]
                       :deck      [copper copper silver estate]
                       :discard   [estate estate]}]}))
    (is (= (-> {:players [{:hand [night-watchman]
                           :deck [copper copper estate estate silver estate]}]}
               (play 0 :night-watchman)
               (choose [:estate :estate :copper :copper :silver]))
           {:players [{:play-area [night-watchman]
                       :deck      [estate]
                       :discard   [estate estate copper copper silver]}]}))))

(deftest pixie-test
  (let [pixie (assoc pixie :id 0)]
    (testing "Pixie"
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [pixie]
                             :deck    [copper copper copper copper]
                             :actions 1}]}
                 (play 0 :pixie))
             {:boons        {:discard [sea-gift]}
              :players      [{:hand      [copper]
                              :play-area [pixie]
                              :deck      [copper copper copper]
                              :actions   1}]
              :effect-stack [{:text      "You may trash the Pixie to receive The Sea's Gift twice."
                              :player-no 0
                              :card-id   0
                              :choice    [::nocturne/pixie-receive-boon {:boon sea-gift}]
                              :source    :play-area
                              :options   [:pixie]
                              :max       1}]}))
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [pixie]
                             :deck    [copper copper copper copper]
                             :actions 1}]}
                 (play 0 :pixie)
                 (choose nil))
             {:boons   {:discard [sea-gift]}
              :players [{:hand      [copper]
                         :play-area [pixie]
                         :deck      [copper copper copper]
                         :actions   1}]}))
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [pixie]
                             :deck    [copper copper copper copper]
                             :actions 1}]}
                 (play 0 :pixie)
                 (choose :pixie))
             {:boons   {:discard [sea-gift]}
              :players [{:hand    [copper copper copper]
                         :deck    [copper]
                         :actions 1}]
              :trash   [pixie]}))
      (is (= (-> {:boons   {:deck [sea-gift field-gift]}
                  :players [{:hand    [throne-room pixie]
                             :deck    [copper copper copper copper copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :pixie)
                 (choose :pixie))
             {:boons   {:discard [sea-gift field-gift]}
              :players [{:hand      [copper copper copper copper]
                         :play-area [throne-room]
                         :deck      [copper]
                         :actions   2}]
              :trash   [pixie]}))
      (is (= (-> {:boons   {:deck [field-gift sea-gift]}
                  :players [{:hand    [throne-room pixie]
                             :deck    [copper copper copper copper copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :pixie)
                 (choose nil)
                 (choose :pixie))
             {:boons   {:discard [field-gift sea-gift]}
              :players [{:hand      [copper copper copper copper]
                         :play-area [throne-room]
                         :deck      [copper]
                         :actions   2}]
              :trash   [pixie]}))
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [field-gift]}
                  :players [{:hand    [pixie]
                             :actions 1
                             :coins   0}]}
                 (play 0 :pixie)
                 (choose :pixie))
             {:boons   {}
              :players [{:actions  3
                         :coins    2
                         :boons    [field-gift]
                         :triggers [{:id       1
                                     :name     :the-field's-gift
                                     :event    :at-clean-up
                                     :duration :once
                                     :effects  [[:return-boon {:boon-name :the-field's-gift}]]}]}]
              :trash   [pixie]})))
    (testing "Goat"
      (is (= (-> {:players [{:hand  [goat]
                             :coins 0}]}
                 (play 0 :goat))
             {:players [{:play-area [goat]
                         :coins     1}]}))
      (is (= (-> {:players [{:hand  [goat copper]
                             :coins 0}]}
                 (play 0 :goat)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [goat]
                         :coins     1}]}))
      (is (= (-> {:players [{:hand  [goat copper]
                             :coins 0}]}
                 (play 0 :goat)
                 (choose :copper))
             {:players [{:play-area [goat]
                         :coins     1}]
              :trash   [copper]})))))

(deftest pooka-test
  (let [pooka (assoc pooka :id 0)]
    (testing "Pooka"
      (is (= (-> {:players [{:hand    [pooka estate copper cursed-gold]
                             :actions 1}]}
                 (play 0 :pooka))
             {:players      [{:hand      [estate copper cursed-gold]
                              :play-area [pooka]
                              :actions   0}]
              :effect-stack [{:text      "You may trash a Treasure other than Cursed Gold from your hand, for +4 Cards."
                              :player-no 0
                              :card-id   0
                              :choice    ::nocturne/pooka-trash
                              :source    :hand
                              :options   [:copper]
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [pooka estate copper cursed-gold]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 (play 0 :pooka)
                 (choose nil))
             {:players [{:hand      [estate copper cursed-gold]
                         :play-area [pooka]
                         :deck      (repeat 5 copper)
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [pooka estate copper cursed-gold]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 (play 0 :pooka)
                 (choose :copper))
             {:players [{:hand      [estate cursed-gold copper copper copper copper]
                         :play-area [pooka]
                         :deck      [copper]
                         :actions   0}]
              :trash   [copper]})))
    (testing "Cursed Gold"
      (let [curse (assoc curse :id 1)]
        (is (= (-> {:supply  [{:card curse :pile-size 10}]
                    :players [{:hand  [cursed-gold]
                               :coins 0}]}
                   (play 0 :cursed-gold))
               {:supply  [{:card curse :pile-size 9}]
                :players [{:play-area [cursed-gold]
                           :discard   [curse]
                           :coins     3}]}))))))

(deftest sacred-grove-test
  (let [sacred-grove (assoc sacred-grove :id 0)]
    (testing "Sacred Grove"
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [sacred-grove]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}
                            {:deck [estate estate]}]}
                 (play 0 :sacred-grove))
             {:boons        {:discard [sea-gift]}
              :players      [{:hand      [copper]
                              :play-area [sacred-grove]
                              :deck      [copper]
                              :actions   0
                              :coins     3
                              :buys      2}
                             {:deck [estate estate]}]
              :effect-stack [{:text      "You may receive The Sea's Gift."
                              :player-no 1
                              :choice    [::nocturne/sacred-grove-choice {:boon sea-gift}]
                              :source    :special
                              :options   [{:option :yes :text "Yes please!"}
                                          {:option :no :text "No thank you."}]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [sacred-grove]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}
                            {:deck [estate estate]}]}
                 (play 0 :sacred-grove)
                 (choose :no))
             {:boons   {:discard [sea-gift]}
              :players [{:hand      [copper]
                         :play-area [sacred-grove]
                         :deck      [copper]
                         :actions   0
                         :coins     3
                         :buys      2}
                        {:deck [estate estate]}]}))
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [sacred-grove]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}
                            {:deck [estate estate]}]}
                 (play 0 :sacred-grove)
                 (choose :yes))
             {:boons   {:discard [sea-gift]}
              :players [{:hand      [copper]
                         :play-area [sacred-grove]
                         :deck      [copper]
                         :actions   0
                         :coins     3
                         :buys      2}
                        {:hand [estate]
                         :deck [estate]}]}))
      (ut/reset-ids!)
      (is (= (-> {:boons   {:deck [field-gift]}
                  :players [{:hand    [sacred-grove]
                             :actions 1
                             :coins   0
                             :buys    1}
                            {:deck [estate estate]}]}
                 (play 0 :sacred-grove))
             {:boons   {}
              :players [{:play-area [sacred-grove]
                         :actions   1
                         :coins     4
                         :buys      2
                         :boons     [field-gift]
                         :triggers  [{:id       1
                                      :name     :the-field's-gift
                                      :event    :at-clean-up
                                      :duration :once
                                      :effects  [[:return-boon {:boon-name :the-field's-gift}]]}]}
                        {:deck [estate estate]}]}))
      (is (= (-> {:boons   {:deck [river-gift]}
                  :players [{:hand    [sacred-grove]
                             :deck    (repeat 7 copper)
                             :actions 1
                             :coins   0
                             :buys    1
                             :phase   :action}
                            {:hand [copper copper copper copper copper]
                             :deck [estate estate]}]}
                 (play 0 :sacred-grove)
                 (choose :yes)
                 (end-turn 0))
             {:boons          {:discard [river-gift]}
              :current-player 1
              :players        [{:hand    (repeat 6 copper)
                                :deck    [copper]
                                :discard [sacred-grove]
                                :actions 0
                                :coins   0
                                :buys    0
                                :phase   :out-of-turn}
                               {:hand    [copper copper copper copper copper estate]
                                :deck    [estate]
                                :actions 1
                                :coins   0
                                :buys    1}]})))))

(deftest raider-test
  (let [raider (assoc raider :id 0)]
    (testing "Raider"
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand      [raider copper]
                             :play-area [silver conclave]}
                            {:hand [conclave copper raider silver gold]}]}
                 (play 0 :raider))
             {:players      [{:hand      [copper]
                              :play-area [silver conclave raider]
                              :triggers  [(get-trigger raider)]}
                             {:hand [conclave copper raider silver gold]}]
              :effect-stack [{:player-no 1
                              :text      "Discard a copy of a card the attacker has in play."
                              :choice    :discard-from-hand
                              :source    :hand
                              :options   [:conclave :raider :silver]
                              :min       1
                              :max       1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand      [raider copper]
                             :play-area [silver conclave]}
                            {:hand [conclave copper raider silver gold]}]}
                 (play 0 :raider)
                 (choose :silver))
             {:players [{:hand      [copper]
                         :play-area [silver conclave raider]
                         :triggers  [(get-trigger raider)]}
                        {:hand    [conclave copper raider gold]
                         :discard [silver]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand      [raider copper]
                             :play-area [silver conclave]}
                            {:hand [conclave copper raider gold]}]}
                 (play 0 :raider))
             {:players [{:hand      [copper]
                         :play-area [silver conclave raider]
                         :triggers  [(get-trigger raider)]}
                        {:hand [conclave copper raider gold]}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand      [raider copper]
                             :play-area [silver conclave]}
                            {:hand (repeat 5 copper)}]}
                 (play 0 :raider))
             {:players [{:hand      [copper]
                         :play-area [silver conclave raider]
                         :triggers  [(get-trigger raider)]}
                        {:hand           (repeat 5 copper)
                         :revealed-cards {:hand 5}}]}))
      (is (= (-> {:players [{:hand  [raider]
                             :phase :action}]}
                 (play 0 :raider)
                 (end-turn 0))
             {:current-player 0
              :players        [{:play-area [raider]
                                :actions   1
                                :coins     3
                                :buys      1
                                :phase     :action}]})))))

(deftest secret-cave-test
  (let [secret-cave (assoc secret-cave :id 0)]
    (testing "Secret Cave"
      (is (= (-> {:players [{:hand    [secret-cave]
                             :actions 1}]}
                 (play 0 :secret-cave))
             {:players [{:play-area [secret-cave]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [secret-cave estate estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave))
             {:players      [{:hand      [estate estate copper]
                              :play-area [secret-cave]
                              :deck      [copper]
                              :actions   1}]
              :effect-stack [{:text      "You may discard 3 cards, for +$3 next turn."
                              :player-no 0
                              :card-id   0
                              :choice    ::nocturne/secret-cave-discard
                              :source    :hand
                              :options   [:estate :estate :copper]
                              :min       3
                              :max       3
                              :optional? true}]}))
      (is (= (-> {:players [{:hand    [secret-cave estate estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose nil))
             {:players [{:hand      [estate estate copper]
                         :play-area [secret-cave]
                         :deck      [copper]
                         :actions   1}]}))
      (ut/reset-ids!)
      (is (= (-> {:players [{:hand    [secret-cave estate estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose [:estate :estate :copper]))
             {:players [{:play-area [secret-cave]
                         :deck      [copper]
                         :discard   [estate estate copper]
                         :actions   1
                         :triggers  [(merge secret-cave-trigger
                                            {:id      1
                                             :card-id 0
                                             :name    :secret-cave})]}]}))
      (is (= (-> {:players [{:hand    [secret-cave estate estate]
                             :deck    (repeat 7 copper)
                             :actions 1
                             :phase   :action}]}
                 (play 0 :secret-cave)
                 (choose [:estate :estate :copper])
                 (end-turn 0))
             {:current-player 0
              :players        [{:hand      (repeat 5 copper)
                                :play-area [secret-cave]
                                :deck      [copper]
                                :discard   [estate estate copper]
                                :actions   1
                                :coins     3
                                :buys      1
                                :phase     :action}]}))
      (is (= (-> {:players [{:hand    [secret-cave estate]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose [:estate :copper]))
             {:players [{:play-area [secret-cave]
                         :deck      [copper]
                         :discard   [estate copper]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [secret-cave]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [secret-cave]
                         :deck      [copper]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [secret-cave]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :secret-cave)
                 (choose [:copper]))
             {:players [{:play-area [secret-cave]
                         :deck      [copper]
                         :discard   [copper]
                         :actions   1}]}))))
  (let [magic-lamp (assoc magic-lamp :id 0)]
    (testing "Magic Lamp"
      (let [wish (assoc wish :id 1)]
        (is (= (-> {:players [{:hand  [magic-lamp]
                               :coins 0}]}
                   (play 0 :magic-lamp))
               {:players [{:play-area [magic-lamp]
                           :coins     1}]}))
        (is (= (-> {:extra-cards [{:card wish :pile-size 12}]
                    :players     [{:hand      [magic-lamp]
                                   :play-area [secret-cave gold silver copper]
                                   :coins     6}]}
                   (play 0 :magic-lamp))
               {:extra-cards [{:card wish :pile-size 12}]
                :players     [{:play-area [secret-cave gold silver copper magic-lamp]
                               :coins     7}]}))
        (is (= (-> {:extra-cards [{:card wish :pile-size 12}]
                    :players     [{:hand      [magic-lamp]
                                   :play-area [secret-cave shepherd gold silver copper]
                                   :coins     6}]}
                   (play 0 :magic-lamp))
               {:extra-cards [{:card wish :pile-size 9}]
                :players     [{:play-area [secret-cave shepherd gold silver copper]
                               :discard   [wish wish wish]
                               :coins     7}]
                :trash       [magic-lamp]}))
        (is (= (-> {:extra-cards [{:card wish :pile-size 12}]
                    :players     [{:hand      [magic-lamp]
                                   :play-area [secret-cave shepherd shepherd gold silver copper]
                                   :coins     6}]}
                   (play 0 :magic-lamp))
               {:extra-cards [{:card wish :pile-size 12}]
                :players     [{:play-area [secret-cave shepherd shepherd gold silver copper magic-lamp]
                               :coins     7}]}))))))

(deftest shepherd-test
  (let [shepherd (assoc shepherd :id 0)]
    (testing "Shepherd"
      (is (= (-> {:players [{:hand    [shepherd copper copper]
                             :actions 1}]}
                 (play 0 :shepherd))
             {:players [{:hand      [copper copper]
                         :play-area [shepherd]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [shepherd copper copper estate estate]
                             :actions 1}]}
                 (play 0 :shepherd)
                 (choose nil))
             {:players [{:hand      [copper copper estate estate]
                         :play-area [shepherd]
                         :actions   1}]}))
      (is (= (-> {:players [{:hand    [shepherd copper copper estate estate]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 (play 0 :shepherd)
                 (choose :estate))
             {:players [{:hand           [copper copper estate copper copper]
                         :play-area      [shepherd]
                         :deck           (repeat 3 copper)
                         :discard        [estate]
                         :revealed-cards {:discard 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [shepherd copper copper estate estate]
                             :deck    (repeat 5 copper)
                             :actions 1}]}
                 (play 0 :shepherd)
                 (choose [:estate :estate]))
             {:players [{:hand           [copper copper copper copper copper copper]
                         :play-area      [shepherd]
                         :deck           [copper]
                         :discard        [estate estate]
                         :revealed-cards {:discard 2}
                         :actions        1}]})))
    (testing "Pasture"
      (is (= (-> {:players [{:hand  [pasture]
                             :coins 0}]}
                 (play 0 :pasture))
             {:players [{:play-area [pasture]
                         :coins     1}]}))
      (is (= (calc-victory-points {:deck [pasture]})
             0))
      (is (= (calc-victory-points {:deck [pasture estate]})
             2))
      (is (= (calc-victory-points {:deck [pasture estate estate]})
             4))
      (is (= (calc-victory-points {:deck [pasture estate estate estate]})
             6)))))

(deftest skulk-test
  (let [skulk (assoc skulk :id 0)]
    (testing "Skulk"
      (is (= (-> {:hexes   {:deck [poverty plague]}
                  :players [{:hand    [skulk]
                             :actions 1
                             :buys    1}
                            {:hand [copper copper copper copper copper]}
                            {:hand [copper copper copper estate estate]}]}
                 (play 0 :skulk)
                 (choose [:copper :copper])
                 (choose [:estate :estate]))
             {:hexes   {:deck    [plague]
                        :discard [poverty]}
              :players [{:play-area [skulk]
                         :actions   0
                         :buys      2}
                        {:hand    [copper copper copper]
                         :discard [copper copper]}
                        {:hand    [copper copper copper]
                         :discard [estate estate]}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:supply  [{:card skulk :pile-size 10}
                              {:card gold :pile-size 30}]
                    :players [{}]}
                   (gain {:player-no 0 :card-name :skulk}))
               {:supply  [{:card skulk :pile-size 9}
                          {:card gold :pile-size 29}]
                :players [{:discard [gold skulk]}]}))))))

(deftest tormentor-test
  (let [tormentor (assoc tormentor :id 0)
        imp       (assoc imp :id 1)]
    (testing "Tormentor"
      (is (= (-> {:hexes       {:deck [poverty]}
                  :extra-cards [{:card imp :pile-size 13}]
                  :players     [{:hand    [tormentor]
                                 :actions 1
                                 :coins   0}
                                {:hand [copper copper copper copper copper]}]}
                 (play 0 :tormentor))
             {:hexes       {:deck [poverty]}
              :extra-cards [{:card imp :pile-size 12}]
              :players     [{:play-area [tormentor]
                             :discard   [imp]
                             :actions   0
                             :coins     2}
                            {:hand [copper copper copper copper copper]}]}))
      (is (= (-> {:hexes       {:deck [poverty]}
                  :extra-cards [{:card imp :pile-size 12}]
                  :players     [{:hand      [tormentor]
                                 :play-area [imp]
                                 :actions   1
                                 :coins     0}
                                {:hand [copper copper copper copper copper]}]}
                 (play 0 :tormentor)
                 (choose [:copper :copper]))
             {:hexes       {:discard [poverty]}
              :extra-cards [{:card imp :pile-size 12}]
              :players     [{:play-area [imp tormentor]
                             :actions   0
                             :coins     2}
                            {:hand    [copper copper copper]
                             :discard [copper copper]}]})))))

(deftest tracker-test
  (let [tracker (assoc tracker :id 0)]
    (testing "Tracker"
      (is (= (-> {:boons   {:deck [sea-gift]}
                  :players [{:hand    [tracker]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :tracker))
             {:boons   {:discard [sea-gift]}
              :players [{:hand      [copper]
                         :play-area [tracker]
                         :deck      [copper]
                         :actions   0
                         :coins     1}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:supply  [{:card gold :pile-size 30}]
                    :players [{:play-area [tracker]
                               :deck      [copper]}]}
                   (gain {:player-no 0 :card-name :gold})
                   (choose :gold))
               {:supply  [{:card gold :pile-size 29}]
                :players [{:play-area [tracker]
                           :deck      [gold copper]}]}))))
    (testing "Pouch"
      (is (= (-> {:players [{:hand  [pouch]
                             :coins 0
                             :buys  1}]}
                 (play 0 :pouch))
             {:players [{:play-area [pouch]
                         :coins     1
                         :buys      2}]})))))

(deftest tragic-hero-test
  (let [tragic-hero (assoc tragic-hero :id 0)]
    (testing "Tragic Hero"
      (is (= (-> {:players [{:hand    [tragic-hero copper copper copper copper]
                             :deck    [estate estate estate copper]
                             :actions 1
                             :buys    1}]}
                 (play 0 :tragic-hero))
             {:players [{:hand      [copper copper copper copper estate estate estate]
                         :play-area [tragic-hero]
                         :deck      [copper]
                         :actions   0
                         :buys      2}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:supply  [{:card gold :pile-size 30}]
                    :players [{:hand    [tragic-hero copper copper copper copper copper]
                               :deck    [estate estate estate copper]
                               :actions 1
                               :buys    1}]}
                   (play 0 :tragic-hero)
                   (choose :gold))
               {:supply  [{:card gold :pile-size 29}]
                :players [{:hand    [copper copper copper copper copper estate estate estate]
                           :deck    [copper]
                           :discard [gold]
                           :actions 0
                           :buys    2}]
                :trash   [tragic-hero]}))))))

(deftest vampire-test
  (let [vampire (assoc vampire :id 0)
        bat     (assoc bat :id 1)]
    (testing "Vampire"
      (let [tragic-hero (assoc tragic-hero :id 2)]
        (is (= (-> {:hexes       {:deck [poverty]}
                    :extra-cards [{:card bat :pile-size 10}]
                    :supply      [{:card tragic-hero :pile-size 10}
                                  {:card vampire :pile-size 9}]
                    :players     [{:hand [vampire]}
                                  {:hand [copper copper copper copper copper]}]}
                   (play 0 :vampire)
                   (choose [:copper :copper])
                   (choose :tragic-hero))
               {:hexes       {:discard [poverty]}
                :extra-cards [{:card bat :pile-size 9}]
                :supply      [{:card tragic-hero :pile-size 9}
                              {:card vampire :pile-size 10}]
                :players     [{:discard [tragic-hero bat]}
                              {:hand    [copper copper copper]
                               :discard [copper copper]}]}))
        (is (thrown-with-msg? AssertionError #"Choose error: Vampire is not a valid option."
                              (-> {:hexes       {:deck [poverty]}
                                   :extra-cards [{:card bat :pile-size 10}]
                                   :supply      [{:card tragic-hero :pile-size 10}
                                                 {:card vampire :pile-size 9}]
                                   :players     [{:hand [vampire]}]}
                                  (play 0 :vampire)
                                  (choose :vampire))))
        (is (= (-> {:hexes       {:deck [poverty]}
                    :extra-cards [{:card bat :pile-size 0}]
                    :supply      [{:card tragic-hero :pile-size 10}
                                  {:card vampire :pile-size 9}]
                    :players     [{:hand [vampire]}]}
                   (play 0 :vampire)
                   (choose :tragic-hero))
               {:hexes       {:discard [poverty]}
                :extra-cards [{:card bat :pile-size 0}]
                :supply      [{:card tragic-hero :pile-size 9}
                              {:card vampire :pile-size 9}]
                :players     [{:play-area [vampire]
                               :discard   [tragic-hero]}]}))))
    (testing "Bat"
      (is (= (-> {:extra-cards [{:card bat :pile-size 9}]
                  :supply      [{:card vampire :pile-size 10}]
                  :players     [{:hand [bat copper copper copper copper]}]}
                 (play 0 :bat)
                 (choose [:copper :copper]))
             {:extra-cards [{:card bat :pile-size 10}]
              :supply      [{:card vampire :pile-size 9}]
              :players     [{:hand    [copper copper]
                             :discard [vampire]}]
              :trash       [copper copper]}))
      (is (= (-> {:extra-cards [{:card bat :pile-size 9}]
                  :supply      [{:card vampire :pile-size 10}]
                  :players     [{:hand [bat copper copper copper copper]}]}
                 (play 0 :bat)
                 (choose :copper))
             {:extra-cards [{:card bat :pile-size 10}]
              :supply      [{:card vampire :pile-size 9}]
              :players     [{:hand    [copper copper copper]
                             :discard [vampire]}]
              :trash       [copper]}))
      (is (= (-> {:extra-cards [{:card bat :pile-size 9}]
                  :supply      [{:card vampire :pile-size 10}]
                  :players     [{:hand [bat copper copper copper copper]}]}
                 (play 0 :bat)
                 (choose nil))
             {:extra-cards [{:card bat :pile-size 9}]
              :supply      [{:card vampire :pile-size 10}]
              :players     [{:hand      [copper copper copper copper]
                             :play-area [bat]}]}))
      (is (= (-> {:extra-cards [{:card bat :pile-size 9}]
                  :supply      [{:card vampire :pile-size 10}]
                  :players     [{:hand [bat copper]}]}
                 (play 0 :bat)
                 (choose :copper))
             {:extra-cards [{:card bat :pile-size 10}]
              :supply      [{:card vampire :pile-size 9}]
              :players     [{:discard [vampire]}]
              :trash       [copper]}))
      (is (= (-> {:extra-cards [{:card bat :pile-size 9}]
                  :supply      [{:card vampire :pile-size 10}]
                  :players     [{:hand [bat copper]}]}
                 (play 0 :bat)
                 (choose nil))
             {:extra-cards [{:card bat :pile-size 9}]
              :supply      [{:card vampire :pile-size 10}]
              :players     [{:hand      [copper]
                             :play-area [bat]}]}))
      (is (= (-> {:extra-cards [{:card bat :pile-size 9}]
                  :supply      [{:card vampire :pile-size 10}]
                  :players     [{:hand [bat]}]}
                 (play 0 :bat))
             {:extra-cards [{:card bat :pile-size 9}]
              :supply      [{:card vampire :pile-size 10}]
              :players     [{:play-area [bat]}]})))))

(deftest werewolf-test
  (let [werewolf (assoc werewolf :id 0)]
    (testing "Werewolf"
      (is (= (-> {:players [{:hand    [werewolf]
                             :deck    [estate estate estate copper]
                             :actions 1}]}
                 (play 0 :werewolf))
             {:players [{:hand      [estate estate estate]
                         :play-area [werewolf]
                         :deck      [copper]
                         :actions   0}]}))
      (is (= (-> {:hexes   {:deck [poverty]}
                  :players [{:hand  [werewolf]
                             :deck  [estate estate estate copper]
                             :phase :night}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :werewolf)
                 (choose [:copper :copper]))
             {:hexes   {:discard [poverty]}
              :players [{:play-area [werewolf]
                         :deck      [estate estate estate copper]
                         :phase     :night}
                        {:hand    [copper copper copper]
                         :discard [copper copper]}]})))))

(deftest will-o-wisp-test
  (testing "Will-o'-Wisp"
    (let [will-o-wisp (assoc will-o-wisp :id 0)
          estate      (assoc estate :id 1)]
      (is (= (-> {:players [{:hand    [will-o-wisp]
                             :deck    [silver silver copper]
                             :actions 1}]}
                 (play 0 :will-o'-wisp))
             {:players [{:hand           [silver]
                         :play-area      [will-o-wisp]
                         :deck           [silver copper]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [will-o-wisp]
                             :deck    [silver estate copper]
                             :actions 1}]}
                 (play 0 :will-o'-wisp))
             {:players [{:hand           [silver estate]
                         :play-area      [will-o-wisp]
                         :deck           [copper]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [will-o-wisp]
                             :deck    [silver]
                             :actions 1}]}
                 (play 0 :will-o'-wisp))
             {:players [{:hand      [silver]
                         :play-area [will-o-wisp]
                         :actions   1}]})))))

(deftest wish-test
  (testing "Wish"
    (let [wish (assoc wish :id 0)]
      (is (= (-> {:supply      (base/supply 2 8)
                  :extra-cards [{:card wish :pile-size 11}]
                  :players     [{:hand    [wish]
                                 :actions 1}]}
                 (play 0 :wish))
             {:supply       (base/supply 2 8)
              :extra-cards  [{:card wish :pile-size 12}]
              :players      [{:actions 1}]
              :effect-stack [{:text      "Gain a card to your hand costing up to $6."
                              :player-no 0
                              :card-id   0
                              :choice    :gain-to-hand
                              :source    :supply
                              :options   [:curse :estate :duchy :copper :silver :gold]
                              :min       1
                              :max       1}]}))
      (let [gold (assoc gold :id 1)]
        (is (= (-> {:supply      [{:card gold :pile-size 30}]
                    :extra-cards [{:card wish :pile-size 11}]
                    :players     [{:hand    [wish]
                                   :actions 1}]}
                   (play 0 :wish)
                   (choose :gold))
               {:supply      [{:card gold :pile-size 29}]
                :extra-cards [{:card wish :pile-size 12}]
                :players     [{:hand    [gold]
                               :actions 1}]}))
        (is (= (-> {:supply      [{:card gold :pile-size 30}]
                    :extra-cards [{:card wish :pile-size 11}]
                    :players     [{:hand    [throne-room wish]
                                   :actions 1}]}
                   (play 0 :throne-room)
                   (choose :wish)
                   (choose :gold))
               {:supply      [{:card gold :pile-size 29}]
                :extra-cards [{:card wish :pile-size 12}]
                :players     [{:hand      [gold]
                               :play-area [throne-room]
                               :actions   2}]}))))))