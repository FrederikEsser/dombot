(ns dombot.cards.dark-ages-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.dark-ages :as dark-ages :refer :all]
            [dombot.cards.dominion :refer [militia throne-room]]
            [dombot.cards.intrigue :refer [harem lurker]]
            [dombot.cards.seaside :refer [caravan]]
            [dombot.cards.adventures :refer [warrior]]
            [dombot.cards.empires :refer [engineer fortune]]
            [dombot.cards.menagerie :refer [village-green]]
            [dombot.cards.kingdom :refer [setup-game]]
            [dombot.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 234 (f)))

(use-fixtures :each fixture)

(deftest altar-test
  (let [altar (assoc altar :id 0)
        duchy (assoc duchy :id 1)]
    (testing "Altar"
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [altar copper]
                             :actions 1}]}
                 (play 0 :altar)
                 (choose :copper)
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:play-area [altar]
                         :discard   [duchy]
                         :actions   0}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [altar]
                             :actions 1}]}
                 (play 0 :altar)
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:play-area [altar]
                         :discard   [duchy]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 0}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [altar copper]
                             :actions 1}]}
                 (play 0 :altar)
                 (choose :copper))
             {:supply  [{:card duchy :pile-size 0}
                        {:card gold :pile-size 30}]
              :players [{:play-area [altar]
                         :actions   0}]
              :trash   [copper]})))))

(deftest armory-test
  (let [armory (assoc armory :id 0)
        duchy  (assoc duchy :id 1)]
    (testing "Armory"
      (is (= (-> {:supply  [{:card armory :pile-size 9}]
                  :players [{:hand    [armory]
                             :actions 1}]}
                 (play 0 :armory)
                 (choose :armory))
             {:supply  [{:card armory :pile-size 8}]
              :players [{:play-area [armory]
                         :deck      [armory]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [armory]
                             :actions 1}]}
                 (play 0 :armory))
             {:supply  [{:card duchy :pile-size 8}]
              :players [{:play-area [armory]
                         :actions   0}]})))))

(deftest band-of-misfits-test
  (let [band-of-misfits (assoc band-of-misfits :id 0)]
    (testing "Band of Misfits"
      (is (= (-> {:supply  [{:card fortress :pile-size 10}]
                  :players [{:hand    [band-of-misfits]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :band-of-misfits)
                 (choose :fortress))
             {:supply  [{:card fortress :pile-size 10}]
              :players [{:hand      [copper]
                         :play-area [band-of-misfits]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:supply  [{:split-pile [{:card sir-martin :pile-size 1}
                                          {:card sir-destry :pile-size 1}]}]
                  :players [{:hand    [band-of-misfits]
                             :actions 1
                             :buys    1}]}
                 (play 0 :band-of-misfits)
                 (choose :sir-martin))
             {:supply  [{:split-pile [{:card sir-martin :pile-size 1}
                                      {:card sir-destry :pile-size 1}]}]
              :players [{:play-area [band-of-misfits]
                         :actions   0
                         :buys      3}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}
                            {:card engineer :pile-size 10}
                            {:card fortress :pile-size 0}
                            {:card band-of-misfits :pile-size 9}
                            {:card bandit-camp :pile-size 10}
                            {:split-pile [{:card sir-destry :pile-size 1}
                                          {:card sir-martin :pile-size 1}]}]
                  :players [{:hand    [band-of-misfits]
                             :actions 1}]}
                 (play 0 :band-of-misfits))
             {:supply  [{:card silver :pile-size 40}
                        {:card engineer :pile-size 10}
                        {:card fortress :pile-size 0}
                        {:card band-of-misfits :pile-size 9}
                        {:card bandit-camp :pile-size 10}
                        {:split-pile [{:card sir-destry :pile-size 1}
                                      {:card sir-martin :pile-size 1}]}]
              :players [{:play-area [band-of-misfits]
                         :actions   0}]}))
      (is (= (-> {:cost-reductions [{:reduction 1}]
                  :supply          [{:card fortress :pile-size 10}]
                  :players         [{:hand    [band-of-misfits]
                                     :deck    [copper copper]
                                     :actions 1}]}
                 (play 0 :band-of-misfits)
                 (choose :fortress))
             {:cost-reductions [{:reduction 1}]
              :supply          [{:card fortress :pile-size 10}]
              :players         [{:hand      [copper]
                                 :play-area [band-of-misfits]
                                 :deck      [copper]
                                 :actions   2}]}))
      (is (= (-> {:cost-reductions [{:reduction 1}]
                  :supply          [{:card silver :pile-size 40}
                                    {:card engineer :pile-size 10}
                                    {:card band-of-misfits :pile-size 9}
                                    {:card bandit-camp :pile-size 10}]
                  :players         [{:hand    [band-of-misfits]
                                     :actions 1}]}
                 (play 0 :band-of-misfits))
             {:cost-reductions [{:reduction 1}]
              :supply          [{:card silver :pile-size 40}
                                {:card engineer :pile-size 10}
                                {:card band-of-misfits :pile-size 9}
                                {:card bandit-camp :pile-size 10}]
              :players         [{:play-area [band-of-misfits]
                                 :actions   0}]}))
      (is (= (-> {:cost-reductions [{:reduction 5}]
                  :supply          [{:card fortress :pile-size 10}]
                  :players         [{:hand    [band-of-misfits]
                                     :actions 1}]}
                 (play 0 :band-of-misfits))
             {:cost-reductions [{:reduction 5}]
              :supply          [{:card fortress :pile-size 10}]
              :players         [{:play-area [band-of-misfits]
                                 :actions   0}]}))
      (is (= (-> {:supply  [{:card death-cart :pile-size 10}]
                  :players [{:hand    [band-of-misfits ruined-village]
                             :actions 1
                             :coins   0}]}
                 (play 0 :band-of-misfits)
                 (choose :death-cart)
                 (choose {:area :hand :card-name :ruined-village}))
             {:supply  [{:card death-cart :pile-size 10}]
              :players [{:play-area [band-of-misfits]
                         :actions   0
                         :coins     5}]
              :trash   [ruined-village]}))
      (is (= (-> {:supply  [{:card death-cart :pile-size 10}]
                  :players [{:hand    [band-of-misfits]
                             :actions 1
                             :coins   0}]}
                 (play 0 :band-of-misfits)
                 (choose :death-cart))
             {:supply  [{:card death-cart :pile-size 10}]
              :players [{:play-area [band-of-misfits]
                         :actions   0
                         :coins     0}]}))
      (is (= (-> {:supply  [{:card throne-room :pile-size 10}]
                  :players [{:hand    [band-of-misfits fortress]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :band-of-misfits)
                 (choose :throne-room)
                 (choose :fortress))
             {:supply  [{:card throne-room :pile-size 10}]
              :players [{:hand      [copper copper]
                         :play-area [band-of-misfits fortress]
                         :actions   4}]}))
      (ut/reset-ids!)
      (is (= (-> {:supply  [{:card caravan :pile-size 10}]
                  :players [{:hand    [band-of-misfits]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :band-of-misfits)
                 (choose :caravan))
             {:supply  [{:card caravan :pile-size 10}]
              :players [{:hand          [copper]
                         :play-area     [band-of-misfits]
                         :deck          [copper]
                         :actions       1
                         :triggers      [(get-trigger (assoc caravan :id 1) 2)]
                         :repeated-play [{:source 0 :target 1}]}]}))
      (is (= (-> {:supply  [{:card caravan :pile-size 10}]
                  :players [{:hand    [band-of-misfits]
                             :deck    [silver copper copper copper copper copper]
                             :actions 1
                             :phase   :action}]}
                 (play 0 :band-of-misfits)
                 (choose :caravan)
                 (end-turn 0))
             {:current-player 0
              :supply         [{:card caravan :pile-size 10}]
              :players        [{:hand      [copper copper copper copper copper silver]
                                :play-area [band-of-misfits]
                                :actions   1
                                :coins     0
                                :buys      1
                                :phase     :action}]}))
      (ut/reset-ids!)
      #_(let [caravan (assoc caravan :id 42)]
          (is (= (-> {:supply  [{:card throne-room :pile-size 10}]
                      :players [{:hand    [band-of-misfits caravan]
                                 :deck    [copper copper]
                                 :actions 1}]}
                     (play 0 :band-of-misfits)
                     (choose :throne-room)
                     (choose :caravan))
                 {:supply  [{:card throne-room :pile-size 10}]
                  :players [{:hand          [copper copper]
                             :play-area     [band-of-misfits caravan]
                             :actions       2
                             :triggers      [(get-trigger caravan 2)
                                             (get-trigger caravan 3)]
                             :repeated-play [{:source 0 :target 42}]}]}))
          (is (= (-> {:supply  [{:card throne-room :pile-size 10}]
                      :players [{:hand    [band-of-misfits caravan]
                                 :deck    [silver silver copper copper copper copper copper]
                                 :actions 1
                                 :phase   :action}]}
                     (play 0 :band-of-misfits)
                     (choose :throne-room)
                     (choose :caravan)
                     (end-turn 0))
                 {:supply  [{:card throne-room :pile-size 10}]
                  :players [{:hand      [copper copper copper copper copper silver silver]
                             :play-area [band-of-misfits caravan]
                             :actions   1
                             :coins     0
                             :buys      1
                             :phase     :action}]}))))
    (is (= (-> {:supply  [{:card poor-house :pile-size 10}
                          {:card fortress :pile-size 10}]
                :players [{:hand    [throne-room band-of-misfits]
                           :deck    [silver silver]
                           :actions 1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :band-of-misfits)
               (choose :poor-house)
               (choose :fortress))
           {:supply  [{:card poor-house :pile-size 10}
                      {:card fortress :pile-size 10}]
            :players [{:hand      [silver]
                       :play-area [throne-room band-of-misfits]
                       :deck      [silver]
                       :actions   2
                       :coins     4}]}))))

(deftest bandit-camp-test
  (let [bandit-camp (assoc bandit-camp :id 0)]
    (testing "Bandit Camp"
      (let [spoils (assoc spoils :id 1)]
        (is (= (-> {:extra-cards [{:card spoils :pile-size 15}]
                    :players     [{:hand    [bandit-camp]
                                   :deck    [copper copper]
                                   :actions 1}]}
                   (play 0 :bandit-camp))
               {:extra-cards [{:card spoils :pile-size 14}]
                :players     [{:hand      [copper]
                               :play-area [bandit-camp]
                               :deck      [copper]
                               :discard   [spoils]
                               :actions   2}]}))
        (is (= (-> {:extra-cards [{:card spoils :pile-size 0}]
                    :players     [{:hand    [bandit-camp]
                                   :deck    [copper copper]
                                   :actions 1}]}
                   (play 0 :bandit-camp))
               {:extra-cards [{:card spoils :pile-size 0}]
                :players     [{:hand      [copper]
                               :play-area [bandit-camp]
                               :deck      [copper]
                               :actions   2}]})))
      (testing "setup"
        (is (= (-> {:supply [{:card bandit-camp :pile-size 10}]}
                   setup-game)
               {:extra-cards [{:card spoils :pile-size 15}]
                :supply      [{:card bandit-camp :pile-size 10}]}))))))

(deftest beggar-test
  (let [beggar (assoc beggar :id 0)
        copper (assoc copper :id 1)
        silver (assoc silver :id 2)]
    (testing "Beggar"
      (is (= (-> {:supply  [{:card copper :pile-size 46}]
                  :players [{:hand    [beggar]
                             :actions 1}]}
                 (play 0 :beggar))
             {:supply  [{:card copper :pile-size 43}]
              :players [{:hand      [copper copper copper]
                         :play-area [beggar]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 2}]
                  :players [{:hand    [beggar]
                             :actions 1}]}
                 (play 0 :beggar))
             {:supply  [{:card copper :pile-size 0}]
              :players [{:hand      [copper copper]
                         :play-area [beggar]
                         :actions   0}]}))
      (testing "Reaction"
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [militia]
                               :actions 1
                               :coins   0}
                              {:hand [beggar copper copper copper copper]}]}
                   (play 0 :militia)
                   (choose nil)                             ; do not react with Beggar
                   (choose [:copper :copper]))              ; discard to Militia
               {:supply  [{:card silver :pile-size 40}]
                :players [{:play-area [militia]
                           :actions   0
                           :coins     2}
                          {:hand    [beggar copper copper]
                           :discard [copper copper]}]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [militia]
                               :actions 1
                               :coins   0}
                              {:hand [beggar copper copper copper copper]}]}
                   (play 0 :militia)
                   (choose :beggar)                         ; react with Beggar
                   (choose :copper))                        ; discard to Militia
               {:supply  [{:card silver :pile-size 38}]
                :players [{:play-area [militia]
                           :actions   0
                           :coins     2}
                          {:hand    [copper copper copper]
                           :deck    [silver]
                           :discard [beggar silver copper]}]}))))))

(deftest catacombs-test
  (let [catacombs (assoc catacombs :id 0)]
    (testing "Catacombs"
      (is (= (-> {:players [{:hand    [catacombs]
                             :deck    [copper copper copper copper]
                             :actions 1}]}
                 (play 0 :catacombs)
                 (choose :take))
             {:players [{:hand      [copper copper copper]
                         :play-area [catacombs]
                         :deck      [copper]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [catacombs]
                             :deck    [copper copper copper silver silver silver]
                             :actions 1}]}
                 (play 0 :catacombs)
                 (choose :discard))
             {:players [{:hand      [silver silver silver]
                         :play-area [catacombs]
                         :discard   [copper copper copper]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [catacombs]
                             :deck    [copper copper village-green silver silver silver]
                             :actions 1}]}
                 (play 0 :catacombs)
                 (choose :discard)
                 (choose nil))                              ; do not play Village Green
             {:players [{:hand      [silver silver silver]
                         :play-area [catacombs]
                         :discard   [copper copper village-green]
                         :actions   0}]}))
      (testing "on trash"
        (let [feodum (assoc feodum :id 1)]
          (is (= (-> {:supply  [{:card feodum :pile-size 8}]
                      :players [{:hand    [forager catacombs]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                     (play 0 :forager)
                     (choose :catacombs)
                     (choose :feodum))
                 {:supply  [{:card feodum :pile-size 7}]
                  :players [{:play-area [forager]
                             :discard   [feodum]
                             :actions   1
                             :coins     0
                             :buys      2}]
                  :trash   [catacombs]}))
          (is (= (-> {:supply  [{:card catacombs :pile-size 9}]
                      :players [{:hand    [forager catacombs]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                     (play 0 :forager)
                     (choose :catacombs))
                 {:supply  [{:card catacombs :pile-size 9}]
                  :players [{:play-area [forager]
                             :actions   1
                             :coins     0
                             :buys      2}]
                  :trash   [catacombs]})))))))

(deftest count-test
  (let [count' (assoc count' :id 0)
        copper (assoc copper :id 1)
        duchy  (assoc duchy :id 2)]
    (testing "Count"
      (is (= (-> {:players [{:hand    [count' copper copper silver]
                             :actions 1
                             :coins   0}]}
                 (play 0 :count)
                 (choose :discard)
                 (choose [:copper :copper])
                 (choose :coins))
             {:players [{:hand      [silver]
                         :play-area [count']
                         :discard   [copper copper]
                         :actions   0
                         :coins     3}]}))
      (is (= (-> {:players [{:hand    [count' copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :count)
                 (choose :discard)
                 (choose :copper)
                 (choose :coins))
             {:players [{:play-area [count']
                         :discard   [copper]
                         :actions   0
                         :coins     3}]}))
      (is (= (-> {:players [{:hand    [count']
                             :actions 1
                             :coins   0}]}
                 (play 0 :count)
                 (choose :discard)
                 (choose :coins))
             {:players [{:play-area [count']
                         :actions   0
                         :coins     3}]}))
      (is (= (-> {:players [{:hand    [count']
                             :actions 1
                             :coins   0}]}
                 (play 0 :count)
                 (choose :topdeck)
                 (choose :coins))
             {:players [{:play-area [count']
                         :actions   0
                         :coins     3}]}))
      (is (= (-> {:players [{:hand    [count' copper copper estate silver]
                             :actions 1}]}
                 (play 0 :count)
                 (choose :topdeck)
                 (choose :silver)
                 (choose :trash))
             {:players [{:play-area [count']
                         :deck      [silver]
                         :actions   0}]
              :trash   [copper copper estate]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card duchy :pile-size 8}]
                  :players [{:hand    [count']
                             :actions 1}]}
                 (play 0 :count)
                 (choose :copper)
                 (choose :duchy))
             {:supply  [{:card copper :pile-size 45}
                        {:card duchy :pile-size 7}]
              :players [{:play-area [count']
                         :discard   [copper duchy]
                         :actions   0}]})))))

(deftest counterfeit-test
  (let [counterfeit (assoc counterfeit :id 0)]
    (testing "Counterfeit"
      (is (= (-> {:players [{:hand  [counterfeit]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit))
             {:players [{:play-area [counterfeit]
                         :coins     1
                         :buys      2}]}))
      (is (= (-> {:players [{:hand  [counterfeit copper]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose nil))
             {:players [{:hand      [copper]
                         :play-area [counterfeit]
                         :coins     1
                         :buys      2}]}))
      (is (= (-> {:players [{:hand  [counterfeit copper]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :copper))
             {:players [{:play-area [counterfeit]
                         :coins     3
                         :buys      2}]
              :trash   [copper]}))
      (is (= (-> {:players [{:hand  [counterfeit silver]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :silver))
             {:players [{:play-area [counterfeit]
                         :coins     5
                         :buys      2}]
              :trash   [silver]}))
      (is (= (-> {:extra-cards [{:card spoils :pile-size 14}]
                  :players     [{:hand  [counterfeit spoils]
                                 :coins 0
                                 :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :spoils))
             {:extra-cards [{:card spoils :pile-size 14}]
              :players     [{:play-area [counterfeit]
                             :coins     7
                             :buys      2}]
              :trash       [spoils]}))
      (is (= (-> {:players [{:hand  [counterfeit counterfeit copper copper]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :copper)
                 (play 0 :counterfeit)
                 (choose :copper))
             {:players [{:play-area [counterfeit counterfeit]
                         :coins     6
                         :buys      3}]
              :trash   [copper copper]}))
      (is (= (-> {:players [{:hand  [counterfeit counterfeit copper copper]
                             :coins 0
                             :buys  1}]}
                 (play 0 :counterfeit)
                 (choose :counterfeit)
                 (choose :copper)
                 (choose :copper))
             {:players [{:play-area [counterfeit]
                         :coins     7
                         :buys      4}]
              :trash   [copper copper counterfeit]})))))

(deftest cultist-test
  (let [cultist        (assoc cultist :id 0)
        ruined-market  (assoc ruined-market :id 1)
        ruined-library (assoc ruined-library :id 1)]
    (testing "Cultist"
      (is (= (-> {:supply  [{:split-pile [{:card ruined-market :pile-size 1}
                                          {:card ruined-library :pile-size 1}
                                          {:card {:name :ruins} :pile-size 0}]}]
                  :players [{:hand    [cultist]
                             :deck    [copper copper copper]
                             :actions 1}
                            {}]}
                 (play 0 :cultist))
             {:supply  [{:split-pile [{:card ruined-market :pile-size 0}
                                      {:card ruined-library :pile-size 1}
                                      {:card {:name :ruins} :pile-size 0}]}]
              :players [{:hand      [copper copper]
                         :play-area [cultist]
                         :deck      [copper]
                         :actions   0}
                        {:discard [ruined-market]}]}))
      (is (= (-> {:supply  [{:split-pile [{:card ruined-market :pile-size 1}
                                          {:card ruined-library :pile-size 1}
                                          {:card {:name :ruins} :pile-size 0}]}]
                  :players [{:hand    [cultist]
                             :deck    [copper cultist copper copper]
                             :actions 1}
                            {}]}
                 (play 0 :cultist)
                 (choose :cultist))
             {:supply  [{:split-pile [{:card ruined-market :pile-size 0}
                                      {:card ruined-library :pile-size 0}
                                      {:card {:name :ruins} :pile-size 0}]}]
              :players [{:hand      [copper copper copper]
                         :play-area [cultist cultist]
                         :actions   0}
                        {:discard [ruined-market ruined-library]}]}))
      (is (= (-> {:supply  [{:split-pile [{:card ruined-market :pile-size 1}
                                          {:card ruined-library :pile-size 1}
                                          {:card {:name :ruins} :pile-size 0}]}]
                  :players [{:hand    [cultist]
                             :deck    [copper cultist copper copper]
                             :actions 1}
                            {}]}
                 (play 0 :cultist)
                 (choose nil))
             {:supply  [{:split-pile [{:card ruined-market :pile-size 0}
                                      {:card ruined-library :pile-size 1}
                                      {:card {:name :ruins} :pile-size 0}]}]
              :players [{:hand      [copper cultist]
                         :play-area [cultist]
                         :deck      [copper copper]
                         :actions   0}
                        {:discard [ruined-market]}]}))
      (testing "on-trash"
        (is (= (-> {:players [{:hand    [junk-dealer cultist]
                               :deck    [province copper copper gold copper]
                               :actions 1
                               :coins   0}]}
                   (play 0 :junk-dealer)
                   (choose :cultist))
               {:players [{:hand      [province copper copper gold]
                           :play-area [junk-dealer]
                           :deck      [copper]
                           :actions   1
                           :coins     1}]
                :trash   [cultist]})))
      (testing "setup"
        (let [game (setup-game {:supply  [{:card cultist :pile-size 10}]
                                :players [{}
                                          {}]})]
          (is (ut/get-pile-idx game :supply :ruins #{:include-empty-split-piles})))))))

(deftest death-cart-test
  (testing "Death Cart"
    (let [death-cart    (assoc death-cart :id 0)
          survivors     (assoc survivors :id 1)
          ruined-market (assoc ruined-market :id 2)]
      (is (= (-> {:players [{:hand    [death-cart ruined-market]
                             :actions 1
                             :coins   0}]}
                 (play 0 :death-cart)
                 (choose {:area :hand :card-name :ruined-market}))
             {:players [{:play-area [death-cart]
                         :actions   0
                         :coins     5}]
              :trash   [ruined-market]}))
      (is (= (-> {:players [{:hand    [death-cart copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :death-cart)
                 (choose {:area :play-area :card-name :death-cart}))
             {:players [{:hand    [copper]
                         :actions 0
                         :coins   5}]
              :trash   [death-cart]}))
      (is (= (-> {:players [{:hand    [death-cart scavenger]
                             :actions 1
                             :coins   0}]}
                 (play 0 :death-cart)
                 (choose nil))
             {:players [{:hand      [scavenger]
                         :play-area [death-cart]
                         :actions   0
                         :coins     0}]}))
      (is (= (-> {:players [{:hand    [throne-room death-cart]
                             :actions 1
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :death-cart)
                 (choose {:area :play-area :card-name :death-cart}))
             {:players [{:play-area [throne-room]
                         :actions   0
                         :coins     5}]
              :trash   [death-cart]}))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [death-cart copper]
                                            :actions 1
                                            :coins   0}]}
                                (play 0 :death-cart)
                                (choose {:area :hand :card-name :copper}))))
      (is (= (-> {:supply  [{:split-pile [{:card ruined-market :pile-size 1}
                                          {:card survivors :pile-size 1}
                                          {:card survivors :pile-size 1}
                                          {:card {:name :ruins} :pile-size 0}]}
                            {:card death-cart :pile-size 10}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-card 0 :death-cart))
             {:supply  [{:split-pile [{:card ruined-market :pile-size 0}
                                      {:card survivors :pile-size 0}
                                      {:card survivors :pile-size 1}
                                      {:card {:name :ruins} :pile-size 0}]}
                        {:card death-cart :pile-size 9}]
              :players [{:discard [ruined-market survivors death-cart]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:split-pile [{:card ruined-market :pile-size 0}
                                          {:card survivors :pile-size 0}
                                          {:card survivors :pile-size 1}
                                          {:card {:name :ruins} :pile-size 0}]}
                            {:card death-cart :pile-size 9}]
                  :players [{:coins 4
                             :buys  1}]}
                 (buy-card 0 :death-cart))
             {:supply  [{:split-pile [{:card ruined-market :pile-size 0}
                                      {:card survivors :pile-size 0}
                                      {:card survivors :pile-size 0}
                                      {:card {:name :ruins} :pile-size 0}]}
                        {:card death-cart :pile-size 8}]
              :players [{:discard [survivors death-cart]
                         :coins   0
                         :buys    0}]})))))

(deftest feodum-test
  (testing "Feodum"
    (testing "on trash"
      (let [feodum (assoc feodum :id 0)
            silver (assoc silver :id 0)]
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [forager feodum]
                               :actions 1
                               :coins   0
                               :buys    1}]}
                   (play 0 :forager)
                   (choose :feodum))
               {:supply  [{:card silver :pile-size 37}]
                :players [{:play-area [forager]
                           :discard   [silver silver silver]
                           :actions   1
                           :coins     0
                           :buys      2}]
                :trash   [feodum]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [warrior]
                               :actions 1}
                              {:deck [feodum]}]}
                   (play 0 :warrior))
               {:supply  [{:card silver :pile-size 37}]
                :players [{:play-area [warrior]
                           :actions   0}
                          {:discard [silver silver silver]}]
                :trash   [feodum]}))))
    (testing "victory points"
      (is (= (calc-victory-points {:deck (concat [feodum]
                                                 (repeat 2 silver))})
             0))
      (is (= (calc-victory-points {:deck (concat [feodum]
                                                 (repeat 3 silver))})
             1))
      (is (= (calc-victory-points {:deck (concat [feodum feodum]
                                                 (repeat 5 silver))})
             2))
      (is (= (calc-victory-points {:deck (concat [feodum feodum]
                                                 (repeat 6 silver))})
             4)))
    (testing "score"
      (is (= (calc-score {:deck [feodum]})
             [{:card            feodum
               :vp-per-card     0
               :number-of-cards 1
               :victory-points  0
               :notes           "0 Silvers"}]))
      (is (= (calc-score {:deck [feodum silver]})
             [{:card            feodum
               :vp-per-card     0
               :number-of-cards 1
               :victory-points  0
               :notes           "1 Silver"}]))
      (is (= (calc-score {:deck (concat [feodum]
                                        (repeat 2 silver))})
             [{:card            feodum
               :vp-per-card     0
               :number-of-cards 1
               :victory-points  0
               :notes           "2 Silvers"}]))
      (is (= (calc-score {:deck (concat [feodum]
                                        (repeat 3 silver))})
             [{:card            feodum
               :vp-per-card     1
               :number-of-cards 1
               :victory-points  1
               :notes           "3 Silvers"}]))
      (is (= (calc-score {:deck (concat [feodum feodum]
                                        (repeat 5 silver))})
             [{:card            feodum
               :vp-per-card     1
               :number-of-cards 2
               :victory-points  2
               :notes           "5 Silvers"}]))
      (is (= (calc-score {:deck (concat [feodum feodum]
                                        (repeat 6 silver))})
             [{:card            feodum
               :vp-per-card     2
               :number-of-cards 2
               :victory-points  4
               :notes           "6 Silvers"}])))))

(deftest forager-test
  (let [forager (assoc forager :id 0)]
    (testing "Forager"
      (is (= (-> {:players [{:hand    [forager estate]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :estate))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     0
                         :buys      2}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand    [forager estate]
                             :actions 1
                             :coins   0
                             :buys    1}]
                  :trash   [copper]}
                 (play 0 :forager)
                 (choose :estate))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     1
                         :buys      2}]
              :trash   [copper estate]}))
      (is (= (-> {:players [{:hand    [forager copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :copper))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     1
                         :buys      2}]
              :trash   [copper]}))
      (is (= (-> {:players [{:hand    [forager copper]
                             :actions 1
                             :coins   0
                             :buys    1}]
                  :trash   [copper]}
                 (play 0 :forager)
                 (choose :copper))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     1
                         :buys      2}]
              :trash   [copper copper]}))
      (is (= (-> {:players [{:hand    [forager copper]
                             :actions 1
                             :coins   0
                             :buys    1}]
                  :trash   [silver]}
                 (play 0 :forager)
                 (choose :copper))
             {:players [{:play-area [forager]
                         :actions   1
                         :coins     2
                         :buys      2}]
              :trash   [silver copper]})))))

(deftest fortress-test
  (let [fortress (assoc fortress :id 0)]
    (testing "Fortress"
      (is (= (-> {:players [{:hand    [fortress]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :fortress))
             {:players [{:hand      [copper]
                         :play-area [fortress]
                         :deck      [copper]
                         :actions   2}]}))
      (testing "on trash"
        (is (= (-> {:players [{:hand    [forager fortress]
                               :actions 1
                               :coins   0
                               :buys    1}]}
                   (play 0 :forager)
                   (choose :fortress))
               {:players [{:hand      [fortress]
                           :play-area [forager]
                           :actions   1
                           :coins     0
                           :buys      2}]}))
        (is (= (-> {:players [{:hand    [warrior]
                               :actions 1}
                              {:deck [fortress]}]}
                   (play 0 :warrior))
               {:players [{:play-area [warrior]
                           :actions   0}
                          {:hand [fortress]}]}))
        (is (= (-> {:supply  [{:card fortress :pile-size 10}]
                    :players [{:hand    [lurker]
                               :actions 1}]}
                   (play 0 :lurker)
                   (choose :trash)
                   (choose :fortress))
               {:supply  [{:card fortress :pile-size 9}]
                :players [{:hand      [fortress]
                           :play-area [lurker]
                           :actions   1}]}))))))

(deftest graverobber-test
  (let [graverobber (assoc graverobber :id 0)
        silver      (assoc silver :id 1)
        gold        (assoc gold :id 2)
        province    (assoc province :id 3)]
    (testing "Graverobber"
      (is (= (-> {:players [{:hand    [graverobber]
                             :actions 1}]
                  :trash   [silver]}
                 (play 0 :graverobber)
                 (choose :gain)
                 (choose :silver))
             {:players [{:play-area [graverobber]
                         :deck      [silver]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [graverobber]
                             :actions 1}]
                  :trash   [gold]}
                 (play 0 :graverobber)
                 (choose :gain)
                 (choose :gold))
             {:players [{:play-area [graverobber]
                         :deck      [gold]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [graverobber]
                             :actions 1}]
                  :trash   [estate copper province]}
                 (play 0 :graverobber)
                 (choose :gain))
             {:players [{:play-area [graverobber]
                         :actions   0}]
              :trash   [estate copper province]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [graverobber armory]
                             :actions 1}]}
                 (play 0 :graverobber)
                 (choose :upgrade)
                 (choose :armory)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [graverobber]
                         :discard   [gold]
                         :actions   0}]
              :trash   [armory]}))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:supply  [{:card gold :pile-size 30}
                                           {:card province :pile-size 8}]
                                 :players [{:hand    [graverobber armory]
                                            :actions 1}]}
                                (play 0 :graverobber)
                                (choose :upgrade)
                                (choose :armory)
                                (choose :province))))
      (is (= (-> {:supply  [{:card province :pile-size 8}]
                  :players [{:hand    [graverobber graverobber]
                             :actions 1}]}
                 (play 0 :graverobber)
                 (choose :upgrade)
                 (choose :graverobber)
                 (choose :province))
             {:supply  [{:card province :pile-size 7}]
              :players [{:play-area [graverobber]
                         :discard   [province]
                         :actions   0}]
              :trash   [graverobber]}))
      (is (= (-> {:supply  [{:card province :pile-size 8}]
                  :players [{:hand    [graverobber curse copper estate duchy gold]
                             :actions 1}]}
                 (play 0 :graverobber)
                 (choose :upgrade))
             {:supply  [{:card province :pile-size 8}]
              :players [{:hand      [curse copper estate duchy gold]
                         :play-area [graverobber]
                         :actions   0}]})))))

(deftest hermit-test
  (let [hermit (assoc hermit :id 0)
        madman (assoc madman :id 1)
        silver (assoc silver :id 2)]
    (testing "Hermit"
      (is (= (-> {:supply  [{:card hermit :pile-size 9}]
                  :players [{:hand    [hermit copper hovel]
                             :discard [copper estate copper]
                             :actions 1}]}
                 (play 0 :hermit)
                 (choose {:area :hand :card-name :hovel})
                 (choose :hermit))
             {:supply  [{:card hermit :pile-size 8}]
              :players [{:hand      [copper]
                         :play-area [hermit]
                         :discard   [copper estate copper hermit]
                         :actions   0}]
              :trash   [hovel]}))
      (is (= (-> {:supply  [{:card silver :pile-size 30}]
                  :players [{:hand    [hermit copper hovel]
                             :discard [copper estate copper]
                             :actions 1}]}
                 (play 0 :hermit)
                 (choose {:area :discard :card-name :estate})
                 (choose :silver))
             {:supply  [{:card silver :pile-size 29}]
              :players [{:hand      [copper hovel]
                         :play-area [hermit]
                         :discard   [copper copper silver]
                         :actions   0}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card armory :pile-size 10}]
                  :players [{:hand    [hermit copper hovel]
                             :discard [copper estate copper]
                             :actions 1}]}
                 (play 0 :hermit)
                 (choose nil))                              ; trash nothing
             {:supply  [{:card armory :pile-size 10}]
              :players [{:hand           [copper hovel]
                         :play-area      [hermit]
                         :discard        [copper estate copper]
                         :revealed-cards {:discard 3}
                         :actions        0}]}))
      (is (= (-> {:players [{:hand    [hermit copper]
                             :discard [copper silver]
                             :actions 1}]}
                 (play 0 :hermit))
             {:players [{:hand           [copper]
                         :play-area      [hermit]
                         :discard        [copper silver]
                         :revealed-cards {:discard 2}
                         :actions        0}]}))
      (is (= (-> {:supply  [{:card hermit :pile-size 1}]
                  :players [{:hand    [throne-room hermit hovel estate]
                             :discard [copper estate copper]
                             :actions 1}]}
                 (play 0 :throne-room)
                 (choose :hermit)
                 (choose {:area :hand :card-name :hovel})
                 (choose :hermit)
                 (choose {:area :hand :card-name :estate}))
             {:supply  [{:card hermit :pile-size 0}]
              :players [{:play-area      [throne-room hermit]
                         :discard        [copper estate copper hermit]
                         :revealed-cards {:discard 4}
                         :actions        0}]
              :trash   [hovel estate]}))
      (is (= (-> {:track-gained-cards? true
                  :extra-cards         [{:card madman :pile-size 10}]
                  :supply              [{:card hermit :pile-size 9}]
                  :players             [{:hand    [hermit]
                                         :actions 1
                                         :coins   3
                                         :buys    1
                                         :phase   :action}]}
                 (play 0 :hermit)
                 (choose :hermit)
                 (buy-card 0 :hermit)
                 (clean-up {:player-no 0}))
             {:track-gained-cards? true
              :extra-cards         [{:card madman :pile-size 10}]
              :supply              [{:card hermit :pile-size 7}]
              :players             [{:hand         [hermit hermit hermit]
                                     :gained-cards [{:name  :hermit
                                                     :types #{:action}
                                                     :cost  3}
                                                    {:name   :hermit
                                                     :types  #{:action}
                                                     :cost   3
                                                     :bought true}]

                                     :actions      0
                                     :coins        0
                                     :buys         0
                                     :phase        :out-of-turn}]}))
      (is (= (-> {:track-gained-cards? true
                  :extra-cards         [{:card madman :pile-size 10}]
                  :supply              [{:card hermit :pile-size 9}]
                  :players             [{:hand    [hermit]
                                         :actions 1
                                         :phase   :action}]}
                 (play 0 :hermit)
                 (choose :hermit)
                 (clean-up {:player-no 0}))
             {:track-gained-cards? true
              :extra-cards         [{:card madman :pile-size 9}]
              :supply              [{:card hermit :pile-size 8}]
              :players             [{:hand         [madman hermit]
                                     :gained-cards [{:name  :hermit
                                                     :types #{:action}
                                                     :cost  3}
                                                    {:name  :madman
                                                     :types #{:action}
                                                     :cost  0}]
                                     :actions      0
                                     :coins        0
                                     :buys         0
                                     :phase        :out-of-turn}]
              :trash               [hermit]}))))
  (testing "setup"
    (is (= (-> {:supply [{:card hermit :pile-size 10}]}
               setup-game)
           {:extra-cards [{:card madman :pile-size 10}]
            :supply      [{:card hermit :pile-size 10}]}))))

(deftest madman-test
  (let [madman (assoc madman :id 0)]
    (testing "Madman"
      (is (= (-> {:extra-cards [{:card madman :pile-size 9}]
                  :players     [{:hand    [madman copper copper copper copper]
                                 :deck    [silver silver silver silver silver]
                                 :actions 1}]}
                 (play 0 :madman))
             {:extra-cards [{:card madman :pile-size 10}]
              :players     [{:hand    [copper copper copper copper silver silver silver silver]
                             :deck    [silver]
                             :actions 2}]}))
      (is (= (-> {:extra-cards [{:card madman :pile-size 9}]
                  :players     [{:hand    [madman copper]
                                 :deck    [silver silver]
                                 :actions 1}]}
                 (play 0 :madman))
             {:extra-cards [{:card madman :pile-size 10}]
              :players     [{:hand    [copper silver]
                             :deck    [silver]
                             :actions 2}]}))
      (is (= (-> {:extra-cards [{:card madman :pile-size 9}]
                  :players     [{:hand    [madman]
                                 :deck    [silver silver]
                                 :actions 1}]}
                 (play 0 :madman))
             {:extra-cards [{:card madman :pile-size 10}]
              :players     [{:deck    [silver silver]
                             :actions 2}]}))
      (is (= (-> {:extra-cards [{:card madman :pile-size 9}]
                  :players     [{:hand    [throne-room madman copper copper copper]
                                 :deck    [silver silver silver estate estate estate]
                                 :actions 1}]}
                 (play 0 :throne-room)
                 (choose :madman))
             {:extra-cards [{:card madman :pile-size 10}]
              :players     [{:hand      [copper copper copper silver silver silver]
                             :play-area [throne-room]
                             :deck      [estate estate estate]
                             :actions   4}]})))))

(deftest hunting-grounds-test
  (let [hunting-grounds (assoc hunting-grounds :id 0)
        estate          (assoc estate :id 1)
        duchy           (assoc duchy :id 2)]
    (testing "Hunting Grounds"
      (is (= (-> {:players [{:hand    [hunting-grounds]
                             :deck    [copper copper copper copper copper]
                             :actions 1}]}
                 (play 0 :hunting-grounds))
             {:players [{:hand      [copper copper copper copper]
                         :play-area [hunting-grounds]
                         :deck      [copper]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card duchy :pile-size 8}]
                  :players [{:hand    [forager hunting-grounds]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :hunting-grounds)
                 (choose :duchy))
             {:supply  [{:card estate :pile-size 8}
                        {:card duchy :pile-size 7}]
              :players [{:play-area [forager]
                         :discard   [duchy]
                         :actions   1
                         :coins     0
                         :buys      2}]
              :trash   [hunting-grounds]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card duchy :pile-size 0}]
                  :players [{:hand    [forager hunting-grounds]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :hunting-grounds)
                 (choose :duchy))
             {:supply  [{:card estate :pile-size 8}
                        {:card duchy :pile-size 0}]
              :players [{:play-area [forager]
                         :actions   1
                         :coins     0
                         :buys      2}]
              :trash   [hunting-grounds]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card duchy :pile-size 8}]
                  :players [{:hand    [forager hunting-grounds]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :hunting-grounds)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 5}
                        {:card duchy :pile-size 8}]
              :players [{:play-area [forager]
                         :discard   [estate estate estate]
                         :actions   1
                         :coins     0
                         :buys      2}]
              :trash   [hunting-grounds]}))
      (is (= (-> {:supply  [{:card estate :pile-size 2}
                            {:card duchy :pile-size 8}]
                  :players [{:hand    [forager hunting-grounds]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :hunting-grounds)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 0}
                        {:card duchy :pile-size 8}]
              :players [{:play-area [forager]
                         :discard   [estate estate]
                         :actions   1
                         :coins     0
                         :buys      2}]
              :trash   [hunting-grounds]})))))

(deftest ironmonger-test
  (let [ironmonger (assoc ironmonger :id 0)]
    (testing "ironmonger"
      (is (= (-> {:players [{:hand    [ironmonger]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :ironmonger)
                 (choose :copper))
             {:players [{:hand           [copper]
                         :play-area      [ironmonger]
                         :discard        [copper]
                         :revealed-cards {:discard 1}
                         :actions        1
                         :coins          1}]}))
      (is (= (-> {:players [{:hand    [ironmonger]
                             :deck    [copper gold]
                             :actions 1
                             :coins   0}]}
                 (play 0 :ironmonger)
                 (choose nil))
             {:players [{:hand           [copper]
                         :play-area      [ironmonger]
                         :deck           [gold]
                         :revealed-cards {:deck 1}
                         :actions        1
                         :coins          1}]}))
      (is (= (-> {:players [{:hand    [ironmonger]
                             :deck    [copper necropolis]
                             :actions 1}]}
                 (play 0 :ironmonger)
                 (choose :necropolis))
             {:players [{:hand           [copper]
                         :play-area      [ironmonger]
                         :discard        [necropolis]
                         :revealed-cards {:discard 1}
                         :actions        2}]}))
      (is (= (-> {:players [{:hand    [ironmonger]
                             :deck    [copper ironmonger]
                             :actions 1}]}
                 (play 0 :ironmonger)
                 (choose nil))
             {:players [{:hand           [copper]
                         :play-area      [ironmonger]
                         :deck           [ironmonger]
                         :revealed-cards {:deck 1}
                         :actions        2}]}))
      (is (= (-> {:players [{:hand    [ironmonger]
                             :deck    [copper estate silver]
                             :actions 1}]}
                 (play 0 :ironmonger)
                 (choose :estate))
             {:players [{:hand           [copper silver]
                         :play-area      [ironmonger]
                         :discard        [estate]
                         :revealed-cards {:discard 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [ironmonger]
                             :deck    [copper harem silver]
                             :actions 1
                             :coins   0}]}
                 (play 0 :ironmonger)
                 (choose nil))
             {:players [{:hand      [copper harem]
                         :play-area [ironmonger]
                         :deck      [silver]
                         :actions   1
                         :coins     1}]}))
      (is (= (-> {:players [{:hand    [ironmonger]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :ironmonger))
             {:players [{:hand      [copper]
                         :play-area [ironmonger]
                         :actions   1}]})))))

(deftest junk-dealer-test
  (let [junk-dealer (assoc junk-dealer :id 0)]
    (testing "Junk Dealer"
      (is (= (-> {:players [{:hand    [junk-dealer estate]
                             :deck    [copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :junk-dealer)
                 (choose :estate))
             {:players [{:hand      [copper]
                         :play-area [junk-dealer]
                         :actions   1
                         :coins     1}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand    [junk-dealer]
                             :actions 1
                             :coins   0}]}
                 (play 0 :junk-dealer))
             {:players [{:play-area [junk-dealer]
                         :actions   1
                         :coins     1}]})))))

(deftest knights-test
  (testing "Knights"
    (is (= (-> {:players [{:hand    [dame-molly]
                           :actions 1}
                          {:deck [copper estate copper]}]}
               (play 0 :dame-molly))
           {:players [{:play-area [dame-molly]
                       :actions   2}
                      {:deck           [copper]
                       :discard        [copper estate]
                       :revealed-cards {:discard 2}}]}))
    (is (= (-> {:players [{:hand    [dame-sylvia]
                           :actions 1
                           :coins   0}
                          {:deck [estate province copper]}]}
               (play 0 :dame-sylvia))
           {:players [{:play-area [dame-sylvia]
                       :actions   0
                       :coins     2}
                      {:deck           [copper]
                       :discard        [estate province]
                       :revealed-cards {:discard 2}}]}))
    (is (= (-> {:players [{:hand    [sir-martin]
                           :actions 1
                           :buys    1}
                          {:deck [silver gold copper]}]}
               (play 0 :sir-martin)
               (choose :silver))
           {:players [{:play-area [sir-martin]
                       :actions   0
                       :buys      3}
                      {:deck           [copper]
                       :discard        [gold]
                       :revealed-cards {:discard 1}}]
            :trash   [silver]}))
    (is (= (-> {:players [{:hand    [dame-josephine]
                           :actions 1}
                          {:deck [copper gold copper]}]}
               (play 0 :dame-josephine)
               (choose :gold))
           {:players [{:play-area [dame-josephine]
                       :actions   0}
                      {:deck           [copper]
                       :discard        [copper]
                       :revealed-cards {:discard 1}}]
            :trash   [gold]}))
    (is (= (calc-victory-points {:deck [dame-josephine]})
           2))
    (let [sir-vander (assoc sir-vander :id 0)
          sir-bailey (assoc sir-bailey :id 1)
          gold       (assoc gold :id 2)]
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [sir-vander]
                             :actions 1}
                            {:deck [estate sir-bailey copper]}]}
                 (play 0 :sir-vander)
                 (choose :sir-bailey))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:discard [gold]
                         :actions 0}
                        {:deck           [copper]
                         :discard        [estate]
                         :revealed-cards {:discard 1}}]
              :trash   [sir-vander sir-bailey]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [sir-bailey]
                             :deck    [silver silver]
                             :actions 1}
                            {:deck [sir-vander gold copper]}]}
                 (play 0 :sir-bailey)
                 (choose :sir-vander))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:hand    [silver]
                         :deck    [silver]
                         :actions 1}
                        {:deck           [copper]
                         :discard        [gold gold]
                         :revealed-cards {:discard 1}}]
              :trash   [sir-bailey sir-vander]}))
      (is (= (-> {:players [{:hand    [sir-michael]
                             :actions 1}
                            {:hand [copper copper copper copper copper]
                             :deck [silver silver copper]}]}
                 (play 0 :sir-michael)
                 (choose [:copper :copper])
                 (choose :silver))
             {:players [{:play-area [sir-michael]
                         :actions   0}
                        {:hand           [copper copper copper]
                         :deck           [copper]
                         :discard        [copper copper silver]
                         :revealed-cards {:discard 1}}]
              :trash   [silver]}))
      (is (= (-> {:players [{:hand    [dame-anna estate copper silver gold]
                             :actions 1}]}
                 (play 0 :dame-anna)
                 (choose [:copper :estate]))
             {:players [{:hand      [silver gold]
                         :play-area [dame-anna]
                         :actions   0}]
              :trash   [copper estate]}))
      (is (= (-> {:players [{:hand    [dame-anna estate copper silver gold]
                             :actions 1}]}
                 (play 0 :dame-anna)
                 (choose nil))
             {:players [{:hand      [estate copper silver gold]
                         :play-area [dame-anna]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [sir-destry]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 (play 0 :sir-destry))
             {:players [{:hand      [copper copper]
                         :play-area [sir-destry]
                         :deck      [copper]
                         :actions   0}]}))
      (let [silver (assoc silver :id 1)]
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [dame-natalie]
                               :actions 1}]}
                   (play 0 :dame-natalie)
                   (choose nil))
               {:supply  [{:card silver :pile-size 40}]
                :players [{:play-area [dame-natalie]
                           :actions   0}]}))
        (is (= (-> {:supply  [{:card silver :pile-size 40}]
                    :players [{:hand    [dame-natalie]
                               :actions 1}]}
                   (play 0 :dame-natalie)
                   (choose :silver))
               {:supply  [{:card silver :pile-size 39}]
                :players [{:play-area [dame-natalie]
                           :discard   [silver]
                           :actions   0}]}))
        (is (= (-> {:supply  [{:card armory :pile-size 10}]
                    :players [{:hand    [dame-natalie]
                               :actions 1}]}
                   (play 0 :dame-natalie))
               {:supply  [{:card armory :pile-size 10}]
                :players [{:play-area [dame-natalie]
                           :actions   0}]}))))))

(deftest marauder-test
  (let [marauder      (assoc marauder :id 0)
        spoils        (assoc spoils :id 1)
        ruined-market (assoc ruined-market :id 2)]
    (testing "Marauder"
      (is (= (-> {:extra-cards [{:card spoils :pile-size 15}]
                  :supply      [{:split-pile [{:card ruined-market :pile-size 1}
                                              {:card ruined-library :pile-size 1}
                                              {:card {:name :ruins} :pile-size 0}]}]
                  :players     [{:hand    [marauder]
                                 :actions 1}
                                {}]}
                 (play 0 :marauder))
             {:extra-cards [{:card spoils :pile-size 14}]
              :supply      [{:split-pile [{:card ruined-market :pile-size 0}
                                          {:card ruined-library :pile-size 1}
                                          {:card {:name :ruins} :pile-size 0}]}]
              :players     [{:play-area [marauder]
                             :discard   [spoils]
                             :actions   0}
                            {:discard [ruined-market]}]}))
      (is (= (-> {:extra-cards [{:card spoils :pile-size 1}]
                  :supply      [{:split-pile [{:card ruined-market :pile-size 0}
                                              {:card ruined-library :pile-size 0}
                                              {:card {:name :ruins} :pile-size 0}]}]
                  :players     [{:hand    [marauder]
                                 :actions 1}
                                {}]}
                 (play 0 :marauder))
             {:extra-cards [{:card spoils :pile-size 0}]
              :supply      [{:split-pile [{:card ruined-market :pile-size 0}
                                          {:card ruined-library :pile-size 0}
                                          {:card {:name :ruins} :pile-size 0}]}]
              :players     [{:play-area [marauder]
                             :discard   [spoils]
                             :actions   0}
                            {}]}))
      (testing "setup"
        (let [game (setup-game {:supply  [{:card marauder :pile-size 10}]
                                :players [{}
                                          {}]})]
          (is (ut/get-pile-idx game :supply :ruins #{:include-empty-split-piles})))))))

(deftest market-square-test
  (let [market-square (assoc market-square :id 0)
        gold          (assoc gold :id 1)]
    (testing "Market Square"
      (is (= (-> {:players [{:hand    [market-square]
                             :deck    [copper copper]
                             :actions 1
                             :buys    1}]}
                 (play 0 :market-square))
             {:players [{:hand      [copper]
                         :play-area [market-square]
                         :deck      [copper]
                         :actions   1
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [market-square junk-dealer]
                             :deck    [estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :junk-dealer)
                 (choose :estate)                           ; trash estate
                 (choose :market-square))                   ; discard market-square for gold
             {:supply  [{:card gold :pile-size 29}]
              :players [{:play-area [junk-dealer]
                         :discard   [market-square gold]
                         :actions   1
                         :coins     1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [market-square junk-dealer]
                             :deck    [estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :junk-dealer)
                 (choose :estate)                           ; trash estate
                 (choose nil))                              ; don't discard market-square for gold
             {:supply  [{:card gold :pile-size 30}]
              :players [{:hand      [market-square]
                         :play-area [junk-dealer]
                         :actions   1
                         :coins     1}]
              :trash   [estate]})))))

(deftest mystic-test
  (testing "Mystic"
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [mystic]
                           :deck    [copper silver]
                           :actions 1
                           :coins   0}]}
               (play 0 :mystic)
               (choose {:area :supply :card-name :copper}))
           {:supply  (base/supply 2 8)
            :players [{:hand           [copper]
                       :play-area      [mystic]
                       :deck           [silver]
                       :revealed-cards {:hand 1}
                       :actions        1
                       :coins          2}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [mystic]
                           :deck    [silver copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :mystic)
               (choose {:area :supply :card-name :gold}))
           {:supply  (base/supply 2 8)
            :players [{:play-area      [mystic]
                       :deck           [silver copper]
                       :revealed-cards {:deck 1}
                       :actions        1
                       :coins          2}]}))))

(deftest pillage-test
  (let [pillage (assoc pillage :id 0)]
    (testing "Pillage"
      (let [spoils (assoc spoils :id 1)]
        (is (= (-> {:extra-cards [{:card spoils :pile-size 15}]
                    :players     [{:hand    [pillage]
                                   :actions 1}
                                  {:hand [copper copper copper gold]}]}
                   (play 0 :pillage))
               {:extra-cards [{:card spoils :pile-size 13}]
                :players     [{:discard [spoils spoils]
                               :actions 0}
                              {:hand [copper copper copper gold]}]
                :trash       [pillage]}))
        (is (= (-> {:extra-cards [{:card spoils :pile-size 15}]
                    :players     [{:hand    [pillage]
                                   :actions 1}
                                  {:hand [copper copper copper gold copper]}]}
                   (play 0 :pillage)
                   (choose :gold))
               {:extra-cards [{:card spoils :pile-size 13}]
                :players     [{:discard [spoils spoils]
                               :actions 0}
                              {:hand    [copper copper copper copper]
                               :discard [gold]}]
                :trash       [pillage]})))
      (testing "setup"
        (is (= (-> {:supply [{:card pillage :pile-size 10}]}
                   setup-game)
               {:extra-cards [{:card spoils :pile-size 15}]
                :supply      [{:card pillage :pile-size 10}]}))))))

(deftest poor-house-test
  (let [poor-house (assoc poor-house :id 0)]
    (testing "Poor House"
      (is (= (-> {:players [{:hand    [poor-house estate]
                             :actions 1
                             :coins   0}]}
                 (play 0 :poor-house))
             {:players [{:hand           [estate]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :coins          4}]}))
      (is (= (-> {:players [{:hand    [poor-house estate copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :poor-house))
             {:players [{:hand           [estate copper]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 2}
                         :actions        0
                         :coins          3}]}))
      (is (= (-> {:players [{:hand    [poor-house copper copper copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :poor-house))
             {:players [{:hand           [copper copper copper copper]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 4}
                         :actions        0
                         :coins          0}]}))
      (is (= (-> {:players [{:hand    [poor-house copper copper copper copper copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :poor-house))
             {:players [{:hand           [copper copper copper copper copper]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 5}
                         :actions        0
                         :coins          0}]}))
      (is (= (-> {:players [{:hand    [poor-house copper copper copper copper copper]
                             :actions 1
                             :coins   2}]}
                 (play 0 :poor-house))
             {:players [{:hand           [copper copper copper copper copper]
                         :play-area      [poor-house]
                         :revealed-cards {:hand 5}
                         :actions        0
                         :coins          1}]})))))

(deftest rats-test
  (let [rats (assoc rats :id 0)]
    (testing "Rats"
      (is (= (-> {:supply  [{:card rats :pile-size 19}]
                  :players [{:hand    [rats estate]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :rats)
                 (choose :estate))
             {:supply  [{:card rats :pile-size 18}]
              :players [{:hand      [copper]
                         :play-area [rats]
                         :discard   [rats]
                         :actions   1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card rats :pile-size 18}]
                  :players [{:hand    [rats gold]
                             :deck    [rats]
                             :actions 1}]}
                 (play 0 :rats)
                 (choose :gold))
             {:supply  [{:card rats :pile-size 17}]
              :players [{:hand      [rats]
                         :play-area [rats]
                         :discard   [rats]
                         :actions   1}]
              :trash   [gold]}))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:supply  [{:card rats :pile-size 18}]
                                 :players [{:hand    [rats gold]
                                            :deck    [rats]
                                            :actions 1}]}
                                (play 0 :rats)
                                (choose :rats))))
      (is (= (-> {:supply  [{:card rats :pile-size 17}]
                  :players [{:hand    [rats rats]
                             :deck    [rats]
                             :actions 1}]}
                 (play 0 :rats))
             {:supply  [{:card rats :pile-size 16}]
              :players [{:hand           [rats rats]
                         :play-area      [rats]
                         :discard        [rats]
                         :revealed-cards {:hand 2}
                         :actions        1}]}))
      (testing "on trash"
        (is (= (-> {:players [{:hand    [forager rats]
                               :deck    [copper copper]
                               :actions 1
                               :coins   0
                               :buys    1}]}
                   (play 0 :forager)
                   (choose :rats))
               {:players [{:hand      [copper]
                           :play-area [forager]
                           :deck      [copper]
                           :actions   1
                           :coins     0
                           :buys      2}]
                :trash   [rats]})))
      (testing "setup"
        (is (= (-> {:supply [{:card rats :pile-size 10}]}
                   setup-game)
               {:supply [{:card rats :pile-size 20}]}))))))

(deftest rebuild-test
  (let [rebuild (assoc rebuild :id 0)
        duchy   (assoc duchy :id 1)]
    (testing "Rebuild"
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [rebuild]
                             :deck    [estate]
                             :actions 1}]}
                 (play 0 :rebuild)
                 (choose {:area :supply :card-name :duchy})
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:play-area [rebuild]
                         :discard   [duchy]
                         :actions   1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [rebuild]
                             :deck    [duchy estate]
                             :actions 1}]}
                 (play 0 :rebuild)
                 (choose {:area :supply :card-name :duchy})
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:play-area [rebuild]
                         :discard   [duchy duchy]
                         :actions   1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [rebuild]
                             :deck    [copper estate]
                             :actions 1}]}
                 (play 0 :rebuild)
                 (choose {:area :supply :card-name :duchy})
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:play-area [rebuild]
                         :discard   [copper duchy]
                         :actions   1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [rebuild]
                             :deck    [copper]
                             :discard [estate]
                             :actions 1}]}
                 (play 0 :rebuild)
                 (choose {:area :supply :card-name :duchy})
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:play-area [rebuild]
                         :discard   [copper duchy]
                         :actions   1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [rebuild]
                             :deck    [copper silver]
                             :discard [estate]
                             :actions 1}]}
                 (play 0 :rebuild)
                 (choose {:area :supply :card-name :duchy})
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:play-area [rebuild]
                         :discard   [copper silver duchy]
                         :actions   1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [rebuild]
                             :discard [estate]
                             :actions 1}]}
                 (play 0 :rebuild)
                 (choose {:area :supply :card-name :duchy})
                 (choose :duchy))
             {:supply  [{:card duchy :pile-size 7}]
              :players [{:play-area [rebuild]
                         :discard   [duchy]
                         :actions   1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [rebuild]
                             :actions 1}]}
                 (play 0 :rebuild))
             {:supply  [{:card duchy :pile-size 8}]
              :players [{:play-area [rebuild]
                         :actions   1}]}))
      (is (= (-> {:supply  [{:card duchy :pile-size 8}]
                  :players [{:hand    [rebuild]
                             :deck    [copper]
                             :discard [silver]
                             :actions 1}]}
                 (play 0 :rebuild)
                 (choose {:area :supply :card-name :duchy}))
             {:supply  [{:card duchy :pile-size 8}]
              :players [{:play-area      [rebuild]
                         :discard        [copper silver]
                         :revealed-cards {:discard 2}
                         :actions        1}]})))))

(deftest rogue-test
  (let [rogue  (assoc rogue :id 0)
        silver (assoc silver :id 1)
        gold   (assoc gold :id 2)]
    (testing "Rogue"
      (is (= (-> {:players [{:hand    [rogue]
                             :actions 1
                             :coins   0}]
                  :trash   [silver]}
                 (play 0 :rogue)
                 (choose :silver))
             {:players [{:play-area [rogue]
                         :discard   [silver]
                         :actions   0
                         :coins     2}]}))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [rogue]
                                            :actions 1
                                            :coins   0}]
                                 :trash   [silver province]}
                                (play 0 :rogue)
                                (choose :province))))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [rogue]
                                            :actions 1
                                            :coins   0}]
                                 :trash   [gold estate]}
                                (play 0 :rogue)
                                (choose :estate))))
      (is (= (-> {:players [{:hand    [rogue]
                             :actions 1
                             :coins   0}
                            {:deck [copper estate]}]
                  :trash   [estate province]}
                 (play 0 :rogue))
             {:players [{:play-area [rogue]
                         :actions   0
                         :coins     2}
                        {:discard        [copper estate]
                         :revealed-cards {:discard 2}}]
              :trash   [estate province]}))
      (is (= (-> {:players [{:hand    [rogue]
                             :actions 1
                             :coins   0}
                            {:deck [silver gold]}]
                  :trash   []}
                 (play 0 :rogue)
                 (choose :silver))
             {:players [{:play-area [rogue]
                         :actions   0
                         :coins     2}
                        {:discard        [gold]
                         :revealed-cards {:discard 1}}]
              :trash   [silver]}))
      (is (= (-> {:players [{:hand    [rogue]
                             :actions 1
                             :coins   0}
                            {:deck [estate gold]}]
                  :trash   []}
                 (play 0 :rogue)
                 (choose :gold))
             {:players [{:play-area [rogue]
                         :actions   0
                         :coins     2}
                        {:discard        [estate]
                         :revealed-cards {:discard 1}}]
              :trash   [gold]}))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [rogue]
                                            :actions 1
                                            :coins   0}
                                           {:deck [estate gold]}]
                                 :trash   []}
                                (play 0 :rogue)
                                (choose :estate)))))))

(deftest ruins-test
  (testing "Ruins"
    (is (= (-> {:players [{:hand    [abandoned-mine]
                           :actions 1
                           :coins   0}]}
               (play 0 :abandoned-mine))
           {:players [{:play-area [abandoned-mine]
                       :actions   0
                       :coins     1}]}))
    (is (= (-> {:players [{:hand    [ruined-library]
                           :deck    [copper copper]
                           :actions 1}]}
               (play 0 :ruined-library))
           {:players [{:hand      [copper]
                       :play-area [ruined-library]
                       :deck      [copper]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [ruined-market]
                           :actions 1
                           :buys    1}]}
               (play 0 :ruined-market))
           {:players [{:play-area [ruined-market]
                       :actions   0
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [ruined-village]
                           :actions 1}]}
               (play 0 :ruined-village))
           {:players [{:play-area [ruined-village]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [survivors]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :survivors)
               (choose :discard))
           {:players [{:play-area [survivors]
                       :deck      [copper]
                       :discard   [copper copper]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [survivors]
                           :deck    [copper gold copper]
                           :actions 1}]}
               (play 0 :survivors)
               (choose :topdeck)
               (choose [:copper :gold]))
           {:players [{:play-area [survivors]
                       :deck      [gold copper copper]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [survivors]
                           :deck    [estate]
                           :actions 1}]}
               (play 0 :survivors)
               (choose :discard))
           {:players [{:play-area [survivors]
                       :discard   [estate]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [survivors]
                           :deck    [gold]
                           :actions 1}]}
               (play 0 :survivors)
               (choose :topdeck)
               (choose :gold))
           {:players [{:play-area [survivors]
                       :deck      [gold]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [survivors]
                           :actions 1}]}
               (play 0 :survivors))
           {:players [{:play-area [survivors]
                       :actions   0}]}))))

(deftest sage-test
  (let [sage (assoc sage :id 0)]
    (testing "Sage"
      (is (= (-> {:players [{:hand    [sage]
                             :deck    [sage copper]
                             :actions 1}]}
                 (play 0 :sage))
             {:players [{:hand           [sage]
                         :play-area      [sage]
                         :deck           [copper]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [sage]
                             :deck    [estate fortune copper]
                             :actions 1}]}
                 (play 0 :sage))
             {:players [{:hand           [fortune]
                         :play-area      [sage]
                         :deck           [copper]
                         :discard        [estate]
                         :revealed-cards {:hand    1
                                          :discard 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [sage]
                             :deck    [estate engineer]
                             :discard [sage]
                             :actions 1}]}
                 (play 0 :sage))
             {:players [{:hand           [sage]
                         :play-area      [sage]
                         :discard        [estate engineer]
                         :revealed-cards {:hand    1
                                          :discard 2}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [sage]
                             :deck    [estate estate]
                             :discard [copper copper]
                             :actions 1}]}
                 (play 0 :sage))
             {:players [{:play-area      [sage]
                         :discard        [estate estate copper copper]
                         :revealed-cards {:discard 4}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [sage]
                             :actions 1}]}
                 (play 0 :sage))
             {:players [{:play-area [sage]
                         :actions   1}]})))))

(deftest scavenger-test
  (let [scavenger (assoc scavenger :id 0)]
    (testing "Scavenger"
      (is (= (-> {:players [{:hand    [scavenger]
                             :deck    [copper copper silver gold]
                             :discard [estate silver]
                             :actions 1
                             :coins   0}]}
                 (play 0 :scavenger)
                 (choose :yes)
                 (choose :gold))
             {:players [{:play-area [scavenger]
                         :deck      [gold]
                         :discard   [estate silver copper copper silver]
                         :actions   0
                         :coins     2}]}))
      (is (= (-> {:players [{:hand    [scavenger]
                             :deck    [copper copper silver gold]
                             :discard [estate silver]
                             :actions 1
                             :coins   0}]}
                 (play 0 :scavenger)
                 (choose :no)
                 (choose :silver))
             {:players [{:play-area [scavenger]
                         :deck      [silver copper copper silver gold]
                         :discard   [estate]
                         :actions   0
                         :coins     2}]}))
      (is (= (-> {:players [{:hand    [scavenger]
                             :discard [estate silver]
                             :actions 1
                             :coins   0}]}
                 (play 0 :scavenger)
                 (choose :silver))
             {:players [{:play-area [scavenger]
                         :deck      [silver]
                         :discard   [estate]
                         :actions   0
                         :coins     2}]}))
      (is (= (-> {:players [{:hand    [scavenger]
                             :deck    [copper copper silver gold]
                             :actions 1
                             :coins   0}]}
                 (play 0 :scavenger)
                 (choose :no))
             {:players [{:play-area [scavenger]
                         :deck      [copper copper silver gold]
                         :actions   0
                         :coins     2}]}))
      (is (= (-> {:players [{:hand    [scavenger]
                             :deck    [village-green]
                             :discard [estate silver]
                             :actions 1
                             :coins   0}]}
                 (play 0 :scavenger)
                 (choose :yes)
                 (choose :silver))
             {:players [{:play-area [scavenger]
                         :deck      [silver]
                         :discard   [estate village-green]
                         :actions   0
                         :coins     2}]})))))

(deftest shelters-test
  (testing "Hovel"
    (let [hovel  (assoc hovel :id 0)
          estate (assoc estate :id 1)
          silver (assoc silver :id 2)]
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand  [hovel]
                             :coins 2
                             :buys  1}]}
                 (buy-card 0 :estate)
                 (choose :hovel))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:discard [estate]
                         :coins   0
                         :buys    0}]
              :trash   [hovel]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand  [hovel]
                             :coins 2
                             :buys  1}]}
                 (buy-card 0 :estate)
                 (choose nil))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:hand    [hovel]
                         :discard [estate]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand  [hovel]
                             :coins 3
                             :buys  1}]}
                 (buy-card 0 :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:hand    [hovel]
                         :discard [silver]
                         :coins   0
                         :buys    0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [hovel armory]
                             :actions 1}]}
                 (play 0 :armory)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:hand      [hovel]
                         :play-area [armory]
                         :deck      [estate]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand  [hovel hovel]
                             :coins 2
                             :buys  1}]}
                 (buy-card 0 :estate)
                 (choose :hovel)
                 (choose :hovel))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:discard [estate]
                         :coins   0
                         :buys    0}]
              :trash   [hovel hovel]}))))
  (testing "Necropolis"
    (let [necropolis (assoc necropolis :id 0)]
      (is (= (-> {:players [{:hand    [necropolis]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :necropolis))
             {:players [{:play-area [necropolis]
                         :deck      [copper]
                         :actions   2}]}))))
  (testing "Overgrown Estate"
    (let [overgrown-estate (assoc overgrown-estate :id 0)]
      (is (= (-> {:players [{:hand    [forager overgrown-estate]
                             :deck    [copper copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :forager)
                 (choose :overgrown-estate))
             {:players [{:hand      [copper]
                         :play-area [forager]
                         :deck      [copper]
                         :actions   1
                         :coins     0
                         :buys      2}]
              :trash   [overgrown-estate]}))
      (is (= (calc-victory-points {:deck [overgrown-estate]})
             0)))))

(deftest spoils-test
  (let [spoils (assoc spoils :id 0)]
    (testing "Spoils"
      (is (= (-> {:extra-cards [{:card spoils :pile-size 14}]
                  :players     [{:hand  [spoils]
                                 :coins 0}]}
                 (play 0 :spoils))
             {:extra-cards [{:card spoils :pile-size 15}]
              :players     [{:coins 3}]})))))

(deftest squire-test
  (let [squire (assoc squire :id 0)
        silver (assoc silver :id 1)]
    (testing "Squire"
      (is (= (-> {:players [{:hand    [squire]
                             :actions 1
                             :coins   0}]}
                 (play 0 :squire)
                 (choose :actions))
             {:players [{:play-area [squire]
                         :actions   2
                         :coins     1}]}))
      (is (= (-> {:players [{:hand    [squire]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :squire)
                 (choose :buys))
             {:players [{:play-area [squire]
                         :actions   0
                         :coins     1
                         :buys      3}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [squire]
                             :actions 1
                             :coins   0}]}
                 (play 0 :squire)
                 (choose :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [squire]
                         :discard   [silver]
                         :actions   0
                         :coins     1}]}))
      (testing "on trash"
        (is (= (-> {:supply  [{:card squire :pile-size 9}]
                    :players [{:hand    [forager squire]
                               :actions 1
                               :coins   0
                               :buys    1}]}
                   (play 0 :forager)
                   (choose :squire))
               {:supply  [{:card squire :pile-size 9}]
                :players [{:play-area [forager]
                           :actions   1
                           :coins     0
                           :buys      2}]
                :trash   [squire]}))
        (let [militia (assoc militia :id 1)]
          (is (= (-> {:supply  [{:card militia :pile-size 10}]
                      :players [{:hand    [forager squire]
                                 :actions 1
                                 :coins   0
                                 :buys    1}]}
                     (play 0 :forager)
                     (choose :squire)
                     (choose :militia))
                 {:supply  [{:card militia :pile-size 9}]
                  :players [{:play-area [forager]
                             :discard   [militia]
                             :actions   1
                             :coins     0
                             :buys      2}]
                  :trash   [squire]})))))))

(deftest storeroom-test
  (let [storeroom (assoc storeroom :id 0)]
    (testing "Storeroom"
      (is (= (-> {:players [{:hand    [storeroom estate estate copper]
                             :deck    [silver copper gold copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :storeroom)
                 (choose [:estate :estate :copper])
                 (choose nil))
             {:players [{:hand      [silver copper gold]
                         :play-area [storeroom]
                         :deck      [copper]
                         :discard   [estate estate copper]
                         :actions   0
                         :coins     0
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [storeroom estate estate copper]
                             :deck    [estate estate gold copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :storeroom)
                 (choose [:estate :estate :copper])
                 (choose [:estate :estate]))
             {:players [{:hand      [gold]
                         :play-area [storeroom]
                         :deck      [copper]
                         :discard   [estate estate copper estate estate]
                         :actions   0
                         :coins     2
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [storeroom gold]
                             :deck    [copper]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :storeroom)
                 (choose nil)
                 (choose nil))
             {:players [{:hand      [gold]
                         :play-area [storeroom]
                         :deck      [copper]
                         :actions   0
                         :coins     0
                         :buys      2}]}))
      (is (= (-> {:players [{:hand    [storeroom]
                             :deck    [gold]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :storeroom))
             {:players [{:play-area [storeroom]
                         :deck      [gold]
                         :actions   0
                         :coins     0
                         :buys      2}]})))))

(deftest urchin-test
  (let [urchin    (assoc urchin :id 0)
        mercenary (assoc mercenary :id 1)
        urchin-2  (assoc urchin :id 2)]
    (testing "Urchin"
      (is (= (-> {:players [{:hand    [urchin]
                             :deck    [copper copper]
                             :actions 1}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :urchin)
                 (choose [:copper]))
             {:players [{:hand      [copper]
                         :play-area [urchin]
                         :deck      [copper]
                         :actions   1}
                        {:hand    [copper copper copper copper]
                         :discard [copper]}]}))
      (is (= (-> {:extra-cards [{:card mercenary :pile-size 10}]
                  :players     [{:hand    [urchin]
                                 :deck    [urchin-2 copper]
                                 :actions 1}
                                {:hand [copper copper copper copper copper]}]}
                 (play 0 :urchin)
                 (choose [:copper])
                 (play 0 :urchin)
                 (choose nil))
             {:extra-cards [{:card mercenary :pile-size 10}]
              :players     [{:hand      [copper]
                             :play-area [urchin urchin-2]
                             :actions   1}
                            {:hand    [copper copper copper copper]
                             :discard [copper]}]}))
      (is (= (-> {:extra-cards [{:card mercenary :pile-size 10}]
                  :players     [{:hand    [urchin]
                                 :deck    [urchin-2 copper]
                                 :actions 1}]}
                 (play 0 :urchin)
                 (play 0 :urchin)
                 (choose :urchin))
             {:extra-cards [{:card mercenary :pile-size 9}]
              :players     [{:hand      [copper]
                             :play-area [urchin-2]
                             :discard   [mercenary]
                             :actions   1}]
              :trash       [urchin]}))
      (is (= (-> {:extra-cards [{:card mercenary :pile-size 10}]
                  :players     [{:hand    [throne-room urchin]
                                 :deck    [copper copper]
                                 :actions 1}]}
                 (play 0 :throne-room)
                 (choose :urchin))
             {:extra-cards [{:card mercenary :pile-size 10}]
              :players     [{:hand      [copper copper]
                             :play-area [throne-room urchin]
                             :actions   2}]}))))
  (testing "setup"
    (is (= (-> {:supply [{:card urchin :pile-size 10}]}
               setup-game)
           {:extra-cards [{:card mercenary :pile-size 10}]
            :supply      [{:card urchin :pile-size 10}]}))))

(deftest mercenary-test
  (let [mercenary (assoc mercenary :id 0)]
    (testing "mercenary"
      (is (= (-> {:players [{:hand    [mercenary estate estate]
                             :deck    [silver silver copper]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :mercenary)
                 (choose [:estate :estate])
                 (choose [:copper :copper]))
             {:players [{:hand      [silver silver]
                         :play-area [mercenary]
                         :deck      [copper]
                         :actions   0
                         :coins     2}
                        {:hand    [copper copper copper]
                         :discard [copper copper]}]
              :trash   [estate estate]}))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [mercenary estate gold]
                                            :deck    [silver silver copper]
                                            :actions 1
                                            :coins   0}
                                           {:hand [copper copper copper copper copper]}]}
                                (play 0 :mercenary)
                                (choose :estate))))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [mercenary estate estate copper]
                                            :deck    [silver silver copper]
                                            :actions 1
                                            :coins   0}
                                           {:hand [copper copper copper copper copper]}]}
                                (play 0 :mercenary)
                                (choose [:estate :estate :copper]))))
      (is (= (-> {:players [{:hand    [mercenary estate]
                             :deck    [silver silver copper]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :mercenary)
                 (choose :estate))
             {:players [{:play-area [mercenary]
                         :deck      [silver silver copper]
                         :actions   0
                         :coins     0}
                        {:hand [copper copper copper copper copper]}]
              :trash   [estate]}))
      (is (= (-> {:players [{:hand    [mercenary gold]
                             :deck    [silver silver copper]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :mercenary)
                 (choose nil))
             {:players [{:hand      [gold]
                         :play-area [mercenary]
                         :deck      [silver silver copper]
                         :actions   0
                         :coins     0}
                        {:hand [copper copper copper copper copper]}]}))
      (is (= (-> {:players [{:hand    [mercenary]
                             :deck    [silver silver copper]
                             :actions 1
                             :coins   0}
                            {:hand [copper copper copper copper copper]}]}
                 (play 0 :mercenary))
             {:players [{:play-area [mercenary]
                         :deck      [silver silver copper]
                         :actions   0
                         :coins     0}
                        {:hand [copper copper copper copper copper]}]})))))

(deftest vagrant-test
  (let [vagrant (assoc vagrant :id 0)]
    (testing "Vagrant"
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper]
                         :play-area      [vagrant]
                         :deck           [copper]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper vagrant]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper]
                         :play-area      [vagrant]
                         :deck           [vagrant]
                         :revealed-cards {:deck 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper curse]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper curse]
                         :play-area      [vagrant]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper estate]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper estate]
                         :play-area      [vagrant]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper harem]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper harem]
                         :play-area      [vagrant]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper ruined-market]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper ruined-market]
                         :play-area      [vagrant]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [vagrant]
                             :deck    [copper necropolis]
                             :actions 1}]}
                 (play 0 :vagrant))
             {:players [{:hand           [copper necropolis]
                         :play-area      [vagrant]
                         :revealed-cards {:hand 1}
                         :actions        1}]})))))

(deftest wandering-minstrel-test
  (let [wandering-minstrel (assoc wandering-minstrel :id 0)]
    (testing "Wandering Minstrel"
      (is (= (-> {:players [{:hand    [wandering-minstrel]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :wandering-minstrel))
             {:players [{:hand      [copper]
                         :play-area [wandering-minstrel]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [wandering-minstrel]
                             :deck    [copper copper copper copper copper]
                             :actions 1}]}
                 (play 0 :wandering-minstrel))
             {:players [{:hand           [copper]
                         :play-area      [wandering-minstrel]
                         :deck           [copper]
                         :discard        [copper copper copper]
                         :revealed-cards {:discard 3}
                         :actions        2}]}))
      (is (= (-> {:players [{:hand    [wandering-minstrel]
                             :deck    [copper wandering-minstrel copper copper copper]
                             :actions 1}]}
                 (play 0 :wandering-minstrel)
                 (choose :wandering-minstrel))
             {:players [{:hand           [copper]
                         :play-area      [wandering-minstrel]
                         :deck           [wandering-minstrel copper]
                         :discard        [copper copper]
                         :revealed-cards {:deck    1
                                          :discard 2}
                         :actions        2}]}))
      (is (= (-> {:players [{:hand    [wandering-minstrel]
                             :deck    [copper wandering-minstrel gold vagrant copper]
                             :actions 1}]}
                 (play 0 :wandering-minstrel)
                 (choose [:wandering-minstrel :vagrant]))
             {:players [{:hand           [copper]
                         :play-area      [wandering-minstrel]
                         :deck           [vagrant wandering-minstrel copper]
                         :discard        [gold]
                         :revealed-cards {:deck    2
                                          :discard 1}
                         :actions        2}]}))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [wandering-minstrel]
                                            :deck    [copper wandering-minstrel gold vagrant copper]
                                            :actions 1}]}
                                (play 0 :wandering-minstrel)
                                (choose [:wandering-minstrel]))))
      (is (thrown-with-msg? AssertionError #"Choose error"
                            (-> {:players [{:hand    [wandering-minstrel]
                                            :deck    [copper wandering-minstrel gold vagrant copper]
                                            :actions 1}]}
                                (play 0 :wandering-minstrel)
                                (choose [:wandering-minstrel :vagrant :gold])))))))
