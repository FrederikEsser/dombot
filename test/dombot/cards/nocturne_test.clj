(ns dombot.cards.nocturne-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.common :refer :all]
            [dombot.cards.nocturne :as nocturne :refer :all]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

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

(deftest den-of-sin-test
  (let [den-of-sin (assoc den-of-sin :id 0)]
    (testing "Den of Sin"
      (is (= (-> {:players [{:hand [den-of-sin]
                             :deck [copper copper copper copper copper copper silver silver]}]}
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

(deftest ghost-town-test
  (let [ghost-town (assoc ghost-town :id 0)]
    (testing "Ghost Town"
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :action}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :pay}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :buy}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (is (= (-> {:players [{:hand  [ghost-town]
                             :phase :night}]}
                 (play 0 :ghost-town))
             {:players [{:play-area [ghost-town]
                         :phase     :night
                         :triggers  [(get-trigger ghost-town)]}]}))
      (is (= (-> {:players [{:hand [ghost-town]
                             :deck [copper copper copper copper copper copper silver]}]}
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
                              :source    :multi
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
                              :source    :multi
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