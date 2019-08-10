(ns dombot.cards.intrigue-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.dominion :refer [moat throne-room]]
            [dombot.cards.common :refer :all]
            [dombot.cards.intrigue :as intrigue :refer :all])
  (:refer-clojure :exclude [replace]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest baron-test
  (let [estate (assoc estate :id 1)]
    (testing "Baron"
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [baron]
                             :actions 1
                             :buys    1}]}
                 (play 0 :baron))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:play-area [baron]
                         :discard   [estate]
                         :actions   0
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [baron estate]
                             :actions 1
                             :buys    1}]}
                 (play 0 :baron))
             {:supply       [{:card estate :pile-size 8}]
              :players      [{:hand      [estate]
                              :play-area [baron]
                              :actions   0
                              :buys      2}]
              :effect-stack [{:text      "You may discard an Estate for +$4."
                              :player-no 0
                              :choice    ::intrigue/baron-choice
                              :source    :hand
                              :options   [:estate]
                              :max       1}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [baron estate]
                             :actions 1
                             :coins   0
                             :buys    1}]}
                 (play 0 :baron)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 8}]
              :players [{:play-area [baron]
                         :discard   [estate]
                         :actions   0
                         :coins     4
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [baron estate]
                             :actions 1
                             :buys    1}]}
                 (play 0 :baron)
                 (choose nil))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:hand      [estate]
                         :play-area [baron]
                         :discard   [estate]
                         :actions   0
                         :buys      2}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 0}]
                  :players [{:hand    [baron]
                             :actions 1
                             :buys    1}]}
                 (play 0 :baron))
             {:supply  [{:card estate :pile-size 0}]
              :players [{:play-area [baron]
                         :actions   0
                         :buys      2}]})))))

(deftest bridge-test
  (testing "Bridge"
    (is (= (-> {:players [{:hand    [bridge]
                           :actions 1
                           :buys    1
                           :coins   0}]}
               (play 0 :bridge))
           {:cost-reductions [{:reduction 1}]
            :players         [{:play-area [bridge]
                               :actions   0
                               :buys      2
                               :coins     1}]}))
    (is (= (-> {:cost-reductions [{:reduction 1}]
                :players         [{:hand    [bridge]
                                   :actions 1
                                   :buys    1
                                   :coins   0}]}
               (play 0 :bridge))
           {:cost-reductions [{:reduction 1} {:reduction 1}]
            :players         [{:play-area [bridge]
                               :actions   0
                               :buys      2
                               :coins     1}]}))
    (is (= (-> {:players [{:hand    [throne-room bridge]
                           :actions 1
                           :buys    1
                           :coins   0}]}
               (play 0 :throne-room)
               (choose :bridge))
           {:cost-reductions [{:reduction 1} {:reduction 1}]
            :players         [{:play-area [throne-room bridge]
                               :actions   0
                               :buys      3
                               :coins     2}]}))))

(deftest conspirator-test
  (let [conspirator (assoc conspirator :id 0)
        throne-room (assoc throne-room :id 1)]
    (testing "Conspirator"
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [conspirator]
                                           :actions 1
                                           :coins   0}]}
                 (play 0 :conspirator))
             {:track-played-actions? true
              :players               [{:play-area      [conspirator]
                                       :actions        0
                                       :actions-played [0]
                                       :coins          2}]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand           [conspirator]
                                           :actions        1
                                           :actions-played [0]
                                           :coins          0}]}
                 (play 0 :conspirator))
             {:track-played-actions? true
              :players               [{:play-area      [conspirator]
                                       :actions        0
                                       :actions-played [0 0]
                                       :coins          2}]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand           [conspirator]
                                           :deck           [copper copper]
                                           :actions        1
                                           :actions-played [0 0]
                                           :coins          0}]}
                 (play 0 :conspirator))
             {:track-played-actions? true
              :players               [{:hand           [copper]
                                       :play-area      [conspirator]
                                       :deck           [copper]
                                       :actions        1
                                       :actions-played [0 0 0]
                                       :coins          2}]}))
      (is (= (-> {:track-played-actions? true
                  :players               [{:hand    [throne-room conspirator]
                                           :deck    [copper copper]
                                           :actions 1
                                           :coins   0}]}
                 (play 0 :throne-room)
                 (choose :conspirator))
             {:track-played-actions? true
              :players               [{:hand           [copper]
                                       :play-area      [throne-room conspirator]
                                       :deck           [copper]
                                       :actions        1
                                       :actions-played [1 0 0]
                                       :coins          4}]})))))

(deftest courtier-test
  (let [gold (assoc gold :id 1)]
    (testing "Courtier"
      (is (= (-> {:players [{:hand    [courtier copper nobles]
                             :actions 1}]}
                 (play 0 :courtier))
             {:players      [{:hand      [copper nobles]
                              :play-area [courtier]
                              :actions   0}]
              :effect-stack [{:text      "Reveal a card from your hand."
                              :player-no 0
                              :choice    ::intrigue/courtier-reveal
                              :source    :hand
                              :options   [:copper :nobles]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [courtier copper nobles]
                             :actions 1}]}
                 (play 0 :courtier)
                 (choose :copper))
             {:players      [{:hand           [nobles copper]
                              :play-area      [courtier]
                              :revealed-cards {:hand 1}
                              :actions        0}]
              :effect-stack [{:text      "Choose one:"
                              :player-no 0
                              :choice    ::intrigue/courtier-choices
                              :source    :special
                              :options   [{:option :action :text "+1 Action"}
                                          {:option :buy :text "+1 Buy"}
                                          {:option :coins :text "+$3"}
                                          {:option :gold :text "Gain a Gold."}]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [courtier copper nobles]
                             :actions 1}]}
                 (play 0 :courtier)
                 (choose :copper)
                 (choose [:action]))
             {:players [{:hand           [nobles copper]
                         :play-area      [courtier]
                         :revealed-cards {:hand 1}
                         :actions        1}]}))
      (is (= (-> {:players [{:hand    [courtier copper nobles]
                             :actions 1
                             :buys    1}]}
                 (play 0 :courtier)
                 (choose :copper)
                 (choose :buy))
             {:players [{:hand           [nobles copper]
                         :play-area      [courtier]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :buys           2}]}))
      (is (= (-> {:players [{:hand    [courtier copper nobles]
                             :actions 1
                             :coins   0}]}
                 (play 0 :courtier)
                 (choose :copper)
                 (choose :coins))
             {:players [{:hand           [nobles copper]
                         :play-area      [courtier]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :coins          3}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [courtier copper nobles]
                             :actions 1}]}
                 (play 0 :courtier)
                 (choose :copper)
                 (choose :gold))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:hand           [nobles copper]
                         :play-area      [courtier]
                         :discard        [gold]
                         :revealed-cards {:hand 1}
                         :actions        0}]}))
      (is (= (-> {:players [{:hand    [courtier copper nobles]
                             :actions 1}]}
                 (play 0 :courtier)
                 (choose :nobles))
             {:players      [{:hand           [copper nobles]
                              :play-area      [courtier]
                              :revealed-cards {:hand 1}
                              :actions        0}]
              :effect-stack [{:text      "Choose two:"
                              :player-no 0
                              :choice    ::intrigue/courtier-choices
                              :source    :special
                              :options   [{:option :action :text "+1 Action"}
                                          {:option :buy :text "+1 Buy"}
                                          {:option :coins :text "+$3"}
                                          {:option :gold :text "Gain a Gold."}]
                              :min       2
                              :max       2}]}))
      (is (= (-> {:supply  [{:card gold :pile-size 30}]
                  :players [{:hand    [courtier copper nobles]
                             :actions 1
                             :coins   0}]}
                 (play 0 :courtier)
                 (choose :nobles)
                 (choose [:coins :gold]))
             {:supply  [{:card gold :pile-size 29}]
              :players [{:hand           [copper nobles]
                         :play-area      [courtier]
                         :discard        [gold]
                         :revealed-cards {:hand 1}
                         :actions        0
                         :coins          3}]}))
      (is (= (-> {:players [{:hand    [courtier]
                             :actions 1}]}
                 (play 0 :courtier))
             {:players [{:play-area [courtier]
                         :actions   0}]})))))

(deftest courtyard-test
  (testing "Courtyard"
    (is (= (play {:players [{:hand    [courtyard]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 0 :courtyard)
           {:players      [{:hand      [copper copper copper]
                            :play-area [courtyard]
                            :actions   0}]
            :effect-stack [{:text      "Put a card from your hand onto your deck."
                            :player-no 0
                            :choice    :topdeck-from-hand
                            :source    :hand
                            :options   [:copper :copper :copper]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [courtyard]
                           :deck    [copper copper copper]
                           :actions 1}]}
               (play 0 :courtyard)
               (choose :copper))
           {:players [{:hand      [copper copper]
                       :play-area [courtyard]
                       :deck      [copper]
                       :actions   0}]}))))

(deftest diplomat-test
  (testing "Diplomat"
    (testing "Action"
      (is (= (-> {:players [{:hand    [diplomat copper copper copper copper]
                             :deck    [estate estate estate]
                             :actions 1}]}
                 (play 0 :diplomat))
             {:players [{:hand      [copper copper copper copper estate estate]
                         :play-area [diplomat]
                         :deck      [estate]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [diplomat copper copper copper]
                             :deck    [estate estate estate]
                             :actions 1}]}
                 (play 0 :diplomat))
             {:players [{:hand      [copper copper copper estate estate]
                         :play-area [diplomat]
                         :deck      [estate]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [diplomat copper copper copper copper]
                             :deck    [estate]
                             :actions 1}]}
                 (play 0 :diplomat))
             {:players [{:hand      [copper copper copper copper estate]
                         :play-area [diplomat]
                         :actions   2}]})))
    (testing "Reaction"
      (is (= (-> {:players [{:hand    [minion]
                             :actions 1}
                            {:hand [diplomat estate estate estate silver]
                             :deck [copper copper]}]}
                 (play 0 :minion))
             {:players      [{:play-area [minion]
                              :actions   0}
                             {:hand [diplomat estate estate estate silver]
                              :deck [copper copper]}]
              :effect-stack [{:text      "You may reveal a Reaction to react to the Attack."
                              :player-no 1
                              :choice    :reveal-reaction
                              :source    :hand
                              :options   [:diplomat]
                              :max       1}
                             {:player-no 0
                              :effect    [:give-actions 1]}
                             {:player-no 0
                              :effect    [:give-choice {:text    "Choose one:"
                                                        :choice  ::intrigue/minion-choice
                                                        :options [:special
                                                                  {:option :coins :text "+$2"}
                                                                  {:option :discard :text "Discard your hand, +4 Cards."}]
                                                        :min     1
                                                        :max     1}]}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand    [minion]
                             :actions 1}
                            {:hand [diplomat estate estate estate silver]
                             :deck [copper copper]}]}
                 (play 0 :minion)
                 (choose :diplomat))
             {:players      [{:play-area [minion]
                              :actions   0}
                             {:hand [diplomat estate estate estate silver copper copper]}]
              :effect-stack [{:text      "Discard 3 cards."
                              :player-no 1
                              :choice    :discard-from-hand
                              :source    :hand
                              :options   [:diplomat :estate :estate :estate :silver :copper :copper]
                              :min       3
                              :max       3}
                             {:effect    [:give-choice
                                          {:choice  :reveal-reaction
                                           :max     1
                                           :options [:player
                                                     :hand
                                                     {:reacts-to :attack
                                                      :type      :reaction}]
                                           :text    "You may reveal a Reaction to react to the Attack."}]
                              :player-no 1}
                             {:player-no 0
                              :effect    [:give-actions 1]}
                             {:player-no 0
                              :effect    [:give-choice {:text    "Choose one:"
                                                        :choice  ::intrigue/minion-choice
                                                        :options [:special
                                                                  {:option :coins :text "+$2"}
                                                                  {:option :discard :text "Discard your hand, +4 Cards."}]
                                                        :min     1
                                                        :max     1}]}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand    [minion]
                             :deck    (repeat 5 copper)
                             :actions 1}
                            {:hand [diplomat estate estate estate silver]
                             :deck [copper copper]}]}
                 (play 0 :minion)
                 (choose :diplomat)
                 (choose [:estate :estate :estate])
                 (choose :discard))
             {:players [{:hand      (repeat 4 copper)
                         :play-area [minion]
                         :deck      [copper]
                         :actions   1}
                        {:hand    [diplomat silver copper copper]
                         :discard [estate estate estate]}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [minion]
                             :actions 1
                             :coins   0}
                            {:hand [diplomat silver copper copper]
                             :deck [estate]}]}
                 (play 0 :minion)
                 (choose :coins))
             {:supply  (base/supply 2 8)
              :players [{:play-area [minion]
                         :actions   1
                         :coins     2}
                        {:hand [diplomat silver copper copper]
                         :deck [estate]}]}))
      (is (= (-> {:players [{:hand    [minion]
                             :actions 1}
                            {:hand [diplomat moat estate estate silver]
                             :deck [copper copper]}]}
                 (play 0 :minion))
             {:players      [{:play-area [minion]
                              :actions   0}
                             {:hand [diplomat moat estate estate silver]
                              :deck [copper copper]}]
              :effect-stack [{:text      "You may reveal a Reaction to react to the Attack."
                              :player-no 1
                              :choice    :reveal-reaction
                              :source    :hand
                              :options   [:diplomat :moat]
                              :max       1}
                             {:player-no 0
                              :effect    [:give-actions 1]}
                             {:player-no 0
                              :effect    [:give-choice {:text    "Choose one:"
                                                        :choice  ::intrigue/minion-choice
                                                        :options [:special
                                                                  {:option :coins :text "+$2"}
                                                                  {:option :discard :text "Discard your hand, +4 Cards."}]
                                                        :min     1
                                                        :max     1}]}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand    [minion]
                             :actions 1}
                            {:hand [diplomat moat estate estate silver]
                             :deck [copper copper]}]}
                 (play 0 :minion)
                 (choose :moat))
             {:players      [{:play-area [minion]
                              :actions   0}
                             {:hand       [diplomat moat estate estate silver]
                              :deck       [copper copper]
                              :unaffected [{:works :once}]}]
              :effect-stack [{:text      "You may reveal a Reaction to react to the Attack."
                              :player-no 1
                              :choice    :reveal-reaction
                              :source    :hand
                              :options   [:diplomat]
                              :max       1}
                             {:player-no 0
                              :effect    [:give-actions 1]}
                             {:player-no 0
                              :effect    [:give-choice {:text    "Choose one:"
                                                        :choice  ::intrigue/minion-choice
                                                        :options [:special
                                                                  {:option :coins :text "+$2"}
                                                                  {:option :discard :text "Discard your hand, +4 Cards."}]
                                                        :min     1
                                                        :max     1}]}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand    [minion]
                             :actions 1}
                            {:hand [diplomat moat estate estate silver]
                             :deck [copper copper]}]}
                 (play 0 :minion)
                 (choose :moat)
                 (choose nil))
             {:players      [{:play-area [minion]
                              :actions   1}
                             {:hand       [diplomat moat estate estate silver]
                              :deck       [copper copper]
                              :unaffected [{:works :once}]}]
              :effect-stack [{:text      "Choose one:"
                              :player-no 0
                              :choice    ::intrigue/minion-choice
                              :source    :special
                              :options   [{:option :coins :text "+$2"}
                                          {:option :discard :text "Discard your hand, +4 Cards."}]
                              :min       1
                              :max       1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand    [minion]
                             :actions 1}
                            {:hand [diplomat copper estate estate silver]
                             :deck [moat copper]}]}
                 (play 0 :minion)
                 (choose :diplomat)
                 (choose [:estate :estate :copper]))
             {:players      [{:play-area [minion]
                              :actions   0}
                             {:hand    [diplomat silver moat copper]
                              :discard [estate estate copper]}]
              :effect-stack [{:text      "You may reveal a Reaction to react to the Attack."
                              :player-no 1
                              :choice    :reveal-reaction
                              :source    :hand
                              :options   [:moat]
                              :max       1}
                             {:player-no 0
                              :effect    [:give-actions 1]}
                             {:player-no 0
                              :effect    [:give-choice {:text    "Choose one:"
                                                        :choice  ::intrigue/minion-choice
                                                        :options [:special
                                                                  {:option :coins :text "+$2"}
                                                                  {:option :discard :text "Discard your hand, +4 Cards."}]
                                                        :min     1
                                                        :max     1}]}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]})))))

(deftest duke-test
  (testing "Duke"
    (doseq [duchies (range 9)
            dukes   (range 9)]
      (is (= (calc-victory-points {:deck (concat (repeat duchies duchy)
                                                 (repeat dukes duke))})
             (+ (* dukes duchies)
                (* duchies 3)))))))

(deftest harem-test
  (testing "Harem"
    (is (= (-> {:players [{:hand  [harem]
                           :coins 0}]}
               (play 0 :harem))
           {:players [{:play-area [harem]
                       :coins     2}]}))
    (is (= (calc-victory-points {:deck [harem]})
           2))))

(deftest ironworks-test
  (let [estate    (assoc estate :id 1)
        silver    (assoc silver :id 2)
        courtyard (assoc courtyard :id 3)
        mill      (assoc mill :id 4)]
    (testing "Ironworks"
      (is (= (-> {:supply  [{:card estate :pile-size 8}
                            {:card duchy :pile-size 8}
                            {:card silver :pile-size 40}
                            {:card courtyard :pile-size 10}]
                  :players [{:hand    [ironworks]
                             :actions 1}]}
                 (play 0 :ironworks))
             {:supply       [{:card estate :pile-size 8}
                             {:card duchy :pile-size 8}
                             {:card silver :pile-size 40}
                             {:card courtyard :pile-size 10}]
              :players      [{:play-area [ironworks]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing up to $4."
                              :player-no 0
                              :choice    ::intrigue/ironworks-gain
                              :source    :supply
                              :options   [:estate :silver :courtyard]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card estate :pile-size 8}]
                  :players [{:hand    [ironworks]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :ironworks)
                 (choose :estate))
             {:supply  [{:card estate :pile-size 7}]
              :players [{:hand      [copper]
                         :play-area [ironworks]
                         :deck      [copper]
                         :discard   [estate]
                         :actions   0}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [ironworks]
                             :actions 1
                             :coins   0}]}
                 (play 0 :ironworks)
                 (choose :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:play-area [ironworks]
                         :discard   [silver]
                         :actions   0
                         :coins     1}]}))
      (is (= (-> {:supply  [{:card courtyard :pile-size 10}]
                  :players [{:hand    [ironworks]
                             :actions 1}]}
                 (play 0 :ironworks)
                 (choose :courtyard))
             {:supply  [{:card courtyard :pile-size 9}]
              :players [{:play-area [ironworks]
                         :discard   [courtyard]
                         :actions   1}]}))
      (is (= (-> {:supply  [{:card mill :pile-size 8}]
                  :players [{:hand    [ironworks]
                             :deck    [copper copper]
                             :actions 1}]}
                 (play 0 :ironworks)
                 (choose :mill))
             {:supply  [{:card mill :pile-size 7}]
              :players [{:hand      [copper]
                         :play-area [ironworks]
                         :deck      [copper]
                         :discard   [mill]
                         :actions   1}]})))))

(deftest lurker-test
  (let [upgrade (assoc upgrade :id 1)]
    (testing "Lurker"
      (is (= (-> {:players [{:hand    [lurker]
                             :actions 1}]}
                 (play 0 :lurker))
             {:players      [{:play-area [lurker]
                              :actions   1}]
              :effect-stack [{:text      "Choose one:"
                              :player-no 0
                              :choice    ::intrigue/lurker-choice
                              :source    :special
                              :options   [{:option :trash :text "Trash an Action card from the Supply."}
                                          {:option :gain :text "Gain an Action card from the trash."}]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card lurker :pile-size 9}
                            {:card mining-village :pile-size 0}
                            {:card upgrade :pile-size 10}]
                  :players [{:hand    [lurker]
                             :actions 1}]}
                 (play 0 :lurker)
                 (choose :trash))
             {:supply       [{:card lurker :pile-size 9}
                             {:card mining-village :pile-size 0}
                             {:card upgrade :pile-size 10}]
              :players      [{:play-area [lurker]
                              :actions   1}]
              :effect-stack [{:text      "Trash an Action card from the Supply."
                              :player-no 0
                              :choice    :trash-from-supply
                              :source    :supply
                              :options   [:lurker :upgrade]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card lurker :pile-size 9}
                            {:card mining-village :pile-size 0}
                            {:card upgrade :pile-size 10}]
                  :players [{:hand    [lurker]
                             :actions 1}]}
                 (play 0 :lurker)
                 (choose :trash)
                 (choose :upgrade))
             {:supply  [{:card lurker :pile-size 9}
                        {:card mining-village :pile-size 0}
                        {:card upgrade :pile-size 9}]
              :players [{:play-area [lurker]
                         :actions   1}]
              :trash   [upgrade]}))
      (is (= (-> {:players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [upgrade copper estate]}
                 (play 0 :lurker)
                 (choose :gain))
             {:players      [{:play-area [lurker]
                              :actions   1}]
              :trash        [upgrade copper estate]
              :effect-stack [{:text      "Gain an Action card from the trash."
                              :player-no 0
                              :choice    :gain-from-trash
                              :source    :trash
                              :options   [:upgrade]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:track-gained-cards? true
                  :current-player      0
                  :supply              [{:card upgrade}]
                  :players             [{:hand    [lurker]
                                         :actions 1}]
                  :trash               [upgrade copper estate]}
                 (play 0 :lurker)
                 (choose :gain)
                 (choose :upgrade))
             {:track-gained-cards? true
              :current-player      0
              :supply              [{:card upgrade}]
              :players             [{:play-area    [lurker]
                                     :discard      [upgrade]
                                     :actions      1
                                     :gained-cards [{:name  :upgrade
                                                     :types #{:action}
                                                     :cost  5}]}]
              :trash               [copper estate]})))))

(deftest masquerade-test
  (testing "Masquerade"
    (is (= (-> {:players [{:hand    [masquerade]
                           :deck    [estate estate copper]
                           :actions 1}
                          {:hand [copper]}]}
               (play 0 :masquerade))
           {:players      [{:hand      [estate estate]
                            :play-area [masquerade]
                            :deck      [copper]
                            :actions   0}
                           {:hand [copper]}]
            :effect-stack [{:text      "Pass a card to the next player."
                            :player-no 0
                            :choice    ::intrigue/masquerade-pass
                            :source    :hand
                            :options   [:estate :estate]
                            :min       1
                            :max       1}
                           {:player-no 1
                            :effect    [:give-choice {:text    "Pass a card to the next player."
                                                      :choice  ::intrigue/masquerade-pass
                                                      :options [:player :hand]
                                                      :min     1
                                                      :max     1}]}
                           {:player-no 0
                            :effect    [:all-players {:effects [[::intrigue/masquerade-take]]}]}
                           {:player-no 0
                            :effect    [:give-choice {:text    "You may trash a card from your hand."
                                                      :choice  :trash-from-hand
                                                      :options [:player :hand]
                                                      :max     1}]}]}))
    (is (= (-> {:players [{:hand    [masquerade]
                           :deck    [estate estate copper]
                           :actions 1}
                          {:hand [copper]}]}
               (play 0 :masquerade)
               (choose :estate))
           {:players      [{:hand      [estate]
                            :play-area [masquerade]
                            :deck      [copper]
                            :actions   0}
                           {:hand              [copper]
                            :masquerade-passed [estate]}]
            :effect-stack [{:text      "Pass a card to the next player."
                            :player-no 1
                            :choice    ::intrigue/masquerade-pass
                            :source    :hand
                            :options   [:copper]
                            :min       1
                            :max       1}
                           {:player-no 0
                            :effect    [:all-players {:effects [[::intrigue/masquerade-take]]}]}
                           {:player-no 0
                            :effect    [:give-choice {:text    "You may trash a card from your hand."
                                                      :choice  :trash-from-hand
                                                      :options [:player :hand]
                                                      :max     1}]}]}))
    (is (= (-> {:players [{:hand    [masquerade]
                           :deck    [estate estate copper]
                           :actions 1}
                          {:hand [copper]}]}
               (play 0 :masquerade)
               (choose :estate)
               (choose :copper))
           {:players      [{:hand      [estate copper]
                            :play-area [masquerade]
                            :deck      [copper]
                            :actions   0}
                           {:hand [estate]}]
            :effect-stack [{:text      "You may trash a card from your hand."
                            :player-no 0
                            :choice    :trash-from-hand
                            :source    :hand
                            :options   [:estate :copper]
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [masquerade]
                           :deck    [estate estate copper]
                           :actions 1}
                          {:hand [copper]}]}
               (play 0 :masquerade)
               (choose :estate)
               (choose :copper)
               (choose :estate))
           {:players [{:hand      [copper]
                       :play-area [masquerade]
                       :deck      [copper]
                       :actions   0}
                      {:hand [estate]}]
            :trash   [estate]}))
    (is (= (-> {:players [{:hand    [masquerade]
                           :deck    [estate estate copper]
                           :actions 1}
                          {:hand [copper]}]}
               (play 0 :masquerade)
               (choose :estate)
               (choose :copper)
               (choose nil))
           {:players [{:hand      [estate copper]
                       :play-area [masquerade]
                       :deck      [copper]
                       :actions   0}
                      {:hand [estate]}]}))
    (is (= (-> {:players [{:hand    [masquerade]
                           :deck    [estate estate copper]
                           :actions 1}
                          {:hand [copper]}
                          {:hand [silver]}]}
               (play 0 :masquerade))
           {:players      [{:hand      [estate estate]
                            :play-area [masquerade]
                            :deck      [copper]
                            :actions   0}
                           {:hand [copper]}
                           {:hand [silver]}]
            :effect-stack [{:text      "Pass a card to the next player."
                            :player-no 0
                            :choice    ::intrigue/masquerade-pass
                            :source    :hand
                            :options   [:estate :estate]
                            :min       1
                            :max       1}
                           {:player-no 2
                            :effect    [:give-choice {:text    "Pass a card to the next player."
                                                      :choice  ::intrigue/masquerade-pass
                                                      :options [:player :hand]
                                                      :min     1
                                                      :max     1}]}
                           {:player-no 1
                            :effect    [:give-choice {:text    "Pass a card to the next player."
                                                      :choice  ::intrigue/masquerade-pass
                                                      :options [:player :hand]
                                                      :min     1
                                                      :max     1}]}
                           {:player-no 0
                            :effect    [:all-players {:effects [[::intrigue/masquerade-take]]}]}
                           {:player-no 0
                            :effect    [:give-choice {:text    "You may trash a card from your hand."
                                                      :choice  :trash-from-hand
                                                      :options [:player :hand]
                                                      :max     1}]}]}))
    (is (= (-> {:mode    :swift
                :players [{:hand    [masquerade]
                           :deck    [estate estate copper]
                           :actions 1}
                          {:hand [copper copper]}
                          {:hand [silver silver]}]}
               (play 0 :masquerade))
           {:mode         :swift
            :players      [{:hand      [estate silver]
                            :play-area [masquerade]
                            :deck      [copper]
                            :actions   0}
                           {:hand [copper estate]}
                           {:hand [silver copper]}]
            :effect-stack [{:text      "You may trash a card from your hand."
                            :player-no 0
                            :choice    :trash-from-hand
                            :source    :hand
                            :options   [:estate :silver]
                            :max       1}]}))))

(deftest mill-test
  (testing "Mill"
    (is (= (-> {:players [{:hand    [mill copper silver]
                           :deck    [estate copper]
                           :actions 1}]}
               (play 0 :mill))
           {:players      [{:hand      [copper silver estate]
                            :play-area [mill]
                            :deck      [copper]
                            :actions   1}]
            :effect-stack [{:text      "You may discard 2 cards, for +$2."
                            :player-no 0
                            :choice    ::intrigue/mill-discard
                            :source    :hand
                            :options   [:copper :silver :estate]
                            :min       2
                            :max       2
                            :optional? true}]}))
    (is (= (-> {:players [{:hand    [mill copper silver]
                           :deck    [estate copper]
                           :actions 1
                           :coins   0}]}
               (play 0 :mill)
               (choose [:copper :estate]))
           {:players [{:hand      [silver]
                       :play-area [mill]
                       :deck      [copper]
                       :discard   [copper estate]
                       :actions   1
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [mill copper silver]
                           :deck    [gold copper]
                           :actions 1}]}
               (play 0 :mill)
               (choose nil))
           {:players [{:hand      [copper silver gold]
                       :play-area [mill]
                       :deck      [copper]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [mill]
                           :deck    [estate copper]
                           :actions 1}]}
               (play 0 :mill)
               (choose nil))
           {:players [{:hand      [estate]
                       :play-area [mill]
                       :deck      [copper]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [mill]
                           :deck    [estate copper]
                           :actions 1}]}
               (play 0 :mill)
               (choose :estate))
           {:players [{:play-area [mill]
                       :deck      [copper]
                       :discard   [estate]
                       :actions   1}]}))
    (is (= (calc-victory-points {:deck [mill]})
           1))))

(deftest mining-village-test
  (testing "Mining Village"
    (let [mining-village-1 (assoc mining-village :id 1)
          mining-village-2 (assoc mining-village :id 2)]
      (is (= (-> {:players [{:hand    [mining-village-1]
                             :deck    [silver copper]
                             :actions 1}]}
                 (play 0 :mining-village))
             {:players      [{:hand      [silver]
                              :play-area [mining-village-1]
                              :deck      [copper]
                              :actions   2}]
              :effect-stack [{:text      "You may trash this for +$2."
                              :player-no 0
                              :card-id   1
                              :choice    ::intrigue/mining-village-trash
                              :source    :play-area
                              :options   [:mining-village]
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [mining-village-1]
                             :deck    [silver copper]
                             :actions 1}]}
                 (play 0 :mining-village)
                 (choose nil))
             {:players [{:hand      [silver]
                         :play-area [mining-village-1]
                         :deck      [copper]
                         :actions   2}]}))
      (is (= (-> {:players [{:hand    [mining-village-1]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :mining-village)
                 (choose :mining-village))
             {:players [{:hand    [silver]
                         :deck    [copper]
                         :actions 2
                         :coins   2}]
              :trash   [mining-village-1]}))
      (is (= (-> {:players [{:hand    [throne-room mining-village-1]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :mining-village)
                 (choose :mining-village))
             {:players [{:hand      [silver copper]
                         :play-area [throne-room]
                         :actions   4
                         :coins     2}]
              :trash   [mining-village-1]}))
      (is (= (-> {:players [{:hand    [throne-room mining-village-1]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :mining-village)
                 (choose nil)
                 (choose :mining-village))
             {:players [{:hand      [silver copper]
                         :play-area [throne-room]
                         :actions   4
                         :coins     2}]
              :trash   [mining-village-1]}))
      (is (= (-> {:players [{:hand    [mining-village-1 mining-village-2]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :mining-village)
                 (choose nil)
                 (play 0 :mining-village)
                 (choose :mining-village))
             {:players [{:hand      [silver copper]
                         :play-area [mining-village-1]
                         :actions   3
                         :coins     2}]
              :trash   [mining-village-2]}))
      (is (= (-> {:players [{:hand    [mining-village-1 throne-room mining-village-2]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :mining-village)
                 (choose nil)
                 (play 0 :throne-room)
                 (choose :mining-village)
                 (choose :mining-village))
             {:players [{:hand      [silver copper]
                         :play-area [mining-village-1 throne-room]
                         :actions   5
                         :coins     2}]
              :trash   [mining-village-2]}))
      (is (= (-> {:players [{:hand    [throne-room throne-room mining-village-1 mining-village-2]
                             :deck    [silver copper]
                             :actions 1
                             :coins   0}]}
                 (play 0 :throne-room)
                 (choose :throne-room)                      ; play throne-room-2 twice
                 (choose :mining-village)                   ; play mining-village-1 twice
                 (choose nil)                               ; don't trash mining-village-1
                 (choose nil)                               ; don't trash mining-village-1 (again)
                 (choose :mining-village)                   ; play mining-village-2 twice
                 (choose :mining-village))                  ; trash mining-village-2
             {:players [{:hand      [silver copper]
                         :play-area [throne-room throne-room mining-village-1]
                         :actions   8
                         :coins     2}]
              :trash   [mining-village-2]})))))

(deftest minion-test
  (testing "Minion"
    (is (= (-> {:players [{:hand    [minion]
                           :actions 1}]}
               (play 0 :minion))
           {:players      [{:play-area [minion]
                            :actions   1}]
            :effect-stack [{:text      "Choose one:"
                            :player-no 0
                            :choice    ::intrigue/minion-choice
                            :source    :special
                            :options   [{:option :coins :text "+$2"}
                                        {:option :discard :text "Discard your hand, +4 Cards."}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [minion]
                           :actions 1
                           :coins   0}]}
               (play 0 :minion)
               (choose :coins))
           {:players [{:play-area [minion]
                       :actions   1
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [minion estate copper]
                           :deck    [minion copper silver estate copper]
                           :actions 1}
                          {:hand (repeat 5 copper)
                           :deck (repeat 5 estate)}]}
               (play 0 :minion)
               (choose :discard))
           {:players [{:hand      [minion copper silver estate]
                       :play-area [minion]
                       :deck      [copper]
                       :discard   [estate copper]
                       :actions   1}
                      {:hand    (repeat 4 estate)
                       :deck    [estate]
                       :discard (repeat 5 copper)}]}))
    (is (= (-> {:players [{:hand    [minion estate copper]
                           :deck    [minion copper silver estate copper]
                           :actions 1}
                          {:hand (repeat 4 copper)
                           :deck (repeat 3 silver)}]}
               (play 0 :minion)
               (choose :discard))
           {:players [{:hand      [minion copper silver estate]
                       :play-area [minion]
                       :deck      [copper]
                       :discard   [estate copper]
                       :actions   1}
                      {:hand (repeat 4 copper)
                       :deck (repeat 3 silver)}]}))))

(deftest nobles-test
  (testing "Nobles"
    (is (= (-> {:players [{:hand    [nobles]
                           :actions 1}]}
               (play 0 :nobles))
           {:players      [{:play-area [nobles]
                            :actions   0}]
            :effect-stack [{:text      "Choose one:"
                            :player-no 0
                            :choice    ::intrigue/nobles-choice
                            :source    :special
                            :options   [{:option :cards :text "+3 Cards"}
                                        {:option :actions :text "+2 Actions"}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [nobles]
                           :deck    [copper copper estate estate]
                           :actions 1}]}
               (play 0 :nobles)
               (choose :cards))
           {:players [{:hand      [copper copper estate]
                       :play-area [nobles]
                       :deck      [estate]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [nobles]
                           :actions 1}]}
               (play 0 :nobles)
               (choose :actions))
           {:players [{:play-area [nobles]
                       :actions   2}]}))
    (is (= (-> {:players [{:hand    [nobles throne-room]
                           :deck    [copper copper estate estate]
                           :actions 1}]}
               (play 0 :throne-room)
               (choose :nobles)
               (choose :cards))
           {:players      [{:hand      [copper copper estate]
                            :play-area [throne-room nobles]
                            :deck      [estate]
                            :actions   0}]
            :effect-stack [{:text      "Choose one:"
                            :player-no 0
                            :choice    ::intrigue/nobles-choice
                            :source    :special
                            :options   [{:option :cards :text "+3 Cards"}
                                        {:option :actions :text "+2 Actions"}]
                            :min       1
                            :max       1}
                           {:player-no 0
                            :effect    [:register-repeated-play {:target-id nil}]}]}))
    (is (= (calc-victory-points {:deck [nobles]})
           2))))

(deftest patrol-test
  (testing "Patrol"
    (is (= (-> {:players [{:hand    [patrol]
                           :deck    (repeat 8 copper)
                           :actions 1}]}
               (play 0 :patrol))
           {:players      [{:hand      (repeat 3 copper)
                            :play-area [patrol]
                            :deck      [copper]
                            :revealed  (repeat 4 copper)
                            :actions   0}]
            :effect-stack [{:text      "Put the revealed cards back on your deck."
                            :player-no 0
                            :choice    :topdeck-from-revealed
                            :source    :revealed
                            :options   [:copper :copper :copper :copper]
                            :min       4
                            :max       4}]}))
    (is (= (-> {:players [{:hand    [patrol]
                           :deck    (repeat 8 copper)
                           :actions 1}]}
               (play 0 :patrol)
               (choose [:copper :copper :copper :copper]))
           {:players [{:hand           (repeat 3 copper)
                       :play-area      [patrol]
                       :deck           (repeat 5 copper)
                       :revealed-cards {:deck 4}
                       :actions        0}]}))
    (is (= (-> {:players [{:hand    [patrol]
                           :deck    [copper copper copper copper estate silver curse copper]
                           :actions 1}]}
               (play 0 :patrol))
           {:players      [{:hand           [copper copper copper estate curse]
                            :play-area      [patrol]
                            :deck           [copper]
                            :revealed       [copper silver]
                            :revealed-cards {:hand 2}
                            :actions        0}]
            :effect-stack [{:text      "Put the revealed cards back on your deck."
                            :player-no 0
                            :choice    :topdeck-from-revealed
                            :source    :revealed
                            :options   [:copper :silver]
                            :min       2
                            :max       2}]}))
    (is (= (-> {:players [{:hand    [patrol]
                           :deck    [copper copper copper copper estate silver curse copper]
                           :actions 1}]}
               (play 0 :patrol)
               (choose [:silver :copper]))
           {:players [{:hand           [copper copper copper estate curse]
                       :play-area      [patrol]
                       :deck           [copper silver copper]
                       :revealed-cards {:hand 2
                                        :deck 2}
                       :actions        0}]}))
    (is (= (-> {:mode    :swift
                :players [{:hand    [patrol]
                           :deck    [copper copper copper copper estate copper harem silver]
                           :actions 1}]}
               (play 0 :patrol))
           {:mode    :swift
            :players [{:hand           [copper copper copper estate harem]
                       :play-area      [patrol]
                       :deck           [copper copper silver]
                       :revealed-cards {:hand 2
                                        :deck 2}
                       :actions        0}]}))))

(deftest pawn-test
  (testing "Pawn"
    (is (= (-> {:players [{:hand    [pawn]
                           :actions 1}]}
               (play 0 :pawn))
           {:players      [{:play-area [pawn]
                            :actions   0}]
            :effect-stack [{:text      "Choose two:"
                            :player-no 0
                            :choice    ::intrigue/pawn-choices
                            :source    :special
                            :options   [{:option :card :text "+1 Card"}
                                        {:option :action :text "+1 Action"}
                                        {:option :buy :text "+1 Buy"}
                                        {:option :coin :text "+$1"}]
                            :min       2
                            :max       2}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1}]}
               (play 0 :pawn)
               (choose [:card :action]))
           {:players [{:hand      [copper]
                       :deck      [estate]
                       :play-area [pawn]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :buys    1}]}
               (play 0 :pawn)
               (choose [:card :buy]))
           {:players [{:hand      [copper]
                       :deck      [estate]
                       :play-area [pawn]
                       :actions   0
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :pawn)
               (choose [:card :coin]))
           {:players [{:hand      [copper]
                       :deck      [estate]
                       :play-area [pawn]
                       :actions   0
                       :coins     1}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :buys    1}]}
               (play 0 :pawn)
               (choose [:action :buy]))
           {:players [{:deck      [copper estate]
                       :play-area [pawn]
                       :actions   1
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :pawn)
               (choose [:action :coin]))
           {:players [{:deck      [copper estate]
                       :play-area [pawn]
                       :actions   1
                       :coins     1}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :buys    1
                           :coins   0}]}
               (play 0 :pawn)
               (choose [:buy :coin]))
           {:players [{:deck      [copper estate]
                       :play-area [pawn]
                       :actions   0
                       :buys      2
                       :coins     1}]}))
    (is (thrown-with-msg? AssertionError #"The choices must be different."
                          (-> {:players [{:hand    [pawn]
                                          :deck    [copper estate]
                                          :actions 1}]}
                              (play 0 :pawn)
                              (choose [:card :card]))))))

(deftest replace-test
  (let [silver (assoc silver :id 1)
        duchy  (assoc duchy :id 2)
        curse  (assoc curse :id 3)
        nobles (assoc nobles :id 4)]
    (testing "Replace"
      (is (= (play {:players [{:hand    [replace copper estate]
                               :actions 1}]}
                   0 :replace)
             {:players      [{:hand      [copper estate]
                              :play-area [replace]
                              :actions   0}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :choice    ::intrigue/replace-trash
                              :source    :hand
                              :options   [:copper :estate]
                              :min       1
                              :max       1}]}))
      (is (= (play {:players [{:hand    [replace]
                               :actions 1}]}
                   0 :replace)
             {:players [{:play-area [replace]
                         :actions   0}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand    [replace copper estate]
                             :actions 1}]}
                 (play 0 :replace)
                 (choose :estate))
             {:supply       (base/supply 2 8)
              :players      [{:hand      [copper]
                              :play-area [replace]
                              :actions   0}]
              :effect-stack [{:text      "Gain a card costing up to $4."
                              :player-no 0
                              :choice    ::intrigue/replace-gain
                              :source    :supply
                              :options   [:curse :estate :copper :silver]
                              :min       1
                              :max       1}]
              :trash        [estate]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [replace silver estate]
                             :deck    [copper]
                             :actions 1}]}
                 (play 0 :replace)
                 (choose :estate)
                 (choose :silver))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:hand      [silver]
                         :play-area [replace]
                         :deck      [silver copper]
                         :actions   0}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card duchy :pile-size 8}]
                  :players [{:hand    [replace silver estate]
                             :deck    [copper]
                             :actions 1}
                            {}]}
                 (play 0 :replace)
                 (choose :silver)
                 (choose :duchy))
             {:supply  [{:card curse :pile-size 9}
                        {:card duchy :pile-size 7}]
              :players [{:hand      [estate]
                         :play-area [replace]
                         :deck      [copper]
                         :discard   [duchy]
                         :actions   0}
                        {:discard [curse]}]
              :trash   [silver]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}
                            {:card nobles :pile-size 8}]
                  :players [{:hand    [replace silver duchy]
                             :deck    [copper]
                             :actions 1}
                            {}]}
                 (play 0 :replace)
                 (choose :duchy)
                 (choose :nobles))
             {:supply  [{:card curse :pile-size 9}
                        {:card nobles :pile-size 7}]
              :players [{:hand      [silver]
                         :play-area [replace]
                         :deck      [nobles copper]
                         :actions   0}
                        {:discard [curse]}]
              :trash   [duchy]})))))

(deftest secret-passage-test
  (testing "Secret Passage"
    (is (= (-> {:players [{:hand    [secret-passage]
                           :deck    [silver silver copper copper copper]
                           :actions 1}]}
               (play 0 :secret-passage))
           {:players      [{:hand      [silver silver]
                            :play-area [secret-passage]
                            :deck      [copper copper copper]
                            :actions   1}]
            :effect-stack [{:text      "Put a card from your hand anywhere in your deck."
                            :player-no 0
                            :choice    ::intrigue/secret-passage-take
                            :source    :hand
                            :options   [:silver :silver]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [secret-passage]
                           :deck    [silver silver copper copper copper]
                           :actions 1}]}
               (play 0 :secret-passage)
               (choose :silver))
           {:players      [{:hand           [silver]
                            :play-area      [secret-passage]
                            :secret-passage [silver]
                            :deck           [copper copper copper]
                            :actions        1}]
            :effect-stack [{:text      "Put the Silver anywhere in your deck."
                            :player-no 0
                            :choice    ::intrigue/secret-passage-put
                            :source    :deck-position
                            :options   [0 1 2 3]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [secret-passage]
                           :deck    [silver silver copper copper copper]
                           :actions 1}]}
               (play 0 :secret-passage)
               (choose :silver)
               (choose 0))
           {:players [{:hand      [silver]
                       :play-area [secret-passage]
                       :deck      [silver copper copper copper]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [secret-passage]
                           :deck    [silver silver copper copper copper]
                           :actions 1}]}
               (play 0 :secret-passage)
               (choose :silver)
               (choose 1))
           {:players [{:hand      [silver]
                       :play-area [secret-passage]
                       :deck      [copper silver copper copper]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [secret-passage]
                           :deck    [silver silver copper copper copper]
                           :actions 1}]}
               (play 0 :secret-passage)
               (choose :silver)
               (choose 2))
           {:players [{:hand      [silver]
                       :play-area [secret-passage]
                       :deck      [copper copper silver copper]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [secret-passage]
                           :deck    [silver silver copper copper copper]
                           :actions 1}]}
               (play 0 :secret-passage)
               (choose :silver)
               (choose 3))
           {:players [{:hand      [silver]
                       :play-area [secret-passage]
                       :deck      [copper copper copper silver]
                       :actions   1}]}))
    (is (= (-> {:players [{:hand    [secret-passage]
                           :deck    [silver silver]
                           :actions 1}]}
               (play 0 :secret-passage)
               (choose :silver))
           {:players      [{:hand           [silver]
                            :play-area      [secret-passage]
                            :secret-passage [silver]
                            :actions        1}]
            :effect-stack [{:text      "Put the Silver anywhere in your deck."
                            :player-no 0
                            :choice    ::intrigue/secret-passage-put
                            :source    :deck-position
                            :options   [0]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [secret-passage]
                           :deck    [silver silver]
                           :actions 1}]}
               (play 0 :secret-passage)
               (choose :silver)
               (choose 0))
           {:players [{:hand      [silver]
                       :play-area [secret-passage]
                       :deck      [silver]
                       :actions   1}]}))))

(deftest shanty-town-test
  (testing "Shanty Town"
    (is (= (-> {:players [{:hand    [shanty-town shanty-town copper copper copper]
                           :deck    [silver silver estate]
                           :actions 1}]}
               (play 0 :shanty-town))
           {:players [{:hand           [shanty-town copper copper copper]
                       :play-area      [shanty-town]
                       :deck           [silver silver estate]
                       :actions        2
                       :revealed-cards {:hand 4}}]}))
    (is (= (-> {:players [{:hand           [shanty-town copper copper copper]
                           :play-area      [shanty-town]
                           :deck           [silver silver estate]
                           :actions        2
                           :revealed-cards {:hand 4}}]}
               (play 0 :shanty-town))
           {:players [{:hand      [copper copper copper silver silver]
                       :play-area [shanty-town shanty-town]
                       :deck      [estate]
                       :actions   3}]}))))

(deftest steward-test
  (testing "Steward"
    (is (= (-> {:players [{:hand    [steward]
                           :actions 1}]}
               (play 0 :steward))
           {:players      [{:play-area [steward]
                            :actions   0}]
            :effect-stack [{:text      "Choose one:"
                            :player-no 0
                            :choice    ::intrigue/steward-choices
                            :source    :special
                            :options   [{:option :cards :text "+2 Cards"}
                                        {:option :coins :text "+$2"}
                                        {:option :trash :text "Trash 2 cards from your hand."}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [steward]
                           :deck    [silver copper estate]
                           :actions 1}]}
               (play 0 :steward)
               (choose :cards))
           {:players [{:hand      [silver copper]
                       :deck      [estate]
                       :play-area [steward]
                       :actions   0}]}))
    (is (= (-> {:players [{:hand    [steward]
                           :deck    [copper estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :steward)
               (choose :coins))
           {:players [{:deck      [copper estate]
                       :play-area [steward]
                       :actions   0
                       :coins     2}]}))
    (is (= (-> {:players [{:hand    [steward copper estate silver]
                           :actions 1}]}
               (play 0 :steward)
               (choose :trash))
           {:players      [{:hand      [copper estate silver]
                            :play-area [steward]
                            :actions   0}]
            :effect-stack [{:text      "Trash 2 cards from your hand."
                            :player-no 0
                            :choice    :trash-from-hand
                            :source    :hand
                            :options   [:copper :estate :silver]
                            :min       2
                            :max       2}]}))
    (is (= (-> {:players [{:hand    [steward copper estate silver]
                           :actions 1}]}
               (play 0 :steward)
               (choose :trash)
               (choose [:estate :copper]))
           {:players [{:hand      [silver]
                       :play-area [steward]
                       :actions   0}]
            :trash   [estate copper]}))))

(deftest swindler-test
  (testing "Swindler"
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [swindler]
                           :actions 1
                           :coins   0}
                          {:deck [copper]}]}
               (play 0 :swindler))
           {:supply       (base/supply 2 8)
            :players      [{:play-area [swindler]
                            :actions   0
                            :coins     2}
                           {}]
            :effect-stack [{:text      "Gain a card costing $0 (attacker chooses)."
                            :player-no 1
                            :choice    :gain
                            :source    :supply
                            :options   [:curse :copper]
                            :min       1
                            :max       1}
                           {:player-no 1
                            :effect    [:clear-unaffected {:works :once}]}]
            :trash        [copper]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [swindler]
                           :actions 1
                           :coins   0}
                          {:discard [copper]}]}
               (play 0 :swindler))
           {:supply       (base/supply 2 8)
            :players      [{:play-area [swindler]
                            :actions   0
                            :coins     2}
                           {}]
            :effect-stack [{:text      "Gain a card costing $0 (attacker chooses)."
                            :player-no 1
                            :choice    :gain
                            :source    :supply
                            :options   [:curse :copper]
                            :min       1
                            :max       1}
                           {:player-no 1
                            :effect    [:clear-unaffected {:works :once}]}]
            :trash        [copper]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [swindler]
                           :actions 1
                           :coins   0}
                          {}]}
               (play 0 :swindler))
           {:supply  (base/supply 2 8)
            :players [{:play-area [swindler]
                       :actions   0
                       :coins     2}
                      {}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [swindler]
                           :actions 1
                           :coins   0}
                          {:deck [mining-village]}]}
               (play 0 :swindler))
           {:supply  (base/supply 2 8)
            :players [{:play-area [swindler]
                       :actions   0
                       :coins     2}
                      {}]
            :trash   [mining-village]}))
    (let [curse  (assoc curse :id 1)
          copper (assoc copper :id 2)]
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [swindler]
                             :actions 1
                             :coins   0}
                            {:deck [copper]}]}
                 (play 0 :swindler)
                 (choose :curse))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:play-area [swindler]
                         :actions   0
                         :coins     2}
                        {:discard [curse]}]
              :trash   [copper]}))
      (is (= (-> {:supply  [{:card curse :pile-size 1}
                            {:card copper :pile-size 46}]
                  :players [{:hand    [swindler]
                             :actions 1
                             :coins   0}
                            {:deck [copper]}
                            {:deck [curse]}]}
                 (play 0 :swindler)
                 (choose :curse))
             {:supply       [{:card curse :pile-size 0}
                             {:card copper :pile-size 46}]
              :players      [{:play-area [swindler]
                              :actions   0
                              :coins     2}
                             {:discard [curse]}
                             {}]
              :effect-stack [{:text      "Gain a card costing $0 (attacker chooses)."
                              :player-no 2
                              :choice    :gain
                              :source    :supply
                              :options   [:copper]
                              :min       1
                              :max       1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}
                             {:player-no 2
                              :effect    [:clear-unaffected {:works :once}]}]
              :trash        [copper curse]})))))

(deftest torturer-test
  (let [curse (assoc curse :id 1)]
    (testing "Torturer"
      (is (= (-> {:players [{:hand    [torturer]
                             :deck    [copper copper copper copper]
                             :actions 1}
                            {:hand [copper copper copper copper]}]}
                 (play 0 :torturer))
             {:players      [{:hand      [copper copper copper]
                              :play-area [torturer]
                              :deck      [copper]
                              :actions   0}
                             {:hand [copper copper copper copper]}]
              :effect-stack [{:text                "Choose one:"
                              :player-no           1
                              :attacking-player-no 0
                              :choice              ::intrigue/torturer-choice
                              :source              :special
                              :options             [{:option :discard :text "Discard 2 cards."}
                                                    {:option :curse :text "Gain a Curse to your hand."}]
                              :min                 1
                              :max                 1}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand    [torturer]
                             :deck    [copper copper copper copper]
                             :actions 1}
                            {:hand [copper copper copper copper]}]}
                 (play 0 :torturer)
                 (choose :discard))
             {:players      [{:hand      [copper copper copper]
                              :play-area [torturer]
                              :deck      [copper]
                              :actions   0}
                             {:hand [copper copper copper copper]}]
              :effect-stack [{:text      "Discard 2 cards."
                              :player-no 1
                              :choice    :discard-from-hand
                              :source    :hand
                              :options   [:copper :copper :copper :copper]
                              :min       2
                              :max       2}
                             {:player-no 1
                              :effect    [:clear-unaffected {:works :once}]}]}))
      (is (= (-> {:players [{:hand    [torturer]
                             :deck    [copper copper copper copper]
                             :actions 1}
                            {:hand [copper copper copper copper]}]}
                 (play 0 :torturer)
                 (choose :discard)
                 (choose [:copper :copper]))
             {:players [{:hand      [copper copper copper]
                         :play-area [torturer]
                         :deck      [copper]
                         :actions   0}
                        {:hand    [copper copper]
                         :discard [copper copper]}]}))
      (is (= (-> {:players [{:hand    [torturer]
                             :deck    [copper copper copper copper]
                             :actions 1}
                            {:hand [copper]}]}
                 (play 0 :torturer)
                 (choose :discard)
                 (choose :copper))
             {:players [{:hand      [copper copper copper]
                         :play-area [torturer]
                         :deck      [copper]
                         :actions   0}
                        {:discard [copper]}]}))
      (is (= (-> {:players [{:hand    [torturer]
                             :deck    [copper copper copper copper]
                             :actions 1}
                            {}]}
                 (play 0 :torturer)
                 (choose :discard))
             {:players [{:hand      [copper copper copper]
                         :play-area [torturer]
                         :deck      [copper]
                         :actions   0}
                        {}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [torturer]
                             :deck    [copper copper copper copper]
                             :actions 1}
                            {:hand [copper copper copper copper]}]}
                 (play 0 :torturer)
                 (choose :curse))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:hand      [copper copper copper]
                         :play-area [torturer]
                         :deck      [copper]
                         :actions   0}
                        {:hand [copper copper copper copper curse]}]}))
      (is (= (-> {:supply  [{:card curse :pile-size 0}]
                  :players [{:hand    [torturer]
                             :deck    [copper copper copper copper]
                             :actions 1}
                            {:hand [copper copper copper copper]}]}
                 (play 0 :torturer)
                 (choose :curse))
             {:supply  [{:card curse :pile-size 0}]
              :players [{:hand      [copper copper copper]
                         :play-area [torturer]
                         :deck      [copper]
                         :actions   0}
                        {:hand [copper copper copper copper]}]})))))

(deftest trading-post-test
  (let [silver (assoc silver :id 1)]
    (testing "Trading Post"
      (is (= (-> {:players [{:hand    [trading-post copper estate copper]
                             :actions 1}]}
                 (play 0 :trading-post))
             {:players      [{:hand      [copper estate copper]
                              :play-area [trading-post]
                              :actions   0}]
              :effect-stack [{:text      "Trash 2 cards from your hand."
                              :player-no 0
                              :choice    ::intrigue/trading-post-trash
                              :source    :hand
                              :options   [:copper :estate :copper]
                              :min       2
                              :max       2}]}))
      (is (= (-> {:supply  [{:card silver :pile-size 40}]
                  :players [{:hand    [trading-post copper estate copper]
                             :actions 1}]}
                 (play 0 :trading-post)
                 (choose [:copper :estate]))
             {:supply  [{:card silver :pile-size 39}]
              :players [{:hand      [copper silver]
                         :play-area [trading-post]
                         :actions   0}]
              :trash   [copper estate]}))
      (is (= (-> {:players [{:hand    [trading-post]
                             :actions 1}]}
                 (play 0 :trading-post))
             {:players [{:play-area [trading-post]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [trading-post copper]
                             :actions 1}]}
                 (play 0 :trading-post)
                 (choose :copper))
             {:players [{:play-area [trading-post]
                         :actions   0}]
              :trash   [copper]})))))

(deftest upgrade-test
  (let [silver (assoc silver :id 1)
        estate (assoc estate :id 2)]
    (testing "Upgrade"
      (is (= (-> {:players [{:hand    [upgrade copper copper estate estate]
                             :deck    [silver]
                             :actions 1}]}
                 (play 0 :upgrade))
             {:players      [{:hand      [copper copper estate estate silver]
                              :play-area [upgrade]
                              :actions   1}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :choice    :upgrade-trash
                              :source    :hand
                              :options   [:copper :copper :estate :estate :silver]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [upgrade copper copper estate estate]
                             :deck    [silver]
                             :actions 1}]}
                 (play 0 :upgrade)
                 (choose :estate))
             {:supply       [{:card copper :pile-size 46}
                             {:card silver :pile-size 40}
                             {:card gold :pile-size 30}]
              :players      [{:hand      [copper copper estate silver]
                              :play-area [upgrade]
                              :actions   1}]
              :effect-stack [{:text      "Gain a card costing exactly $3."
                              :player-no 0
                              :choice    :gain
                              :source    :supply
                              :options   [:silver]
                              :min       1
                              :max       1}]
              :trash        [estate]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card silver :pile-size 40}
                            {:card gold :pile-size 30}]
                  :players [{:hand    [upgrade copper copper estate estate]
                             :deck    [silver]
                             :actions 1}]}
                 (play 0 :upgrade)
                 (choose :estate)
                 (choose :silver))
             {:supply  [{:card copper :pile-size 46}
                        {:card silver :pile-size 39}
                        {:card gold :pile-size 30}]
              :players [{:hand      [copper copper estate silver]
                         :play-area [upgrade]
                         :discard   [silver]
                         :actions   1}]
              :trash   [estate]}))
      (is (= (-> {:supply  [{:card copper :pile-size 46}
                            {:card estate :pile-size 8}
                            {:card silver :pile-size 40}]
                  :players [{:hand    [upgrade copper copper estate estate]
                             :deck    [silver]
                             :actions 1}]}
                 (play 0 :upgrade)
                 (choose :copper))
             {:supply  [{:card copper :pile-size 46}
                        {:card estate :pile-size 8}
                        {:card silver :pile-size 40}]
              :players [{:hand      [copper estate estate silver]
                         :play-area [upgrade]
                         :actions   1}]
              :trash   [copper]}))
      (testing "with cost reduction"
        (is (= (-> {:supply          (base/supply 2 8)
                    :cost-reductions [{:reduction 1}]
                    :players         [{:hand    [upgrade copper copper estate estate]
                                       :deck    [silver]
                                       :actions 1}]}
                   (play 0 :upgrade)
                   (choose :copper))
               {:supply          (base/supply 2 8)
                :cost-reductions [{:reduction 1}]
                :players         [{:hand      [copper estate estate silver]
                                   :play-area [upgrade]
                                   :actions   1}]
                :effect-stack    [{:text      "Gain a card costing exactly $1."
                                   :player-no 0
                                   :choice    :gain
                                   :source    :supply
                                   :options   [:estate]
                                   :min       1
                                   :max       1}]
                :trash           [copper]}))
        (is (= (-> {:supply          (base/supply 2 8)
                    :cost-reductions [{:reduction 1}]
                    :players         [{:hand    [upgrade copper copper estate estate]
                                       :deck    [silver]
                                       :actions 1}]}
                   (play 0 :upgrade)
                   (choose :estate))
               {:supply          (base/supply 2 8)
                :cost-reductions [{:reduction 1}]
                :players         [{:hand      [copper copper estate silver]
                                   :play-area [upgrade]
                                   :actions   1}]
                :effect-stack    [{:text      "Gain a card costing exactly $2."
                                   :player-no 0
                                   :choice    :gain
                                   :source    :supply
                                   :options   [:silver]
                                   :min       1
                                   :max       1}]
                :trash           [estate]}))))))

(deftest wishing-well-test
  (testing "Wishing Well"
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [wishing-well]
                           :deck    [silver copper]
                           :actions 1}]}
               (play 0 :wishing-well))
           {:supply       (base/supply 2 8)
            :players      [{:hand      [silver]
                            :play-area [wishing-well]
                            :deck      [copper]
                            :actions   1}]
            :effect-stack [{:text      "Name a card."
                            :player-no 0
                            :choice    ::intrigue/wishing-well-guess
                            :source    :supply
                            :options   [:curse :estate :duchy :province :copper :silver :gold]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [wishing-well]
                           :deck    [silver copper]
                           :actions 1}]}
               (play 0 :wishing-well)
               (choose :copper))
           {:supply  (base/supply 2 8)
            :players [{:hand           [silver copper]
                       :play-area      [wishing-well]
                       :actions        1
                       :revealed-cards {:hand 1}}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [wishing-well]
                           :deck    [silver copper]
                           :actions 1}]}
               (play 0 :wishing-well)
               (choose :gold))
           {:supply  (base/supply 2 8)
            :players [{:hand           [silver]
                       :play-area      [wishing-well]
                       :deck           [copper]
                       :actions        1
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [wishing-well]
                           :deck    [silver]
                           :discard [silver copper]
                           :actions 1}]}
               (play 0 :wishing-well)
               (choose :copper))
           {:supply  (base/supply 2 8)
            :players [{:hand           [silver]
                       :play-area      [wishing-well]
                       :deck           [silver copper]
                       :actions        1
                       :revealed-cards {:deck 1}}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [wishing-well]
                           :deck    [silver]
                           :discard [silver copper]
                           :actions 1}]}
               (play 0 :wishing-well)
               (choose :copper))
           {:supply  (base/supply 2 8)
            :players [{:hand           [silver copper]
                       :play-area      [wishing-well]
                       :deck           [silver]
                       :actions        1
                       :revealed-cards {:hand 1}}]}))
    (is (= (-> {:supply  [{:card wishing-well :pile-size 0}]
                :players [{:hand    [wishing-well]
                           :deck    [silver copper]
                           :actions 1}]}
               (play 0 :wishing-well))
           {:supply       [{:card wishing-well :pile-size 0}]
            :players      [{:hand      [silver]
                            :play-area [wishing-well]
                            :deck      [copper]
                            :actions   1}]
            :effect-stack [{:text      "Name a card."
                            :player-no 0
                            :choice    ::intrigue/wishing-well-guess
                            :source    :supply
                            :options   [:wishing-well]
                            :min       1
                            :max       1}]})))
  (is (= (-> {:supply  (base/supply 2 8)
              :players [{:hand    [wishing-well]
                         :deck    [silver]
                         :actions 1}]}
             (play 0 :wishing-well))
         {:supply  (base/supply 2 8)
          :players [{:hand      [silver]
                     :play-area [wishing-well]
                     :actions   1}]})))
