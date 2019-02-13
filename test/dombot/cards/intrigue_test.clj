(ns dombot.cards.intrigue-test
  (:require [clojure.test :refer :all]
            [dombot.test-utils :refer :all]
            [dombot.operations :refer :all]
            [dombot.cards.base-cards :as base :refer :all]
            [dombot.cards.dominion :as dominion :refer [throne-room]]
            [dombot.cards.common :refer :all]
            [dombot.cards.intrigue :as intrigue :refer :all]
            [dombot.utils :as ut]))

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
              :players [{:hand      []
                         :play-area [baron]
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
              :effect-stack [{:text      "You may discard an Estate for +$4. If you don't, gain an Estate."
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
              :players [{:hand      []
                         :play-area [baron]
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
              :players [{:hand      []
                         :play-area [baron]
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
            :players         [{:hand      []
                               :play-area [bridge]
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
            :players         [{:hand      []
                               :play-area [bridge]
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
            :players         [{:hand      []
                               :play-area [throne-room bridge]
                               :actions   0
                               :buys      3
                               :coins     2}]}))))

(deftest courtyard-test
  (testing "Courtyard"
    (is (= (play {:players [{:hand    [courtyard]
                             :deck    [copper copper copper]
                             :actions 1}]}
                 0 :courtyard)
           {:players      [{:hand      [copper copper copper]
                            :play-area [courtyard]
                            :deck      []
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
  (testing "Diplimat"
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
                         :deck      []
                         :actions   2}]})))
    (testing "Reaction"
      (is (= (-> {:players [{:hand    [minion]
                             :actions 1}
                            {:hand [diplomat estate estate estate silver]
                             :deck [copper copper]}]}
                 (play 0 :minion))
             {:players      [{:hand      []
                              :play-area [minion]
                              :actions   0}
                             {:hand [diplomat estate estate estate silver]
                              :deck [copper copper]}]
              :effect-stack [{:text      "You may reveal a Reaction to react to the Minion Attack."
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
                                                                  {:option :discard :text "Discard your hand, +4 Cards, and each other player with at least 5 cards in hand discards their hand and draws 4 cards."}]
                                                        :min     1
                                                        :max     1}]}]}))
      (is (= (-> {:players [{:hand    [minion]
                             :actions 1}
                            {:hand [diplomat estate estate estate silver]
                             :deck [copper copper]}]}
                 (play 0 :minion)
                 (choose :diplomat))
             {:players      [{:hand      []
                              :play-area [minion]
                              :actions   0}
                             {:hand [diplomat estate estate estate silver copper copper]
                              :deck []}]
              :effect-stack [{:text      "Discard 3 cards."
                              :player-no 1
                              :choice    :discard-from-hand
                              :source    :hand
                              :options   [:diplomat :estate :estate :estate :silver :copper :copper]
                              :min       3
                              :max       3}
                             {:player-no 0
                              :effect    [:give-actions 1]}
                             {:player-no 0
                              :effect    [:give-choice {:text    "Choose one:"
                                                        :choice  ::intrigue/minion-choice
                                                        :options [:special
                                                                  {:option :coins :text "+$2"}
                                                                  {:option :discard :text "Discard your hand, +4 Cards, and each other player with at least 5 cards in hand discards their hand and draws 4 cards."}]
                                                        :min     1
                                                        :max     1}]}]}))
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
                         :deck    []
                         :discard [estate estate estate]}]}))
      (is (= (-> {:supply  (base/supply 2 8)
                  :players [{:hand      [minion]
                             :play-area []
                             :actions   1
                             :coins     0}
                            {:hand [diplomat silver copper copper]
                             :deck [estate]}]}
                 (play 0 :minion)
                 (choose :coins))
             {:supply  (base/supply 2 8)
              :players [{:hand      []
                         :play-area [minion]
                         :actions   1
                         :coins     2}
                        {:hand [diplomat silver copper copper]
                         :deck [estate]}]})))))

(deftest harem-test
  (testing "Harem"
    (is (= (-> {:players [{:hand  [harem]
                           :coins 0}]}
               (play 0 :harem))
           {:players [{:hand      []
                       :play-area [harem]
                       :coins     2}]}))
    (is (= (calc-victory-points {:deck [harem]})
           2))))

(deftest lurker-test
  (let [upgrade (assoc upgrade :id 1)]
    (testing "Lurker"
      (is (= (-> {:players [{:hand    [lurker]
                             :actions 1}]}
                 (play 0 :lurker))
             {:players      [{:hand      []
                              :play-area [lurker]
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
              :players      [{:hand      []
                              :play-area [lurker]
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
              :players [{:hand      []
                         :play-area [lurker]
                         :actions   1}]
              :trash   [upgrade]}))
      (is (= (-> {:players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [upgrade copper estate]}
                 (play 0 :lurker)
                 (choose :gain))
             {:players      [{:hand      []
                              :play-area [lurker]
                              :actions   1}]
              :trash        [upgrade copper estate]
              :effect-stack [{:text      "Gain an Action card from the trash."
                              :player-no 0
                              :choice    :gain-from-trash
                              :source    :trash
                              :options   [:upgrade]
                              :min       1
                              :max       1}]}))
      (is (= (-> {:players [{:hand    [lurker]
                             :actions 1}]
                  :trash   [upgrade copper estate]}
                 (play 0 :lurker)
                 (choose :gain)
                 (choose :upgrade))
             {:players [{:hand      []
                         :play-area [lurker]
                         :discard   [upgrade]
                         :actions   1}]
              :trash   [copper estate]})))))

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
             {:players [{:hand      [silver]
                         :play-area []
                         :deck      [copper]
                         :actions   2
                         :coins     2}]
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
                         :deck      []
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
                         :deck      []
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
                         :deck      []
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
                         :deck      []
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
                         :deck      []
                         :actions   8
                         :coins     2}]
              :trash   [mining-village-2]}))
      (is (thrown-with-msg? AssertionError #"Card has no id, but is referring to :this in :play-area."
                            (-> {:players [{:hand    [mining-village]
                                            :deck    [copper]
                                            :actions 1}]}
                                (play 0 :mining-village)))))))

(deftest minion-test
  (testing "Minion"
    (is (= (-> {:players [{:hand    [minion]
                           :actions 1}]}
               (play 0 :minion))
           {:players      [{:hand      []
                            :play-area [minion]
                            :actions   1}]
            :effect-stack [{:text      "Choose one:"
                            :player-no 0
                            :choice    ::intrigue/minion-choice
                            :source    :special
                            :options   [{:option :coins :text "+$2"}
                                        {:option :discard :text "Discard your hand, +4 Cards, and each other player with at least 5 cards in hand discards their hand and draws 4 cards."}]
                            :min       1
                            :max       1}]}))
    (is (= (-> {:players [{:hand    [minion]
                           :actions 1
                           :coins   0}]}
               (play 0 :minion)
               (choose :coins))
           {:players [{:hand      []
                       :play-area [minion]
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
           {:players      [{:hand      []
                            :play-area [nobles]
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
           {:players [{:hand      []
                       :play-area [nobles]
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
                            :max       1}]}))
    (is (= (calc-victory-points {:deck [nobles]})
           2))))

(deftest pawn-test
  (testing "Pawn"
    (is (= (-> {:players [{:hand    [pawn]
                           :actions 1}]}
               (play 0 :pawn))
           {:players      [{:hand      []
                            :play-area [pawn]
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
           {:players [{:hand      []
                       :deck      [copper estate]
                       :play-area [pawn]
                       :actions   1
                       :buys      2}]}))
    (is (= (-> {:players [{:hand    [pawn]
                           :deck    [copper estate]
                           :actions 1
                           :coins   0}]}
               (play 0 :pawn)
               (choose [:action :coin]))
           {:players [{:hand      []
                       :deck      [copper estate]
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
           {:players [{:hand      []
                       :deck      [copper estate]
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
           {:players [{:hand           [copper copper copper silver silver]
                       :play-area      [shanty-town shanty-town]
                       :deck           [estate]
                       :actions        3
                       :revealed-cards {}}]}))))

(deftest steward-test
  (testing "Steward"
    (is (= (-> {:players [{:hand    [steward]
                           :actions 1}]}
               (play 0 :steward))
           {:players      [{:hand      []
                            :play-area [steward]
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
           {:players [{:hand      []
                       :deck      [copper estate]
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
            :players      [{:hand      []
                            :play-area [swindler]
                            :actions   0
                            :coins     2}
                           {:deck []}]
            :effect-stack [{:text      "Gain a card costing $0 (attacker chooses)."
                            :player-no 1
                            :choice    :gain
                            :source    :supply
                            :options   [:curse :copper]
                            :min       1
                            :max       1}]
            :trash        [copper]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [swindler]
                           :actions 1
                           :coins   0}
                          {:discard [copper]}]}
               (play 0 :swindler))
           {:supply       (base/supply 2 8)
            :players      [{:hand      []
                            :play-area [swindler]
                            :actions   0
                            :coins     2}
                           {:deck    []
                            :discard []}]
            :effect-stack [{:text      "Gain a card costing $0 (attacker chooses)."
                            :player-no 1
                            :choice    :gain
                            :source    :supply
                            :options   [:curse :copper]
                            :min       1
                            :max       1}]
            :trash        [copper]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [swindler]
                           :actions 1
                           :coins   0}
                          {:deck []}]}
               (play 0 :swindler))
           {:supply  (base/supply 2 8)
            :players [{:hand      []
                       :play-area [swindler]
                       :actions   0
                       :coins     2}
                      {:deck []}]}))
    (is (= (-> {:supply  (base/supply 2 8)
                :players [{:hand    [swindler]
                           :actions 1
                           :coins   0}
                          {:deck [mining-village]}]}
               (play 0 :swindler))
           {:supply  (base/supply 2 8)
            :players [{:hand      []
                       :play-area [swindler]
                       :actions   0
                       :coins     2}
                      {:deck []}]
            :trash   [mining-village]}))
    (let [curse (assoc curse :id 1)
          copper (assoc copper :id 2)]
      (is (= (-> {:supply  [{:card curse :pile-size 10}]
                  :players [{:hand    [swindler]
                             :actions 1
                             :coins   0}
                            {:deck [copper]}]}
                 (play 0 :swindler)
                 (choose :curse))
             {:supply  [{:card curse :pile-size 9}]
              :players [{:hand      []
                         :play-area [swindler]
                         :actions   0
                         :coins     2}
                        {:deck    []
                         :discard [curse]}]
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
              :players      [{:hand      []
                              :play-area [swindler]
                              :actions   0
                              :coins     2}
                             {:deck    []
                              :discard [curse]}
                             {:deck []}]
              :effect-stack [{:text      "Gain a card costing $0 (attacker chooses)."
                              :player-no 2
                              :choice    :gain
                              :source    :supply
                              :options   [:copper]
                              :min       1
                              :max       1}]
              :trash        [copper curse]})))))

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
             {:players [{:hand      []
                         :play-area [trading-post]
                         :actions   0}]}))
      (is (= (-> {:players [{:hand    [trading-post copper]
                             :actions 1}]}
                 (play 0 :trading-post)
                 (choose :copper))
             {:players [{:hand      []
                         :play-area [trading-post]
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
                              :deck      []
                              :actions   1}]
              :effect-stack [{:text      "Trash a card from your hand."
                              :player-no 0
                              :choice    ::intrigue/upgrade-trash
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
                              :deck      []
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
                         :deck      []
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
                         :deck      []
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
                                   :deck      []
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
                                   :deck      []
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
                       :deck           []
                       :actions        1
                       :revealed       []
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
                       :revealed       []
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
                       :discard        []
                       :actions        1
                       :revealed       []
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
                       :discard        []
                       :actions        1
                       :revealed       []
                       :revealed-cards {:hand 1}}]}))))
