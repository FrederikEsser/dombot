(ns dombot.front-end-view
  (:require [dombot.utils :as ut]
            [dombot.specs :as specs]
            [clojure.spec.alpha :as s]))

(defn- choice-interaction [name area {:keys [source options max choice-opts]}]
  (let [interaction (if (= 1 (or max (count options)))
                      {:interaction :quick-choosable}
                      (merge {:interaction :choosable}
                             (when choice-opts
                               {:choice-opts choice-opts})))]
    (cond
      (and (= area source) ((set options) name)) interaction
      (= :mixed source) (let [card-names (->> options
                                              (filter (comp #{area} :area))
                                              (map :card-name)
                                              set)]
                          (when (card-names name)
                            (merge interaction
                                   {:choice-value {:area area :card-name name}}))))))

(defn- get-coins [{:keys [coins debt]
                   :or   {debt 0}}]
  (merge {:coins (max (- coins debt) 0)}
         (when (> debt coins)
           {:debt (- debt coins)})))

(defn view-card [{{:keys [buys player-no phase]
                   :as   player} :player
                  choice         :choice
                  :as            game}
                 {{:keys [name bane?] :as card} :card
                  :keys                         [tokens pile-size total-pile-size]
                  :or                           {pile-size 0}}]
  (let [{:keys [coins debt]} (get-coins player)
        types (ut/get-types game card)
        cost  (ut/get-cost game card)
        {:keys [coin-cost] :as buy-cost} (ut/get-buy-cost game player-no card)]
    (merge {:name            name
            :name-ui         (ut/format-name name)
            :types           types
            :mixed-cost      cost
            :number-of-cards pile-size}
           (when (and total-pile-size
                      (> total-pile-size pile-size))
             {:total-number-of-cards total-pile-size})
           (when (not= cost buy-cost)
             {:buy-cost buy-cost})
           (when (and (#{:action :pay :buy} phase)
                      (not choice)
                      (pos? pile-size)
                      buys (pos? buys)
                      coins (>= coins coin-cost)
                      (not debt)
                      (ut/card-buyable? game player-no card))
             {:interaction :buyable})
           (choice-interaction name :supply choice)
           (when tokens
             {:tokens (->> tokens
                           (map (fn [[token {:keys [number-of-tokens]}]]
                                  {:token-type       token
                                   :number-of-tokens number-of-tokens})))})
           (when bane?
             {:bane? true}))))

(defn view-supply [{:keys [supply choice] :as game}]
  (->> supply
       (map (fn [{:keys [split-pile hidden?] :as pile}]
              (let [top-card  (view-card game (ut/access-top-card pile))
                    all-cards (some->> split-pile
                                       (filter :pile-size)
                                       (map (fn [{:keys [card pile-size]
                                                  :or   {pile-size 0}}]
                                              (let [{:keys [name bane?]} card
                                                    types (ut/get-types game card)
                                                    cost  (ut/get-cost game card)]
                                                (if (= name (:name top-card))
                                                  (dissoc top-card
                                                          :total-number-of-cards
                                                          (when hidden? :number-of-cards))
                                                  (merge {:name       name
                                                          :name-ui    (ut/format-name name)
                                                          :types      types
                                                          :mixed-cost cost}
                                                         (when-not hidden?
                                                           {:number-of-cards pile-size})
                                                         (choice-interaction name :supply choice)
                                                         (when bane?
                                                           {:bane? true})))))))]
                (merge {:card top-card}
                       (when split-pile
                         {:cards (cond-> all-cards
                                         hidden? (->> distinct
                                                      (sort-by :name-ui)))})))))))

(defn view-extra-cards [{extra-cards :extra-cards
                         choice      :choice
                         :as         game}]
  (->> extra-cards
       (map (fn [{{:keys [name] :as card} :card
                  number-of-cards         :pile-size}]
              (let [types (ut/get-types game card)
                    cost  (ut/get-cost game card)]
                {:card (merge {:name            name
                               :name-ui         (ut/format-name name)
                               :types           types
                               :mixed-cost      cost
                               :number-of-cards number-of-cards}
                              (choice-interaction name :extra-cards choice))})))))

(defn view-non-pile-cards [{non-pile-cards :non-pile-cards
                            choice         :choice
                            :as            game}]
  (->> non-pile-cards
       (map (fn [{:keys [name] :as card}]
              (let [types (ut/get-types game card)
                    cost  (ut/get-cost game card)]
                {:card (merge {:name       name
                               :name-ui    (ut/format-name name)
                               :types      types
                               :mixed-cost cost}
                              (choice-interaction name :non-pile-cards choice))})))))

(defn view-events [{events         :events
                    {:keys [buys phase bought-events]
                     :as   player} :player
                    choice         :choice}]
  (let [{:keys [coins debt]} (get-coins player)]
    (->> events
         vals
         (map (fn [{:keys [name type cost]}]
                (let [{:keys [coin-cost] :as mixed-cost} (ut/normalize-cost cost)]
                  (merge {:name       name
                          :name-ui    (ut/format-name name)
                          :type       type
                          :mixed-cost mixed-cost}
                         (when (and (#{:action :pay :buy} phase)
                                    (not choice)
                                    buys (pos? buys)
                                    coins (<= coin-cost coins)
                                    (not debt)
                                    (not (contains? bought-events name)))
                           {:interaction :buyable}))))))))

(defn view-landmarks [{:keys [landmarks]}]
  (->> landmarks
       vals
       (map (fn [{:keys [name type vp-tokens chosen-cards]}]
              (merge {:name    name
                      :name-ui (ut/format-name name)
                      :type    type}
                     (when vp-tokens
                       {:vp-tokens vp-tokens})
                     (when chosen-cards
                       {:chosen-cards chosen-cards}))))))

(defn view-projects [{projects       :projects
                      {:keys [buys player-no phase]
                       :as   player} :player
                      choice         :choice
                      :as            game}]
  (let [{:keys [coins debt]} (get-coins player)]
    (->> projects
         vals
         (map (fn [{:keys [name type cost participants]}]
                (merge {:name    name
                        :name-ui (ut/format-name name)
                        :type    type
                        :cost    cost}
                       (when (not-empty participants)
                         {:participants (->> participants
                                             (map (fn [{:keys [player-no tokens]}]
                                                    (str "["
                                                         (-> (get-in game [:players player-no :name])
                                                             ut/format-name-short)
                                                         (when tokens (str ":" tokens))
                                                         "]"))))})
                       (when (and (#{:action :pay :buy} phase)
                                  (not choice)
                                  (not-any? (comp #{player-no} :player-no) participants)
                                  buys (pos? buys)
                                  (not debt)
                                  coins (<= cost coins))
                         {:interaction :buyable})
                       (choice-interaction name :projects choice)))))))

(defn view-boon
  ([boon]
   (view-boon nil nil boon))
  ([choice area {:keys [name type]}]
   (merge {:name    name
           :name-ui (ut/format-name name)
           :type    type}
          (choice-interaction name area choice))))

(defn view-boons [{:keys [deck discard]}]
  (let [boon-discard (->> discard
                          reverse
                          (map view-boon))]
    (merge {:number-of-cards (count deck)
            :boon-discard    boon-discard}
           (when (not-empty boon-discard)
             {:top-boon (first boon-discard)}))))

(defn view-hex
  ([hex]
   (view-hex nil hex))
  ([choice {:keys [name type]}]
   {:name    name
    :name-ui (ut/format-name name)
    :type    type}))

(defn view-hexes [{:keys [deck discard]}]
  (let [hex-discard (->> discard
                         reverse
                         (map view-hex))]
    (merge {:number-of-cards (count deck)
            :hex-discard     hex-discard}
           (when (not-empty hex-discard)
             {:top-hex (first hex-discard)}))))

(defn- types-sort-order [types]
  (cond (:action types) 1
        (:treasure types) 2
        (:night types) 3
        (:curse types) 4
        (:victory types) 5
        :else 6))

(defn view-area [area {{:keys [phase actions player-no triggers] :as player} :player
                       choice                                                :choice
                       active?                                               :active-player?
                       :as                                                   game}
                 & [position number-of-cards]]
  (let [take-fn (if (= :bottom position) take-last take)
        cards   (cond->> (get player area)
                         number-of-cards (take-fn number-of-cards))]
    (-> cards
        (->>
          (map (fn [{:keys [id name] :as card}]
                 (let [types     (ut/get-types game card)
                       set-aside (->> triggers
                                      (filter (comp #{id} :card-id))
                                      (mapcat :set-aside))]
                   (merge {:name    name
                           :name-ui (ut/format-name name)
                           :types   types}
                          (when (ut/stay-in-play game player-no card)
                            {:stay-in-play true})
                          (when (and (= :hand area)
                                     (not choice)
                                     (or (and (:action types)
                                              (#{:action} phase)
                                              (pos? actions))
                                         (and (:treasure types)
                                              (#{:action :pay} phase))
                                         (and (:night types)
                                              (#{:action :pay :buy :night} phase))))
                            {:interaction :playable})
                          (choice-interaction name area choice)
                          (when (not-empty set-aside)
                            {:set-aside (map (if active? (comp ut/format-name :name) (constantly "Card")) set-aside)})))))
          frequencies
          (map (fn [[card number-of-cards]]
                 (cond-> card
                         (< 1 number-of-cards) (assoc :number-of-cards number-of-cards)))))
        (cond->> (not number-of-cards) (sort-by (juxt (comp types-sort-order (partial ut/get-types game))
                                                      :name))))))

(defn view-hand [{active-player?                      :active-player?
                  {:keys [hand revealed-cards phase]} :player
                  {:keys [hide-hand?] :as choice}     :choice
                  :as                                 data}]
  (let [revealed-cards-in-hand (:hand revealed-cards)]
    (if (and (or active-player?
                 (= revealed-cards-in-hand (count hand))
                 choice
                 (= :end-of-game phase))
             (not hide-hand?))
      (view-area :hand data)
      (if (empty? hand)
        {}
        (merge {:number-of-cards (count hand)}
               (when revealed-cards-in-hand
                 {:visible-cards (view-area :hand data revealed-cards-in-hand)}))))))

(defn view-deck [{{:keys [deck
                          look-at
                          revealed
                          revealed-cards]} :player
                  :as                      data}]
  (let [full-deck              (concat look-at revealed deck)
        revealed-cards-in-deck (:deck revealed-cards)]
    (if (empty? full-deck)
      {}
      (merge {:number-of-cards (count full-deck)}
             (when (or (not-empty look-at)
                       (not-empty revealed)
                       revealed-cards-in-deck)
               {:visible-cards (concat (view-area :look-at data)
                                       (view-area :revealed data)
                                       (when revealed-cards-in-deck
                                         (view-area :deck data :top revealed-cards-in-deck)))})))))

(defn view-discard [{{:keys [discard
                             gaining
                             approx-discard-size
                             revealed-cards]} :player
                     :as                      data}]
  (let [full-discard (concat gaining discard)]
    (if (empty? full-discard)
      {}
      (let [revealed-cards-in-discard (or (:discard revealed-cards)
                                          1)]
        {:visible-cards   (concat (view-area :gaining data)
                                  (view-area :discard data :bottom revealed-cards-in-discard))
         :number-of-cards (max approx-discard-size revealed-cards-in-discard)}))))

(defn view-options [options]
  (->> options
       (map (fn [option] (select-keys option [:option :text])))))

(defn view-choice [{:keys [text source options min max optional?] :as choice}]
  (->> (merge {:text          text
               :min           (or min 0)
               :max           (or max (count options))
               :quick-choice? (and (= 1 min (or max (count options)))
                                   (not optional?))}
              (case source
                :special {:options (view-options options)}
                :overpay {:options (->> options
                                        (map (fn [o] {:option o
                                                      :text   (str o)})))}
                :deck-position {:interval {:from (first options)
                                           :to   (last options)}}
                {})
              (when-not (nil? optional?)
                {:optional? optional?}))
       (s/assert* ::specs/choice)))

(defn view-score [{:keys [card number-of-cards landmark state] :as score}]
  (merge
    (select-keys score [:description :vp-per-card :number-of-cards :victory-points :notes])
    (when card {:card (merge (select-keys card [:name :types])
                             {:name-ui (ut/format-name (:name card))}
                             (when (and number-of-cards
                                        (< 1 number-of-cards))
                               {:number-of-cards number-of-cards}))})
    (when landmark {:landmark (merge (select-keys landmark [:name :type :chosen-cards])
                                     {:name-ui (ut/format-name (:name landmark))})})
    (when state {:state {:name    (:name state)
                         :name-ui (ut/format-name (:name state))
                         :types   #{(:type state)}}})))

(defn view-player [{{:keys [name
                            phase
                            actions
                            buys
                            set-aside
                            island-mat
                            native-village-mat
                            tavern-mat
                            exile
                            pirate-ship-coins
                            vp-tokens
                            journey-token
                            coffers
                            villagers
                            boons
                            states
                            score
                            victory-points
                            winner]
                     :as   player} :player
                    choice         :choice
                    active-player? :active-player?
                    artifacts      :artifacts
                    :as            data}]
  (merge {:name-ui   (ut/format-name name)
          :hand      (view-hand data)
          :play-area (concat (view-area :play-area-duration data)
                             (view-area :play-area data))
          :deck      (view-deck data)
          :discard   (view-discard data)
          :actions   actions
          :buys      buys
          :active?   active-player?}
         (get-coins player)
         (when (not-empty set-aside)
           {:set-aside (view-area :set-aside data)})
         (when (not-empty island-mat)
           {:island-mat (view-area :island-mat data)})
         (when (not-empty native-village-mat)
           {:native-village-mat (if (or active-player? choice)
                                  (view-area :native-village-mat data)
                                  {:number-of-cards (count native-village-mat)})})
         (when (not-empty tavern-mat)
           {:tavern-mat (view-area :tavern-mat data)})
         (when (not-empty exile)
           {:exile-mat (view-area :exile data)})
         (when pirate-ship-coins
           {:pirate-ship-coins pirate-ship-coins})
         (when vp-tokens
           {:vp-tokens vp-tokens})
         (when journey-token
           {:journey-token journey-token})
         (when (and coffers (pos? coffers))
           {:coffers (merge {:number coffers}
                            (when (and (not choice)
                                       (#{:action :pay} phase))
                              {:interaction :spendable}))})
         (when (and villagers (pos? villagers))
           {:villagers (merge {:number villagers}
                              (when (and (not choice)
                                         (#{:action} phase))
                                {:interaction :spendable}))})
         (when (not-empty artifacts)
           {:artifacts (->> artifacts (map (fn [{:keys [name type]}]
                                             (merge {:name    name
                                                     :name-ui (ut/format-name name)
                                                     :types   #{type}}
                                                    (choice-interaction name :artifacts choice)))))})
         (when (not-empty states)
           {:states (->> states (map (fn [{:keys [name type]}]
                                       {:name    name
                                        :name-ui (ut/format-name name)
                                        :types   #{type}})))})
         (when choice
           {:choice (view-choice choice)})
         (when boons
           {:boons (map (partial view-boon choice :boons) boons)})
         (when score
           {:score          (map view-score score)
            :victory-points victory-points})
         (when-not (nil? winner)
           {:winner? winner})))

(defn view-trash [{:keys [trash choice] :as game}]
  (merge
    (when (not-empty trash)
      {:card (let [{:keys [name] :as card} (last trash)
                   types (ut/get-types game card)]
               {:name    name
                :name-ui (ut/format-name name)
                :types   types})})
    {:cards           (if (empty? trash)
                        []
                        (->> trash
                             (map (fn [{:keys [name] :as card}]
                                    (let [types (ut/get-types game card)]
                                      (merge {:name    name
                                              :name-ui (ut/format-name name)
                                              :types   types}
                                             (choice-interaction name :trash choice)))))
                             frequencies
                             (map (fn [[card number-of-cards]]
                                    (cond-> card
                                            (< 1 number-of-cards) (assoc :number-of-cards number-of-cards))))))
     :number-of-cards (count trash)}))

(defn view-commands [{:keys [players effect-stack current-player can-undo?] :as game}]
  (let [{:keys [hand phase actions coins debt buys]} (get players current-player)
        [choice] effect-stack
        can-play-treasures? (boolean (and (not choice)
                                          (#{:action :pay} phase)
                                          (some (comp :treasure (partial ut/get-types game)) hand)))
        potential-coins     (cond-> coins
                                    debt (- debt)
                                    can-play-treasures? (+ (->> hand (keep :coin-value) (apply +))))]
    {:can-undo?           (boolean can-undo?)
     :can-goto-buy-phase? (boolean (and (= :action phase)
                                        (pos? actions)
                                        (some (fn [card]
                                                (let [types (ut/get-types game card)]
                                                  (and (:action types)
                                                       (or (:treasure types)
                                                           (:night types)))))
                                              hand)))
     :can-play-treasures? can-play-treasures?
     :can-end-turn?       (and (not choice)
                               (not= phase :end-of-game))
     :confirm-end-turn    (cond (and (= :action phase)
                                     (pos? actions)
                                     (some (comp :action (partial ut/get-types game)) hand)) "You can still play actions."
                                (and (#{:action :pay :buy} phase)
                                     (pos? buys)
                                     (<= 3 potential-coins)) "You can buy a card."
                                (some (comp :night (partial ut/get-types game)) hand) "You can play Night cards.")}))

(defn view-game [{:keys [supply extra-cards non-pile-cards artifacts events landmarks projects druid-boons boons hexes
                         trade-route-mat players effect-stack current-player] :as game}]
  (let [[{:keys [player-no] :as choice}] effect-stack
        {:keys [phase] :as player} (get players current-player)]
    (->> (merge
           {:supply      (view-supply (merge game {:player (assoc player :player-no current-player)
                                                   :choice choice}))
            :prosperity? (->> supply (some (comp #{:platinum :colony} :name :card)) boolean)
            :ruins?      (->> supply (some (comp #{:ruins} :name :card last :split-pile)) boolean)
            :players     (->> players
                              (map-indexed (fn [idx player]
                                             (let [active-player? (and (= idx current-player)
                                                                       (or (nil? choice)
                                                                           (= idx player-no))
                                                                       (not= phase :end-of-game))]
                                               (view-player (merge game {:active-player? active-player?
                                                                         :player         (assoc player :player-no idx)
                                                                         :artifacts      (->> artifacts vals (filter (comp #{idx} :owner)))}
                                                                   (when (= idx player-no)
                                                                     {:choice choice})))))))
            :trash       (view-trash (merge game {:choice choice}))
            :commands    (view-commands game)}
           (when extra-cards
             {:extra-cards (view-extra-cards (merge game {:player (assoc player :player-no current-player)
                                                          :choice choice}))})
           (when non-pile-cards
             {:non-pile-cards (view-non-pile-cards (merge game {:player (assoc player :player-no current-player)
                                                                :choice choice}))})
           (when trade-route-mat
             {:trade-route-mat trade-route-mat})
           (when events
             {:events (view-events (merge game {:player (assoc player :player-no current-player)
                                                :choice choice}))})
           (when landmarks
             {:landmarks (view-landmarks game)})
           (when projects
             {:projects (view-projects (merge game {:player (assoc player :player-no current-player)
                                                    :choice choice}))})
           (when druid-boons
             {:druid-boons (map (partial view-boon choice :druid-boons) druid-boons)})
           (when boons
             {:boons (view-boons boons)})
           (when hexes
             {:hexes (view-hexes hexes)}))
         (s/assert* ::specs/game))))