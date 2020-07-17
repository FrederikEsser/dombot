(ns dombot.core
  (:require
    [reagent.core :as r]
    [dombot.commands :as cmd]
    [dombot.cards.kingdom :as kingdom]
    [clojure.string :as string]
    [dombot.utils :as ut]))

;; -------------------------
;; Views

(def all-sets (->> kingdom/kingdom-cards
                   (map :set)
                   set))

(defonce state (r/atom {:sets            all-sets
                        :setup-game?     true
                        :selection       []
                        :trash-unfolded? false
                        :num-players     2
                        :players         ["Big Johnny" "Ivor the Engine Driver" "Dirty Maggie Mae"]}))

(defn select! [option]
  (swap! state update :selection conj option))

(defn remove-idx [coll idx]
  (let [v     (vec coll)
        start (subvec v 0 idx)
        end   (subvec v (inc idx) (count v))]
    (vec (concat start end))))

(defn deselect! [idx]
  (swap! state update :selection remove-idx idx))

(defn button-style [& [disabled types number-of-cards]]
  (merge {:color            (cond disabled :grey
                                  (:night types) :white
                                  (:landmark types) "#256A3D"
                                  (:hex types) "#5A487A"
                                  :else :black)
          :font-weight      :bold
          :background-color (cond
                              (:night types) "#464040"
                              (:duration types) "#FF9E37"
                              (:reaction types) "#A8BFD3"
                              (:reserve types) "#D3B86B"
                              (:action types) "#F3EEDF"
                              (:treasure types) "#FFE64F"
                              (:victory types) "#9FD688"
                              (:curse types) "#B890D7"
                              (:artifact types) "#F9CD88"
                              (:event types) "#C6C8C5"
                              (:landmark types) "#60B574"
                              (:project types) "#FCA19A"
                              (:boon types) "#F6E359"
                              (:hex types) "#9677B3"
                              (:state types) "#F1EBEB")
          :border-color     (cond
                              (zero? number-of-cards) :red
                              (:curse types) "#9F76B8"
                              (:shelter types) "#E76F59"
                              (:victory types) "#6DB954"
                              (:treasure types) "#EFD34E"
                              (:reaction types) "#6295CE"
                              (:reserve types) "#C6A85C"
                              (:duration types) "#F1820E"
                              (:attack types) "#B40000"
                              (:action types) "#DED7C4"
                              (:night types) "#413B3B"
                              (:artifact types) "#B4763B"
                              (:event types) "#97998E"
                              (:landmark types) "#459A5D"
                              (:project types) "#EF8984"
                              (:boon types) "#AD9727"
                              (:hex types) "#5A487A"
                              (:state types) "#CE9883"
                              :else :grey)
          :border-width     2}
         (when (:attack types)
           {:border-style :dotted})))

(defn view-card
  ([card]
   (view-card nil card))
  ([max {:keys [name name-ui choice-value choice-opts types mixed-cost buy-cost set-aside number-of-cards total-number-of-cards
                interaction tokens bane?] :as card}]
   (if (map? card)
     (let [selection       (:selection @state)
           num-selected    (->> selection (filter #{name choice-value}) count)
           number-of-cards (if (and (= :choosable interaction)
                                    (not (:leave-in-play-area choice-opts)))
                             (let [num (- (or number-of-cards 1) num-selected)]
                               (if (= 1 num) nil num))
                             number-of-cards)
           disabled        (or (nil? interaction)
                               (and (= :choosable interaction)
                                    (= (count selection) max))
                               (and (:unique choice-opts)
                                    (some #{name choice-value} selection))
                               (and (:similar choice-opts)
                                    (some (comp not #{name choice-value}) selection)))]
       (when-not (and (= :choosable interaction)
                      (= 0 number-of-cards))
         [:div
          [:button {:style    (button-style disabled types number-of-cards)
                    :disabled disabled
                    :on-click (when interaction
                                (fn [] (case interaction
                                         :playable (swap! state assoc :game (cmd/play name))
                                         :choosable (select! (or choice-value name))
                                         :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name)))
                                         :buyable (swap! state assoc :game (cmd/buy name)))))}
           (str (when tokens (str (->> tokens
                                       (map (fn [{:keys [token-type number-of-tokens]}]
                                              (str "("
                                                   (when (> number-of-tokens 1) number-of-tokens)
                                                   (ut/format-name-short token-type)
                                                   ")")))
                                       (string/join " "))
                                  " "))
                name-ui
                (when bane? " - Bane")
                (when mixed-cost (str " (" (ut/format-cost mixed-cost buy-cost) ")"))
                (when set-aside (str " (" (string/join ", " set-aside) ")"))
                (when number-of-cards (str " x" number-of-cards))
                (when total-number-of-cards (str "(" total-number-of-cards ")")))]]))
     card)))

(defn view-event
  [{:keys [name name-ui type mixed-cost interaction]}]
  (let [disabled (nil? interaction)]
    [:div
     [:button {:style    (button-style disabled #{type} 1)
               :disabled disabled
               :on-click (when interaction
                           (fn [] (case interaction
                                    :buyable (swap! state assoc :game (cmd/buy-event name)))))}
      (str name-ui
           (when mixed-cost (str " (" (ut/format-cost mixed-cost) ")")))]]))

(defn view-landmark
  [{:keys [name-ui type vp-tokens chosen-cards]}]
  [:div
   [:button {:style    (button-style false #{type} 1)
             :disabled true}
    (str name-ui
         (when chosen-cards
           (str " of " (->> chosen-cards
                            (map ut/format-name)
                            (string/join "/"))))
         (when (and vp-tokens (pos? vp-tokens))
           (str " ("
                (when (> vp-tokens 1) vp-tokens)
                "VP)")))]])

(defn view-project
  [{:keys [name name-ui choice-value type cost interaction participants]}]
  (let [disabled (nil? interaction)]
    [:div
     [:button {:style    (button-style disabled #{type} 1)
               :disabled disabled
               :on-click (when interaction
                           (fn [] (case interaction
                                    :buyable (swap! state assoc :game (cmd/buy-project name))
                                    :quick-choosable (swap! state assoc :game (cmd/choose choice-value)))))}
      (str name-ui
           (when cost (str " ($" cost ")"))
           (when participants (str " " (->> participants (string/join " ")))))]]))

(defn view-boon [{:keys [name name-ui choice-value type interaction]} & [{:keys [on-click orientation]}]]
  (let [disabled    (nil? interaction)
        boon-button [:button {:style    (button-style disabled #{type} 1)
                              :on-click (if interaction
                                          (fn [] (case interaction
                                                   :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name)))))
                                          on-click)}
                     name-ui]]
    (if (= :horizontal orientation)
      boon-button
      [:div boon-button])))

(defn view-hex [{:keys [name-ui type]} & [{:keys [on-click]}]]
  [:div
   [:button {:style    (button-style false #{type} 1)
             :on-click on-click}
    name-ui]])

(defn mapk [f coll]
  (->> coll
       (map (fn [e]
              (-> e f (with-meta {:key (random-uuid)}))))
       doall))

(defn mapk-indexed [f coll]
  (->> coll
       (map-indexed (fn [i e]
                      (with-meta (f i e) {:key (random-uuid)})))
       doall))

(defn map-tag [tag coll]
  (mapk (fn [x] [tag x]) coll))

(defn view-row [row]
  [:tr (->> row
            (map view-card)
            (mapk (fn [card] [:td card])))])

(defn view-pile [pile max]
  [:div
   (mapk (partial view-card max) (:visible-cards pile))
   (when (:number-of-cards pile)
     (str (:number-of-cards pile) " Cards"))])

(defn set-selector []
  (fn [sets set-name]
    [:div
     (ut/format-name set-name)
     [:input {:type      :checkbox
              :checked   (contains? sets set-name)
              :on-change #(if (contains? sets set-name)
                            (swap! state update :sets disj set-name)
                            (swap! state update :sets conj set-name))}]]))

(defn setup-player [idx]
  [:div
   (str "Player " (inc idx) ": ")
   [:input {:type      :text
            :on-change #(swap! state assoc-in [:players idx] (-> % .-target .-value))
            :value     (get-in @state [:players idx]) #_player}]])

(defn create-game []
  (fn []
    (let [{:keys [sets num-players players]} @state]
      [:div
       [:button {:style    (button-style)
                 :on-click (fn [] (swap! state assoc
                                         :game (cmd/start-game (->> players
                                                                    (map clojure.string/trim)
                                                                    (filter not-empty)
                                                                    #_(take num-players))
                                                               :sets sets)
                                         :setup-game? false))}
        "Create game"]
       #_#_"Number of players: " [:input {:type      :number
                                          :min       2
                                          :max       4
                                          :on-change (fn [e] (swap! state assoc :num-players (js/parseInt (-> e .-target .-value))))
                                          :value     num-players}]
       (setup-player 0)
       (setup-player 1)
       (setup-player 2)
       (setup-player 3)

       #_(mapk-indexed setup-player players)

       (mapk (fn [s]
               [set-selector sets s])
             (sort all-sets))])))

(defn home-page []
  (fn []
    (let [{:keys [setup-game? selection trash-unfolded? boons-unfolded? hexes-unfolded?]} @state]
      [:div [:h2 "Dominion"]

       (when setup-game?
         [create-game])

       [:div [:button {:style    (button-style)
                       :on-click (fn [] (swap! state update :setup-game? not))}
              "Game setup"]
        [:button {:style    (button-style false)
                  :on-click (fn [] (if (js/confirm "Are you sure you want to restart the current game? All progress will be lost.")
                                     (swap! state assoc :game (cmd/restart) :selection [])))}
         "Restart"]
        (let [disabled (-> @state :game :commands :can-undo? not)]
          [:button {:style    (button-style disabled)
                    :disabled disabled
                    :on-click (fn [] (swap! state assoc :game (cmd/undo) :selection []))}
           "Undo"])]
       (when (-> @state :game :extra-cards)
         [:div "Extra cards"
          [:table
           [:tbody
            (view-row (-> @state :game :extra-cards))]]])
       [:div "Supply"
        (let [supply     (-> (:game @state) :supply)
              properity? (-> (:game @state) :prosperity?)
              [row1 supply] (split-at (if properity? 5 4) supply)
              [row2 supply] (split-at (if properity? 4 3) supply)
              [row3 row4] (split-at 5 supply)]
          [:table
           [:tbody
            (view-row row1)
            (view-row (concat [nil] row2))
            (view-row row3)
            (view-row row4)]])]
       (when (get-in @state [:game :druid-boons])
         [:div [:button {:style {:color            "#4F4D91"
                                 :font-weight      :bold
                                 :background-color "#C5E3BF"
                                 :border-color     "#788B5D"
                                 :border-width     2}}
                "Druid Boons"]
          (mapk #(view-boon % {:orientation :horizontal}) (get-in @state [:game :druid-boons]))])
       (let [{:keys [events landmarks projects boons hexes]} (:game @state)]
         (when (or events landmarks projects boons hexes)
           [:div "Landscape"
            [:table
             [:tbody
              [:tr
               (->> events
                    (map view-event)
                    (mapk (fn [event] [:td event])))
               (->> landmarks
                    (map view-landmark)
                    (mapk (fn [landmark] [:td landmark])))
               (->> projects
                    (map view-project)
                    (mapk (fn [project] [:td project])))
               (when boons
                 (let [{:keys [number-of-cards boon-discard top-boon]} boons]
                   (if (or boons-unfolded? (nil? top-boon))
                     [:td [:button {:style    {:color            "#4F4D91"
                                               :font-weight      :bold
                                               :background-color "#C5E3BF"
                                               :border-color     "#788B5D"
                                               :border-width     2}
                                    :on-click (when (not-empty boon-discard)
                                                (fn [] (swap! state assoc :boons-unfolded? false)))}
                           (str "Boons x" number-of-cards)]
                      (mapk view-boon boon-discard)]
                     [:td (view-boon top-boon {:on-click (fn [] (swap! state assoc :boons-unfolded? true))})])))
               (when hexes
                 (let [{:keys [number-of-cards hex-discard top-hex]} hexes]
                   (if (or hexes-unfolded? (nil? top-hex))
                     [:td [:button {:style    {:color            "#4F4D91"
                                               :font-weight      :bold
                                               :background-color "#D2A4CB"
                                               :border-color     "#8D4E7F"
                                               :border-width     2}
                                    :on-click (when (not-empty hex-discard)
                                                (fn [] (swap! state assoc :hexes-unfolded? false)))}
                           (str "Hexes x" number-of-cards)]
                      (mapk view-hex hex-discard)]
                     [:td (view-hex top-hex {:on-click (fn [] (swap! state assoc :hexes-unfolded? true))})])))]]]]))
       (when (-> @state :game :trade-route-mat)
         [:div "Trade Route Mat: " (-> @state :game :trade-route-mat)])
       [:div "Players"
        [:table
         [:tbody
          [:tr (map-tag :th ["Name" "Hand" "Play area" "Deck" "Discard"])]
          (->> (get-in @state [:game :players])
               (mapk (fn [{:keys               [name-ui hand play-area deck discard
                                                actions coins debt buys set-aside
                                                coffers villagers artifacts states
                                                island-mat native-village-mat pirate-ship-coins
                                                boons tavern-mat exile-mat journey-token
                                                vp-tokens active? victory-points winner?]
                           {:keys [text
                                   options
                                   interval
                                   min
                                   max
                                   quick-choice?
                                   optional?]} :choice}]
                       [:tr
                        [:td
                         (when active?
                           [:div "Active"])
                         [:div name-ui]
                         (when journey-token
                           [:div [:img {:src   (if (= :face-up journey-token)
                                                 "journey_token.png"
                                                 "face_down_token.png")
                                        :alt   (if (= :face-up journey-token)
                                                 "Journey token: Face up"
                                                 "Journey token: Face down")
                                        :width 20
                                        :align :top}]])]
                        [:td (if (:number-of-cards hand)
                               (view-pile hand max)
                               (mapk (partial view-card max) hand))]
                        [:td (mapk (partial view-card max) (as-> (group-by (comp boolean :stay-in-play) play-area) grouped-cards
                                                                 (if (< 1 (count grouped-cards))
                                                                   (concat (get grouped-cards true)
                                                                           [[:hr]]
                                                                           (get grouped-cards false))
                                                                   (-> grouped-cards vals first))))]
                        [:td (view-pile deck max)]
                        [:td (view-pile discard max)]
                        [:td (if victory-points
                               [:div
                                (when winner? [:div "WINNER!"])
                                [:div "Victory Points: " victory-points]]
                               [:div
                                [:div "Actions: " actions]
                                (when villagers
                                  (let [{:keys [number interaction]} villagers
                                        disabled (nil? interaction)]
                                    [:div [:button {:style    (button-style disabled #{:action} nil)
                                                    :disabled disabled
                                                    :on-click (when interaction
                                                                (fn [] (case interaction
                                                                         :spendable (swap! state assoc :game (cmd/spend-villager)))))}
                                           (str number " Villager" (when (< 1 number) "s"))]]))
                                [:div "Coins: " coins]
                                (when debt
                                  [:div "Debt: " debt])
                                (when coffers
                                  (let [{:keys [number interaction]} coffers
                                        disabled (nil? interaction)]
                                    [:div [:button {:style    (button-style disabled #{:treasure} nil)
                                                    :disabled disabled
                                                    :on-click (when interaction
                                                                (fn [] (case interaction
                                                                         :spendable (swap! state assoc :game (cmd/spend-coffer)))))}
                                           (str number " Coffer" (when (< 1 number) "s"))]]))
                                [:div "Buys: " buys]
                                (mapk view-boon boons)
                                (when pirate-ship-coins
                                  [:div "Pirate Ship: " pirate-ship-coins])
                                (when vp-tokens
                                  [:div "Victory Points: " vp-tokens])
                                (when artifacts
                                  (mapk view-card artifacts))
                                (when states
                                  (mapk view-card states))])]
                        (if text
                          [:td text
                           [:div (mapk (fn [{:keys [option text]}]
                                         (let [disabled (and (not quick-choice?)
                                                             (or (= max (count selection))
                                                                 (-> selection set option)))]
                                           [:button {:style    (button-style disabled)
                                                     :disabled disabled
                                                     :on-click (fn [] (if quick-choice?
                                                                        (swap! state assoc :game (cmd/choose option))
                                                                        (select! option)))}
                                            text])) options)]
                           (when interval
                             [:div [:button {:style    (button-style)
                                             :on-click (fn [] (swap! state assoc
                                                                     :game (cmd/choose 0)
                                                                     :selection []))}
                                    "Top"]
                              (when (pos? (:to interval))
                                [:span [:input {:type      :number
                                                :min       1
                                                :max       (dec (:to interval))
                                                :on-change (fn [e] (swap! state assoc :selection [(js/parseInt (-> e .-target .-value))]))
                                                :value     (or (-> @state :selection first) 0)}]
                                 [:button {:style    (button-style)
                                           :on-click (fn [] (swap! state assoc
                                                                   :game (cmd/choose (:to interval))
                                                                   :selection []))}
                                  "Bottom"]])])
                           (when (or (not quick-choice?) interval)
                             [:div
                              (when (< 1 max)
                                [:div "Selected: " (mapk-indexed (fn [idx selected]
                                                                   [:button {:style    (button-style)
                                                                             :on-click (fn [] (deselect! idx))}
                                                                    (ut/format-name selected)]) selection)])
                              (let [disabled (and min (< (count selection) min)
                                                  (not (and optional? (empty? selection))))]
                                [:button {:style    (button-style disabled)
                                          :disabled disabled
                                          :on-click (fn [] (swap! state assoc
                                                                  :game (cmd/choose selection)
                                                                  :selection []))}
                                 "Done"])])]
                          [:td
                           (when active?
                             [:div
                              [:div (let [disabled (-> @state :game :commands :can-play-treasures? not)]
                                      [:button {:style    (button-style disabled)
                                                :disabled disabled
                                                :on-click (fn [] (swap! state assoc :game (cmd/play-treasures)))}
                                       "Play Treasures"])]
                              (when (get-in @state [:game :commands :can-goto-buy-phase?])
                                [:div
                                 [:button {:style    (button-style false)
                                           :on-click (fn [] (swap! state assoc :game (cmd/goto-buy-phase)))}
                                  "=> Buy Phase"]])
                              [:div (let [disabled     (-> @state :game :commands :can-end-turn? not)
                                          confirm-text (-> @state :game :commands :confirm-end-turn)]
                                      [:button {:style    (button-style disabled)
                                                :disabled disabled
                                                :on-click (fn [] (if (or (not confirm-text)
                                                                         (js/confirm (str confirm-text
                                                                                          "\nAre you sure you want to end your turn?")))
                                                                   (swap! state assoc :game (cmd/end-turn))))}
                                       "End Turn"])]])])
                        (when exile-mat
                          [:td
                           [:div "Exile"]
                           [:div (mapk (partial view-card max) exile-mat)]])
                        (when set-aside
                          [:td
                           [:div "Set aside"]
                           [:div (mapk (partial view-card max) set-aside)]])
                        (when tavern-mat
                          [:td
                           [:div "Tavern"]
                           [:div (mapk (partial view-card max) tavern-mat)]])
                        (when island-mat
                          [:td
                           [:div "Island"]
                           [:div (mapk (partial view-card max) island-mat)]])
                        (when native-village-mat
                          [:td
                           [:div "Native Village"]
                           [:div (if (:number-of-cards native-village-mat)
                                   (view-pile native-village-mat max)
                                   (mapk view-card native-village-mat))]])])))]]]
       (let [{:keys [compact full]} (get-in @state [:game :trash])]
         [:div "Trash " [:button {:on-click (fn [] (swap! state update :trash-unfolded? not))}
                         (if trash-unfolded? "Hide" "Show")]
          [:table
           [:tbody
            (if trash-unfolded?
              [:tr [:td (mapk view-card full)]]
              [:tr
               [:td (view-pile compact nil)]])]]])])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
