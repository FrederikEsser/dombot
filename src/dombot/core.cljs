(ns dombot.core
  (:require
    [reagent.core :as r]
    [dombot.commands :as cmd]
    [clojure.string :as string]
    [dombot.utils :as ut]))

;; -------------------------
;; Views
Tror du at det compiler nu?
(defonce state (r/atom {:sets            #{:dominion :intrigue :seaside :promos}
                        :selection       []
                        :trash-unfolded? false
                        :num-players     2
                        :players         [:frederik
                                          :kasper
                                          :jonas
                                          :marie]}))

(defn select! [option]
  (swap! state update :selection conj option))

(defn remove-idx [coll idx]
  (let [v (vec coll)
        start (subvec v 0 idx)
        end (subvec v (inc idx) (count v))]
    (vec (concat start end))))

(defn deselect! [idx]
  (swap! state update :selection remove-idx idx))

(defn button-style [& [disabled types number-of-cards]]
  (merge {:color            (if disabled :grey :black)
          :font-weight      :bold
          :background-color (cond (:duration types) "#FF9E37"
                                  (:reaction types) "#77ADE3"
                                  (:action types) "#F3EEDF"
                                  (:treasure types) "#FFE64F"
                                  (:victory types) "#9FD688"
                                  (:curse types) "#B890D7")
          :border-color     (cond
                              (zero? number-of-cards) :red
                              (:curse types) "#9F76B8"
                              (:victory types) "#6DB954"
                              (:reaction types) "#6295CE"
                              (:treasure types) "#EFD34E"
                              (:duration types) "#F1820E"
                              (:attack types) "#940000"
                              (:action types) "#DED7C4"
                              :else :grey)
          :border-width     2}
         (when (:attack types)
           {:border-style :dotted})))

(defn view-card
  ([card]
   (view-card nil card))
  ([max {:keys [name name-ui types cost set-aside number-of-cards interaction] :as card}]
   (if (map? card)
     (let [selection (:selection @state)
           num-selected (->> selection (filter #{name}) count)
           number-of-cards (if (= :choosable interaction)
                             (let [num (- (or number-of-cards 1) num-selected)]
                               (if (= 1 num) nil num))
                             number-of-cards)
           disabled (or (nil? interaction)
                        (and (= :choosable interaction)
                             (= (count selection) max)))]
       (when-not (and (= :choosable interaction)
                      (= 0 number-of-cards))
         [:div
          [:button {:style    (button-style disabled types number-of-cards)
                    :disabled disabled
                    :on-click (when interaction
                                (fn [] (case interaction
                                         :playable (swap! state assoc :game (cmd/play name))
                                         :choosable (select! name)
                                         :quick-choosable (swap! state assoc :game (cmd/choose name))
                                         :buyable (swap! state assoc :game (cmd/buy name)))))}
           (str name-ui (when cost (str " ($" cost ")")) (when set-aside (str " (" (string/join ", " set-aside) ")")) (when number-of-cards (str " x" number-of-cards)))]]))
     card)))

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

(defn home-page []
  (fn []
    (let [{:keys [sets selection trash-unfolded? num-players players]} @state]
      [:div [:h2 "Dominion"]
       "Number of players: " [:input {:type      :number
                                      :min       2
                                      :max       4
                                      :on-change (fn [event] (swap! state assoc :num-players (js/parseInt (-> event .-target .-value))))
                                      :value     num-players}]
       [set-selector sets :dominion]
       [set-selector sets :intrigue]
       [set-selector sets :seaside]
       [:div [:button {:style    (button-style)
                       :on-click (fn [] (swap! state assoc :game (cmd/start-game (take num-players players)
                                                                                 :sets sets)))}
              "Start Game"]
        (let [disabled (-> @state :game :commands :can-undo? not)]
          [:button {:style    (button-style disabled)
                    :disabled disabled
                    :on-click (fn [] (swap! state assoc :game (cmd/undo) :selection []))}
           "Undo"])]
       [:div "Supply"
        (let [supply (-> (:game @state) :supply)
              row1 (->> supply (take 4))
              row2 (->> supply (drop 4) (take 3))
              row3 (->> supply (drop 7) (take 5))
              row4 (->> supply (drop 12) (take 5))]
          [:table
           [:tbody
            (view-row row1)
            (view-row (concat [nil] row2))
            (view-row row3)
            (view-row row4)]])]
       [:div "Players"
        [:table
         [:tbody
          [:tr (map-tag :th ["Name" "Hand" "Play area" "Deck" "Discard"])]
          (->> (get-in @state [:game :players])
               (mapk (fn [{:keys               [name-ui hand play-area deck discard
                                                actions coins buys set-aside island-mat
                                                active? victory-points winner?]
                           {:keys [text
                                   options
                                   interval
                                   min
                                   max
                                   quick-choice?
                                   optional?]} :choice}]
                       [:tr
                        [:td
                         (when active? [:div "Active"])
                         [:div name-ui]]
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
                                [:div "Coins: " coins]
                                [:div "Buys: " buys]])]
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
                              (when (< 0 (:to interval))
                                [:span [:input {:type      :number
                                                :min       1
                                                :max       (dec (:to interval))
                                                :on-change (fn [event] (swap! state assoc :selection [(js/parseInt (-> event .-target .-value))]))
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
                              [:div (let [disabled (-> @state :game :commands :can-end-turn? not)]
                                      [:button {:style    (button-style disabled)
                                                :disabled disabled
                                                :on-click (fn [] (swap! state assoc :game (cmd/end-turn)))}
                                       "End Turn"])]])])
                        (when set-aside
                          [:td
                           [:div "Set aside"]
                           [:div (mapk (partial view-card max) set-aside)]])
                        (when island-mat
                          [:td
                           [:div "Island Mat"]
                           [:div (mapk (partial view-card max) island-mat)]])])))]]]
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
