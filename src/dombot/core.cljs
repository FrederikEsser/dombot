(ns dombot.core
  (:require
    [reagent.core :as r]
    [dombot.commands :as cmd]
    [clojure.string :as string]
    [dombot.utils :as ut]))

;; -------------------------
;; Views

(defonce state (r/atom {:sets            #{:dominion :intrigue :promos}
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
          :background-color (cond (:action types) "#F3EEDF"
                                  (:treasure types) "#FFE64F"
                                  (:reaction types) "#77ADE3"
                                  (:victory types) "#9FD688"
                                  (:curse types) "#B890D7")
          :border-color     (cond
                              (zero? number-of-cards) :red
                              (:curse types) "#9F76B8"
                              (:victory types) "#6DB954"
                              (:reaction types) "#6295CE"
                              (:treasure types) "#EFD34E"
                              (:attack types) "#940000"
                              (:action types) "#DED7C4"
                              :else :grey)
          :border-width     2}
         (when (:attack types)
           {:border-style :dotted})))

(defn view-card
  ([card]
   (view-card nil card))
  ([max {:keys [name name-ui types cost number-of-cards interaction]}]
   (let [num-selected (->> (:selection @state) (filter #{name}) count)
         number-of-cards (if (= :choosable interaction)
                           (let [num (- (or number-of-cards 1) num-selected)]
                             (if (= 1 num) nil num))
                           number-of-cards)
         disabled (or (nil? interaction)
                      (and (= :choosable interaction)
                           (= (count (:selection @state)) max)))]
     (when-not (and (= :choosable interaction)
                    (= 0 number-of-cards))
       [:div {:key [:supply name]}
        [:button {:style    (button-style disabled types number-of-cards)
                  :disabled disabled
                  :on-click (when interaction
                              (fn [] (case interaction
                                       :playable (swap! state assoc :game (cmd/play name))
                                       :choosable (select! name)
                                       :quick-choosable (swap! state assoc :game (cmd/choose name))
                                       :buyable (swap! state assoc :game (cmd/buy name)))))}
         (str name-ui (when cost (str " ($" cost ")")) (when number-of-cards (str " x" number-of-cards)))]]))))

(defn map-tag [tag coll]
  (map (fn [x] [tag x]) coll))

(defn view-row [row]
  [:tr (->> row
            (map view-card)
            (map (fn [card] [:td card])))])

(defn view-pile [pile max]
  [:div
   (map (partial view-card max) (:visible-cards pile))
   (when (:number-of-cards pile)
     (str (:number-of-cards pile) " Cards"))])

(defn home-page []
  [:div [:h2 "Dominion"]
   "Number of players: " [:input {:type      :number
                                  :min       2
                                  :max       4
                                  :on-change (fn [event] (swap! state assoc :num-players (js/parseInt (-> event .-target .-value))))
                                  :value     (:num-players @state)}]
   [:div "Dominion" [:input {:type      :checkbox
                             :checked   (-> @state :sets :dominion)
                             :on-change (fn [event] (swap! state update :sets (if (-> event .-target .-checked) conj disj) :dominion))}]]
   [:div "Intrigue" [:input {:type      :checkbox
                             :checked   (-> @state :sets :intrigue)
                             :on-change (fn [event] (swap! state update :sets (if (-> event .-target .-checked) conj disj) :intrigue))}]]
   [:div [:button {:style    (button-style)
                   :on-click (fn [] (swap! state assoc :game (cmd/start-game (take (:num-players @state) (:players @state))
                                                                             :sets (:sets @state))))}
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
       (view-row row1)
       (view-row (concat [{}] row2))
       (view-row row3)
       (view-row row4)])]
   [:div "Players"
    [:table
     [:tr (map-tag :th ["Name" "Hand" "Play area" "Deck" "Discard"])]
     (->> (get-in @state [:game :players])
          (map (fn [{:keys               [name-ui hand play-area deck discard
                                          actions coins buys set-aside
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
                         (map (partial view-card max) hand))]
                  [:td (map (partial view-card max) play-area)]
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
                     [:div (map (fn [{:keys [option text]}]
                                  (let [disabled (and (not quick-choice?)
                                                      (or (= max (count (:selection @state)))
                                                          (-> (:selection @state) set option)))]
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
                          [:div "Selected: " (map-indexed (fn [idx selected]
                                                            [:button {:style    (button-style)
                                                                      :on-click (fn [] (deselect! idx))}
                                                             (ut/format-name selected)]) (:selection @state))])
                        (let [disabled (and min (< (count (:selection @state)) min)
                                            (not (and optional? (empty? (:selection @state)))))]
                          [:button {:style    (button-style disabled)
                                    :disabled disabled
                                    :on-click (fn [] (swap! state assoc
                                                            :game (cmd/choose (:selection @state))
                                                            :selection []))}
                           "Done"])])]
                    (when active?
                      [:td (let [disabled (-> @state :game :commands :can-play-treasures? not)]
                             [:button {:style    (button-style disabled)
                                       :disabled disabled
                                       :on-click (fn [] (swap! state assoc :game (cmd/play-treasures)))}
                              "Play Treasures"])
                       (let [disabled (-> @state :game :commands :can-end-turn? not)]
                         [:button {:style    (button-style disabled)
                                   :disabled disabled
                                   :on-click (fn [] (swap! state assoc :game (cmd/end-turn)))}
                          "End Turn"])]))
                  (when set-aside
                    [:td
                     [:div "Set aside"]
                     [:div (map (partial view-card max) set-aside)]])])))]]
   (let [{:keys [compact full]} (get-in @state [:game :trash])]
     [:div "Trash " [:button {:on-click (fn [] (swap! state update :trash-unfolded? not))}
                     (if (:trash-unfolded? @state) "Hide" "Show")]
      [:table
       (if (get @state :trash-unfolded?)
         [:tr [:td (map view-card full)]]
         [:tr
          [:td (view-pile compact nil)]])]])])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
