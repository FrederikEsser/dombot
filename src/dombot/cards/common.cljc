(ns dombot.cards.common
  (:require [dombot.operations :refer [gain move-card move-cards give-choice card-effect push-effect-stack]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn give-actions [game player-no n]
  (assert (get-in game [:players player-no :actions]) (str ":actions is not specified for player " player-no))
  (update-in game [:players player-no :actions] + n))

(effects/register {:give-actions give-actions})

(defn give-coins [game player-no n]
  (assert (get-in game [:players player-no :coins]) (str ":coins is not specified for player " player-no))
  (update-in game [:players player-no :coins] + n))

(effects/register {:give-coins give-coins})

(defn give-buys [game player-no n]
  (assert (get-in game [:players player-no :buys]) (str ":buys is not specified for player " player-no))
  (update-in game [:players player-no :buys] + n))

(effects/register {:give-buys give-buys})

(defn gain-to-hand [game player-no card-name]
  (gain game player-no card-name {:to :hand}))

(effects/register {:gain-to-hand gain-to-hand})

(defn gain-to-topdeck [game player-no card-name]
  (gain game player-no card-name {:to          :deck
                                  :to-position :top}))

(effects/register {:gain-to-topdeck gain-to-topdeck})

(defn gain-from-trash [game player-no card-name]
  (move-card game player-no {:card-name card-name
                             :from      :trash
                             :to        :discard}))

(effects/register {:gain-from-trash gain-from-trash})

(defn play-from-hand [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name card-name
                                          :from      :hand
                                          :to        :play-area})))

(effects/register {:play play-from-hand})

(defn play-action-twice [game player-no card-name]
  (let [{card :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (cond-> game
            card-name (push-effect-stack player-no [[:play card-name]
                                                    [:card-effect card]
                                                    [:card-effect card]]))))

(effects/register {:play-action-twice play-action-twice})

(defn play-from-discard [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name     card-name
                                          :from          :discard
                                          :from-position :bottom
                                          :to            :play-area})))

(effects/register {:play-from-discard play-from-discard})

(defn discard-from-hand [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :hand
                              :to         :discard}))

(effects/register {:discard-from-hand discard-from-hand})

(defn discard-down-to [game player-no n]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (> (count hand) n) (give-choice player-no {:text    (str "Discard down to " n " cards in hand.")
                                                       :choice  :discard-from-hand
                                                       :options [:player :hand]
                                                       :min     (- (count hand) n)
                                                       :max     (- (count hand) n)}))))

(effects/register {:discard-down-to discard-down-to})

(defn discard-all-revealed [game player-no]
  (let [revealed (get-in game [:players player-no :revealed])]
    (move-cards game player-no {:card-names (map :name revealed)
                                :from       :revealed
                                :to         :discard})))

(effects/register {:discard-all-revealed discard-all-revealed})

(defn discard-from-look-at [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :look-at
                              :to         :discard}))

(effects/register {:discard-from-look-at discard-from-look-at})

(defn discard-from-topdeck [game player-no number-of-cards]
  (move-card game player-no {:number-of-cards number-of-cards
                             :from            :deck
                             :from-position   :top
                             :to              :discard}))

(effects/register {:discard-from-topdeck discard-from-topdeck})

(defn discard-all-set-aside [game player-no]
  (let [set-aside (get-in game [:players player-no :set-aside])]
    (move-cards game player-no {:card-names (map :name set-aside)
                                :from       :set-aside
                                :to         :discard})))

(effects/register {:discard-all-set-aside discard-all-set-aside})

(defn discard-all-hand [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (move-cards game player-no {:card-names (map :name hand)
                                :from       :hand
                                :to         :discard})))

(effects/register {:discard-all-hand discard-all-hand})

(defn topdeck-from-hand [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name   card-name
                                          :from        :hand
                                          :to          :deck
                                          :to-position :top})))

(effects/register {:topdeck-from-hand topdeck-from-hand})

(defn topdeck-from-discard [game player-no card-name]
  (cond-> game
          card-name (move-card player-no {:card-name   card-name
                                          :from        :discard
                                          :to          :deck
                                          :to-position :top})))

(effects/register {:topdeck-from-discard topdeck-from-discard})

(defn topdeck-from-look-at [game player-no card-names]
  (move-cards game player-no {:card-names  card-names
                              :from        :look-at
                              :to          :deck
                              :to-position :top}))

(effects/register {:topdeck-from-look-at topdeck-from-look-at})

(defn topdeck-from-revealed [game player-no card-names]
  (move-cards game player-no {:card-names  card-names
                              :from        :revealed
                              :to          :deck
                              :to-position :top}))

(effects/register {:topdeck-from-revealed topdeck-from-revealed})

(defn trash-from-hand [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :hand
                              :to         :trash}))

(effects/register {:trash-from-hand trash-from-hand})

(defn trash-last-from-play-area [game player-no card-name]
  (move-card game player-no {:card-name     card-name
                             :from          :play-area
                             :from-position :bottom
                             :to            :trash}))

(effects/register {:trash-last-from-play-area trash-last-from-play-area})

(defn trash-from-revealed [game player-no card-name]
  (move-card game player-no {:card-name card-name
                             :from      :revealed
                             :to        :trash}))

(effects/register {:trash-from-revealed trash-from-revealed})

(defn trash-from-look-at [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :look-at
                              :to         :trash}))

(effects/register {:trash-from-look-at trash-from-look-at})

(defn trash-from-supply [game player-no card-name]
  (gain game player-no card-name {:to :trash}))

(effects/register {:trash-from-supply trash-from-supply})

(defn trash-from-topdeck [game player-no]
  (move-card game player-no {:from          :deck
                             :from-position :top
                             :to            :trash}))

(effects/register {:trash-from-topdeck trash-from-topdeck})

(defn reveal [game player-no card-names]
  (assoc-in game [:players player-no :revealed-cards :hand] (ut/count-as-coll card-names)))

(effects/register {:reveal reveal})

(defn reveal-hand [game player-no]
  (let [hand (get-in game [:players player-no :hand])]
    (reveal game player-no (map :name hand))))

(effects/register {:reveal-hand reveal-hand})

(defn reveal-from-hand [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :hand
                              :to         :revealed}))

(effects/register {:reveal-from-hand reveal-from-hand})

(defn reveal-from-deck [game player-no number-of-cards]
  (move-cards game player-no {:number-of-cards number-of-cards
                              :from            :deck
                              :from-position   :top
                              :to              :revealed}))

(effects/register {:reveal-from-deck reveal-from-deck})

(defn look-at [game player-no number-of-cards]
  (move-cards game player-no {:number-of-cards number-of-cards
                              :from            :deck
                              :from-position   :top
                              :to              :look-at}))

(effects/register {:look-at look-at})

(defn put-revealed-into-hand [game player-no card-names]
  (move-cards game player-no {:card-names card-names
                              :from       :revealed
                              :to         :hand}))

(effects/register {:put-revealed-into-hand put-revealed-into-hand})

(defn put-revealed-types-into-hand [game player-no types]
  (let [card-names (->> (get-in game [:players player-no :revealed])
                        (filter (comp (partial some types) :types))
                        (map :name))]
    (move-cards game player-no {:card-names card-names
                                :from       :revealed
                                :to         :hand})))

(effects/register {:put-revealed-types-into-hand put-revealed-types-into-hand})

(defn add-trigger [game player-no trigger]
  (update-in game [:players player-no :triggers] concat [trigger]))

(effects/register {:add-trigger add-trigger})

(defn add-cost-reduction [game player-no reduction]
  (update game :cost-reductions concat [{:reduction reduction}]))

(effects/register {:add-cost-reduction add-cost-reduction})
