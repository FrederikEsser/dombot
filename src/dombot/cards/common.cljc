(ns dombot.cards.common
  (:require [dombot.operations :refer [gain move-card move-cards give-choice push-effect-stack stay-in-play]]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(defn give-actions [game {:keys [player-no arg]}]
  (assert (get-in game [:players player-no :actions]) (str ":actions is not specified for player " player-no))
  (update-in game [:players player-no :actions] + arg))

(effects/register {:give-actions give-actions})

(defn give-coins [game {:keys [player-no arg]}]
  (assert (get-in game [:players player-no :coins]) (str ":coins is not specified for player " player-no))
  (update-in game [:players player-no :coins] + arg))

(effects/register {:give-coins give-coins})

(defn give-buys [game {:keys [player-no arg]}]
  (assert (get-in game [:players player-no :buys]) (str ":buys is not specified for player " player-no))
  (update-in game [:players player-no :buys] + arg))

(effects/register {:give-buys give-buys})

(defn give-coffers [game {:keys [player-no arg]}]
  (cond-> game
          (< 0 arg) (update-in [:players player-no :coffers] ut/plus arg)))

(effects/register {:give-coffers give-coffers})

(defn give-villagers [game {:keys [player-no arg]}]
  (cond-> game
          (< 0 arg) (update-in [:players player-no :villagers] ut/plus arg)))

(effects/register {:give-villagers give-villagers})

(defn gain-to-hand [game args]
  (gain game (merge args {:to :hand})))

(effects/register {:gain-to-hand gain-to-hand})

(defn gain-to-topdeck [game args]
  (gain game (merge args {:to          :deck
                          :to-position :top})))

(effects/register {:gain-to-topdeck gain-to-topdeck})

(defn gain-from-trash [game args]
  (push-effect-stack game (merge args {:effects [[:move-card (merge args {:from :trash
                                                                          :to   :discard})]
                                                 [:on-gain args]]})))

(effects/register {:gain-from-trash gain-from-trash})

(defn gain-from-trash-to-hand [game args]
  (push-effect-stack game (merge args {:effects [[:move-card (merge args {:from :trash
                                                                          :to   :hand})]
                                                 [:on-gain args]]})))

(effects/register {:gain-from-trash-to-hand gain-from-trash-to-hand})

(defn play-from-hand [game {:keys [card-name] :as args}]
  (cond-> game
          card-name (move-card (merge args {:from :hand
                                            :to   :play-area}))))


(effects/register {:play play-from-hand})

(defn check-stay-in-play [game {:keys [player-no card-id target-id]}]
  (let [{{:keys [types] :as card} :card} (ut/get-card-idx game [:players player-no :play-area] {:id target-id})]
    (cond-> game
            (and (:duration types) (stay-in-play card)) (ut/update-in-vec [:players player-no :play-area] {:id card-id} update :at-start-turn concat [[]]))))

(effects/register {:check-stay-in-play check-stay-in-play})

(defn play-action-twice [game {:keys [player-no card-id card-name]}]
  (if card-name
    (let [{card :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
      (-> game
          (push-effect-stack {:player-no player-no
                              :card-id   card-id
                              :effects   [[:play {:card-name card-name}]
                                          [:card-effect {:card card}]
                                          [:card-effect {:card card}]
                                          [:check-stay-in-play {:target-id (:id card)}]]})))
    game))

(effects/register {:play-action-twice play-action-twice})

(defn play-from-discard [game {:keys [card-name] :as args}]
  (cond-> game
          card-name (move-card (merge args {:from          :discard
                                            :from-position :bottom
                                            :to            :play-area}))))

(effects/register {:play-from-discard play-from-discard})

(defn discard-from-hand [game args]
  (move-cards game (merge args {:from :hand
                                :to   :discard})))

(effects/register {:discard-from-hand discard-from-hand})

(defn discard-down-to [game {:keys [player-no arg]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (> (count hand) arg) (give-choice {:player-no player-no
                                               :text      (str "Discard down to " arg " cards in hand.")
                                               :choice    :discard-from-hand
                                               :options   [:player :hand]
                                               :min       (- (count hand) arg)
                                               :max       (- (count hand) arg)}))))

(effects/register {:discard-down-to discard-down-to})

(defn discard-all-revealed [game {:keys [player-no]}]
  (let [revealed (get-in game [:players player-no :revealed])]
    (move-cards game {:player-no  player-no
                      :card-names (map :name revealed)
                      :from       :revealed
                      :to         :discard})))

(effects/register {:discard-all-revealed discard-all-revealed})

(defn discard-from-look-at [game args]
  (move-cards game (merge args {:from :look-at
                                :to   :discard})))

(effects/register {:discard-from-look-at discard-from-look-at})

(defn discard-all-look-at [game {:keys [player-no]}]
  (let [look-at (get-in game [:players player-no :look-at])]
    (move-cards game {:player-no  player-no
                      :card-names (map :name look-at)
                      :from       :look-at
                      :to         :discard})))

(effects/register {:discard-all-look-at discard-all-look-at})

(defn discard-from-topdeck [game {:keys [player-no arg]}]
  (move-card game {:player-no       player-no
                   :number-of-cards arg
                   :from            :deck
                   :from-position   :top
                   :to              :discard}))

(effects/register {:discard-from-topdeck discard-from-topdeck})

(defn discard-all-set-aside [game {:keys [player-no]}]
  (let [set-aside (get-in game [:players player-no :set-aside])]
    (move-cards game {:player-no  player-no
                      :card-names (map :name set-aside)
                      :from       :set-aside
                      :to         :discard})))

(effects/register {:discard-all-set-aside discard-all-set-aside})

(defn discard-all-hand [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (move-cards game {:player-no  player-no
                      :card-names (map :name hand)
                      :from       :hand
                      :to         :discard})))

(effects/register {:discard-all-hand discard-all-hand})

(defn set-aside [game args]
  (move-cards game (merge args {:from          :deck
                                :from-position :top
                                :to            :set-aside})))

(effects/register {:set-aside set-aside})

(defn topdeck-from-hand [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name
              card-names) (move-cards (merge args {:from        :hand
                                                   :to          :deck
                                                   :to-position :top}))))

(effects/register {:topdeck-from-hand topdeck-from-hand})

(defn topdeck-down-to [game {:keys [player-no arg]}]
  (let [hand (get-in game [:players player-no :hand])]
    (cond-> game
            (> (count hand) arg) (give-choice {:player-no player-no
                                               :text      (str "Put cards from your hand onto your deck until you have " arg " cards in hand.")
                                               :choice    :topdeck-from-hand
                                               :options   [:player :hand]
                                               :min       (- (count hand) arg)
                                               :max       (- (count hand) arg)}))))

(effects/register {:topdeck-down-to topdeck-down-to})

(defn topdeck-this-from-play-area [game {:keys [player-no card-id]}]
  (move-card game {:player-no    player-no
                   :move-card-id card-id
                   :from         :play-area
                   :to           :deck
                   :to-position  :top}))

(effects/register {:topdeck-this-from-play-area topdeck-this-from-play-area})

(defn topdeck-from-discard [game {:keys [card-name] :as args}]
  (cond-> game
          card-name (move-card (merge args {:from        :discard
                                            :to          :deck
                                            :to-position :top}))))

(effects/register {:topdeck-from-discard topdeck-from-discard})

(defn topdeck-from-look-at [game args]
  (move-cards game (merge args {:from        :look-at
                                :to          :deck
                                :to-position :top})))

(effects/register {:topdeck-from-look-at topdeck-from-look-at})

(defn topdeck-from-revealed [game args]
  (move-cards game (merge args {:from        :revealed
                                :to          :deck
                                :to-position :top})))

(effects/register {:topdeck-from-revealed topdeck-from-revealed})

(defn trash-from-hand [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name card-names) (move-cards (merge args {:from :hand
                                                             :to   :trash}))))

(effects/register {:trash-from-hand trash-from-hand})

(defn trash-this [game {:keys [player-no card-id] :as args}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})]
    (cond-> game
            card (move-card (merge args {:move-card-id card-id
                                         :from         :play-area
                                         :to           :trash})))))

(effects/register {:trash-this trash-this})

(defn trash-from-revealed [game args]
  (move-cards game (merge args {:from :revealed
                                :to   :trash})))

(effects/register {:trash-from-revealed trash-from-revealed})

(defn trash-from-look-at [game args]
  (move-cards game (merge args {:from :look-at
                                :to   :trash})))

(effects/register {:trash-from-look-at trash-from-look-at})

(defn do-trash-from-supply [game {:keys [card-name] :as args}]
  (assert card-name (str "Can't trash unspecified card: " args))
  (let [{:keys [idx card pile-size]} (ut/get-pile-idx game card-name)]
    (assert pile-size (str "Trash error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (cond-> game
            (< 0 pile-size) (-> (update-in [:supply idx :pile-size] dec)
                                (update :trash concat [(ut/give-id! card)])))))

(defn trash-from-supply [game args]
  (push-effect-stack game (merge args {:effects [[:do-trash-from-supply args]
                                                 [:on-trash args]]})))

(effects/register {:do-trash-from-supply do-trash-from-supply
                   :trash-from-supply    trash-from-supply})

(defn trash-from-topdeck [game {:keys [player-no]}]
  (move-card game {:player-no     player-no
                   :from          :deck
                   :from-position :top
                   :to            :trash}))

(effects/register {:trash-from-topdeck trash-from-topdeck})

(defn reveal [game args]
  (push-effect-stack game (merge args
                                 {:effects [[:move-cards (merge args
                                                                {:from :hand
                                                                 :to   :revealed})]
                                            [:move-cards (merge args
                                                                {:from :revealed
                                                                 :to   :hand})]]})))

(effects/register {:reveal reveal})

(defn reveal-hand [game {:keys [player-no]}]
  (let [hand (get-in game [:players player-no :hand])]
    (reveal game {:player-no  player-no
                  :card-names (map :name hand)})))

(effects/register {:reveal-hand reveal-hand})

(defn reveal-from-hand [game args]
  (move-cards game (merge args {:from :hand
                                :to   :revealed})))

(effects/register {:reveal-from-hand reveal-from-hand})

(defn reveal-from-deck [game {:keys [player-no arg]}]
  (move-cards game {:player-no       player-no
                    :number-of-cards arg
                    :from            :deck
                    :from-position   :top
                    :to              :revealed}))

(effects/register {:reveal-from-deck reveal-from-deck})

(defn look-at [game {:keys [player-no arg from-position]}]
  (move-cards game {:player-no       player-no
                    :number-of-cards arg
                    :from            :deck
                    :from-position   (or from-position :top)
                    :to              :look-at}))

(effects/register {:look-at look-at})

(defn put-revealed-into-hand [game args]
  (move-cards game (merge args {:from :revealed
                                :to   :hand})))

(effects/register {:put-revealed-into-hand put-revealed-into-hand})

(defn put-revealed-types-into-hand [game {:keys [player-no types]}]
  (let [card-names (->> (get-in game [:players player-no :revealed])
                        (filter (comp (partial some types) :types))
                        (map :name))]
    (move-cards game {:player-no  player-no
                      :card-names card-names
                      :from       :revealed
                      :to         :hand})))

(effects/register {:put-revealed-types-into-hand put-revealed-types-into-hand})

(defn put-set-aside-into-hand [game {:keys [player-no card-id card-name]}]
  game
  (let [{duration-card :card} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})
        {:keys [idx card]} (ut/get-card-idx duration-card [:set-aside] {:name card-name})]
    (-> game
        (ut/update-in-vec [:players player-no :play-area] {:id card-id} update :set-aside ut/vec-remove idx)
        (update-in [:players player-no :hand] concat [card]))))

(effects/register {:put-set-aside-into-hand put-set-aside-into-hand})

(defn take-from-discard [game {:keys [card-name] :as args}]
  (cond-> game
          card-name (move-card (merge args {:from :discard
                                            :to   :hand}))))

(effects/register {:take-from-discard take-from-discard})

(defn add-trigger [game {:keys [player-no trigger]}]
  (update-in game [:players player-no :triggers] concat [trigger]))

(effects/register {:add-trigger add-trigger})

(defn add-cost-reduction [game {:keys [arg]}]
  (update game :cost-reductions concat [{:reduction arg}]))

(effects/register {:add-cost-reduction add-cost-reduction})
