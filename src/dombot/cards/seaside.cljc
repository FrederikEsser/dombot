(ns dombot.cards.seaside
  (:require [dombot.operations :refer []]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def caravan {:name    :caravan
              :set     :seaside
              :types   #{:action :duration}
              :cost    4
              :effects [[:draw 1]
                        [:give-actions 1]]
              :duration [[:draw 1]]})

(def kingdom-cards [caravan])
