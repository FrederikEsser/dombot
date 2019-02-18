(ns dombot.cards.seaside
  (:require [dombot.operations :refer []]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def caravan {:name     :caravan
              :set      :seaside
              :types    #{:action :duration}
              :cost     4
              :effects  [[:draw 1]
                         [:give-actions 1]]
              :duration [[:draw 1]]})

(def fishing-village {:name     :fishing-village
                      :set      :seaside
                      :types    #{:action :duration}
                      :cost     3
                      :effects  [[:give-actions 2]
                                 [:give-coins 1]]
                      :duration [[:give-actions 1]
                                 [:give-coins 1]]})

(def wharf {:name     :wharf
            :set      :seaside
            :types    #{:action :duration}
            :cost     5
            :effects  [[:draw 2]
                       [:give-buys 1]]
            :duration [[:draw 2]
                       [:give-buys 1]]})

(def kingdom-cards [caravan
                    fishing-village
                    wharf])
