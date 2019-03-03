(ns dombot.cards.renaissance
  (:require [dombot.operations :refer []]
            [dombot.cards.common :refer []]
            [dombot.utils :as ut]
            [dombot.effects :as effects]))

(def scholar {:name    :scholar
              :set     :renaissance
              :types   #{:action}
              :cost    5
              :effects [[:discard-all-hand]
                        [:draw 7]]})

(def kingdom-cards [scholar])
