(ns dombot.specs
  (:require [clojure.spec.alpha :as s]))

(s/def ::name keyword?)

(s/def ::name-ui string?)

(s/def ::type #{:curse :victory :treasure :action :attack :reaction})

(s/def ::types (s/coll-of ::type :distinct true))

(s/def ::cost nat-int?)

(s/def ::number-of-cards nat-int?)

(s/def ::interaction #{:buyable :playable :choosable :quick-choosable})

(s/def ::card (s/keys :opt-un [::name
                               ::name-ui
                               ::types
                               ::cost
                               ::number-of-cards
                               ::interaction]))

(s/def ::cards (s/coll-of ::card))

(s/def ::supply ::cards)

(s/def ::hand ::cards)

(s/def ::play-area ::cards)

(s/def ::visible-cards ::cards)

(s/def ::pile (s/keys :opt-un [::number-of-cards
                               ::visible-cards]))

(s/def ::deck ::pile)

(s/def ::discard ::pile)

(s/def ::actions nat-int?)

(s/def ::coins nat-int?)

(s/def ::buys nat-int?)

(s/def ::active? boolean?)

(s/def ::set-aside ::cards)

(s/def ::text string?)

(s/def ::min nat-int?)

(s/def ::max nat-int?)

(s/def ::option keyword?)

(s/def ::options (s/coll-of (s/keys :req-un [::text
                                             ::option])))

(s/def ::quick-choice? boolean?)

(s/def ::choice (s/keys :req-un [::text]
                        :opt-un [::min
                                 ::max
                                 ::options
                                 ::quick-choice?]))

(s/def ::victory-points nat-int?)

(s/def ::winner? boolean?)

(s/def ::player (s/keys :req-un [::name.ui
                                 ::hand
                                 ::play-area
                                 ::deck
                                 ::discard
                                 ::actions
                                 ::coins
                                 ::buys]
                        :opt-un [::active?
                                 ::set-aside
                                 ::choice
                                 ::victory-points
                                 ::winner?]))

(s/def ::players (s/coll-of ::player))

(s/def ::compact ::cards)

(s/def ::full ::cards)

(s/def ::trash (s/keys :req-un [::compact
                                ::full]))

(s/def ::can-undo? boolean?)

(s/def ::can-play-treasures? boolean?)

(s/def ::can-end-turn? boolean?)

(s/def ::commands (s/keys :req-un [::can-undo?
                                   ::can-play-treasures?
                                   ::can-end-turn?]))

(s/def ::game (s/keys :req-un [::supply
                               ::players
                               ::trash
                               ::commands]))