(ns dombot.specs
  (:require [clojure.spec.alpha :as s]))

(s/def ::name keyword?)

(s/def ::name-ui string?)

(s/def ::type #{:curse :victory :treasure :action :attack :reaction :duration :artifact})

(s/def ::types (s/coll-of ::type :distinct true))

(s/def ::cost nat-int?)

(s/def ::number-of-cards nat-int?)

(s/def ::interaction #{:buyable :playable :choosable :quick-choosable :spendable})

(s/def ::stay-in-play boolean?)

(s/def ::token-type #{:embargo})

(s/def ::token (s/keys :req-un [::token-type]))

(s/def ::tokens (s/coll-of ::token))

(s/def ::card (s/keys :req-un [::name
                               ::name-ui
                               ::types]
                      :opt-un [::cost
                               ::number-of-cards
                               ::interaction
                               ::stay-in-play
                               ::tokens]))

(s/def ::cards (s/coll-of ::card))

(s/def ::supply ::cards)

(s/def ::prosperity? boolean?)

(s/def ::active? boolean?)

(s/def ::pile (s/keys :opt-un [::number-of-cards
                               ::visible-cards]))

(s/def ::hand (s/or :shown ::cards
                    :hidden ::pile))

(s/def ::play-area ::cards)

(s/def ::visible-cards ::cards)

(s/def ::deck ::pile)

(s/def ::discard ::pile)

(s/def ::actions nat-int?)

(s/def ::coins nat-int?)

(s/def ::buys nat-int?)

(s/def ::set-aside ::cards)

(s/def ::island-mat ::cards)

(s/def ::native-village-mat (s/or :shown ::cards
                                  :hidden ::pile))

(s/def ::pirate-ship-coins nat-int?)

(s/def ::number pos-int?)

(s/def ::resource (s/keys :req-un [::number]
                          :opt-un [::interaction]))

(s/def ::coffers ::resource)

(s/def ::villagers ::resource)

(s/def ::artifacts ::cards)

(s/def ::text string?)

(s/def ::min nat-int?)

(s/def ::max nat-int?)

(s/def ::option keyword?)

(s/def ::option-elem (s/keys :req-un [::text
                                      ::option]))

(s/def ::options (s/coll-of ::option-elem))

(s/def ::from nat-int?)

(s/def ::to nat-int?)

(s/def ::interval (s/keys :req-un [::from
                                   ::to]))

(s/def ::quick-choice? boolean?)

(s/def ::optional? boolean?)

(s/def ::choice (s/keys :req-un [::text
                                 ::min
                                 ::max
                                 ::quick-choice?]
                        :opt-un [::options
                                 ::interval
                                 ::optional?]))

(s/def ::victory-points integer?)

(s/def ::winner? boolean?)

(s/def ::player (s/keys :req-un [::name-ui
                                 ::active?
                                 ::hand
                                 ::play-area
                                 ::deck
                                 ::discard
                                 ::actions
                                 ::coins
                                 ::buys]
                        :opt-un [::set-aside
                                 ::island-mat
                                 ::native-village-mat
                                 ::pirate-ship-coins
                                 ::coffers
                                 ::villagers
                                 ::artifacts
                                 ::choice
                                 ::victory-points
                                 ::winner?]))

(s/def ::players (s/coll-of ::player))

(s/def ::compact ::pile)

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
                               ::prosperity?
                               ::players
                               ::trash
                               ::commands]))
