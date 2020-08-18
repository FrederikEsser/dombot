(ns dombot.specs
  (:require [clojure.spec.alpha :as s]))

(s/def ::name keyword?)

(s/def ::name-ui string?)

(s/def ::type #{:curse :victory :treasure :action :attack :reaction :duration :artifact :prize
                :shelter :knight :looter :ruins :command :traveller :reserve :gathering :castle
                :event :landmark :project :night :heirloom :spirit :zombie :fate :boon :doom :hex :state})

(s/def ::types (s/coll-of ::type :distinct true))

(s/def ::cost nat-int?)

(s/def ::number-of-cards nat-int?)

(s/def ::interaction #{:buyable :playable :choosable :quick-choosable :spendable})

(s/def ::area #{:hand :play-area :discard :tavern-mat :projects :artifacts :boons :supply :extra-cards})

(s/def ::card-name ::name)

(s/def ::choice-value (s/keys :req-un [::area
                                       ::card-name]))

(s/def ::choice-option #{:unique :similar :leave-in-play-area})

(s/def ::choice-opts (s/coll-of ::choice-option))

(s/def ::stay-in-play boolean?)

(s/def ::token-type #{:embargo :trade-route :victory-point :debt})

(s/def ::token (s/keys :req-un [::token-type
                                ::number-of-tokens]))

(s/def ::tokens (s/coll-of ::token))

(s/def ::coin-cost nat-int?)

(s/def ::debt-cost nat-int?)

(s/def ::mixed-cost (s/keys :req-un [::coin-cost]
                            :opt-un [::debt-cost]))

(s/def ::card (s/keys :req-un [::name
                               ::name-ui
                               ::types]
                      :opt-un [::mixed-cost
                               ::number-of-cards
                               ::interaction
                               ::choice-value
                               ::choice-opts
                               ::stay-in-play
                               ::tokens]))

(s/def ::cards (s/coll-of ::card))

(s/def ::kingdom-card (s/keys :req-un [::card]
                              :opt-un [::cards]))

(s/def ::supply (s/coll-of ::kingdom-card))

(s/def ::extra-cards (s/coll-of ::kingdom-card))

(s/def ::participants (s/coll-of string?))

(s/def ::events (s/coll-of (s/keys :req-un [::name
                                            ::name-ui
                                            ::type
                                            ::mixed-cost]
                                   :opt-un [::interaction])))

(s/def ::chosen-cards (s/coll-of ::name))

(s/def ::landmarks (s/coll-of (s/keys :req-un [::name
                                               ::name-ui
                                               ::type]
                                      :opt-un [::interaction
                                               ::vp-tokens
                                               ::chosen-cards])))

(s/def ::projects (s/coll-of (s/keys :req-un [::name
                                              ::name-ui
                                              ::type
                                              ::cost]
                                     :opt-un [::interaction
                                              ::choice-value
                                              ::participants])))

(s/def ::boon (s/keys :req-un [::name
                               ::name-ui
                               ::type]
                      :opt-un [::interaction]))

(s/def ::top-boon ::boon)

(s/def ::boon-discard (s/coll-of ::boon))

(s/def ::boons (s/keys :req-un [::boon-discard
                                ::number-of-cards]
                       :opt-un [::top-boon]))

(s/def ::druid-boons (s/coll-of ::boon))

(s/def ::hex (s/keys :req-un [::name
                              ::name-ui
                              ::type]))

(s/def ::top-hex ::hex)

(s/def ::hex-discard (s/coll-of ::hex))

(s/def ::hexes (s/keys :req-un [::hex-discard
                                ::number-of-cards]
                       :opt-un [::top-hex]))

(s/def ::prosperity? boolean?)

(s/def ::ruins? boolean?)

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

(s/def ::debt nat-int?)

(s/def ::buys nat-int?)

(s/def ::set-aside ::cards)

(s/def ::island-mat ::cards)

(s/def ::native-village-mat (s/or :shown ::cards
                                  :hidden ::pile))

(s/def ::tavern-mat ::cards)

(s/def ::exile-mat ::cards)

(s/def ::pirate-ship-coins nat-int?)

(s/def ::vp-tokens nat-int?)

(s/def ::number pos-int?)

(s/def ::resource (s/keys :req-un [::number]
                          :opt-un [::interaction]))

(s/def ::journey-token #{:face-up :face-down})

(s/def ::coffers ::resource)

(s/def ::villagers ::resource)

(s/def ::artifacts ::cards)

(s/def ::states ::cards)

(s/def ::text string?)

(s/def ::min nat-int?)

(s/def ::max nat-int?)

(s/def ::option (s/or :keyword keyword?
                      :int integer?))

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
                        :opt-un [::debt
                                 ::set-aside
                                 ::island-mat
                                 ::native-village-mat
                                 ::pirate-ship-coins
                                 ::vp-tokens
                                 ::tavern-mat
                                 ::exile-mat
                                 ::journey-token
                                 ::coffers
                                 ::villagers
                                 ;::boons ; todo: spec for player boons
                                 ::artifacts
                                 ::states
                                 ::choice
                                 ::victory-points
                                 ::winner?]))

(s/def ::players (s/coll-of ::player))

(s/def ::trash (s/keys :req-un [::cards
                                ::number-of-cards]
                       :opt-un [::card]))

(s/def ::can-undo? boolean?)

(s/def ::can-goto-buy-phase? boolean?)

(s/def ::can-play-treasures? boolean?)

(s/def ::can-end-turn? boolean?)

(s/def ::commands (s/keys :req-un [::can-undo?
                                   ::can-goto-buy-phase?
                                   ::can-play-treasures?
                                   ::can-end-turn?]))

(s/def ::trade-route-mat pos-int?)

(s/def ::game (s/keys :req-un [::supply
                               ::prosperity?
                               ::ruins?
                               ::players
                               ::trash
                               ::commands]
                      :opt-un [::extra-cards
                               ::events
                               ::landmarks
                               ::projects
                               ::boons
                               ::druid-boons
                               ::hexes
                               ::trade-route-mat]))
