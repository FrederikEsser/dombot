(ns dombot.effects)

(defonce registered-effects (atom {}))

(defn register [effects]
  (swap! registered-effects merge effects))

(defn get-effect [name]
  (assert (contains? @registered-effects name) (str name " effect has not been registered to effects-map."))
  (get @registered-effects name))
