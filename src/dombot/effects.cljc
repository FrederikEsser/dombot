(ns dombot.effects)

(defonce registered-effects (atom {}))

(defn register [effects]
  (swap! registered-effects merge effects))

(defn get-effect [name]
  (assert (contains? @registered-effects name) (str name " effect has not been registered to effects-map."))
  (get @registered-effects name))

(defonce registered-options (atom {}))

(defn register-options [options]
  (swap! registered-options merge options))

(defn get-option [name]
  (assert (contains? @registered-options name) (str name " option has not been registered to options-map."))
  (get @registered-options name))

