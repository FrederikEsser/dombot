(ns dombot.prod
  (:require
    [dombot.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
