(ns ^:figwheel-no-load dombot.dev
  (:require
    [dombot.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
