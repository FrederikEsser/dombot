(ns user
 (:require [figwheel-sidecar.repl-api :as ra]))

(defn start-fw []
 (ra/start-figwheel!)
 (ra/cljs-repl))

(defn stop-fw []
 (ra/stop-figwheel!))

(defn cljs []
 (ra/cljs-repl))
