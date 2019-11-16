(ns dev
  (:require
   [kaocha.repl :as kaocha]))

(defn unit-test
  "Run unit tests"
  []
  (kaocha.repl/run :unit))


(comment

  (unit-test)

  )
