(ns dev
  (:require
   [kaocha.repl :as kaocha]))


(defn run-test
  "Run test suit"
  ([]
   (run-test :clj))
  ([suit]
   (kaocha.repl/run suit)))


(comment

  (run-test :clj)

  (run-test :cljs)

  )
