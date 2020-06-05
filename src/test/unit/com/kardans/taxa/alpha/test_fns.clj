(ns com.kardans.taxa.alpha.test-fns
  (:require [com.kardans.taxa :as taxa]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test fns
;;;

(defn f1
  [thing]
  (taxa/taxon (assoc thing :f1 "f1")))

(defn f2
  ([thing]
   (prn "@f2:1")
   (f2 thing "f2"))
  ([thing arg]
   (prn "@f2:2")
   (clojure.pprint/pprint thing)
   (clojure.pprint/pprint arg)
   (taxa/taxon (assoc thing :f2 arg))))

(defn f3
  [thing]
  (taxa/taxon (assoc thing :f3 "f3") ::taxa/err))

(defn f4
  [thing]
  (let [{:keys [token]} (meta thing)]
    (taxa/taxon (assoc thing :f4 token) ::success)))
