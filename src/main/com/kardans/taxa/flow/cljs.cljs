(ns com.kardans.taxa.flow.cljs
  (:require-macros [com.kardans.taxa.flow.cljs])
  (:require [com.kardans.taxa :as taxa]))

(defn do-while->
  [val forms {:keys [hierarchy parent]}]
  (reduce (fn [t {:keys [args f]}]
            (if (taxa/taxed? hierarchy t parent)
              (apply f (concat (list t) args))
              (reduced t)))
          val
          forms))
