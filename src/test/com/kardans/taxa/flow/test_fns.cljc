(ns com.kardans.taxa.flow.test-fns
  (:require [com.kardans.taxa :as taxa]))

(defn f
  ([t k v]
   (f t k v (taxa/tag t)))
  ([t k v tag]
   (-> t
       taxa/thing
       (taxa/taxon tag)
       (assoc-in [::taxa/thing k] v))))

(defn f1
  ([t]
   (f1 t "f1"))
  ([t arg]
   (f t :f1 arg)))

(defn f2
  ([t]
   (f2 t "f2"))
  ([t arg]
   (f t :f2 arg)))

(defn f3
  [t]
  (f t :f3 "f3" ::taxa/err))

(defn f4
  [t]
  (assoc-in t [::taxa/thing :f4]  "f4"))
