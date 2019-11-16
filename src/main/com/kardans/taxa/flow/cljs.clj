(ns com.kardans.taxa.flow.cljs
  (:require
   [com.kardans.taxa :as taxa]))


(defn coerce-opts
  [{:keys [parent hierarchy] :or {parent ::taxa/ok
                                  hierarchy 'com.kardans.taxa/taxa}}]
  {:hierarchy hierarchy
   :parent parent})

(defmacro taxed-let
  "Like if-let but tests in taxa with common taxa options {:hierarchy :parent}
  isa relationship"
  ([bindings then]
   `(taxed-let ~bindings {} ~then nil))
  ([bindings then else]
   `(taxed-let ~bindings {} ~then ~else))
  ([bindings opts then else]
   (assert (vector? bindings) "a vector for its binding")
   (assert (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (assert (map? opts) "opts should be a map")
   (let [form (bindings 0)
         t (bindings 1)
         {:keys [hierarchy parent]} (coerce-opts opts)]
     `(let [~form ~t]
        (if (taxa/taxed? ~hierarchy ~form ~parent)
          ~then
          ~else)))))

(defn parse-args
  [[head & tail :as forms]]
  (if (map? head)
    [(coerce-opts head) tail]
    [(coerce-opts nil) forms]))

(defmacro while->
  "Thread start through the forms while condition."
  [start & args]
  (let [[opts forms] (parse-args args)
        fs (mapv (fn [form]
                   (if (seq? form)
                     `{:f (var ~(first form))
                       :args [~@(next form)]}
                     `{:f (var ~form)
                       :args nil}))
                 forms)]
    `(com.kardans.taxa.flow.cljs/do-while->
      (as-> ~start v#
        (if (list? v#) (eval v#) v#)
        (if (taxa/taxon? v#) v# (taxa/taxon v#)))
      ~fs
      ~opts)))
