(ns com.kardans.taxa.flow.clj
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

(defn taxonize
  "Ensure start value is a taxon and not value or function."
  [start]
  (as-> start v
    (if (list? v) (eval v) v)
    (if (taxa/taxon? v) v (taxa/taxon v))))

(defn fn-resolver
  "Depending on provided form, resolve the function"
  [form]
  (if (seq? form)
    (cons (-> form first resolve) (next form))
    (resolve form)))

(defn do-while->
  [val forms {:keys [hierarchy parent]}]
  (reduce (fn [t f]
            (if (taxa/taxed? hierarchy t parent)
              (if (seq? f)
                (apply (-> f first) (cons t (next f)))
                (f t))
              (reduced t)))
          val
          forms))

(defmacro while->
  "Thread start through the forms while condition.

  (while-> {:k :v} fn1 fn2 ...) will thread while fns return a taxon of ::taxa:ok
  "
  ;; {:arglists '([start] [start & args])}
  [start & args]
  (let [[opts forms] (parse-args args)
        val (taxonize start)
        resolved-forms (map fn-resolver forms)]
    (if (seq forms)
      `(com.kardans.taxa.flow.clj/do-while-> ~val '~resolved-forms ~opts)
      val)))
