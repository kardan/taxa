(ns ^{:author "Daniel Sunnerek"
      :doc "Taxa, an experiment in hierarchical domain logic

## Taxon
Just a spin on variants - think of it as a data wrapping that is the foundation
to utility fns. In short a taxon is a map of keys :com.kardans.taxa/tag and
:com.kardans.taxa/thing.

``` clojure
#:com.kardans.taxa{:tag :com.kardans.taxa/root,
                   :thing {:k :v}}
```

## What hierarchy

Taxa is a system of taxons put into a hierarchy based on taxons tag. Essentially
a taxon is a tag and a thing and the tag is put into the hierarchy. Taxa
provides a starting ground of tree tags (root, err & ok). You are encuraged to
build from these.

  ::root
   /  \\
::err ::ok

## Utility fn
Beside the taxon and the hierarchy, Taxa also provides the following utility fns
for use:

``` clojure
(in-taxa)  ;; Operate on hierarchy
(rel?)     ;; Boolean check for relationship
(when-rel) ;; Like if-let but testing with rel?
(rel->)    ;; Threading, but will short circut if not rel?
```

"}
    com.kardans.taxa)

(set! *warn-on-reflection* true)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tag
;;;

(defmulti tag
  "Returns tag from either tag, taxon or variant."
  {:arglists '([from])}
  type)

(defmethod tag clojure.lang.PersistentArrayMap
  [{::keys [tag]}] tag)

(defmethod tag clojure.lang.Keyword
  [tag] tag)

(defmethod tag clojure.lang.PersistentVector
  [[tag _]] tag)

(defmethod tag :default
  [_] nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thing
;;;

(defmulti thing
  "Returns thing from taxon or variant."
  {:arglists '([from])}
  type)

(defmethod thing clojure.lang.PersistentArrayMap
  [{::keys [thing]}] thing)

(defmethod thing clojure.lang.PersistentVector
  [[_ thing]] thing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Taxon
;;;

(defn taxon
  "Creates a taxon from a thing, tag defaults to :com.kardans.taxa/ok"
  ([thing] (taxon thing ::ok))
  ([thing tag] {::tag tag
                ::thing thing}))

(defn taxon? [t]
  (and (map? t)
       (every? #(contains? t %) '(::tag ::thing))))

(defn taxon->variant
  "Returns an old school vector variant [tag thing]."
  [{::keys [tag thing]}] [tag thing])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hierarchy
;;;

(def taxa
  (atom (make-hierarchy)))

(def effecting-fns
  (atom #{clojure.core/derive
          clojure.core/underive}))

(def non-effecting-fns
  (atom #{clojure.core/ancestors
          clojure.core/descendants
          clojure.core/isa?
          clojure.core/parents}))

(defn in-taxa
  "If hierarchy is provided it has to be wrapped in an atom."
  [f & [hierarchy? & tag-pair :as non-hierarchy-args]]
  (let [[hierarchy & args] (if (= (type hierarchy?) clojure.lang.Atom)
                             (conj (map tag tag-pair) hierarchy?)
                             (conj (map tag non-hierarchy-args) taxa))]
    (condp contains? f
      @effecting-fns (apply (partial swap! hierarchy f) args)
      @non-effecting-fns (->>
                          (conj args @hierarchy)
                          (apply f))
      (throw (Exception. "Provided fn not supported.")))))

;; Derive defaults
(in-taxa derive ::ok ::root)
(in-taxa derive ::err ::root)


(defn taxed?
  "Is member in hierarchy? Member can be tag, taxon or variant."
  ([member]
   (in-taxa isa? member ::root))
  ([member hierarchy parent]
   (in-taxa isa? hierarchy member parent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rel?
;;;

(defn rel?
  "Returns true if t is related (same or descended) to relative, default to
  :com.kardans.taxa/ok."
  ([t]
   (rel? t ::ok))
  ([t relative]
   (isa? @taxa (tag t) (tag relative))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; when-rel
;;;

(defmacro when-rel
  "bindings => binding-form taxon

  If taxon is kin of tag, evaluates then with binding-form bound to the
  value of taxon, if not returns taxon. Tag defaults to :com.kardans.taxa/ok"
  ([bindings then]
   `(when-rel ~bindings ~then ::ok))
  ([bindings then tag & oldform]
   (assert (vector? bindings) "a vector for its binding")
   (assert (taxed? tag) "Tag needs to be in hierarchy")
   (assert (nil? oldform) "1 or 2 forms after binding vector")
   (assert (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [t# ~tst]
        (if (rel? t# ~tag)
          (let [~form t#]
            ~then)
          t#)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rel->
;;;

(defmacro rel->
  "Thread start through the forms while condition, default to ::ok."
  [start & [head & tail :as forms]]
  (let [[condition forms] (if (taxed? head)
                            [head tail]
                            [:com.kardans.taxa/ok forms])]
    `(let [m# (meta ~start)
           v# (reduce (fn [t# f#]
                        (let [thing# (if (taxon? t#) (thing t#) t#)
                              txn# (f# (with-meta thing# (merge m# (meta thing#))))]
                          (if (rel? txn# ~condition)
                            (thing txn#)
                            (reduced (assoc txn#
                                            :com.kardans.taxa/err-ctx
                                            #:com.kardans.taxa{:err-fn f#
                                                               :err-thing thing#})))))
                      ~start
                      ~(vec forms))]
       (if (taxon? v#)
         v#
         (taxon v# ~condition)))))
