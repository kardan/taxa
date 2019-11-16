(ns ^{:author "Daniel Sunnerek"
      :doc "Taxa, an experiment in hierarchical domain logic
"}
    com.kardans.taxa
  (:require [clojure.set :as set]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tag
;;;

(defmulti tag "Tag from either tag, taxon or variant."
  {:arglists '([from])}
  (fn [from]
    (cond
      (map? from) :taxon
      (keyword? from) :tag
      (vector? from) :variant)))

(defmethod tag :taxon
  [{::keys [tag]}] tag)

(defmethod tag :tag
  [tag] tag)

(defmethod tag :variant
  [[tag _]] tag)

(defmethod tag :default
  [_] nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Thing
;;;

(defmulti thing
  "Returns thing from taxon or variant."
  {:arglists '([from])}
  (fn [from]
    (cond
      (map? from) :taxon
      (vector? from) :variant)))

(defmethod thing :taxon
  [{::keys [thing]}] thing)

(defmethod thing :variant
  [[_ thing]] thing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Taxon
;;;

(defn taxon
  "Creates a taxon from a thing, tag defaults to :com.kardans.taxa/ok."
  ([thing] (taxon thing ::ok))
  ([thing tag] {::tag tag
                ::thing thing}))

(defn taxon?
  "Returns true if t is a taxon."
  [t]
  (and (map? t)
       (every? #(contains? t %) '(::tag ::thing))))

(defn taxon->variant
  "Returns an old school vector variant [tag thing]."
  [{::keys [tag thing]}]
  [tag thing])

(defn taxonable?
  "True for taxon or keyword."
  [t]
  (or (taxon? t) (keyword t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hierarchy
;;;

(def taxa
  "Default hierarchy."
  (atom (make-hierarchy)))

(def effecting-fns
  "Fns with effects on hierarchy."
  (atom #?(:clj #{clojure.core/derive
                  clojure.core/underive}

           :cljs #{cljs.core/derive
                   cljs.core/underive})))

(def non-effecting-fns
  "Fns without effect on hierarchy."
  (atom #?(:clj #{clojure.core/ancestors
                  clojure.core/descendants
                  clojure.core/isa?
                  clojure.core/parents}

           :cljs #{cljs.core/ancestors
                   cljs.core/descendants
                   cljs.core/isa?
                   cljs.core/parents})))

(defn atom?
  "Returns true if x is an Atom."
  [x]
  (instance? #?(:clj clojure.lang.IAtom
                :cljs cljs.core/Atom)
             x))

(defn in-taxa
  "Run provided function in default or provided hierarchy."
  [f & args]
  (let [?h (first args)
        [h f-args] (if (atom? ?h)
                     [?h (next args)]
                     [taxa args])]
    (condp contains? f
      @effecting-fns (apply (partial swap! h f) f-args)
      @non-effecting-fns (let [args2 (cons @h f-args)]
                           (apply f args2))
      (throw (ex-info "Unsupported operation"
                      {:provided f
                       :supported (set/union @effecting-fns @non-effecting-fns)})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Derive defaults tags in default hierarchy.

(in-taxa derive ::ok ::root)
(in-taxa derive ::err ::root)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates

(defn taxed?
  "Returns true if member has an isa relationship in hierarchy default parent
  :com.kardans.taxa/root."
  ([member]
   (taxed? taxa member ::root))
  ([member parent]
   (taxed? taxa member parent))
  ([hierarchy member parent]
   (in-taxa isa? hierarchy (tag member) parent)))

(defn ok?
  "Returns true if member has an isa relationship with com.kardans.taxa/ok in
  default hierarchy."
  [member]
  (taxed? member ::ok))

(defn err?
  "Returns true if member has an isa relationship with com.kardans.taxa/err in
  default hierarchy."
  [member]
  (taxed? member ::err))
