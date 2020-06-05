(ns ^{:author "Daniel Sunnerek"
      :doc "Taxa, an experiment in hierarchical domain logic
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

(defn taxon?
  "Returns true if t is a taxon"
  [t]
  (and (map? t)
       (every? #(contains? t %) '(::tag ::thing))))

(defn taxon->variant
  "Returns an old school vector variant [tag thing]."
  [{::keys [tag thing]}] [tag thing])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hierarchy
;;;

(def taxa
  "Default hierarchy"
  (atom (make-hierarchy)))

(def effecting-fns
  "Fns effecting a hierarchy"
  (atom #{clojure.core/derive
          clojure.core/underive}))

(def non-effecting-fns
  "Fns without effect on hierarchy"
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
      (throw (UnsupportedOperationException. "Provided fn not supported.")))))

;; Derive defaults
(in-taxa derive ::ok ::root)
(in-taxa derive ::err ::root)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates
;;;

(defn taxed?
  "Returns true if member has an isa relationship in hierarchy"
  ([member]
   (taxed? member {}))
  ([member {:keys [hierarchy parent] :or {hierarchy taxa parent ::root} :as opts}]
   (in-taxa isa? hierarchy member parent)))

(defn ok?
  "Returns true if member has an isa relationship with com.kardans.taxa/ok in
  default hierarchy"
  [member]
  (taxed? member {:parent ::ok}))

(defn err?
  "Returns true if member has an isa relationship with com.kardans.taxa/err in
  default hierarchy"
  [member]
  (taxed? member {:parent ::err}))
