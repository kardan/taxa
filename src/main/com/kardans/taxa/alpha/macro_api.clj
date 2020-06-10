(ns com.kardans.taxa.alpha.macro-api
  (:require [com.kardans.taxa :as taxa]
            [clojure.pprint :refer [pprint]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; when-taxed?
;;;

(defmacro when-taxed?
  ([bindings then]
   `(when-taxed? ~bindings ~then :com.kardans.taxa/ok))
  ([bindings then tag]
   `(when-taxed? ~bindings ~then ~tag com.kardans.taxa/taxa))
  ([bindings then tag hierarchy]
   (assert (vector? bindings) "a vector for its binding")
   (assert (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0)
         tst (bindings 1)]
     `(let [temp# ~tst]
        (if (taxa/taxed? temp# {:parent ~tag
                                :hierarchy ~hierarchy})
          (let [~form temp#]
            ~then)
          temp#)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; when-rel
;;;

#_(defmacro when-rel
    "bindings => binding-form taxon

  If taxon is kin of tag, evaluates then with binding-form bound to the
  value of taxon, if not returns taxon. Tag defaults to :com.kardans.taxa/ok"
    ([bindings then]
     `(when-rel ~bindings ~then ::taxa/ok))
    ([bindings then tag & oldform]
     (assert (vector? bindings) "a vector for its binding")
     (assert (taxa/taxed? tag) "Tag needs to be in hierarchy")
     (assert (nil? oldform) "1 or 2 forms after binding vector")
     (assert (= 2 (count bindings)) "exactly 2 forms in binding vector")
     (let [form (bindings 0) tst (bindings 1)]
       `(let [t# ~tst]
          (if (taxa/taxed? t# {:parent ~tag})
            (let [~form t#]
              ~then)
            t#)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rel->
;;;

(defmacro rel->
  "Thread start through the forms while condition, default to ::ok."
  [start & [head & tail :as forms]]
  #_(prn "@rel->")
  #_(prn forms)
  (let [[condition forms] (if (taxa/taxed? head)
                            [head tail]
                            [::taxa/ok forms])]
    `(let [m# (meta ~start)
           v# (reduce (fn [t# f#]
                        (let [thing# (if (taxa/taxon? t#) (taxa/thing t#) t#)
                              ;; txn# (f# (with-meta thing# (merge m# (meta thing#))))
                              txn# (if (seq? f#)
                                     #_(conj (next f#) (with-meta thing# (merge m# (meta thing#))) (first f#))
                                     (f# (with-meta thing# (merge m# (meta thing#))) "F22")
                                     (f# (with-meta thing# (merge m# (meta thing#)))))]
                          (if (taxa/taxed? txn# {:parent ~condition})
                            (taxa/thing txn#)
                            (reduced (assoc txn#
                                            :com.kardans.taxa/err-ctx
                                            #:com.kardans.taxa{:err-fn f#
                                                               :err-thing thing#})))))
                      ~start
                      ~(vec forms))]
       (if (taxa/taxon? v#)
         v#
         (taxa/taxon v# ~condition)))))

(comment

  (prn "---")

  (pprint (rel-> {:k :v}
                 (fn [m]
                   (assoc m :f1 :done))))

  (rel-> {:k :v}
         (fn [m]
           (taxa/taxon {:message "Fail"} ::taxa/err))
         (fn [m]
           (assoc m :f1 :done)))

  )


#_(defmacro rel->
    "Thread start through the forms while condition, default to ::ok."
    [start & [head & tail :as forms]]
    #_(prn "@rel->")
    #_(prn forms)
    (let [[condition forms] (if (taxa/taxed? head)
                              [head tail]
                              [::taxa/ok forms])]
      `(let [m# (meta ~start)
             v# (reduce (fn [t# f#]
                          (let [thing# (if (taxa/taxon? t#) (taxa/thing t#) t#)
                                ;; txn# (f# (with-meta thing# (merge m# (meta thing#))))
                                txn# (if (seq? f#)
                                       #_(conj (next f#) (with-meta thing# (merge m# (meta thing#))) (first f#))
                                       (f# (with-meta thing# (merge m# (meta thing#))) "F22")
                                       (f# (with-meta thing# (merge m# (meta thing#)))))]
                            (if (taxa/taxed? txn# {:parent ~condition})
                              (taxa/thing txn#)
                              (reduced (assoc txn#
                                              :com.kardans.taxa/err-ctx
                                              #:com.kardans.taxa{:err-fn f#
                                                                 :err-thing thing#})))))
                        ~start
                        ~(vec forms))]
         (if (taxa/taxon? v#)
           v#
           (taxa/taxon v# ~condition)))))
